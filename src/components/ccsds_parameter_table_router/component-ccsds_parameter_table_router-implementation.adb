--------------------------------------------------------------------------------
-- Ccsds_Parameter_Table_Router Component Implementation Body
--------------------------------------------------------------------------------

with Parameter_Enums;
with Parameter_Types; use Parameter_Types;
with Ccsds_Parameter_Table_Router_Enums;
with Parameter_Table_Operation_Failure_Info;
package body Component.Ccsds_Parameter_Table_Router.Implementation is

   use Parameter_Enums.Parameter_Table_Update_Status;
   use Ccsds_Parameter_Table_Router_Enums.Table_Status;

   -- Comparison operators for binary tree:
   function Less_Than (Left, Right : Router_Table_Entry) return Boolean is
   begin
      return Left.Table_Id < Right.Table_Id;
   end Less_Than;

   function Greater_Than (Left, Right : Router_Table_Entry) return Boolean is
   begin
      return Left.Table_Id > Right.Table_Id;
   end Greater_Than;

   ---------------------------------------
   -- Helper subprograms
   ---------------------------------------

   -- Search the routing table for a Table_Id. On success, returns True
   -- with the entry in Found. On failure, emits Unrecognized_Table_Id
   -- event and returns False.
   function Find_Table_Entry (
      Self : in out Instance;
      Table_Id : in Parameter_Types.Parameter_Table_Id;
      Found : out Router_Table_Entry
   ) return Boolean is
      -- Construct a search key with only Table_Id populated. Destinations is
      -- null here which would fail the Init assertion, but the binary tree
      -- comparison only uses Table_Id so this is safe for searching:
      Search_Key : constant Router_Table_Entry := (Table_Id => Table_Id, Destinations => null);
      Found_Index : Positive;
   begin
      if Self.Table.Search (Search_Key, Found, Found_Index) then
         return True;
      else
         Self.Event_T_Send_If_Connected (Self.Events.Unrecognized_Table_Id (
            Self.Sys_Time_T_Get, (Id => Table_Id)
         ));
         return False;
      end if;
   end Find_Table_Entry;

   -- Check if a destination list has a Load_From entry.
   -- If found, returns True with the connector index in Load_From_Idx.
   function Find_Load_From_Index (
      Destinations : in Destination_Table_Access;
      Load_From_Idx : out Connector_Types.Connector_Index_Type
   ) return Boolean is
   begin
      for Dest of Destinations.all loop
         if Dest.Load_From then
            Load_From_Idx := Dest.Connector_Index;
            return True;
         end if;
      end loop;
      Load_From_Idx := Connector_Types.Connector_Index_Type'First;
      return False;
   end Find_Load_From_Index;

   -- Wait for a response from a downstream component after a send.
   -- Handles timeout and failure event emission internally.
   -- Callers should NOT inspect Self.Response.Get_Var to distinguish
   -- timeout from failure — this function handles that.
   -- Returns True on success, False on timeout or failure.
   function Wait_For_Response (
      Self : in out Instance;
      Id : in Parameter_Types.Parameter_Table_Id;
      Index : in Connector_Types.Connector_Index_Type;
      Is_Load : in Boolean := False
   ) return Boolean is
      Wait_Timed_Out : Boolean;
   begin
      -- OK wait for the response.
      Self.Sync_Object.Wait (Wait_Timed_Out);

      -- Check the wait return value:
      if Wait_Timed_Out then
         Self.Event_T_Send_If_Connected (Self.Events.Table_Update_Timeout (
            Self.Sys_Time_T_Get, (Table_Id => Id, Connector_Index => Index)
         ));
         return False;
      end if;

      -- Response was set by Parameters_Memory_Region_Release_T_Recv_Sync.
      -- Only read it here, after confirming we are not timed out:
      declare
         Release : constant Parameters_Memory_Region_Release.T := Self.Response.Get_Var;
      begin
         if Release.Status /= Success then
            declare
               Failure_Info : constant Parameter_Table_Operation_Failure_Info.T := (
                  Table_Id => Id,
                  Connector_Index => Index,
                  Release => Release
               );
            begin
               if Is_Load then
                  Self.Event_T_Send_If_Connected (Self.Events.Table_Load_Failure (Self.Sys_Time_T_Get, Failure_Info));
               else
                  Self.Event_T_Send_If_Connected (Self.Events.Table_Update_Failure (Self.Sys_Time_T_Get, Failure_Info));
               end if;
            end;
            return False;
         end if;
      end;

      return True;
   end Wait_For_Response;

   -- Reset sync object, send region to a destination, and wait.
   -- Returns True on success, False on timeout or failure.
   function Send_And_Wait (
      Self : in out Instance;
      Index : in Connector_Types.Connector_Index_Type;
      Region : in Parameters_Memory_Region.T;
      Id : in Parameter_Types.Parameter_Table_Id;
      Is_Load : in Boolean := False
   ) return Boolean is
   begin
      -- First, clear the state of the synchronization
      -- object. This prevents us from just "falling through" the
      -- wait call below if some errant response was sent through
      -- to us while we were not listening.
      -- This also resets the timeout counter, so we start
      -- fresh.
      Self.Sync_Object.Reset;

      -- Send the request to the component.
      Self.Parameters_Memory_Region_T_Send_If_Connected (Index, Region);

      -- OK now we wait for and check the response.
      return Self.Wait_For_Response (Id, Index, Is_Load);
   end Send_And_Wait;

   -- Send Set to all destinations for a table entry.
   -- Sends to non-Load_From destinations first (in order), then Load_From last.
   -- Returns True if all succeeded.
   function Send_Table_To_Destinations (Self : in out Instance; Table_Ent : in Router_Table_Entry; Region : in Parameters_Memory_Region.T) return Boolean is
      Load_From_Idx : Connector_Types.Connector_Index_Type;
      Has_Load_From : constant Boolean := Find_Load_From_Index (Table_Ent.Destinations, Load_From_Idx);
   begin
      -- First phase - send to non-Load_From destinations in order:
      for Dest of Table_Ent.Destinations.all loop
         if not Dest.Load_From then
            if not Self.Send_And_Wait (Dest.Connector_Index, Region, Table_Ent.Table_Id) then
               return False;
            end if;
         end if;
      end loop;

      -- Second phase - send to Load_From destination last so we don't persist an
      -- invalid table if validation fails at another destination:
      if Has_Load_From then
         if not Self.Send_And_Wait (Load_From_Idx, Region, Table_Ent.Table_Id) then
            return False;
         end if;
      end if;

      return True;
   end Send_Table_To_Destinations;

   -- Core load logic - Get table data from the Load_From destination and
   -- forward to all other destinations. Caller has already resolved the
   -- table entry and Load_From index.
   function Do_Table_Load (
      Self : in out Instance;
      Table_Ent : in Router_Table_Entry;
      Load_From_Idx : in Connector_Types.Connector_Index_Type
   ) return Boolean is
   begin
      Self.Event_T_Send_If_Connected (Self.Events.Loading_Table (
         Self.Sys_Time_T_Get, (Id => Table_Ent.Table_Id)
      ));

      -- Send Get to Load_From destination to retrieve the table.
      -- We provide the full staging buffer capacity for the store to write into.
      -- Warning: if a table is being received via CCSDS at the same time, the
      -- staging buffer will be overwritten and that upload will fail.
      declare
         Get_Region : constant Parameters_Memory_Region.T := (
            Region => Self.Staging_Buffer.Get_Full_Buffer_Region,
            Operation => Parameter_Enums.Parameter_Table_Operation_Type.Get
         );
      begin
         if not Self.Send_And_Wait (Load_From_Idx, Get_Region, Table_Ent.Table_Id, Is_Load => True) then
            return False;
         end if;
      end;

      -- The Load_From destination populated the region. The response contains
      -- the actual data length. Forward this to non-Load_From destinations:
      declare
         Release : constant Parameters_Memory_Region_Release.T := Self.Response.Get_Var;
         Set_Region : constant Parameters_Memory_Region.T := (
            Region => Release.Region,
            Operation => Parameter_Enums.Parameter_Table_Operation_Type.Set
         );
      begin
         for Dest of Table_Ent.Destinations.all loop
            if not Dest.Load_From then
               if not Self.Send_And_Wait (Dest.Connector_Index, Set_Region, Table_Ent.Table_Id) then
                  return False;
               end if;
            end if;
         end loop;
      end;

      Self.Event_T_Send_If_Connected (Self.Events.Table_Loaded (
         Self.Sys_Time_T_Get, (Id => Table_Ent.Table_Id)
      ));
      return True;
   end Do_Table_Load;

   -- Load a single table by ID from its Load_From source.
   -- Fails with events if the table ID is unrecognized or has no Load_From.
   function Load_Table (Self : in out Instance; Table_Id : in Parameter_Types.Parameter_Table_Id) return Boolean is
      Found : Router_Table_Entry;
      Load_From_Idx : Connector_Types.Connector_Index_Type;
   begin
      if not Self.Find_Table_Entry (Table_Id, Found) then
         return False;
      end if;

      if not Find_Load_From_Index (Found.Destinations, Load_From_Idx) then
         Self.Event_T_Send_If_Connected (Self.Events.No_Load_Source (
            Self.Sys_Time_T_Get, (Id => Table_Id)
         ));
         return False;
      end if;

      return Self.Do_Table_Load (Found, Load_From_Idx);
   end Load_Table;

   -- Execute Load_All logic, shared between command and Set_Up.
   -- Returns True if all table loads succeeded.
   function Do_Load_All_Parameter_Tables (Self : in out Instance) return Boolean is
      All_Succeeded : Boolean := True;
      Load_From_Idx : Connector_Types.Connector_Index_Type;
   begin
      Self.Event_T_Send_If_Connected (Self.Events.Loading_All_Parameter_Tables (Self.Sys_Time_T_Get));

      for Idx in Self.Table.Get_First_Index .. Self.Table.Get_Last_Index loop
         declare
            Tbl_Entry : constant Router_Table_Entry := Self.Table.Get (Idx);
         begin
            -- Skip entries without a Load_From destination:
            if Find_Load_From_Index (Tbl_Entry.Destinations, Load_From_Idx) then
               if not Self.Do_Table_Load (Tbl_Entry, Load_From_Idx) then
                  All_Succeeded := False;
               end if;
            end if;
         end;
      end loop;

      Self.Event_T_Send_If_Connected (Self.Events.All_Parameter_Tables_Loaded (Self.Sys_Time_T_Get));
      return All_Succeeded;
   end Do_Load_All_Parameter_Tables;

   --------------------------------------------------
   -- Subprogram for implementation init method:
   --------------------------------------------------
   -- Initialization parameters for the Parameter Table Router.
   --
   -- Init Parameters:
   -- Table : Ccsds_Parameter_Table_Router_Types.Router_Table - The routing table mapping
   -- parameter table IDs to destination connector indexes. Typically produced by the
   -- generator.
   -- Buffer_Size : Positive - The size in bytes of the internal staging buffer for
   -- reassembling segmented CCSDS packets.
   -- Ticks_Until_Timeout : Natural - The number of timeout ticks to wait for a
   -- response from a downstream component before declaring a timeout.
   -- Load_All_Parameter_Tables_On_Set_Up : Boolean - If True, all parameter tables
   -- that have a load_from source will be loaded from persistent storage during
   -- Set_Up.
   --
   overriding procedure Init (Self : in out Instance; Table : in Ccsds_Parameter_Table_Router_Types.Router_Table; Buffer_Size : in Positive; Ticks_Until_Timeout : in Natural; Load_All_Parameter_Tables_On_Set_Up : in Boolean := False) is
   begin
      -- Set timeout limit:
      Self.Sync_Object.Set_Timeout_Limit (Ticks_Until_Timeout);

      -- Store configuration:
      Self.Load_All_On_Set_Up := Load_All_Parameter_Tables_On_Set_Up;

      -- Create staging buffer:
      Self.Staging_Buffer.Create (Buffer_Size);

      -- Initialize binary tree and populate from table:
      Self.Table.Init (Table'Length);
      for Table_Ent of Table loop
         -- Destinations must not be null:
         pragma Assert (Table_Ent.Destinations /= null);

         -- Validate: at most one Load_From per entry:
         declare
            Load_From_Count : Natural := 0;
         begin
            for Dest of Table_Ent.Destinations.all loop
               if Dest.Load_From then
                  Load_From_Count := @ + 1;
               end if;
               -- Make sure destination connector index is in range:
               pragma Assert (
                  Dest.Connector_Index >= Self.Connector_Parameters_Memory_Region_T_Send'First
                  and then Dest.Connector_Index <= Self.Connector_Parameters_Memory_Region_T_Send'Last
               );
            end loop;
            -- At most one Load_From per entry:
            pragma Assert (Load_From_Count <= 1);
         end;

         -- Make sure the table ID is not already in the tree:
         declare
            Ignore_1 : Router_Table_Entry;
            Ignore_2 : Natural;
            Ret : Boolean;
         begin
            Ret := Self.Table.Search (Table_Ent, Ignore_1, Ignore_2);
            -- Duplicate table IDs are not allowed:
            pragma Assert (not Ret);
            -- Add entry to the table:
            Ret := Self.Table.Add (Table_Ent);
            -- Tree should have enough capacity since we initialized with Table'Length:
            pragma Assert (Ret);
         end;
      end loop;
   end Init;

   not overriding procedure Final (Self : in out Instance) is
   begin
      Self.Table.Destroy;
      Self.Staging_Buffer.Destroy;
   end Final;

   ---------------------------------------
   -- Set Up Procedure
   ---------------------------------------
   overriding procedure Set_Up (Self : in out Instance) is
      The_Time : constant Sys_Time.T := Self.Sys_Time_T_Get;
   begin
      -- Publish initial data product values:
      Self.Data_Product_T_Send_If_Connected (Self.Data_Products.Num_Packets_Received (The_Time, (Value => Self.Packet_Count)));
      Self.Data_Product_T_Send_If_Connected (Self.Data_Products.Num_Packets_Rejected (The_Time, (Value => Self.Reject_Count.Get_Count)));
      Self.Data_Product_T_Send_If_Connected (Self.Data_Products.Num_Tables_Updated (The_Time, (Value => Self.Valid_Table_Count)));
      Self.Data_Product_T_Send_If_Connected (Self.Data_Products.Num_Tables_Invalid (The_Time, (Value => Self.Invalid_Table_Count)));
      Self.Data_Product_T_Send_If_Connected (Self.Data_Products.Last_Table_Received (The_Time, (
         Table_Id => 0,
         Status => Table_Update_Success,
         Bytes_Received => 0,
         Packets_Received => 0,
         Timestamp => The_Time
      )));

      -- Load all parameter tables if configured. We ignore the result here
      -- because Set_Up cannot return failure. Individual load failures are
      -- reported via events, and downstream components (e.g. Parameters)
      -- report table load status in their own data products.
      if Self.Load_All_On_Set_Up then
         declare
            Ignore_Result : constant Boolean := Self.Do_Load_All_Parameter_Tables;
         begin
            null;
         end;
      end if;
   end Set_Up;

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- Receives segmented CCSDS packets containing parameter table data.
   overriding procedure Ccsds_Space_Packet_T_Recv_Async (Self : in out Instance; Arg : in Ccsds_Space_Packet.T) is
      use Parameter_Table_Buffer;

      The_Time : constant Sys_Time.T := Self.Sys_Time_T_Get;
      Table_Status_Val : Ccsds_Parameter_Table_Router_Enums.Table_Status.E := Table_Update_Success;

      -- Helper to increment the reject counter, publish the reject DP, and emit the given event.
      procedure Reject_Packet (Evnt : in Event.T) is
      begin
         Self.Reject_Count.Increment_Count;
         Self.Data_Product_T_Send_If_Connected (Self.Data_Products.Num_Packets_Rejected (The_Time, (Value => Self.Reject_Count.Get_Count)));
         Self.Event_T_Send_If_Connected (Evnt);
      end Reject_Packet;

      -- Helper to increment the invalid table counter and publish the DP.
      procedure Reject_Table is
      begin
         Self.Invalid_Table_Count := @ + 1;
         Self.Data_Product_T_Send_If_Connected (Self.Data_Products.Num_Tables_Invalid (The_Time, (Value => Self.Invalid_Table_Count)));
      end Reject_Table;

      -- Note: CCSDS Packet_Length field value is one less than the actual data
      -- length per the CCSDS standard, hence the seemingly missing "-1" in the
      -- slice below.
      Data : Basic_Types.Byte_Array renames Arg.Data (Arg.Data'First .. Arg.Data'First + Natural (Arg.Header.Packet_Length));
      -- Append to staging buffer:
      Status : constant Append_Status := Self.Staging_Buffer.Append (Data => Data, Sequence_Flag => Arg.Header.Sequence_Flag);
   begin
      -- Based on append status we need to increment different counters, throw different events, etc.
      case Status is
         when Packet_Ignored =>
            Reject_Packet (Self.Events.Packet_Ignored (The_Time, Arg.Header));
            Table_Status_Val := Packet_Ignored;

         when Too_Small_Table =>
            Reject_Packet (Self.Events.Too_Small_Table (The_Time, Arg.Header));
            Table_Status_Val := Too_Small;

         when New_Table =>
            -- Emit event with table ID. The ID will be validated against the
            -- routing table when the complete table arrives:
            Self.Event_T_Send_If_Connected (Self.Events.Receiving_New_Table (
               The_Time, (Id => Self.Staging_Buffer.Get_Table_Id)
            ));
            Table_Status_Val := Receiving_Table;

         when Buffering_Table =>
            Table_Status_Val := Receiving_Table;

         when Complete_Table =>
            declare
               Tid : constant Parameter_Table_Id.T := (Id => Self.Staging_Buffer.Get_Table_Id);
               Found : Router_Table_Entry;
            begin
               Self.Event_T_Send_If_Connected (Self.Events.Table_Received (The_Time, Tid));

               if not Self.Find_Table_Entry (Tid.Id, Found) then
                  Reject_Table; -- Error event already sent
                  Table_Status_Val := Unrecognized_Id;
               else
                  declare
                     Set_Region : constant Parameters_Memory_Region.T := (
                        Region => Self.Staging_Buffer.Get_Table_Region,
                        Operation => Parameter_Enums.Parameter_Table_Operation_Type.Set
                     );
                  begin
                     if Send_Table_To_Destinations (Self, Found, Set_Region) then
                        Self.Event_T_Send_If_Connected (Self.Events.Table_Updated (The_Time, Tid));
                        Table_Status_Val := Table_Update_Success;
                        -- Only count tables that were successfully distributed:
                        Self.Valid_Table_Count := @ + 1;
                        Self.Data_Product_T_Send_If_Connected (Self.Data_Products.Num_Tables_Updated (The_Time, (Value => Self.Valid_Table_Count)));
                     else
                        Reject_Table; -- Error event already sent
                        Table_Status_Val := Table_Update_Failed;
                     end if;
                  end;
               end if;
            end;

         when Buffer_Overflow =>
            Reject_Packet (Self.Events.Staging_Buffer_Overflow (The_Time, Arg.Header));
            Table_Status_Val := Buffer_Overflow;
      end case;

      -- Increment and publish packet counter:
      Self.Packet_Count := @ + 1;
      Self.Data_Product_T_Send_If_Connected (Self.Data_Products.Num_Packets_Received (The_Time, (Value => Self.Packet_Count)));

      -- Update last table received data product with current status:
      Self.Data_Product_T_Send_If_Connected (Self.Data_Products.Last_Table_Received (The_Time, (
         Table_Id => Self.Staging_Buffer.Get_Table_Id,
         Status => Table_Status_Val,
         Bytes_Received => Self.Staging_Buffer.Get_Table_Length,
         Packets_Received => Self.Staging_Buffer.Get_Packet_Count,
         Timestamp => The_Time
      )));
   end Ccsds_Space_Packet_T_Recv_Async;

   -- This procedure is called when a Ccsds_Space_Packet_T_Recv_Async message is dropped due to a full queue.
   overriding procedure Ccsds_Space_Packet_T_Recv_Async_Dropped (Self : in out Instance; Arg : in Ccsds_Space_Packet.T) is
   begin
      -- Note: This handler runs in the sender's task context, not this
      -- component's task. The reject counter is protected to handle this:
      Self.Reject_Count.Increment_Count;
      Self.Event_T_Send_If_Connected (Self.Events.Packet_Dropped (Self.Sys_Time_T_Get, Arg.Header));
   end Ccsds_Space_Packet_T_Recv_Async_Dropped;

   -- The command receive connector.
   overriding procedure Command_T_Recv_Async (Self : in out Instance; Arg : in Command.T) is
      -- Execute the command:
      Stat : constant Command_Response_Status.E := Self.Execute_Command (Arg);
   begin
      -- Send the return status:
      Self.Command_Response_T_Send_If_Connected ((Source_Id => Arg.Header.Source_Id, Registration_Id => Self.Command_Reg_Id, Command_Id => Arg.Header.Id, Status => Stat));
   end Command_T_Recv_Async;

   -- This procedure is called when a Command_T_Recv_Async message is dropped due to a full queue.
   overriding procedure Command_T_Recv_Async_Dropped (Self : in out Instance; Arg : in Command.T) is
   begin
      Self.Event_T_Send_If_Connected (Self.Events.Command_Dropped (Self.Sys_Time_T_Get, Arg.Header));
   end Command_T_Recv_Async_Dropped;

   -- Periodic tick used for timeout counting when waiting for downstream responses.
   overriding procedure Timeout_Tick_Recv_Sync (Self : in out Instance; Arg : in Tick.T) is
      Ignore : Tick.T renames Arg;
   begin
      -- Increment the timeout counter. This will only cause a timeout if the
      -- sync object is currently waiting and the counter exceeds the limit:
      Self.Sync_Object.Increment_Timeout_If_Waiting;
   end Timeout_Tick_Recv_Sync;

   -- Synchronous response from downstream components after a Set or Get operation.
   overriding procedure Parameters_Memory_Region_Release_T_Recv_Sync (Self : in out Instance; Arg : in Parameters_Memory_Region_Release.T) is
   begin
      -- Store the response from the downstream component. Error handling based
      -- on the contents of this response is done by this component's task
      -- (executing the command or table update):
      Self.Response.Set_Var (Arg);

      -- Signal to the component that a response has been received:
      Self.Sync_Object.Release;

      -- Note: There is a possible race condition where we store the response
      -- and then release the sync object, but before the waiting task reads
      -- the data, another response could arrive and overwrite it. This should
      -- never occur in practice since downstream components should not send
      -- unprovoked responses. The protected buffer and sync object prevent
      -- data corruption; only ordering could be affected if the assembly is
      -- designed incorrectly.
   end Parameters_Memory_Region_Release_T_Recv_Sync;

   -----------------------------------------------
   -- Command handler primitives:
   -----------------------------------------------
   -- Description:
   --    Commands for the Parameter Table Router component.
   -- Load a single parameter table from its load_from source and distribute to other
   -- destinations.
   overriding function Load_Parameter_Table (Self : in out Instance; Arg : in Parameter_Table_Id.T) return Command_Execution_Status.E is
      use Command_Execution_Status;
   begin
      if Self.Load_Table (Arg.Id) then
         return Success;
      else
         return Failure;
      end if;
   end Load_Parameter_Table;

   -- Load all parameter tables that have a load_from source configured and
   -- distribute to their destinations.
   overriding function Load_All_Parameter_Tables (Self : in out Instance) return Command_Execution_Status.E is
      use Command_Execution_Status;
   begin
      if Self.Do_Load_All_Parameter_Tables then
         return Success;
      else
         return Failure;
      end if;
   end Load_All_Parameter_Tables;

   -- Invalid command handler. This procedure is called when a command's arguments are found to be invalid:
   overriding procedure Invalid_Command (Self : in out Instance; Cmd : in Command.T; Errant_Field_Number : in Unsigned_32; Errant_Field : in Basic_Types.Poly_Type) is
   begin
      Self.Event_T_Send_If_Connected (Self.Events.Invalid_Command_Received (
         Self.Sys_Time_T_Get,
         (Id => Cmd.Header.Id, Errant_Field_Number => Errant_Field_Number, Errant_Field => Errant_Field)
      ));
   end Invalid_Command;

end Component.Ccsds_Parameter_Table_Router.Implementation;
