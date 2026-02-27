--------------------------------------------------------------------------------
-- Product_Packetizer Component Implementation Body
--------------------------------------------------------------------------------

with Product_Packetizer_Commands;
with Packet_Types;
with Data_Product_Enums;
with Data_Product_Types;
with Packed_Natural;
with Sys_Time.Arithmetic;

package body Component.Product_Packetizer.Implementation is

   --------------------------------------------------
   -- Subprogram for implementation init method:
   --------------------------------------------------
   -- This initialization function is used to initialize the roll-over value for the packetizer's internal counter. It is calculated as the largest 32-bit multiple of all the provided periods in the Packet_List. This ensures that no packets are skipped or sent too often when a rollover occurs. Note, that this only guarantees expected roll-over behavior if the period of the packets are not changed during runtime via command. If this happens, then the user accepts that a rollover may cause unwanted behavior.
   overriding procedure Init (Self : in out Instance; Commands_Dispatched_Per_Tick : in Positive := 3) is
      use Product_Packet_Types;
      Common_Multiple : Positive := 1;
      Duplicate_Period : Boolean;
      Current_Period : Natural;
      Scale_Factor : Natural;
   begin
      -- Set the commands dispatched:
      Self.Commands_Dispatched_Per_Tick := Commands_Dispatched_Per_Tick;

      -- Find a common multiple for the packet periods. We do this to make sure
      -- that periods are upheld even in the case of rollover of the counter value.
      for Idx in Self.Packet_List.all'Range loop
         -- Make sure all packet items are not null in Init, so we don't get a surprise later.
         pragma Assert (Self.Packet_List.all (Idx).Items /= null);

         -- Extract the period from this index:
         Current_Period := Self.Packet_List.all (Idx).Period;

         if Current_Period > 0 then
            -- See if there is a duplicate period somewhere after this period
            -- in the list. We only want to multiply the last value found of each
            -- unique period.
            Duplicate_Period := False;
            for Jdx in Idx + 1 .. Self.Packet_List.all'Last loop
               if Self.Packet_List.all (Jdx).Period = Current_Period then
                  Duplicate_Period := True;
               end if;
            end loop;

            -- If there is not a duplicate period, then multiply into our product:
            if not Duplicate_Period then
               Common_Multiple := @ * Current_Period;
            end if;
         end if;
      end loop;

      -- OK, we have a common multiple, now we want to find a value that is a multiple of this value
      -- and that is near Natural'Last - 1. This will make roll over as rare as possible, which is good if periods
      -- change over time due to commands.
      Scale_Factor := (Natural'Last - 1) / Common_Multiple; -- Expect truncation
      -- Set the roll over value:
      Self.Roll_Over_Value := Scale_Factor * Common_Multiple;
      -- ^ The rollover must be less than Natural'Last or a Constraint_Error will be raised when incrementing the
      -- Self.Count variable. We enforce this using the type of Self.Roll_Over_Value

      -- Make sure the packet count for each is set to 0:
      for Idx in Self.Packet_List.all'Range loop
         Self.Packet_List.all (Idx).Count := Packet_Types.Sequence_Count_Mod_Type'First;
         -- Initialize packet emission times to zero:
         Self.Packet_List.all (Idx).Last_Emission_Time := Sys_Time.Arithmetic.Sys_Time_Zero;
      end loop;
   end Init;

   ---------------------------------------
   -- Private helper functions:
   ---------------------------------------

   function Build_Packet (
      Self : in out Instance;
      Tick_Time : in Sys_Time.T;
      Packet_Desc : in Product_Packet_Types.Packet_Description_Type;
      Check_On_Change : in Boolean;
      Changed : out Boolean
   ) return Packet.T is
      use Data_Product_Types;
      use Data_Product_Enums.Fetch_Status;
      use Sys_Time.Arithmetic;

      -- Initialize the packet with the correct ID and sequence count.
      The_Packet : Packet.T := ((Time => Tick_Time, Id => Packet_Desc.Id, Sequence_Count => Packet_Desc.Count, Buffer_Length => 0), Buffer => [others => 0]);

      -- Other local variables:
      D_Prod_Ret : Data_Product_Return.T;
      Curr_Index : Natural := The_Packet.Buffer'First;
      Time_Set : Boolean := False;
      Do_Copy : Boolean;
   begin
      -- Initialize out parameter:
      Changed := False;

      -- Fill packet:
      for Item of Packet_Desc.Items.all loop
         -- If this item does not have an ID then it is simply padding.
         -- Otherwise request a dataproduct from the database:
         Do_Copy := True;
         if Item.Data_Product_Id /= 0 then

            -- If this is a special packet period item then instead of fetching the
            -- data product we construct it from scratch using our own internal data
            if Item.Packet_Period_Item then
               -- Create a fake data product with the appropriate data:
               D_Prod_Ret := (
                  The_Status => Success,
                  The_Data_Product => (
                     Header => (
                        Time => Self.Sys_Time_T_Get,
                        Id => Item.Data_Product_Id,
                        Buffer_Length => Packed_Natural.Size_In_Bytes
                     ),
                     Buffer => [others => 0]
                  )
               );
               declare
                  Packet_List_Index : constant Natural := Natural (Item.Data_Product_Id);
               begin
                  -- Don't trust the packet list index stored as a data product ID. Only use it if it is in range,
                  -- otherwise alert with an event.
                  if Packet_List_Index < Self.Packet_List.all'First or else Packet_List_Index > Self.Packet_List.all'Last then
                     -- Throw event:
                     Self.Event_T_Send_If_Connected (Self.Events.Packet_Period_Item_Bad_Id (Self.Sys_Time_T_Get, (
                        Packet_Id => Packet_Desc.Id,
                        Data_Product_Id => Item.Data_Product_Id)
                     ));
                     D_Prod_Ret.The_Status := Id_Out_Of_Range;
                  else
                     -- Set the value to the correct packet period:
                     D_Prod_Ret.The_Data_Product.Buffer (0 .. Packed_Natural.Size_In_Bytes - 1)
                        := Packed_Natural.Serialization.To_Byte_Array ((Value => Self.Packet_List.all (Natural (Item.Data_Product_Id)).Period));
                  end if;
               end;
            else
               -- Request data product from outside component:
               D_Prod_Ret := Self.Data_Product_Fetch_T_Request ((Id => Item.Data_Product_Id));
            end if;

            -- Check status and throw appropriate events:
            case D_Prod_Ret.The_Status is
               when Success =>
                  null; -- Nothing to do here.

               when Not_Available =>
                  -- Throw event if configured to do so:
                  if Item.Event_On_Missing then
                     -- Throw event:
                     Self.Event_T_Send_If_Connected (Self.Events.Data_Product_Missing_On_Fetch (Self.Sys_Time_T_Get, (
                        Packet_Id => Packet_Desc.Id,
                        Data_Product_Id => Item.Data_Product_Id)
                     ));
                  end if;
                  Do_Copy := False;

               when Id_Out_Of_Range => -- Ignore, this is an error, but expect database to report it.
                  Do_Copy := False;
            end case;

            -- Copy the timestamp of the data product if necessary, but increment
            -- the current index in the packet regardless of data product fetch
            -- success or failure.
            if Item.Include_Timestamp then
               if Do_Copy then
                  The_Packet.Buffer (Curr_Index .. (Curr_Index + Sys_Time.Serialization.Serialized_Length - 1))
                     := Sys_Time.Serialization.To_Byte_Array (D_Prod_Ret.The_Data_Product.Header.Time);
               end if;

               -- Increment the current index:
               Curr_Index := @ + Sys_Time.Serialization.Serialized_Length;
            end if;

            -- If the status was such that we need to copy the data to the packet then
            -- do that:
            if Do_Copy then
               -- Set packet timestamp if necessary:
               if Item.Use_Timestamp then
                  The_Packet.Header.Time := D_Prod_Ret.The_Data_Product.Header.Time;
                  Time_Set := True;
               end if;

               -- If this in an on-change packet, then see if this data product
               -- changed (only if we haven't already detected a change).
               if Check_On_Change and then not Changed and then
                  Item.Used_For_On_Change and then
                  D_Prod_Ret.The_Data_Product.Header.Time > Packet_Desc.Last_Emission_Time
               then
                  Changed := True;
               end if;

               -- Check the length of the data product to make sure it what we expect:
               if D_Prod_Ret.The_Data_Product.Header.Buffer_Length /= Item.Size then
                  -- Throw event:
                  Self.Event_T_Send_If_Connected (Self.Events.Data_Product_Length_Mismatch (Self.Sys_Time_T_Get, (Header => D_Prod_Ret.The_Data_Product.Header, Expected_Length => Item.Size)));
               else
                  -- Copy data product buffer into the packet:
                  The_Packet.Buffer (
                     Curr_Index ..
                     (Curr_Index + D_Prod_Ret.The_Data_Product.Header.Buffer_Length - 1))
                  := D_Prod_Ret.The_Data_Product.Buffer (
                     D_Prod_Ret.The_Data_Product.Buffer'First ..
                     (D_Prod_Ret.The_Data_Product.Buffer'First + D_Prod_Ret.The_Data_Product.Header.Buffer_Length - 1)
                  );
               end if;
            end if;
         end if;

         -- Increment the index by the size of the data product or padding:
         Curr_Index := @ + Item.Size;
      end loop;

      -- If the packet time has not been set, then set it:
      if not Time_Set then
         if not Packet_Desc.Use_Tick_Timestamp then
            The_Packet.Header.Time := Self.Sys_Time_T_Get;
         end if;
      end if;

      -- Set the packet length:
      The_Packet.Header.Buffer_Length := Curr_Index;

      -- Return the packet:
      return The_Packet;
   end Build_Packet;

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- This is the base tick for the component. It should be received at least as fast as the maximum desired product creation frequency.
   overriding procedure Tick_T_Recv_Sync (Self : in out Instance; Arg : in Tick.T) is
      use Packet_Types;
      use Product_Packet_Types;
      Ignore : Tick.T renames Arg;
      Messages_Dispatched : Natural;

      -- Helper to build a packet (without on-change evaluation) and immediately dispatch it.
      procedure Build_And_Send (
         Packet_Desc : in out Product_Packet_Types.Packet_Description_Type;
         Send_Only_On_Change : Boolean := False
      ) is
         Changed : Boolean;
         Built_Packet : constant Packet.T := Self.Build_Packet (
            Tick_Time => Arg.Time,
            Packet_Desc => Packet_Desc,
            Check_On_Change => Send_Only_On_Change,
            Changed => Changed
         );
      begin
         -- Send packet if we are not configured for Send_Only_On_Change or,
         -- if we are configured for Send_Only_On_Change, only if the packet
         -- actually changed.
         if not Send_Only_On_Change or else Changed then
            Self.Packet_T_Send_If_Connected (Built_Packet);
            Packet_Desc.Last_Emission_Time := Arg.Time;
            Packet_Desc.Count := @ + 1;
         end if;
      end Build_And_Send;
   begin
      -- Handle any commands in queue. Service up to N commands per tick:
      Messages_Dispatched := Self.Dispatch_N (Self.Commands_Dispatched_Per_Tick);
      pragma Assert (Messages_Dispatched <= Self.Commands_Dispatched_Per_Tick);

      -- Construct and send out packets:
      for Packet_Desc of Self.Packet_List.all loop
         -- See if send command was sent:
         if Packet_Desc.Send_Now then
            Build_And_Send (Packet_Desc);
            Packet_Desc.Send_Now := False;
         -- Check if packet is enabled or on change:
         elsif (Packet_Desc.Enabled = Product_Packet_Types.Enabled or else
               Packet_Desc.Enabled = Product_Packet_Types.On_Change) and then
               Packet_Desc.Period > 0
         then
            -- Is it time to send packet based on its period?
            if (Self.Count mod Packet_Desc.Period) = (Packet_Desc.Offset mod Packet_Desc.Period) then
               Build_And_Send (Packet_Desc, Send_Only_On_Change => (Packet_Desc.Enabled = Product_Packet_Types.On_Change));
            end if;
         end if;
      end loop;

      -- Increment internal counter:
      Self.Count := @ + 1;

      -- Check roll over:
      if Self.Count > Self.Roll_Over_Value then
         Self.Count := 1;
      end if;
   end Tick_T_Recv_Sync;

   -- This is the command receive connector, used for configuring the packetizer during runtime.
   overriding procedure Command_T_Recv_Async (Self : in out Instance; Arg : in Command.T) is
      -- Execute the command:
      Stat : constant Command_Response_Status.E := Self.Execute_Command (Arg);
   begin
      -- Send the return status:
      Self.Command_Response_T_Send_If_Connected ((Source_Id => Arg.Header.Source_Id, Registration_Id => Self.Command_Reg_Id, Command_Id => Arg.Header.Id, Status => Stat));
   end Command_T_Recv_Async;

   -----------------------------------------------
   -- Command handler primitive:
   -----------------------------------------------

   -- Description:
   --    These are the commands for the product packetizer component.

   -- Command to change the period of packet generation for a given packet id.
   overriding function Set_Packet_Period (Self : in out Instance; Arg : in Packet_Period.T) return Command_Execution_Status.E is
      use Command_Execution_Status;
      use Packet_Types;
      Id_Found : Boolean := False;
      Commands : Product_Packetizer_Commands.Instance;
      The_Period : Natural := 1;
   begin
      for Packet_Desc of Self.Packet_List.all loop
         if Packet_Desc.Id = Arg.Id then
            Packet_Desc.Period := Arg.Period;
            The_Period := Packet_Desc.Period;
            Id_Found := True;
            exit;
         end if;
      end loop;

      -- Send event:
      if Id_Found then
         Self.Event_T_Send_If_Connected (Self.Events.Packet_Period_Set (Self.Sys_Time_T_Get, (Id => Arg.Id, Period => The_Period)));
         return Success;
      else
         Commands.Set_Id_Base (Self.Command_Id_Base);
         Self.Event_T_Send_If_Connected (Self.Events.Invalid_Packet_Id_Commanded (Self.Sys_Time_T_Get, (Packet_Id => Arg.Id, Command_Id => Commands.Get_Set_Packet_Period_Id)));
         return Failure;
      end if;
   end Set_Packet_Period;

   -- Command to enable the emission of a packet from the packetizer.
   overriding function Enable_Packet (Self : in out Instance; Arg : in Packet_Id.T) return Command_Execution_Status.E is
      use Command_Execution_Status;
      use Packet_Types;
      use Product_Packet_Types;
      Id_Found : Boolean := False;
      Commands : Product_Packetizer_Commands.Instance;
      The_Period : Natural := 1;
   begin
      for Packet_Desc of Self.Packet_List.all loop
         if Packet_Desc.Id = Arg.Id then
            Packet_Desc.Enabled := Product_Packet_Types.Enabled;
            The_Period := Packet_Desc.Period;
            Id_Found := True;
            exit;
         end if;
      end loop;

      -- Send event:
      if Id_Found then
         Self.Event_T_Send_If_Connected (Self.Events.Packet_Enabled (Self.Sys_Time_T_Get, (Id => Arg.Id, Period => The_Period)));
         return Success;
      else
         Commands.Set_Id_Base (Self.Command_Id_Base);
         Self.Event_T_Send_If_Connected (Self.Events.Invalid_Packet_Id_Commanded (Self.Sys_Time_T_Get, (Packet_Id => Arg.Id, Command_Id => Commands.Get_Enable_Packet_Id)));
         return Failure;
      end if;
   end Enable_Packet;

   -- Command to disable the emission of a packet from the packetizer.
   overriding function Disable_Packet (Self : in out Instance; Arg : in Packet_Id.T) return Command_Execution_Status.E is
      use Command_Execution_Status;
      use Packet_Types;
      use Product_Packet_Types;
      Id_Found : Boolean := False;
      Commands : Product_Packetizer_Commands.Instance;
      The_Period : Natural := 1;
   begin
      for Packet_Desc of Self.Packet_List.all loop
         if Packet_Desc.Id = Arg.Id then
            Packet_Desc.Enabled := Product_Packet_Types.Disabled;
            The_Period := Packet_Desc.Period;
            Id_Found := True;
            exit;
         end if;
      end loop;

      -- Send event:
      if Id_Found then
         Self.Event_T_Send_If_Connected (Self.Events.Packet_Disabled (Self.Sys_Time_T_Get, (Id => Arg.Id, Period => The_Period)));
         return Success;
      else
         Commands.Set_Id_Base (Self.Command_Id_Base);
         Self.Event_T_Send_If_Connected (Self.Events.Invalid_Packet_Id_Commanded (Self.Sys_Time_T_Get, (Packet_Id => Arg.Id, Command_Id => Commands.Get_Disable_Packet_Id)));
         return Failure;
      end if;
   end Disable_Packet;

   -- Command to build specific packet and send it out on the next available tick. The packet is built and sent regardless of the packet being enabled or disabled.
   overriding function Send_Packet (Self : in out Instance; Arg : in Packet_Id.T) return Command_Execution_Status.E is
      use Command_Execution_Status;
      use Packet_Types;
      Id_Found : Boolean := False;
      Commands : Product_Packetizer_Commands.Instance;
   begin
      for Packet_Desc of Self.Packet_List.all loop
         if Packet_Desc.Id = Arg.Id then
            Packet_Desc.Send_Now := True;
            Id_Found := True;
            exit;
         end if;
      end loop;

      -- Send event:
      if not Id_Found then
         Commands.Set_Id_Base (Self.Command_Id_Base);
         Self.Event_T_Send_If_Connected (Self.Events.Invalid_Packet_Id_Commanded (Self.Sys_Time_T_Get, (Packet_Id => Arg.Id, Command_Id => Commands.Get_Send_Packet_Id)));
         return Failure;
      end if;
      return Success;
   end Send_Packet;

   -- Command to enable the emission of a packet from the packetizer only when data products have changed since the last emission.
   overriding function Enable_Packet_On_Change (Self : in out Instance; Arg : in Packet_Id.T) return Command_Execution_Status.E is
      use Command_Execution_Status;
      use Packet_Types;
      use Product_Packet_Types;
      Id_Found : Boolean := False;
      Commands : Product_Packetizer_Commands.Instance;
      The_Period : Natural := 1;
   begin
      for Packet_Desc of Self.Packet_List.all loop
         if Packet_Desc.Id = Arg.Id then
            Packet_Desc.Enabled := Product_Packet_Types.On_Change;
            The_Period := Packet_Desc.Period;
            Id_Found := True;
            exit;
         end if;
      end loop;

      -- Send event:
      if Id_Found then
         Self.Event_T_Send_If_Connected (Self.Events.Packet_Enabled_On_Change (Self.Sys_Time_T_Get, (Id => Arg.Id, Period => The_Period)));
         return Success;
      else
         Commands.Set_Id_Base (Self.Command_Id_Base);
         Self.Event_T_Send_If_Connected (Self.Events.Invalid_Packet_Id_Commanded (Self.Sys_Time_T_Get, (Packet_Id => Arg.Id, Command_Id => Commands.Get_Enable_Packet_On_Change_Id)));
         return Failure;
      end if;
   end Enable_Packet_On_Change;

   -- Invalid command handler. This procedure is called when a command's arguments are found to be invalid:
   overriding procedure Invalid_Command (Self : in out Instance; Cmd : in Command.T; Errant_Field_Number : in Unsigned_32; Errant_Field : in Basic_Types.Poly_Type) is
   begin
      -- Throw event:
      Self.Event_T_Send_If_Connected (Self.Events.Invalid_Command_Received (Self.Sys_Time_T_Get, (
         Id => Cmd.Header.Id,
         Errant_Field_Number => Errant_Field_Number,
         Errant_Field => Errant_Field
      )));
   end Invalid_Command;

   -- This procedure is called when a Command_T_Recv_Async message is dropped due to a full queue.
   overriding procedure Command_T_Recv_Async_Dropped (Self : in out Instance; Arg : in Command.T) is
   begin
      -- We dropped a command because the queue overflowed. Throw an event.
      Self.Event_T_Send_If_Connected (Self.Events.Dropped_Command (Self.Sys_Time_T_Get, Arg.Header));
   end Command_T_Recv_Async_Dropped;

end Component.Product_Packetizer.Implementation;
