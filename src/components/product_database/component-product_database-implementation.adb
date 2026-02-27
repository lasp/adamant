--------------------------------------------------------------------------------
-- Product_Database Component Implementation Body
--------------------------------------------------------------------------------

with Data_Product_Enums;
with Serializer_Types;
with Byte_Array_Util;

package body Component.Product_Database.Implementation is

   ---------------------------------------
   -- Protected Database Wrapper:
   ---------------------------------------
   protected body Protected_Database is

      procedure Init (Minimum_Id : in Data_Product_Types.Data_Product_Id; Maximum_Id : in Data_Product_Types.Data_Product_Id) is
      begin
         Db.Init (Minimum_Id => Minimum_Id, Maximum_Id => Maximum_Id);
      end Init;

      procedure Destroy is
      begin
         Db.Destroy;
      end Destroy;

      procedure Update (Id : in Data_Product_Types.Data_Product_Id; Value : in Data_Product.T; Status : out Data_Product_Database.Update_Status) is
      begin
         Status := Db.Update (Id, Value);
      end Update;

      function Fetch (Id : in Data_Product_Types.Data_Product_Id; Value : out Data_Product.T) return Data_Product_Database.Fetch_Status is
      begin
         return Db.Fetch (Id, Value);
      end Fetch;

      procedure Override (Id : in Data_Product_Types.Data_Product_Id; Value : in Data_Product.T; Status : out Data_Product_Database.Update_Status) is
      begin
         Status := Db.Override (Id, Value);
      end Override;

      procedure Clear_Override (Id : in Data_Product_Types.Data_Product_Id; Status : out Data_Product_Database.Clear_Override_Status) is
      begin
         Status := Db.Clear_Override (Id);
      end Clear_Override;

      procedure Clear_Override_All is
      begin
         Db.Clear_Override_All;
      end Clear_Override_All;

      function Overridden return Basic_Enums.Enable_Disable_Type.E is
         use Basic_Enums.Enable_Disable_Type;
      begin
         case Db.Any_Overridden is
            when True => return Enabled;
            when False => return Disabled;
         end case;
      end Overridden;

   end Protected_Database;

   --------------------------------------------------
   -- Subprogram for implementation init method:
   --------------------------------------------------
   -- This component requires the minimum and maximum acceptable data product IDs in order to size its internal database. Memory will be allocated to store a maximum sized data product for every ID in the range provided.
   --
   -- Init Parameters:
   -- Minimum_Data_Product_Id : Data_Product_Types.Data_Product_Id - The minimum data product identifier that the database will accept.
   -- Maximum_Data_Product_Id : Data_Product_Types.Data_Product_Id - The maximum data product identifier that the database will accept. This value combined with the Minimum_Data_Product_Id are used to allocate a table on the heap. Ids stored in this database should come from a compact Id space for most efficient memory usage.
   -- Send_Event_On_Missing : Boolean - By default the product database will send an event every time a data product is fetched that is missing. Sometimes this is expected behavior and the message is annoying. This flag allows that event to be disabled permanently on startup if needed.
   --
   overriding procedure Init (Self : in out Instance; Minimum_Data_Product_Id : in Data_Product_Types.Data_Product_Id; Maximum_Data_Product_Id : in Data_Product_Types.Data_Product_Id; Send_Event_On_Missing : in Boolean := True) is
   begin
      Self.Db.Init (Minimum_Id => Minimum_Data_Product_Id, Maximum_Id => Maximum_Data_Product_Id);
      Self.Send_Event_On_Missing := Send_Event_On_Missing;

      -- Make sure that data product can serialize into packet without issue.
      if Self.Is_Packet_T_Send_Connected then
         pragma Assert (Data_Product.Size_In_Bytes <= Packet_Types.Packet_Buffer_Type'Length, "The dump packet requires that a data product be able to serialize into a packet type!");
      end if;
   end Init;

   not overriding procedure Final (Self : in out Instance) is
   begin
      Self.Db.Destroy;
   end Final;

   overriding procedure Set_Up (Self : in out Instance) is
   begin
      -- Publish the initial override state of the database:
      Self.Data_Product_T_Send_If_Connected (Self.Data_Products.Database_Override (Self.Sys_Time_T_Get, (State => Self.Db.Overridden)));
   end Set_Up;

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- Store a data product item in the database.
   overriding procedure Data_Product_T_Recv_Sync (Self : in out Instance; Arg : in Data_Product.T) is
      use Data_Product_Database;
      Status : Update_Status;
   begin
      -- Update the database:
      Self.Db.Update (Id => Arg.Header.Id, Value => Arg, Status => Status);

      -- Check the status:
      case Status is
         when Success =>
            null; -- Yay!
         when Id_Out_Of_Range =>
            Self.Event_T_Send_If_Connected (Self.Events.Data_Product_Update_Id_Out_Of_Range (Self.Sys_Time_T_Get, (Id => Arg.Header.Id)));
         when Serialization_Failure =>
            -- If this ever happens then the data product type itself is malformed, or there is bug. Based on the definition of the type
            -- for a data product, the length cannot be larger than the size of the byte array without a constraint error being thrown
            -- first. For someone to create a data product like this they would need to bypass the Ada type system to create it, which
            -- is never necessary to do in practice.
            pragma Assert (False, "Data product update serialization error. This should never happen.");
      end case;
   end Data_Product_T_Recv_Sync;

   -- Fetch a data product item from the database.
   overriding function Data_Product_Fetch_T_Service (Self : in out Instance; Arg : in Data_Product_Fetch.T) return Data_Product_Return.T is
      use Data_Product_Database;

      -- Fetch from the database:
      To_Return : Data_Product_Return.T;
      Stat : constant Data_Product_Database.Fetch_Status := Self.Db.Fetch (Id => Arg.Id, Value => To_Return.The_Data_Product);
   begin
      -- Check the status:
      case Stat is
         when Success =>
            To_Return.The_Status := Data_Product_Enums.Fetch_Status.Success;
         when Data_Not_Available =>
            To_Return.The_Status := Data_Product_Enums.Fetch_Status.Not_Available;
            if Self.Send_Event_On_Missing then
               Self.Event_T_Send_If_Connected (Self.Events.Data_Product_Fetch_Id_Not_Available (Self.Sys_Time_T_Get, (Id => Arg.Id)));
            end if;
         when Id_Out_Of_Range =>
            To_Return.The_Status := Data_Product_Enums.Fetch_Status.Id_Out_Of_Range;
            Self.Event_T_Send_If_Connected (Self.Events.Data_Product_Fetch_Id_Out_Of_Range (Self.Sys_Time_T_Get, (Id => Arg.Id)));
      end case;
      return To_Return;
   end Data_Product_Fetch_T_Service;

   -- This is the command receive connector. This does not need to be connected if the command for this component will not be used.
   overriding procedure Command_T_Recv_Sync (Self : in out Instance; Arg : in Command.T) is
      -- Execute the command:
      Stat : constant Command_Response_Status.E := Self.Execute_Command (Arg);
   begin
      -- Send the return status:
      Self.Command_Response_T_Send_If_Connected ((Source_Id => Arg.Header.Source_Id, Registration_Id => Self.Command_Reg_Id, Command_Id => Arg.Header.Id, Status => Stat));
   end Command_T_Recv_Sync;

   -----------------------------------------------
   -- Command handler primitives:
   -----------------------------------------------
   -- Description:
   --    These are the commands for the Product Database.
   -- Clear the override condition for the data product of the provided ID.
   overriding function Clear_Override (Self : in out Instance; Arg : in Data_Product_Id.T) return Command_Execution_Status.E is
      use Command_Execution_Status;
      use Data_Product_Database;
      Status : Clear_Override_Status;
      The_Time : constant Sys_Time.T := Self.Sys_Time_T_Get;
   begin
      -- Update the database:
      Self.Db.Clear_Override (Id => Arg.Id, Status => Status);

      -- Check the status:
      case Status is
         when Success =>
            -- Update override status data product:
            Self.Data_Product_T_Send_If_Connected (Self.Data_Products.Database_Override (The_Time, (State => Self.Db.Overridden)));
            -- Send status event:
            Self.Event_T_Send_If_Connected (Self.Events.Override_Cleared (The_Time, Arg));
            return Success;
         when Id_Out_Of_Range =>
            Self.Event_T_Send_If_Connected (Self.Events.Data_Product_Clear_Override_Id_Out_Of_Range (The_Time, Arg));
            return Failure;
      end case;
   end Clear_Override;

   -- Clear the override condition for all data products in the product store.
   overriding function Clear_Override_For_All (Self : in out Instance) return Command_Execution_Status.E is
      use Command_Execution_Status;
      The_Time : constant Sys_Time.T := Self.Sys_Time_T_Get;
   begin
      -- Clear override for the entire database:
      Self.Db.Clear_Override_All;

      -- Update override status data product:
      Self.Data_Product_T_Send_If_Connected (Self.Data_Products.Database_Override (The_Time, (State => Self.Db.Overridden)));

      -- Send info event:
      Self.Event_T_Send_If_Connected (Self.Events.Override_Cleared_For_All (The_Time));

      return Success;
   end Clear_Override_For_All;

   -- Override the value of a data product in the data product store. The value of this data product will be fixed to the commanded value, ignoring all other updates, until the override is cleared.
   overriding function Override (Self : in out Instance; Arg : in Data_Product.T) return Command_Execution_Status.E is
      use Command_Execution_Status;
      use Data_Product_Database;
      Status : Update_Status;
      The_Time : constant Sys_Time.T := Self.Sys_Time_T_Get;
   begin
      -- Update the database:
      Self.Db.Override (Id => Arg.Header.Id, Value => Arg, Status => Status);

      -- Check the status:
      case Status is
         when Success =>
            -- Update override status data product:
            Self.Data_Product_T_Send_If_Connected (Self.Data_Products.Database_Override (The_Time, (State => Self.Db.Overridden)));
            -- Send info event:
            Self.Event_T_Send_If_Connected (Self.Events.Data_Product_Overridden (The_Time, Arg.Header));
            return Success;
         when Id_Out_Of_Range =>
            Self.Event_T_Send_If_Connected (Self.Events.Data_Product_Override_Id_Out_Of_Range (The_Time, (Id => Arg.Header.Id)));
            return Failure;
         when Serialization_Failure =>
            -- If this ever happens then the data product type itself is malformed, or there is bug. Based on the definition of the type
            -- for a data product, the length cannot be larger than the size of the byte array without a constraint error being thrown
            -- first. For someone to create a data product like this they would need to bypass the Ada type system to create it, which
            -- is never necessary to do in practice.
            --
            -- Note: A malformed data product sent by command would be rejected in the command validation logic before this function is
            -- called.
            pragma Assert (False, "Data product update serialization error. This should never happen.");
            return Failure;
      end case;
   end Override;

   -- Dump the data product of the provided ID in a packet.
   overriding function Dump (Self : in out Instance; Arg : in Data_Product_Id.T) return Command_Execution_Status.E is
      use Command_Execution_Status;
      use Data_Product_Database;

      -- Fetch from the database:
      Dp : Data_Product.T;
      The_Time : constant Sys_Time.T := Self.Sys_Time_T_Get;
      Stat : constant Data_Product_Database.Fetch_Status := Self.Db.Fetch (Id => Arg.Id, Value => Dp);
   begin
      -- Check the status:
      case Stat is
         when Success =>
            if Self.Is_Packet_T_Send_Connected then
               declare
                  use Serializer_Types;
                  Pkt : Packet.T;
                  Status : constant Serialization_Status := Self.Packets.Dump_Packet (The_Time, Dp, Pkt);
               begin
                  -- Checked in Init.
                  pragma Assert (Status = Success);

                  -- Send out packet:
                  Self.Packet_T_Send (Pkt);
                  Self.Event_T_Send_If_Connected (Self.Events.Data_Product_Dumped (The_Time, Dp.Header));
               end;
            end if;

            return Success;
         when Data_Not_Available =>
            Self.Event_T_Send_If_Connected (Self.Events.Data_Product_Dump_Id_Not_Available (The_Time, Arg));
            return Failure;
         when Id_Out_Of_Range =>
            Self.Event_T_Send_If_Connected (Self.Events.Data_Product_Dump_Id_Out_Of_Range (The_Time, Arg));
            return Failure;
      end case;
   end Dump;

   -- Dump the data product of the provided ID into a poly type based on the provided offset and length.
   overriding function Dump_Poly_Type (Self : in out Instance; Arg : in Data_Product_Poly_Extract.T) return Command_Execution_Status.E is
      use Command_Execution_Status;
      use Data_Product_Database;

      -- Fetch from the database:
      The_Time : constant Sys_Time.T := Self.Sys_Time_T_Get;
      Dp : Data_Product.T;
      Stat : constant Data_Product_Database.Fetch_Status := Self.Db.Fetch (Id => Arg.Id, Value => Dp);
   begin
      -- Send info event:
      Self.Event_T_Send_If_Connected (Self.Events.Dumping_Data_Product_Poly_Type (The_Time, Arg));

      -- Check the status:
      case Stat is
         when Success =>
            declare
               use Byte_Array_Util;
               Data : Basic_Types.Poly_32_Type;
               Status : constant Byte_Array_Util.Extract_Poly_Type_Status := Byte_Array_Util.Extract_Poly_Type (
                  Src => Dp.Buffer (
                     Dp.Buffer'First ..
                     Dp.Buffer'First + Dp.Header.Buffer_Length - 1
                  ),
                  Offset => Arg.Offset,
                  Size => Arg.Size,
                  Is_Signed => False,
                  Value => Data
               );
            begin
               -- Check the status of the extraction:
               case Status is
                  when Success =>
                     -- Send out data product:
                     Self.Data_Product_T_Send_If_Connected (Self.Data_Products.Data_Product_Poly_Type_Dump (The_Time, (Time => Dp.Header.Time, Id => Dp.Header.Id, Data => Data)));

                     -- Send out info event:
                     Self.Event_T_Send_If_Connected (Self.Events.Dumped_Data_Product_Poly_Type (The_Time, (Header => Dp.Header, Data => Data)));
                     return Success;
                  when Error =>
                     -- Send out error event:
                     Self.Event_T_Send_If_Connected (Self.Events.Data_Product_Poly_Type_Extraction_Failed (The_Time, Dp.Header));
                     return Failure;
               end case;
            end;
         when Data_Not_Available =>
            Self.Event_T_Send_If_Connected (Self.Events.Data_Product_Dump_Poly_Id_Not_Available (The_Time, (Id => Arg.Id)));
            return Failure;
         when Id_Out_Of_Range =>
            Self.Event_T_Send_If_Connected (Self.Events.Data_Product_Dump_Poly_Id_Out_Of_Range (The_Time, (Id => Arg.Id)));
            return Failure;
      end case;
   end Dump_Poly_Type;

   -- Invalid command handler. This procedure is called when a command's arguments are found to be invalid:
   overriding procedure Invalid_Command (Self : in out Instance; Cmd : in Command.T; Errant_Field_Number : in Unsigned_32; Errant_Field : in Basic_Types.Poly_Type) is
   begin
      -- Throw event:
      Self.Event_T_Send_If_Connected (Self.Events.Invalid_Command_Received (
         Self.Sys_Time_T_Get,
         (Id => Cmd.Header.Id, Errant_Field_Number => Errant_Field_Number, Errant_Field => Errant_Field)
      ));
   end Invalid_Command;

end Component.Product_Database.Implementation;
