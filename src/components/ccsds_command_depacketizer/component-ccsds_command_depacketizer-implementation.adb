--------------------------------------------------------------------------------
-- Ccsds_Command_Depacketizer Component Implementation Body
--------------------------------------------------------------------------------

with Ccsds_Command_Secondary_Header;
with Ccsds_Primary_Header;
with Command_Id;
with Xor_8;
with Ccsds_Enums;

package body Component.Ccsds_Command_Depacketizer.Implementation is

   ---------------------------------------
   -- Initialization:
   ---------------------------------------

   overriding procedure Set_Up (Self : in out Instance) is
      The_Time : constant Sys_Time.T := Self.Sys_Time_T_Get;
   begin
      Self.Rejected_Packet_Count.Set_Count (0);
      Self.Accepted_Packet_Count.Set_Count (0);
      Self.Data_Product_T_Send_If_Connected (Self.Data_Products.Rejected_Packet_Count (The_Time, (Value => Self.Rejected_Packet_Count.Get_Count)));
      Self.Data_Product_T_Send_If_Connected (Self.Data_Products.Accepted_Packet_Count (The_Time, (Value => Self.Accepted_Packet_Count.Get_Count)));
   end Set_Up;

   ---------------------------------------
   -- Private functions:
   ---------------------------------------

   procedure Drop_Packet (Self : in out Instance; Arg : in Ccsds_Space_Packet.T; Evt : in Event.T) is
   begin
      -- Send out event:
      Self.Event_T_Send_If_Connected (Evt);
      -- Send out data product:
      if Self.Is_Data_Product_T_Send_Connected then
         Self.Rejected_Packet_Count.Increment_Count;
         Self.Data_Product_T_Send (Self.Data_Products.Rejected_Packet_Count (Evt.Header.Time, (Value => Self.Rejected_Packet_Count.Get_Count)));
      end if;
      -- Forward the packet out as an error packet:
      Self.Packet_T_Send_If_Connected (Self.Packets.Error_Packet_Truncate (Evt.Header.Time, Arg));
   end Drop_Packet;

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- The ccsds packet receive connector.
   overriding procedure Ccsds_Space_Packet_T_Recv_Sync (Self : in out Instance; Arg : in Ccsds_Space_Packet.T) is
      use Ccsds_Enums;
      use Ccsds_Enums.Ccsds_Secondary_Header_Indicator;
      use Ccsds_Enums.Ccsds_Packet_Type;

      -- Extract data from packet:
      Data_Length : constant Natural := Natural (Arg.Header.Packet_Length) + 1;

      -- Packet secondary header deserialization stuff:
      Ccsds_Command_Secondary_Header_Length : constant Natural := Ccsds_Command_Secondary_Header.Serialization.Serialized_Length;
      Next_Index : Natural := Arg.Data'First;

      -- Checksum value:
      Checksum : Xor_8.Xor_8_Type;

      -- Create a buffer and command type:
      The_Command : Command.T;

      -- Deserialize the secondary header
      Secondary_Header : constant Ccsds_Command_Secondary_Header.T :=
         Ccsds_Command_Secondary_Header.Serialization.From_Byte_Array (Arg.Data (Next_Index .. Next_Index + Ccsds_Command_Secondary_Header_Length - 1));
   begin
      -- First make sure the CCSDS packet length is less than the size of the buffer. We need to do this
      -- before we can calculate a checksum without running off the end of the packet.
      if Data_Length > Arg.Data'Length then
         Self.Drop_Packet (Arg, Self.Events.Packet_Too_Large (Self.Sys_Time_T_Get, (Arg.Header, Length => Data_Length, Length_Bound => Arg.Data'Length)));
      else
         -- First check the checksum. If the checksum is not zero, then it is invalid, since the checksum within
         -- the packet itself should zero out the rest of the computed checksum.
         Checksum := Xor_8.Compute_Xor_8 (Ccsds_Primary_Header.Serialization.To_Byte_Array (Arg.Header)); -- checksum header
         Checksum := Xor_8.Compute_Xor_8 (Arg.Data (Arg.Data'First .. Arg.Data'First + Data_Length - 1), Checksum); -- checksum data
         if Checksum /= 0 then
            Self.Drop_Packet (Arg, Self.Events.Invalid_Packet_Checksum (Self.Sys_Time_T_Get, (
                  Ccsds_Header => (
                     Primary_Header => Arg.Header,
                     Secondary_Header => Secondary_Header
                  ),
                  Computed_Checksum => Checksum,
                  Expected_Checksum => Secondary_Header.Checksum
            )));
         else
            -- Check packet type and make sure it is set for CCSDS_Packet_Type.Telecommand:
            if Arg.Header.Packet_Type /= Ccsds_Packet_Type.Telecommand then
               Self.Drop_Packet (Arg, Self.Events.Invalid_Packet_Type (Self.Sys_Time_T_Get, Arg.Header));
            else
               -- Make sure that there is a secondary header:
               if Arg.Header.Secondary_Header /= Ccsds_Secondary_Header_Indicator.Secondary_Header_Present then
                  Self.Drop_Packet (Arg, Self.Events.No_Secondary_Header (Self.Sys_Time_T_Get, Arg.Header));
               else
                  declare
                     Num_Pad_Bytes : constant Natural := Natural (Secondary_Header.Function_Code);
                     Command_Id_Length : constant Natural := Command_Id.Serialization.Serialized_Length;
                     Meta_Data_Length : constant Natural := Ccsds_Command_Secondary_Header_Length + Command_Id_Length;
                     Adjusted_Data_Length : constant Integer := Data_Length - Num_Pad_Bytes;
                     Argument_Data_Length : constant Integer := Adjusted_Data_Length - Meta_Data_Length;
                  begin
                     -- Make sure that the buffer is big enough to contain a CCSDS command secondary header and
                     -- a command identifier.
                     if Adjusted_Data_Length < Meta_Data_Length then
                        Self.Drop_Packet (Arg, Self.Events.Packet_Too_Small (Self.Sys_Time_T_Get, (Arg.Header, Length => Adjusted_Data_Length, Length_Bound => Meta_Data_Length)));
                     else
                        -- Make sure that the argument data length is not too big to fit into a command:
                        if Argument_Data_Length > The_Command.Arg_Buffer'Length then
                           Self.Drop_Packet (Arg, Self.Events.Packet_Too_Large (Self.Sys_Time_T_Get, (Arg.Header, Length => Argument_Data_Length, Length_Bound => The_Command.Arg_Buffer'Length)));
                        else
                           -- We don't need anything else out of the secondary header, unless an error
                           -- occurs, so just skip right over it.
                           Next_Index := Next_Index + Ccsds_Command_Secondary_Header_Length;

                           -- Set the command header arg buffer length:
                           The_Command.Header.Arg_Buffer_Length := Argument_Data_Length;

                           -- Extract command id:
                           The_Command.Header.Id := Command_Id.Serialization.From_Byte_Array (Arg.Data (Next_Index .. Next_Index + Command_Id_Length - 1)).Id;
                           Next_Index := Next_Index + Command_Id_Length;

                           -- Copy argument buffer:
                           The_Command.Arg_Buffer (The_Command.Arg_Buffer'First .. The_Command.Arg_Buffer'First + Argument_Data_Length - 1) := Arg.Data (Next_Index .. Next_Index + Argument_Data_Length - 1);

                           -- Send out the command:
                           Self.Command_T_Send (The_Command);

                           -- Send out updated packet accept count:
                           if Self.Is_Data_Product_T_Send_Connected then
                              Self.Accepted_Packet_Count.Increment_Count;
                              Self.Data_Product_T_Send (Self.Data_Products.Accepted_Packet_Count (Self.Sys_Time_T_Get, (Value => Self.Accepted_Packet_Count.Get_Count)));
                           end if;
                        end if;
                     end if;
                  end;
               end if;
            end if;
         end if;
      end if;
   end Ccsds_Space_Packet_T_Recv_Sync;

   -- The command receive connector.
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
   --    These are the commands for the component.
   -- This command resets the internal counts for the data products.
   overriding function Reset_Counts (Self : in out Instance) return Command_Execution_Status.E is
      use Command_Execution_Status;
   begin
      -- Call the setup, which resets the counts and sends out the data products.
      Self.Set_Up;

      -- Send out the info event:
      Self.Event_T_Send_If_Connected (Self.Events.Counts_Reset (Self.Sys_Time_T_Get));

      return Success;
   end Reset_Counts;

   -- Invalid command handler. This procedure is called when a command's arguments are found to be invalid:
   overriding procedure Invalid_Command (Self : in out Instance; Cmd : in Command.T; Errant_Field_Number : in Unsigned_32; Errant_Field : in Basic_Types.Poly_Type) is
   begin
      -- Throw event:
      Self.Event_T_Send_If_Connected (Self.Events.Invalid_Command_Received (
         Self.Sys_Time_T_Get,
         (Id => Cmd.Header.Id, Errant_Field_Number => Errant_Field_Number, Errant_Field => Errant_Field)
      ));
   end Invalid_Command;

end Component.Ccsds_Command_Depacketizer.Implementation;
