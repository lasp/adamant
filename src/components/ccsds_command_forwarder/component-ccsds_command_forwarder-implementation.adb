--------------------------------------------------------------------------------
-- Ccsds_Command_Forwarder Component Implementation Body
--------------------------------------------------------------------------------

package body Component.Ccsds_Command_Forwarder.Implementation is

   ---------------------------------------
   -- Set Up Procedure
   ---------------------------------------
   -- This procedure sends out the initial value of the Packets_Forwarded_Count
   -- data product.
   overriding procedure Set_Up (Self : in out Instance) is
      The_Time : constant Sys_Time.T := Self.Sys_Time_T_Get;
   begin
      Self.Data_Product_T_Send_If_Connected (Self.Data_Products.Packets_Forwarded_Count (The_Time, (Value => Self.Packet_Count)));
   end Set_Up;

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- This is the command receive connector.
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
   --    These are the commands for the CCSDS command forwarder component.
   -- Forward the provided CCSDS packet out of the CCSDS space packet send connector.
   overriding function Forward_Packet (Self : in out Instance; Arg : in Ccsds_Command_Space_Packet.T) return Command_Execution_Status.E is
      use Command_Execution_Status;
      The_Time : constant Sys_Time.T := Self.Sys_Time_T_Get;
      -- The number of bytes in the data field of the provided packet. The framework
      -- deserialization performed prior to this handler being called guarantees that
      -- this length fits within the data field of the command argument, which is
      -- smaller than the data field of Ccsds_Space_Packet.T.
      Data_Length : constant Natural := Natural (Arg.Header.Packet_Length) + 1;
      Packet : Ccsds_Space_Packet.T := (Header => Arg.Header, Data => [others => 0]);
   begin
      -- Copy the packet data into the full sized CCSDS packet and forward it on:
      Packet.Data (0 .. Data_Length - 1) := Arg.Data (0 .. Data_Length - 1);
      Self.Ccsds_Space_Packet_T_Send_If_Connected (Packet);

      -- Update the count, then report the event and data product:
      Self.Packet_Count := @ + 1;
      Self.Event_T_Send_If_Connected (Self.Events.Packet_Forwarded (The_Time, Arg.Header));
      Self.Data_Product_T_Send_If_Connected (Self.Data_Products.Packets_Forwarded_Count (The_Time, (Value => Self.Packet_Count)));
      return Success;
   end Forward_Packet;

   -- Invalid command handler. This procedure is called when a command's arguments are found to be invalid:
   overriding procedure Invalid_Command (Self : in out Instance; Cmd : in Command.T; Errant_Field_Number : in Unsigned_32; Errant_Field : in Basic_Types.Poly_Type) is
   begin
      -- Throw event:
      Self.Event_T_Send_If_Connected (Self.Events.Invalid_Command_Received (
         Self.Sys_Time_T_Get,
         (Id => Cmd.Header.Id, Errant_Field_Number => Errant_Field_Number, Errant_Field => Errant_Field)
      ));
   end Invalid_Command;

end Component.Ccsds_Command_Forwarder.Implementation;
