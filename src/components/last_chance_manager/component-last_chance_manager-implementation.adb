--------------------------------------------------------------------------------
-- Last_Chance_Manager Component Implementation Body
--------------------------------------------------------------------------------

with System.Storage_Elements; use System.Storage_Elements;
with Packed_Stack_Trace_Info;

package body Component.Last_Chance_Manager.Implementation is

   --------------------------------------------------
   -- Subprogram for implementation init method:
   --------------------------------------------------
   -- This component requires the memory region which the last chance handler data will be stored.
   --
   -- Init Parameters:
   -- Exception_Data : Packed_Exception_Occurrence.T_Access - The copy of the exception data that is updated by the last chance handler, presumably in a nonvolatile memory region.
   -- Dump_Exception_Data_At_Startup : Boolean - If True, then the exception data will be dumped in packet at startup.
   --
   overriding procedure Init (Self : in out Instance; Exception_Data : in not null Packed_Exception_Occurrence.T_Access; Dump_Exception_Data_At_Startup : in Boolean) is
   begin
      Self.Exception_Data := Exception_Data;
      Self.Dump_Exception_Data_At_Startup := Dump_Exception_Data_At_Startup;
   end Init;

   -- Helper function which sends out a packet and update the data product:
   procedure Send_Out_Packet_And_Data_Product (Self : in out Instance; The_Time : in Sys_Time.T) is
      use System;
      Stack_Trace_Info : constant Packed_Stack_Trace_Info.T := (Stack_Trace_Depth => Self.Exception_Data.Stack_Trace_Depth,
      -- The stack trace bottom address is actually the first reported address in
      -- the stack trace list. The last address on the list is the top of the stack.
      Stack_Trace_Bottom_Address => Self.Exception_Data.Stack_Trace (0));
   begin
      -- Send LCH packet at startup:
      Self.Packet_T_Send_If_Connected (Self.Packets.Lch_Memory_Region_Dump (The_Time, Self.Exception_Data.all));

      -- Update the data product:
      Self.Data_Product_T_Send_If_Connected (Self.Data_Products.Lch_Stack_Trace_Info (The_Time, Stack_Trace_Info));

      -- Throw info event if we detect that the LCH was called:
      if Stack_Trace_Info.Stack_Trace_Depth > 0 or else Stack_Trace_Info.Stack_Trace_Bottom_Address.Address /= To_Address (Integer_Address (0)) then
         Self.Event_T_Send_If_Connected (Self.Events.Last_Chance_Handler_Called (The_Time, Stack_Trace_Info));
      end if;
   end Send_Out_Packet_And_Data_Product;

   -- Report data product:
   overriding procedure Set_Up (Self : in out Instance) is
      The_Time : constant Sys_Time.T := Self.Sys_Time_T_Get;
   begin
      if Self.Dump_Exception_Data_At_Startup then
         -- Send out our data:
         Self.Send_Out_Packet_And_Data_Product (The_Time);
      end if;
   end Set_Up;

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- The command receive connector
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
   --    Commands for the Last Chance Manager component.
   -- Dump the last chance handler memory region into a packet for downlink.
   overriding function Dump_Last_Chance_Handler_Region (Self : in out Instance) return Command_Execution_Status.E is
      use Command_Execution_Status;
      The_Time : constant Sys_Time.T := Self.Sys_Time_T_Get;
   begin
      -- Send out our data:
      Self.Send_Out_Packet_And_Data_Product (The_Time);

      -- Send event:
      Self.Event_T_Send_If_Connected (Self.Events.Dumped_Last_Chance_Handler_Region (The_Time));

      return Success;
   end Dump_Last_Chance_Handler_Region;

   -- Clear the last chance handler memory region by writing all zeros to it.
   overriding function Clear_Last_Chance_Handler_Region (Self : in out Instance) return Command_Execution_Status.E is
      use Command_Execution_Status;
      The_Time : constant Sys_Time.T := Self.Sys_Time_T_Get;
   begin
      -- Clear all the data:
      Self.Exception_Data.all := (Exception_Name => [others => 0], Exception_Message => [others => 0], Stack_Trace_Depth => 0, Stack_Trace => [others => (Address => To_Address (Integer_Address (0)))]);

      -- Send out our data:
      Self.Send_Out_Packet_And_Data_Product (The_Time);

      -- Send event:
      Self.Event_T_Send_If_Connected (Self.Events.Cleared_Last_Chance_Handler_Region (The_Time));

      return Success;
   end Clear_Last_Chance_Handler_Region;

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

end Component.Last_Chance_Manager.Implementation;
