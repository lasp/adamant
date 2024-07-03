--------------------------------------------------------------------------------
-- Last_Chance_Manager Tests Body
--------------------------------------------------------------------------------

with Packed_Exception_Occurrence.Assertion; use Packed_Exception_Occurrence.Assertion;
with System.Storage_Elements; use System.Storage_Elements;
with Basic_Assertions; use Basic_Assertions;
with Invalid_Command_Info.Assertion; use Invalid_Command_Info.Assertion;
with Packed_Stack_Trace_Info.Assertion; use Packed_Stack_Trace_Info.Assertion;
with Command_Response.Assertion; use Command_Response.Assertion;
with Command_Enums; use Command_Enums.Command_Response_Status;
with Command;
with Interfaces;

package body Last_Chance_Manager_Tests.Implementation is

   -------------------------------------------------------------------------
   -- Global data:
   -------------------------------------------------------------------------
   Exception_Data : aliased Packed_Exception_Occurrence.T := (
      Exception_Name => [others => 0],
      Exception_Message => [others => 0],
      Stack_Trace_Depth => 0,
      Stack_Trace => [others => (Address => To_Address (Integer_Address (0)))]
   );

   -------------------------------------------------------------------------
   -- Fixtures:
   -------------------------------------------------------------------------

   overriding procedure Set_Up_Test (Self : in out Instance) is
   begin
      -- Reset data:
      Exception_Data := (
         Exception_Name => [others => 0],
         Exception_Message => [others => 0],
         Stack_Trace_Depth => 0,
         Stack_Trace => [others => (Address => To_Address (Integer_Address (0)))]
      );

      -- Allocate heap memory to component:
      Self.Tester.Init_Base;

      -- Make necessary connections between tester and component:
      Self.Tester.Connect;

      -- Call component init here.
      Self.Tester.Component_Instance.Init (Exception_Data => Exception_Data'Access, Dump_Exception_Data_At_Startup => True);

      -- Call the component set up method that the assembly would normally call.
      Self.Tester.Component_Instance.Set_Up;
   end Set_Up_Test;

   overriding procedure Tear_Down_Test (Self : in out Instance) is
   begin
      -- Free component heap:
      Self.Tester.Final_Base;
   end Tear_Down_Test;

   -------------------------------------------------------------------------
   -- Tests:
   -------------------------------------------------------------------------

   overriding procedure Test_Region_Dump (Self : in out Instance) is
      T : Component.Last_Chance_Manager.Implementation.Tester.Instance_Access renames Self.Tester;
   begin
      -- Make sure Set_Up produced a data product and packet.
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Lch_Memory_Region_Dump_History.Get_Count, 1);
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Lch_Stack_Trace_Info_History.Get_Count, 1);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 0);

      -- Check packet data:
      Packed_Exception_Occurrence_Assert.Eq (T.Lch_Memory_Region_Dump_History.Get (1), (
         Exception_Name => [others => 0],
         Exception_Message => [others => 0],
         Stack_Trace_Depth => 0,
         Stack_Trace => [others => (Address => To_Address (Integer_Address (0)))]
      ));

      -- Check data product data:
      Packed_Stack_Trace_Info_Assert.Eq (T.Lch_Stack_Trace_Info_History.Get (1), (Stack_Trace_Depth => 0, Stack_Trace_Bottom_Address => (Address => To_Address (Integer_Address (0)))));

      -- Ok now dump the region, and expect more packets and data products:
      T.Command_T_Send (T.Commands.Dump_Last_Chance_Handler_Region);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 1);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (1), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Dump_Last_Chance_Handler_Region_Id, Status => Success));

      -- Make sure some events were thrown:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Dumped_Last_Chance_Handler_Region_History.Get_Count, 1);

      -- Check packets and data products:
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Lch_Memory_Region_Dump_History.Get_Count, 2);
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Lch_Stack_Trace_Info_History.Get_Count, 2);

      -- Check packet data:
      Packed_Exception_Occurrence_Assert.Eq (T.Lch_Memory_Region_Dump_History.Get (2), (
         Exception_Name => [others => 0],
         Exception_Message => [others => 0],
         Stack_Trace_Depth => 0,
         Stack_Trace => [others => (Address => To_Address (Integer_Address (0)))]
      ));

      -- Check data product data:
      Packed_Stack_Trace_Info_Assert.Eq (T.Lch_Stack_Trace_Info_History.Get (2), (Stack_Trace_Depth => 0, Stack_Trace_Bottom_Address => (Address => To_Address (Integer_Address (0)))));

      -- OK now modify the region:
      Exception_Data := (Exception_Name => [others => 44], Exception_Message => [others => 33], Stack_Trace_Depth => 0, Stack_Trace => [others => (Address => To_Address (Integer_Address (99)))]);
      Exception_Data.Stack_Trace (0) := (Address => To_Address (Integer_Address (88))); -- set this to unique value to make sure it is seen in dp

      -- Ok now dump the region, and expect more packets and data products:
      T.Command_T_Send (T.Commands.Dump_Last_Chance_Handler_Region);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 2);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (2), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Dump_Last_Chance_Handler_Region_Id, Status => Success));

      -- Make sure some events were thrown:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 3);
      Natural_Assert.Eq (T.Dumped_Last_Chance_Handler_Region_History.Get_Count, 2);
      Natural_Assert.Eq (T.Last_Chance_Handler_Called_History.Get_Count, 1);
      Packed_Stack_Trace_Info_Assert.Eq (T.Last_Chance_Handler_Called_History.Get (1), (Stack_Trace_Depth => 0, Stack_Trace_Bottom_Address => (Address => To_Address (Integer_Address (88)))));

      -- Check packets and data products:
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 3);
      Natural_Assert.Eq (T.Lch_Memory_Region_Dump_History.Get_Count, 3);
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 3);
      Natural_Assert.Eq (T.Lch_Stack_Trace_Info_History.Get_Count, 3);

      -- Check packet data:
      Packed_Exception_Occurrence_Assert.Eq (T.Lch_Memory_Region_Dump_History.Get (3), (
         Exception_Name => [others => 44],
         Exception_Message => [others => 33],
         Stack_Trace_Depth => 0,
         Stack_Trace => [0 => (Address => To_Address (Integer_Address (88))), others => (Address => To_Address (Integer_Address (99)))]
      ));

      -- Check data product data:
      Packed_Stack_Trace_Info_Assert.Eq (T.Lch_Stack_Trace_Info_History.Get (3), (Stack_Trace_Depth => 0, Stack_Trace_Bottom_Address => (Address => To_Address (Integer_Address (88)))));

      -- OK now modify the region:
      Exception_Data := (Exception_Name => [others => 44], Exception_Message => [others => 33], Stack_Trace_Depth => 16, Stack_Trace => [others => (Address => To_Address (Integer_Address (0)))]);

      -- Ok now dump the region, and expect more packets and data products:
      T.Command_T_Send (T.Commands.Dump_Last_Chance_Handler_Region);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 3);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (3), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Dump_Last_Chance_Handler_Region_Id, Status => Success));

      -- Make sure some events were thrown:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 5);
      Natural_Assert.Eq (T.Dumped_Last_Chance_Handler_Region_History.Get_Count, 3);
      Natural_Assert.Eq (T.Last_Chance_Handler_Called_History.Get_Count, 2);
      Packed_Stack_Trace_Info_Assert.Eq (T.Last_Chance_Handler_Called_History.Get (2), (Stack_Trace_Depth => 16, Stack_Trace_Bottom_Address => (Address => To_Address (Integer_Address (0)))));

      -- Check packets and data products:
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 4);
      Natural_Assert.Eq (T.Lch_Memory_Region_Dump_History.Get_Count, 4);
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 4);
      Natural_Assert.Eq (T.Lch_Stack_Trace_Info_History.Get_Count, 4);

      -- Check packet data:
      Packed_Exception_Occurrence_Assert.Eq (T.Lch_Memory_Region_Dump_History.Get (4), (
         Exception_Name => [others => 44],
         Exception_Message => [others => 33],
         Stack_Trace_Depth => 16,
         Stack_Trace => [others => (Address => To_Address (Integer_Address (0)))]
      ));

      -- Check data product data:
      Packed_Stack_Trace_Info_Assert.Eq (T.Lch_Stack_Trace_Info_History.Get (4), (Stack_Trace_Depth => 16, Stack_Trace_Bottom_Address => (Address => To_Address (Integer_Address (0)))));
   end Test_Region_Dump;

   overriding procedure Test_Region_Clear (Self : in out Instance) is
      T : Component.Last_Chance_Manager.Implementation.Tester.Instance_Access renames Self.Tester;
   begin
      -- Make sure Set_Up produced a data product and packet.
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Lch_Memory_Region_Dump_History.Get_Count, 1);
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Lch_Stack_Trace_Info_History.Get_Count, 1);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 0);

      -- Check packet data:
      Packed_Exception_Occurrence_Assert.Eq (T.Lch_Memory_Region_Dump_History.Get (1), (
         Exception_Name => [others => 0],
         Exception_Message => [others => 0],
         Stack_Trace_Depth => 0,
         Stack_Trace => [others => (Address => To_Address (Integer_Address (0)))]
      ));

      -- Check data product data:
      Packed_Stack_Trace_Info_Assert.Eq (T.Lch_Stack_Trace_Info_History.Get (1), (Stack_Trace_Depth => 0, Stack_Trace_Bottom_Address => (Address => To_Address (Integer_Address (0)))));

      -- OK now modify the region:
      Exception_Data := (Exception_Name => [others => 44], Exception_Message => [others => 33], Stack_Trace_Depth => 16, Stack_Trace => [others => (Address => To_Address (Integer_Address (99)))]);

      -- Call Set_Up, make sure, correct stuff thrown with exception data in store.
      Self.Tester.Component_Instance.Set_Up;

      -- Make sure some events were thrown:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Last_Chance_Handler_Called_History.Get_Count, 1);
      Packed_Stack_Trace_Info_Assert.Eq (T.Last_Chance_Handler_Called_History.Get (1), (Stack_Trace_Depth => 16, Stack_Trace_Bottom_Address => (Address => To_Address (Integer_Address (99)))));

      -- Check packets and data products:
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Lch_Memory_Region_Dump_History.Get_Count, 2);
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Lch_Stack_Trace_Info_History.Get_Count, 2);

      -- Check packet data:
      Packed_Exception_Occurrence_Assert.Eq (T.Lch_Memory_Region_Dump_History.Get (2), (
         Exception_Name => [others => 44],
         Exception_Message => [others => 33],
         Stack_Trace_Depth => 16,
         Stack_Trace => [others => (Address => To_Address (Integer_Address (99)))]
      ));

      -- Check data product data:
      Packed_Stack_Trace_Info_Assert.Eq (T.Lch_Stack_Trace_Info_History.Get (2), (Stack_Trace_Depth => 16, Stack_Trace_Bottom_Address => (Address => To_Address (Integer_Address (99)))));

      -- OK now clear the memory region:
      T.Command_T_Send (T.Commands.Clear_Last_Chance_Handler_Region);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 1);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (1), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Clear_Last_Chance_Handler_Region_Id, Status => Success));

      -- Make sure some events were thrown:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Cleared_Last_Chance_Handler_Region_History.Get_Count, 1);

      -- Check stored data:
      Packed_Exception_Occurrence_Assert.Eq (Exception_Data, (
         Exception_Name => [others => 0],
         Exception_Message => [others => 0],
         Stack_Trace_Depth => 0,
         Stack_Trace => [others => (Address => To_Address (Integer_Address (0)))]
      ));

      -- Check packets and data products:
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 3);
      Natural_Assert.Eq (T.Lch_Memory_Region_Dump_History.Get_Count, 3);
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 3);
      Natural_Assert.Eq (T.Lch_Stack_Trace_Info_History.Get_Count, 3);

      -- Check packet data:
      Packed_Exception_Occurrence_Assert.Eq (T.Lch_Memory_Region_Dump_History.Get (3), (
         Exception_Name => [others => 0],
         Exception_Message => [others => 0],
         Stack_Trace_Depth => 0,
         Stack_Trace => [others => (Address => To_Address (Integer_Address (0)))]
      ));

      -- Check data product data:
      Packed_Stack_Trace_Info_Assert.Eq (T.Lch_Stack_Trace_Info_History.Get (3), (Stack_Trace_Depth => 0, Stack_Trace_Bottom_Address => (Address => To_Address (Integer_Address (0)))));
   end Test_Region_Clear;

   overriding procedure Test_Invalid_Command (Self : in out Instance) is
      T : Component.Last_Chance_Manager.Implementation.Tester.Instance_Access renames Self.Tester;
      Cmd : Command.T := T.Commands.Dump_Last_Chance_Handler_Region;
   begin
      -- Make the command invalid by modifying its length.
      Cmd.Header.Arg_Buffer_Length := 1;

      -- Send bad command and expect bad response:
      T.Command_T_Send (Cmd);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 1);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (1), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Dump_Last_Chance_Handler_Region_Id, Status => Length_Error));

      -- Make sure some events were thrown:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Invalid_Command_Received_History.Get_Count, 1);
      Invalid_Command_Info_Assert.Eq (T.Invalid_Command_Received_History.Get (1), (Id => T.Commands.Get_Dump_Last_Chance_Handler_Region_Id, Errant_Field_Number => Interfaces.Unsigned_32'Last, Errant_Field => [0, 0, 0, 0, 0, 0, 0, 1]));
   end Test_Invalid_Command;

end Last_Chance_Manager_Tests.Implementation;
