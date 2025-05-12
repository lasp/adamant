--------------------------------------------------------------------------------
-- Register_Stuffer Tests Body
--------------------------------------------------------------------------------

with Basic_Assertions; use Basic_Assertions;
with Invalid_Command_Info.Assertion; use Invalid_Command_Info.Assertion;
with Command_Response.Assertion; use Command_Response.Assertion;
with Command_Enums; use Command_Enums.Command_Response_Status;
with Command;
with Interfaces; use Interfaces;
with Register_Dump_Packet_Header;
with Register_Value.Assertion; use Register_Value.Assertion;
with Packed_Address.Assertion; use Packed_Address.Assertion;
with Packed_Arm_State.Assertion; use Packed_Arm_State.Assertion;
with Packed_Arm_Timeout.Assertion; use Packed_Arm_Timeout.Assertion;
with Register_Dump_Packet_Header.Assertion; use Register_Dump_Packet_Header.Assertion;
with Register_Dump_Packet.Assertion; use Register_Dump_Packet.Assertion;
with Command_Protector_Enums;
with Ada.Unchecked_Conversion;
with System;
with Register_Dump_Packet_Array;
with Packed_U32;
with System.Storage_Elements; use System.Storage_Elements;
with Packet_Types;
package body Register_Stuffer_Tests.Implementation is

   -------------------------------------------------------------------------
   -- Globals:
   -------------------------------------------------------------------------
   Register_1 : Interfaces.Unsigned_32 := 18;
   Register_2 : Interfaces.Unsigned_32 := 333;

   Register_Array : array (1 .. 4) of Interfaces.Unsigned_32 := [18, 333, 29, 1428];

   -------------------------------------------------------------------------
   -- Fixtures:
   -------------------------------------------------------------------------

   overriding procedure Set_Up_Test (Self : in out Instance) is
   begin
      -- Reset reg values:
      Register_1 := 18;
      Register_2 := 333;
      Register_Array := [18, 333, 29, 1428];

      -- Allocate heap memory to component:
      Self.Tester.Init_Base;

      -- Make necessary connections between tester and component:
      Self.Tester.Connect;

      -- Call component init here.
      Self.Tester.Component_Instance.Init (Protect_Registers => False);

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

   overriding procedure Test_Nominal_Register_Write (Self : in out Instance) is
      T : Component.Register_Stuffer.Implementation.Tester.Instance_Access renames Self.Tester;
   begin
      -- Send command to set register:
      T.Command_T_Send (T.Commands.Write_Register ((Address => Register_1'Address, Value => 17)));
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 1);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (1), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Write_Register_Id, Status => Success));

      -- Check data products:
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Last_Register_Written_History.Get_Count, 1);
      Register_Value_Assert.Eq (T.Last_Register_Written_History.Get (1), (Address => Register_1'Address, Value => 17));

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Register_Written_History.Get_Count, 1);
      Register_Value_Assert.Eq (T.Register_Written_History.Get (1), (Address => Register_1'Address, Value => 17));

      -- Check register value:
      Unsigned_32_Assert.Eq (Register_1, 17);

      -- Send command to set register:
      T.Command_T_Send (T.Commands.Write_Register ((Address => Register_2'Address, Value => 333_789)));
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 2);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (2), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Write_Register_Id, Status => Success));

      -- Check data products:
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Last_Register_Written_History.Get_Count, 2);
      Register_Value_Assert.Eq (T.Last_Register_Written_History.Get (2), (Address => Register_2'Address, Value => 333_789));

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Register_Written_History.Get_Count, 2);
      Register_Value_Assert.Eq (T.Register_Written_History.Get (2), (Address => Register_2'Address, Value => 333_789));

      -- Check register value:
      Unsigned_32_Assert.Eq (Register_1, 17);
      Unsigned_32_Assert.Eq (Register_2, 333_789);
   end Test_Nominal_Register_Write;

   overriding procedure Test_Nominal_Register_Read (Self : in out Instance) is
      T : Component.Register_Stuffer.Implementation.Tester.Instance_Access renames Self.Tester;
   begin
      -- Send command to read register:
      T.Command_T_Send (T.Commands.Read_Register ((Address => Register_1'Address)));
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 1);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (1), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Read_Register_Id, Status => Success));

      -- Check data products:
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Last_Register_Read_History.Get_Count, 1);
      Register_Value_Assert.Eq (T.Last_Register_Read_History.Get (1), (Address => Register_1'Address, Value => 18));

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Register_Read_History.Get_Count, 1);
      Register_Value_Assert.Eq (T.Register_Read_History.Get (1), (Address => Register_1'Address, Value => 18));

      -- Check register value:
      Unsigned_32_Assert.Eq (Register_1, 18);

      -- Send command to read register:
      T.Command_T_Send (T.Commands.Read_Register ((Address => Register_2'Address)));
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 2);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (2), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Read_Register_Id, Status => Success));

      -- Check data products:
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Last_Register_Read_History.Get_Count, 2);
      Register_Value_Assert.Eq (T.Last_Register_Read_History.Get (2), (Address => Register_2'Address, Value => 333));

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Register_Read_History.Get_Count, 2);
      Register_Value_Assert.Eq (T.Register_Read_History.Get (2), (Address => Register_2'Address, Value => 333));

      -- Check register value:
      Unsigned_32_Assert.Eq (Register_1, 18);
      Unsigned_32_Assert.Eq (Register_2, 333);
   end Test_Nominal_Register_Read;

   overriding procedure Test_Bad_Address (Self : in out Instance) is
      T : Component.Register_Stuffer.Implementation.Tester.Instance_Access renames Self.Tester;
      -- Convert address to unsigned integer so we can use the "mod" operation on it.
      -- This component is for 32-bit register addresses only. However, on Linux dev environment
      -- we have 64-bit addresses, so the following warning is produced. We ignore this warning
      -- on Linux. The warning does not appear when compiling for a 32-bit system.
      pragma Warnings (Off, "types for unchecked conversion have different sizes");
      function Convert_To_Address is new Ada.Unchecked_Conversion (Interfaces.Unsigned_32, System.Address);
      pragma Warnings (On, "types for unchecked conversion have different sizes");
   begin
      -- Send command to read register with bad address:
      T.Command_T_Send (T.Commands.Read_Register ((Address => Convert_To_Address (1))));
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 1);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (1), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Read_Register_Id, Status => Failure));

      -- Check data products:
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 0);

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Invalid_Register_Address_History.Get_Count, 1);
      Packed_Address_Assert.Eq (T.Invalid_Register_Address_History.Get (1), (Address => Convert_To_Address (1)));

      -- Send command to read register with bad address:
      T.Command_T_Send (T.Commands.Read_Register ((Address => Convert_To_Address (14))));
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 2);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (2), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Read_Register_Id, Status => Failure));

      -- Check data products:
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 0);

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Invalid_Register_Address_History.Get_Count, 2);
      Packed_Address_Assert.Eq (T.Invalid_Register_Address_History.Get (2), (Address => Convert_To_Address (14)));

      -- Send command to write register with bad address:
      T.Command_T_Send (T.Commands.Write_Register ((Address => Convert_To_Address (15), Value => 99)));
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 3);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (3), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Write_Register_Id, Status => Failure));

      -- Check data products:
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 0);

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 3);
      Natural_Assert.Eq (T.Invalid_Register_Address_History.Get_Count, 3);
      Packed_Address_Assert.Eq (T.Invalid_Register_Address_History.Get (3), (Address => Convert_To_Address (15)));

      -- Send command to write register with bad address:
      T.Command_T_Send (T.Commands.Write_Register ((Address => Convert_To_Address (13), Value => 99)));
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 4);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (4), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Write_Register_Id, Status => Failure));

      -- Check data products:
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 0);

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 4);
      Natural_Assert.Eq (T.Invalid_Register_Address_History.Get_Count, 4);
      Packed_Address_Assert.Eq (T.Invalid_Register_Address_History.Get (4), (Address => Convert_To_Address (13)));

      -- Send command to dump registers with bad address:
      T.Command_T_Send (T.Commands.Dump_Registers ((Start_Address => Convert_To_Address (13), Num_Registers => 99)));
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 5);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (5), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Dump_Registers_Id, Status => Failure));

      -- Check data products:
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 0);

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 5);
      Natural_Assert.Eq (T.Invalid_Register_Address_History.Get_Count, 5);
      Packed_Address_Assert.Eq (T.Invalid_Register_Address_History.Get (5), (Address => Convert_To_Address (13)));

      -- Send command to dump registers with an overflow
      T.Command_T_Send (T.Commands.Dump_Registers ((Start_Address => To_Address (Integer_Address'Last - 3), Num_Registers => 2)));
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 6);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (6), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Dump_Registers_Id, Status => Failure));

      -- Check data products:
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 0);

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 6);
      Natural_Assert.Eq (T.Address_Range_Overflow_History.Get_Count, 1);
      Register_Dump_Packet_Header_Assert.Eq (T.Address_Range_Overflow_History.Get (1), (Start_Address => To_Address (Integer_Address'Last - 3), Num_Registers => 2));

      -- Check register values remained unaltered:
      Unsigned_32_Assert.Eq (Register_1, 18);
      Unsigned_32_Assert.Eq (Register_2, 333);
   end Test_Bad_Address;

   overriding procedure Test_Invalid_Command (Self : in out Instance) is
      T : Component.Register_Stuffer.Implementation.Tester.Instance_Access renames Self.Tester;
      Cmd : Command.T := T.Commands.Write_Register ((Address => Register_1'Address, Value => 17));
      use Command_Protector_Enums.Armed_State;
   begin
      -- Make the command invalid by modifying its length.
      Cmd.Header.Arg_Buffer_Length := 22;

      -- Protect the registers at Init:
      T.Component_Instance.Init (Protect_Registers => True);

      -- Now Arm
      T.Command_T_Send (T.Commands.Arm_Protected_Write ((Timeout => 3)));
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 1);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (1), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Arm_Protected_Write_Id, Status => Success));

      -- Check data products:
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Armed_State_History.Get_Count, 1);
      Natural_Assert.Eq (T.Armed_State_Timeout_History.Get_Count, 1);
      Packed_Arm_State_Assert.Eq (T.Armed_State_History.Get (1), (State => Armed));
      Packed_Arm_Timeout_Assert.Eq (T.Armed_State_Timeout_History.Get (1), (Timeout => 3));

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Armed_History.Get_Count, 1);
      Packed_Arm_Timeout_Assert.Eq (T.Armed_History.Get (1), (Timeout => 3));

      -- Send bad command and expect bad response:
      T.Command_T_Send (Cmd);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 2);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (2), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Write_Register_Id, Status => Length_Error));

      -- Make sure some events were thrown:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 3);
      Natural_Assert.Eq (T.Invalid_Command_Received_History.Get_Count, 1);
      Invalid_Command_Info_Assert.Eq (T.Invalid_Command_Received_History.Get (1), (Id => T.Commands.Get_Write_Register_Id, Errant_Field_Number => Interfaces.Unsigned_32'Last, Errant_Field => [0, 0, 0, 0, 0, 0, 0, 22]));

      Natural_Assert.Eq (T.Armed_State_History.Get_Count, 2);
      Natural_Assert.Eq (T.Armed_State_Timeout_History.Get_Count, 2);
      Packed_Arm_State_Assert.Eq (T.Armed_State_History.Get (2), (State => Unarmed));
      Packed_Arm_Timeout_Assert.Eq (T.Armed_State_Timeout_History.Get (2), (Timeout => 0));
   end Test_Invalid_Command;

   -- This unit test makes sure the protected register write feature works as intended.
   overriding procedure Test_Protected_Register_Write (Self : in out Instance) is
      use Command_Protector_Enums.Armed_State;
      T : Component.Register_Stuffer.Implementation.Tester.Instance_Access renames Self.Tester;
   begin
      -- Protect the registers at Init:
      Self.Tester.Component_Instance.Init (Protect_Registers => True);

      -- Send command to set register:
      T.Command_T_Send (T.Commands.Write_Register ((Address => Register_1'Address, Value => 17)));
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 1);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (1), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Write_Register_Id, Status => Failure));

      -- Check data products:
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 0);

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Rejected_Protected_Register_Write_History.Get_Count, 1);
      Register_Value_Assert.Eq (T.Rejected_Protected_Register_Write_History.Get (1), (Address => Register_1'Address, Value => 17));

      -- Check register value:
      Unsigned_32_Assert.Eq (Register_1, 18);

      -- OK now arm the system:
      T.Command_T_Send (T.Commands.Arm_Protected_Write ((Timeout => 0)));
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 2);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (2), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Arm_Protected_Write_Id, Status => Success));

      -- Check data products:
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Armed_State_History.Get_Count, 1);
      Natural_Assert.Eq (T.Armed_State_Timeout_History.Get_Count, 1);
      Packed_Arm_State_Assert.Eq (T.Armed_State_History.Get (1), (State => Armed));
      Packed_Arm_Timeout_Assert.Eq (T.Armed_State_Timeout_History.Get (1), (Timeout => 0));

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Armed_History.Get_Count, 1);
      Packed_Arm_Timeout_Assert.Eq (T.Armed_History.Get (1), (Timeout => 0));

      -- Send command to set register:
      T.Command_T_Send (T.Commands.Write_Register ((Address => Register_1'Address, Value => 17)));
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 3);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (3), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Write_Register_Id, Status => Success));

      -- Check data products:
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 5);
      Natural_Assert.Eq (T.Last_Register_Written_History.Get_Count, 1);
      Register_Value_Assert.Eq (T.Last_Register_Written_History.Get (1), (Address => Register_1'Address, Value => 17));
      Natural_Assert.Eq (T.Armed_State_History.Get_Count, 2);
      Natural_Assert.Eq (T.Armed_State_Timeout_History.Get_Count, 2);
      Packed_Arm_State_Assert.Eq (T.Armed_State_History.Get (2), (State => Unarmed));
      Packed_Arm_Timeout_Assert.Eq (T.Armed_State_Timeout_History.Get (2), (Timeout => 0));

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 4);
      Natural_Assert.Eq (T.Register_Written_History.Get_Count, 1);
      Register_Value_Assert.Eq (T.Register_Written_History.Get (1), (Address => Register_1'Address, Value => 17));

      -- Check register value:
      Unsigned_32_Assert.Eq (Register_1, 17);

      -- OK now arm the system:
      T.Command_T_Send (T.Commands.Arm_Protected_Write ((Timeout => 3)));
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 4);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (4), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Arm_Protected_Write_Id, Status => Success));

      -- Check data products:
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 7);
      Natural_Assert.Eq (T.Armed_State_History.Get_Count, 3);
      Natural_Assert.Eq (T.Armed_State_Timeout_History.Get_Count, 3);
      Packed_Arm_State_Assert.Eq (T.Armed_State_History.Get (3), (State => Armed));
      Packed_Arm_Timeout_Assert.Eq (T.Armed_State_Timeout_History.Get (3), (Timeout => 3));

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 5);
      Natural_Assert.Eq (T.Armed_History.Get_Count, 2);
      Packed_Arm_Timeout_Assert.Eq (T.Armed_History.Get (2), (Timeout => 3));

      -- Now timeout the arm:
      T.Tick_T_Send (((0, 0), 0));
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 8);
      Natural_Assert.Eq (T.Armed_State_Timeout_History.Get_Count, 4);
      Packed_Arm_Timeout_Assert.Eq (T.Armed_State_Timeout_History.Get (4), (Timeout => 2));
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 5);
      T.Tick_T_Send (((0, 0), 0));
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 9);
      Natural_Assert.Eq (T.Armed_State_Timeout_History.Get_Count, 5);
      Packed_Arm_Timeout_Assert.Eq (T.Armed_State_Timeout_History.Get (5), (Timeout => 1));
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 5);
      T.Tick_T_Send (((0, 0), 0));
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 11);
      Natural_Assert.Eq (T.Armed_State_Timeout_History.Get_Count, 6);
      Packed_Arm_Timeout_Assert.Eq (T.Armed_State_Timeout_History.Get (6), (Timeout => 0));
      Natural_Assert.Eq (T.Armed_State_History.Get_Count, 4);
      Packed_Arm_State_Assert.Eq (T.Armed_State_History.Get (4), (State => Unarmed));
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 6);
      Natural_Assert.Eq (T.Unarmed_Timeout_History.Get_Count, 1);

      -- Send command to set register:
      T.Command_T_Send (T.Commands.Write_Register ((Address => Register_1'Address, Value => 33)));
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 5);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (5), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Write_Register_Id, Status => Failure));

      -- Check data products:
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 11);

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 7);
      Natural_Assert.Eq (T.Rejected_Protected_Register_Write_History.Get_Count, 2);
      Register_Value_Assert.Eq (T.Rejected_Protected_Register_Write_History.Get (2), (Address => Register_1'Address, Value => 33));

      -- Check register value:
      Unsigned_32_Assert.Eq (Register_1, 17);

      -- OK now arm the system:
      T.Command_T_Send (T.Commands.Arm_Protected_Write ((Timeout => 3)));
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 6);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (6), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Arm_Protected_Write_Id, Status => Success));

      -- Check data products:
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 13);
      Natural_Assert.Eq (T.Armed_State_History.Get_Count, 5);
      Natural_Assert.Eq (T.Armed_State_Timeout_History.Get_Count, 7);
      Packed_Arm_State_Assert.Eq (T.Armed_State_History.Get (5), (State => Armed));
      Packed_Arm_Timeout_Assert.Eq (T.Armed_State_Timeout_History.Get (7), (Timeout => 3));

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 8);
      Natural_Assert.Eq (T.Armed_History.Get_Count, 3);
      Packed_Arm_Timeout_Assert.Eq (T.Armed_History.Get (3), (Timeout => 3));

      -- Send register read command
      T.Command_T_Send (T.Commands.Read_Register ((Address => Register_1'Address)));
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 7);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (7), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Read_Register_Id, Status => Success));

      -- Make sure unarm happened
      Natural_Assert.Eq (T.Armed_State_History.Get_Count, 6);
      Natural_Assert.Eq (T.Armed_State_Timeout_History.Get_Count, 8);
      Packed_Arm_State_Assert.Eq (T.Armed_State_History.Get (6), (State => Unarmed));
      Packed_Arm_Timeout_Assert.Eq (T.Armed_State_Timeout_History.Get (8), (Timeout => 0));

      -- Send command to set register:
      T.Command_T_Send (T.Commands.Write_Register ((Address => Register_1'Address, Value => 33)));
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 8);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (8), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Write_Register_Id, Status => Failure));

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 11);
      Natural_Assert.Eq (T.Rejected_Protected_Register_Write_History.Get_Count, 3);
      Register_Value_Assert.Eq (T.Rejected_Protected_Register_Write_History.Get (3), (Address => Register_1'Address, Value => 33));

      -- Check register value:
      Unsigned_32_Assert.Eq (Register_1, 17);

      -- OK now arm the system:
      T.Command_T_Send (T.Commands.Arm_Protected_Write ((Timeout => 3)));
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 9);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (9), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Arm_Protected_Write_Id, Status => Success));

      -- Check data products:
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 18);
      Natural_Assert.Eq (T.Armed_State_History.Get_Count, 7);
      Natural_Assert.Eq (T.Armed_State_Timeout_History.Get_Count, 9);
      Packed_Arm_State_Assert.Eq (T.Armed_State_History.Get (7), (State => Armed));
      Packed_Arm_Timeout_Assert.Eq (T.Armed_State_Timeout_History.Get (9), (Timeout => 3));

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 12);
      Natural_Assert.Eq (T.Armed_History.Get_Count, 4);
      Packed_Arm_Timeout_Assert.Eq (T.Armed_History.Get (4), (Timeout => 3));

      -- Send register dump command
      T.Command_T_Send (T.Commands.Dump_Registers ((Start_Address => Register_Array'Address, Num_Registers => 1)));
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 10);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (10), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Dump_Registers_Id, Status => Success));

      -- Make sure unarm happened
      Natural_Assert.Eq (T.Armed_State_History.Get_Count, 8);
      Natural_Assert.Eq (T.Armed_State_Timeout_History.Get_Count, 10);
      Packed_Arm_State_Assert.Eq (T.Armed_State_History.Get (8), (State => Unarmed));
      Packed_Arm_Timeout_Assert.Eq (T.Armed_State_Timeout_History.Get (10), (Timeout => 0));

      -- Send command to set register:
      T.Command_T_Send (T.Commands.Write_Register ((Address => Register_1'Address, Value => 33)));
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 11);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (11), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Write_Register_Id, Status => Failure));

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 15);
      Natural_Assert.Eq (T.Rejected_Protected_Register_Write_History.Get_Count, 4);
      Register_Value_Assert.Eq (T.Rejected_Protected_Register_Write_History.Get (4), (Address => Register_1'Address, Value => 33));

      -- Check register value:
      Unsigned_32_Assert.Eq (Register_1, 17);

      -- Call setup:
      T.Component_Instance.Set_Up;

      -- Make sure data products produced
      Natural_Assert.Eq (T.Armed_State_History.Get_Count, 9);
      Natural_Assert.Eq (T.Armed_State_Timeout_History.Get_Count, 11);
      Packed_Arm_State_Assert.Eq (T.Armed_State_History.Get (9), (State => Unarmed));
      Packed_Arm_Timeout_Assert.Eq (T.Armed_State_Timeout_History.Get (11), (Timeout => 0));
   end Test_Protected_Register_Write;

   -- This unit test makes sure the component can dump one register by command.
   overriding procedure Test_Nominal_Dump_One_Registers (Self : in out Instance) is
      T : Component.Register_Stuffer.Implementation.Tester.Instance_Access renames Self.Tester;
      Packet_Header_Length : constant Natural := Register_Dump_Packet_Header.Serialization.Serialized_Length;
      Packet_Header : constant Register_Dump_Packet_Header.T := (Start_Address => Register_Array'Address, Num_Registers => Register_Dump_Packet_Header.N_Registers'First);
   begin
      T.Command_T_Send (T.Commands.Dump_Registers (Packet_Header));
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 1);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (1), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Dump_Registers_Id, Status => Success));

      -- Check data products:
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Last_Register_Read_History.Get_Count, 1);
      Register_Value_Assert.Eq (T.Last_Register_Read_History.Get (1), (Address => Register_Array'Address, Value => 18));

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Registers_Dumped_History.Get_Count, 1);
      Register_Dump_Packet_Header_Assert.Eq (T.Registers_Dumped_History.Get (1), Packet_Header);

      -- Check packet:
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 1);

      -- Check packet length:
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get (1).Header.Buffer_Length, Packet_Header_Length + Packed_U32.Size_In_Bytes * 1);

      declare
         Packed_Register_Array : Register_Dump_Packet_Array.T;
      begin
         -- Packed Array is 0-indexed vs Normal is 1-indexed
         for I in Register_Array'Range loop
            Packed_Register_Array (I - 1) := (Value => Register_Array (I));
         end loop;

         -- Check packet contents:
         Register_Dump_Packet_Assert.Eq (
            T.Register_Packet_History.Get (1),
            (
               Header => Packet_Header,
               Buffer => Packed_Register_Array
            )
         );
      end;

   end Test_Nominal_Dump_One_Registers;

   -- This unit test makes sure the component can dump the maximum number of
   -- registers by command.
   overriding procedure Test_Nominal_Dump_Max_Registers (Self : in out Instance) is
      T : Component.Register_Stuffer.Implementation.Tester.Instance_Access renames Self.Tester;

      Large_Index_Array : array (1 .. Register_Dump_Packet_Header.N_Registers'Last) of Packed_U32.T_Le :=
         [for I in 1 .. Register_Dump_Packet_Header.N_Registers'Last => (Value => Interfaces.Unsigned_32 (I))];

      -- Create array using a loop and expression function ((packet_buffer_size - 20) / 8)
      Packet_Header_Length : constant Natural := Register_Dump_Packet_Header.Serialization.Serialized_Length;
      Packet_Header : constant Register_Dump_Packet_Header.T := (Start_Address => Large_Index_Array'Address, Num_Registers => Register_Dump_Packet_Header.N_Registers'Last);
   begin
      -- The Internal is 0-indexed, so we sub 1 to convert from 1->0
      T.Command_T_Send (T.Commands.Dump_Registers (Packet_Header));
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 1);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (1), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Dump_Registers_Id, Status => Success));

      -- Check data products:
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Last_Register_Read_History.Get_Count, 1);
      Register_Value_Assert.Eq (T.Last_Register_Read_History.Get (1),
         (
            Address => Large_Index_Array'Address + Storage_Offset ((Register_Dump_Packet_Header.N_Registers'Last - 1) * Packed_U32.Size_In_Bytes),
            Value => Large_Index_Array (Register_Dump_Packet_Header.N_Registers'Last).Value
         )
      );

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Registers_Dumped_History.Get_Count, 1);
      Register_Dump_Packet_Header_Assert.Eq (T.Registers_Dumped_History.Get (1), Packet_Header);

      -- Check packet:
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 1);

      -- Check packet length:
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get (1).Header.Buffer_Length, Packet_Header_Length + Packed_U32.Size_In_Bytes * Register_Dump_Packet_Header.N_Registers'Last);

      -- Check that the packet is truly full (this also needs to be cognizant of if the packet_buffer_size does not nicely fit with the bounds)
      -- So long as its within Packet_Buffer_Size-3..Packet_Buffer_Size. It has properly filled the Packet_Buffer.

      Natural_Assert.Ge (T.Packet_T_Recv_Sync_History.Get (1).Header.Buffer_Length, Packet_Types.Packet_Buffer_Length_Type'Last - 3);
      Natural_Assert.Le (T.Packet_T_Recv_Sync_History.Get (1).Header.Buffer_Length, Packet_Types.Packet_Buffer_Length_Type'Last);

      -- Check the packet
      declare
         Packed_Register_Array : Register_Dump_Packet_Array.T;
      begin
         -- Packed Array is 0-indexed vs Normal is 1-indexed
         for I in Large_Index_Array'Range loop
            Packed_Register_Array (I - 1) := (Value => Large_Index_Array (I).Value);
         end loop;

         -- Check packet contents:
         Register_Dump_Packet_Assert.Eq (
            T.Register_Packet_History.Get (1),
            (
               Header => Packet_Header,
               Buffer => Packed_Register_Array
            )
         );
      end;

   end Test_Nominal_Dump_Max_Registers;

      -- This unit test makes sure the component returns a packet containing only the
   -- headers.
   overriding procedure Test_Dump_Four_Registers (Self : in out Instance) is
      T : Component.Register_Stuffer.Implementation.Tester.Instance_Access renames Self.Tester;
      Packet_Header_Length : constant Natural := Register_Dump_Packet_Header.Serialization.Serialized_Length;
      Packet_Header : constant Register_Dump_Packet_Header.T := (Start_Address => Register_Array'Address, Num_Registers => 4);
   begin
      T.Command_T_Send (T.Commands.Dump_Registers (Packet_Header));
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 1);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (1), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Dump_Registers_Id, Status => Success));

      -- Check data products:
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Last_Register_Read_History.Get_Count, 1);
      -- 3 since the 1st register is Base + 0*Packed_U32.Size_In_Bytes so the 4th register is Base + 3*Packed_U32.Size_In_Bytes
      Register_Value_Assert.Eq (T.Last_Register_Read_History.Get (1), (Address => Register_Array'Address + Storage_Offset (3 * Packed_U32.Size_In_Bytes), Value => 1428));

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Registers_Dumped_History.Get_Count, 1);
      Register_Dump_Packet_Header_Assert.Eq (T.Registers_Dumped_History.Get (1), Packet_Header);

      -- Check packet:
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 1);

      -- Check packet length:
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get (1).Header.Buffer_Length, Packet_Header_Length + Packed_U32.Size_In_Bytes * 4);

      declare
         Packed_Register_Array : Register_Dump_Packet_Array.T;
      begin
         -- Packed Array is 0-indexed vs Normal is 1-indexed
         for I in Register_Array'Range loop
            Packed_Register_Array (I - 1) := (Value => Register_Array (I));
         end loop;

         -- Check packet contents:
         Register_Dump_Packet_Assert.Eq (
            T.Register_Packet_History.Get (1),
            (
               Header => Packet_Header,
               Buffer => Packed_Register_Array
            )
         );
      end;
   end Test_Dump_Four_Registers;
end Register_Stuffer_Tests.Implementation;
