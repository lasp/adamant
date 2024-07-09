--------------------------------------------------------------------------------
-- Register_Stuffer Tests Body
--------------------------------------------------------------------------------

with Basic_Assertions; use Basic_Assertions;
with Invalid_Command_Info.Assertion; use Invalid_Command_Info.Assertion;
with Command_Response.Assertion; use Command_Response.Assertion;
with Command_Enums; use Command_Enums.Command_Response_Status;
with Command;
with Interfaces;
with Register_Value.Assertion; use Register_Value.Assertion;
with Packed_Address.Assertion; use Packed_Address.Assertion;
with Packed_Arm_State.Assertion; use Packed_Arm_State.Assertion;
with Packed_Arm_Timeout.Assertion; use Packed_Arm_Timeout.Assertion;
with Command_Protector_Enums;
with Ada.Unchecked_Conversion;
with System;

package body Register_Stuffer_Tests.Implementation is

   -------------------------------------------------------------------------
   -- Globals:
   -------------------------------------------------------------------------
   Register_1 : Interfaces.Unsigned_32 := 18;
   Register_2 : Interfaces.Unsigned_32 := 333;

   -------------------------------------------------------------------------
   -- Fixtures:
   -------------------------------------------------------------------------

   overriding procedure Set_Up_Test (Self : in out Instance) is
   begin
      -- Reset reg values:
      Register_1 := 18;
      Register_2 := 333;

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

      -- Check register values remained unaltered:
      Unsigned_32_Assert.Eq (Register_1, 18);
      Unsigned_32_Assert.Eq (Register_2, 333);
   end Test_Bad_Address;

   overriding procedure Test_Invalid_Command (Self : in out Instance) is
      T : Component.Register_Stuffer.Implementation.Tester.Instance_Access renames Self.Tester;
      Cmd : Command.T := T.Commands.Write_Register ((Address => Register_1'Address, Value => 17));
   begin
      -- Make the command invalid by modifying its length.
      Cmd.Header.Arg_Buffer_Length := 22;

      -- Send bad command and expect bad response:
      T.Command_T_Send (Cmd);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 1);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (1), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Write_Register_Id, Status => Length_Error));

      -- Make sure some events were thrown:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Invalid_Command_Received_History.Get_Count, 1);
      Invalid_Command_Info_Assert.Eq (T.Invalid_Command_Received_History.Get (1), (Id => T.Commands.Get_Write_Register_Id, Errant_Field_Number => Interfaces.Unsigned_32'Last, Errant_Field => [0, 0, 0, 0, 0, 0, 0, 22]));
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

      -- Call setup:
      T.Component_Instance.Set_Up;

      -- Make sure data products produced
      Natural_Assert.Eq (T.Armed_State_History.Get_Count, 7);
      Natural_Assert.Eq (T.Armed_State_Timeout_History.Get_Count, 9);
      Packed_Arm_State_Assert.Eq (T.Armed_State_History.Get (7), (State => Unarmed));
      Packed_Arm_Timeout_Assert.Eq (T.Armed_State_Timeout_History.Get (9), (Timeout => 0));
   end Test_Protected_Register_Write;

end Register_Stuffer_Tests.Implementation;
