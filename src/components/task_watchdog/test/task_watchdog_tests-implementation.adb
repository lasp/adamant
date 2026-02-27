--------------------------------------------------------------------------------
-- Task_Watchdog Tests Body
--------------------------------------------------------------------------------

with Ada.Text_IO; use Ada.Text_IO;
with Basic_Assertions; use Basic_Assertions;
with Command_Response.Assertion; use Command_Response.Assertion;
with Command_Enums; use Command_Enums.Command_Response_Status;
with Invalid_Command_Info.Assertion; use Invalid_Command_Info.Assertion;
with Command;
with Interfaces;
with Test_Assembly_Task_Watchdog_List;
with Tick;
with Pet;
with Connector_Types; use Connector_Types;
with Data_Product.Assertion; use Data_Product.Assertion;
with Data_Product_Types;
with Packed_U16;
with Packed_Byte;
with Fault_Types;
with Fault.Assertion; use Fault.Assertion;
with Packed_Connector_Index;
with Task_Watchdog_Types; use Task_Watchdog_Types;
with Task_Watchdog_Enums; use Task_Watchdog_Enums;

package body Task_Watchdog_Tests.Implementation is

   -------------------------------------------------------------------------
   -- Fixtures:
   -------------------------------------------------------------------------

   overriding procedure Set_Up_Test (Self : in out Instance) is
   begin
      -- Allocate heap memory to component:
      Self.Tester.Init_Base (Pet_T_Recv_Sync_Count => 3);

      -- Make necessary connections between tester and component:
      Self.Tester.Connect;

      -- Call component init here.
      Self.Tester.Component_Instance.Init (Task_Watchdog_Entry_Init_List => Test_Assembly_Task_Watchdog_List.Task_Watchdog_Entry_Init_List);

      -- Call the component set up method that the assembly would normally call.
      Self.Tester.Component_Instance.Set_Up;
   end Set_Up_Test;

   overriding procedure Tear_Down_Test (Self : in out Instance) is
   begin
      -- Free component heap:
      Self.Tester.Final_Base;
   end Tear_Down_Test;

   -- Global init Data product count so that its easier to maintain tests, plus the two states Data products.
   Init_Dp_Count : constant Natural := Test_Assembly_Task_Watchdog_List.Task_Watchdog_Entry_Init_List'Length + 2;

   -- Helper function to build the limit DPs to compare to the output
   function Test_Create_Dp_Limit_Type (Id : in Data_Product_Types.Data_Product_Id; Limit_Value : in Interfaces.Unsigned_16) return Data_Product.T is
      Dp : Data_Product.T := (Header => (Id => Id, Time => (0, 0), Buffer_Length => Packed_U16.Serialization.Serialized_Length), Buffer => [others => 0]);
   begin
      Dp.Buffer (Dp.Buffer'First .. Dp.Buffer'First + Packed_U16.Serialization.Serialized_Length - 1) := Packed_U16.Serialization.To_Byte_Array ((Value => Limit_Value));
      return Dp;
   end Test_Create_Dp_Limit_Type;

   -- Helper function to build the limit DPs to compare to the output
   function Test_Create_Fault (Id : in Fault_Types.Fault_Id; Idx : in Connector_Types.Connector_Index_Type) return Fault.T is
      New_Fault : Fault.T := (Header => (Id => Id, Time => (0, 0), Param_Buffer_Length => Packed_Connector_Index.Serialization.Serialized_Length), Param_Buffer => [others => 16#FF#]);
   begin
      New_Fault.Param_Buffer (New_Fault.Param_Buffer'First .. New_Fault.Param_Buffer'First + Packed_Connector_Index.Serialization.Serialized_Length - 1) := Packed_Connector_Index.Serialization.To_Byte_Array ((Index => Idx));
      return New_Fault;
   end Test_Create_Fault;

   -------------------------------------------------------------------------
   -- Tests:
   -------------------------------------------------------------------------

   overriding procedure Test_Received_Pet (Self : in out Instance) is
      T : Component.Task_Watchdog.Implementation.Tester.Instance_Access renames Self.Tester;
      Input_Tick : constant Tick.T := ((0, 0), 0);
      Input_Pet : constant Pet.T := (Count => 0);
   begin
      Put_Line ("");
      Put_Line ("----------------------------------");
      Put_Line ("Testing Received Pet:");
      Put_Line ("----------------------------------");

      -- Make sure we get our data products out first (1 per item in the list + the states)
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, Init_Dp_Count);
      -- Check each data product was sent at startup of the component.
      Natural_Assert.Eq (Natural (T.Data_Product_T_Recv_Sync_History.Get (1).Header.Id), 0);
      Natural_Assert.Eq (Natural (T.Data_Product_T_Recv_Sync_History.Get (2).Header.Id), 1);
      Natural_Assert.Eq (Natural (T.Data_Product_T_Recv_Sync_History.Get (3).Header.Id), 2);
      Natural_Assert.Eq (Natural (T.Data_Product_T_Recv_Sync_History.Get (4).Header.Id), 3);
      Natural_Assert.Eq (Natural (T.Data_Product_T_Recv_Sync_History.Get (5).Header.Id), 4);
      Data_Product_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get (3), Test_Create_Dp_Limit_Type (2, 2));
      Data_Product_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get (4), Test_Create_Dp_Limit_Type (3, 3));
      Data_Product_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get (5), Test_Create_Dp_Limit_Type (4, 1));

      -- Good tick where all checks should pass and the downstream petter should be called.
      -- Note: The first petter fails here with a limit of 1, but is disabled so we don't see action taken.
      T.Tick_T_Send (Input_Tick);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 0);
      Natural_Assert.Eq (T.Fault_T_Recv_Sync_History.Get_Count, 0);
      Natural_Assert.Eq (T.Pet_T_Recv_Sync_History.Get_Count, 1);

      -- Trip our warning case
      T.Tick_T_Send (Input_Tick);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 0);
      Natural_Assert.Eq (T.Pet_T_Recv_Sync_History.Get_Count, 2);
      T.Tick_T_Send (Input_Tick);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Component_Exceeded_Pet_Limit_History.Get_Count, 1);
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, Init_Dp_Count);
      Natural_Assert.Eq (T.Pet_T_Recv_Sync_History.Get_Count, 3);

      -- Now send a few pets to reset the counts
      T.Pet_T_Send (Connector_Count_Type (1), Input_Pet);
      T.Pet_T_Send (Connector_Count_Type (2), Input_Pet);
      T.Pet_T_Send (Connector_Count_Type (3), Input_Pet);

      -- And make sure we go back to what it looks like for the first tick
      T.Tick_T_Send (Input_Tick);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, Init_Dp_Count);
      Natural_Assert.Eq (T.Pet_T_Recv_Sync_History.Get_Count, 4);

      -- Again force the warning case
      T.Tick_T_Send (Input_Tick);
      T.Tick_T_Send (Input_Tick);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Component_Exceeded_Pet_Limit_History.Get_Count, 2);
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, Init_Dp_Count);
      Natural_Assert.Eq (T.Pet_T_Recv_Sync_History.Get_Count, 6);

      -- Now test that we don't get an event again from our warning case
      T.Pet_T_Send (Connector_Count_Type (2), Input_Pet);
      T.Tick_T_Send (Input_Tick);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Component_Exceeded_Pet_Limit_History.Get_Count, 2);
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, Init_Dp_Count);
      Natural_Assert.Eq (T.Pet_T_Recv_Sync_History.Get_Count, 7);

      -- Now send a few more to trip the fault case, this one is also critical so we should not get a pet out
      T.Tick_T_Send (Input_Tick);
      Natural_Assert.Eq (T.Pet_T_Recv_Sync_History.Get_Count, 8);
      T.Tick_T_Send (Input_Tick);
      Natural_Assert.Eq (T.Pet_T_Recv_Sync_History.Get_Count, 9);
      Natural_Assert.Eq (T.Fault_T_Recv_Sync_History.Get_Count, 0);
      T.Tick_T_Send (Input_Tick);
      Natural_Assert.Eq (T.Fault_T_Recv_Sync_History.Get_Count, 1);
      Fault_Assert.Eq (T.Fault_T_Recv_Sync_History.Get (1), Test_Create_Fault (5, 2));
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 4);
      Natural_Assert.Eq (T.Component_Exceeded_Pet_Limit_History.Get_Count, 3);
      Natural_Assert.Eq (T.Critical_Task_Not_Petting_History.Get_Count, 1);
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, Init_Dp_Count);
      Natural_Assert.Eq (T.Pet_T_Recv_Sync_History.Get_Count, 9);

      -- One more time to make sure the logic works for this connector as well.
      T.Tick_T_Send (Input_Tick);
      Natural_Assert.Eq (T.Fault_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Component_Exceeded_Pet_Limit_History.Get_Count, 3);
      Natural_Assert.Eq (T.Critical_Task_Not_Petting_History.Get_Count, 2);
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, Init_Dp_Count);
      Natural_Assert.Eq (T.Pet_T_Recv_Sync_History.Get_Count, 9);

      -- Reset for the next test
      T.Pet_T_Send (Connector_Count_Type (1), Input_Pet);
      T.Pet_T_Send (Connector_Count_Type (2), Input_Pet);
      T.Pet_T_Send (Connector_Count_Type (3), Input_Pet);

   end Test_Received_Pet;

   overriding procedure Test_Watchdog_Petter_Check_Command (Self : in out Instance) is
      T : Component.Task_Watchdog.Implementation.Tester.Instance_Access renames Self.Tester;
      Input_Tick : constant Tick.T := ((0, 0), 0);
      Input_Pet : constant Pet.T := (Count => 0);
      Test_State_Data_Product : Data_Product.T := (Header => (Id => 0, Time => (0, 0), Buffer_Length => Packed_Byte.Serialization.Serialized_Length), Buffer => [others => 16#FF#]);
   begin
      Put_Line ("");
      Put_Line ("----------------------------------");
      Put_Line ("Testing Watchdog Component State Command:");
      Put_Line ("----------------------------------");

      -- Make sure we get our data products out first 1 per item plus 2
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, Init_Dp_Count);
      Natural_Assert.Eq (Natural (T.Data_Product_T_Recv_Sync_History.Get (1).Header.Id), 0);
      Natural_Assert.Eq (Natural (T.Data_Product_T_Recv_Sync_History.Get (2).Header.Id), 1);
      Natural_Assert.Eq (Natural (T.Data_Product_T_Recv_Sync_History.Get (3).Header.Id), 2);
      Natural_Assert.Eq (Natural (T.Data_Product_T_Recv_Sync_History.Get (4).Header.Id), 3);
      Natural_Assert.Eq (Natural (T.Data_Product_T_Recv_Sync_History.Get (5).Header.Id), 4);
      Data_Product_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get (3), Test_Create_Dp_Limit_Type (2, 2));
      Data_Product_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get (4), Test_Create_Dp_Limit_Type (3, 3));
      Data_Product_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get (5), Test_Create_Dp_Limit_Type (4, 1));
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 0);
      Natural_Assert.Eq (T.Fault_T_Recv_Sync_History.Get_Count, 0);

      -- First time around make sure that there are no errors
      T.Tick_T_Send (Input_Tick);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 0);
      Natural_Assert.Eq (T.Pet_T_Recv_Sync_History.Get_Count, 1);

      -- The next time around we should get an error from one of our connectors, but the first one is not critical.
      -- This will make sure that we are enabled for checking the pets
      T.Tick_T_Send (Input_Tick);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 0);
      Natural_Assert.Eq (T.Pet_T_Recv_Sync_History.Get_Count, 2);
      T.Tick_T_Send (Input_Tick);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Component_Exceeded_Pet_Limit_History.Get_Count, 1);
      Natural_Assert.Eq (Natural (T.Component_Exceeded_Pet_Limit_History.Get (1).Index), 1);
      Natural_Assert.Eq (T.Pet_T_Recv_Sync_History.Get_Count, 3);
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, Init_Dp_Count);

      -- Now reset with a pet and set the component checking state to disabled.
      T.Pet_T_Send (Connector_Count_Type (1), Input_Pet);
      T.Pet_T_Send (Connector_Count_Type (2), Input_Pet);
      T.Pet_T_Send (Connector_Count_Type (3), Input_Pet);

      T.Command_T_Send (T.Commands.Disable_Watchdog_Pet_Checks);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 1);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (1), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Disable_Watchdog_Pet_Checks_Id, Status => Success));
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Watchdog_Pet_Checks_Disabled_History.Get_Count, 1);
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, Init_Dp_Count + 1);
      Test_State_Data_Product.Buffer (0) := 0;
      Data_Product_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get (Init_Dp_Count + 1), Test_State_Data_Product);

      -- Now send several ticks and make sure we get no errors but always get pets
      T.Tick_T_Send (Input_Tick);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Pet_T_Recv_Sync_History.Get_Count, 4);
      T.Tick_T_Send (Input_Tick);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Pet_T_Recv_Sync_History.Get_Count, 5);
      T.Tick_T_Send (Input_Tick);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Pet_T_Recv_Sync_History.Get_Count, 6);
      T.Tick_T_Send (Input_Tick);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Pet_T_Recv_Sync_History.Get_Count, 7);
      T.Tick_T_Send (Input_Tick);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Pet_T_Recv_Sync_History.Get_Count, 8);

      -- Now go back to enabled and make sure we start checking again.
      T.Command_T_Send (T.Commands.Enable_Watchdog_Pet_Checks);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 2);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (2), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Enable_Watchdog_Pet_Checks_Id, Status => Success));
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 3);
      Natural_Assert.Eq (T.Watchdog_Pet_Checks_Enabled_History.Get_Count, 1);
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, Init_Dp_Count + 2);
      Test_State_Data_Product.Buffer (0) := 1;
      Data_Product_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get (Init_Dp_Count + 2), Test_State_Data_Product);

      -- Make sure things work again with the component enabled.
      T.Tick_T_Send (Input_Tick);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 3);
      Natural_Assert.Eq (T.Pet_T_Recv_Sync_History.Get_Count, 9);
      T.Tick_T_Send (Input_Tick);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 3);
      Natural_Assert.Eq (T.Pet_T_Recv_Sync_History.Get_Count, 10);
      -- Warning petter tripped
      T.Tick_T_Send (Input_Tick);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 4);
      Natural_Assert.Eq (T.Component_Exceeded_Pet_Limit_History.Get_Count, 2);
      Natural_Assert.Eq (Natural (T.Component_Exceeded_Pet_Limit_History.Get (2).Index), 1);
      Natural_Assert.Eq (T.Pet_T_Recv_Sync_History.Get_Count, 11);
      -- Now get the critical error trip
      T.Tick_T_Send (Input_Tick);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 6);
      Natural_Assert.Eq (T.Component_Exceeded_Pet_Limit_History.Get_Count, 3);
      Natural_Assert.Eq (Natural (T.Component_Exceeded_Pet_Limit_History.Get (3).Index), 2);
      Natural_Assert.Eq (T.Critical_Task_Not_Petting_History.Get_Count, 1);
      Natural_Assert.Eq (Natural (T.Critical_Task_Not_Petting_History.Get (1).Index), 2);
      Natural_Assert.Eq (T.Fault_T_Recv_Sync_History.Get_Count, 1);
      Fault_Assert.Eq (T.Fault_T_Recv_Sync_History.Get (1), Test_Create_Fault (5, 2));
      Natural_Assert.Eq (T.Pet_T_Recv_Sync_History.Get_Count, 11);

      -- At this point nothing should occur except events for critical errors
      T.Tick_T_Send (Input_Tick);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 7);
      Natural_Assert.Eq (T.Critical_Task_Not_Petting_History.Get_Count, 2);
      Natural_Assert.Eq (Natural (T.Critical_Task_Not_Petting_History.Get (2).Index), 2);
      Natural_Assert.Eq (T.Fault_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Pet_T_Recv_Sync_History.Get_Count, 11);
      T.Tick_T_Send (Input_Tick);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 8);
      Natural_Assert.Eq (T.Critical_Task_Not_Petting_History.Get_Count, 3);
      Natural_Assert.Eq (Natural (T.Critical_Task_Not_Petting_History.Get (3).Index), 2);
      Natural_Assert.Eq (T.Fault_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Pet_T_Recv_Sync_History.Get_Count, 11);
      T.Tick_T_Send (Input_Tick);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 9);
      Natural_Assert.Eq (T.Critical_Task_Not_Petting_History.Get_Count, 4);
      Natural_Assert.Eq (Natural (T.Critical_Task_Not_Petting_History.Get (4).Index), 2);
      Natural_Assert.Eq (T.Fault_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Pet_T_Recv_Sync_History.Get_Count, 11);

      -- Reset for the next test
      T.Pet_T_Send (Connector_Count_Type (1), Input_Pet);
      T.Pet_T_Send (Connector_Count_Type (2), Input_Pet);
      T.Pet_T_Send (Connector_Count_Type (3), Input_Pet);

   end Test_Watchdog_Petter_Check_Command;

   overriding procedure Test_Watchdog_Action_Command (Self : in out Instance) is
      T : Component.Task_Watchdog.Implementation.Tester.Instance_Access renames Self.Tester;
      Input_Tick : constant Tick.T := ((0, 0), 0);
      Input_Pet : constant Pet.T := (Count => 0);
      Test_Action_Data_Product : Data_Product.T := (Header => (Id => 1, Time => (0, 0), Buffer_Length => Packed_Byte.Serialization.Serialized_Length), Buffer => [others => 0]);
   begin
      Put_Line ("");
      Put_Line ("----------------------------------");
      Put_Line ("Testing Watchdog Entity State Commands:");
      Put_Line ("----------------------------------");

      -- Make sure we get our data products out first 1 per item plus 2
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, Init_Dp_Count);
      Natural_Assert.Eq (Natural (T.Data_Product_T_Recv_Sync_History.Get (1).Header.Id), 0);
      Natural_Assert.Eq (Natural (T.Data_Product_T_Recv_Sync_History.Get (2).Header.Id), 1);
      Natural_Assert.Eq (Natural (T.Data_Product_T_Recv_Sync_History.Get (3).Header.Id), 2);
      Natural_Assert.Eq (Natural (T.Data_Product_T_Recv_Sync_History.Get (4).Header.Id), 3);
      Natural_Assert.Eq (Natural (T.Data_Product_T_Recv_Sync_History.Get (5).Header.Id), 4);
      Data_Product_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get (3), Test_Create_Dp_Limit_Type (2, 2));
      Data_Product_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get (4), Test_Create_Dp_Limit_Type (3, 3));
      Data_Product_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get (5), Test_Create_Dp_Limit_Type (4, 1));
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 0);
      Natural_Assert.Eq (T.Fault_T_Recv_Sync_History.Get_Count, 0);

      -- -- First time around make sure that there are no errors
      T.Tick_T_Send (Input_Tick);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 0);
      Natural_Assert.Eq (T.Fault_T_Recv_Sync_History.Get_Count, 0);
      Natural_Assert.Eq (T.Pet_T_Recv_Sync_History.Get_Count, 1);

      -- The next time around we should get an error from one of our connectors, but the first one is not critical.
      -- This will make sure that we are enabled for checking the pets
      T.Tick_T_Send (Input_Tick);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 0);
      Natural_Assert.Eq (T.Pet_T_Recv_Sync_History.Get_Count, 2);
      T.Tick_T_Send (Input_Tick);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Component_Exceeded_Pet_Limit_History.Get_Count, 1);
      Natural_Assert.Eq (Natural (T.Component_Exceeded_Pet_Limit_History.Get (1).Index), 1);
      Natural_Assert.Eq (T.Pet_T_Recv_Sync_History.Get_Count, 3);

      -- Now reset with a petters to command the first one for a different action
      T.Pet_T_Send (Connector_Count_Type (1), Input_Pet);
      T.Pet_T_Send (Connector_Count_Type (2), Input_Pet);
      T.Pet_T_Send (Connector_Count_Type (3), Input_Pet);

      -- First change is to promote to fault on one that is appropriate but was not defined as fault to start
      T.Command_T_Send (T.Commands.Set_Watchdog_Action ((Index => 3, New_Action => Watchdog_Action_State.Error_Fault)));
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 1);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (1), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Set_Watchdog_Action_Id, Status => Success));
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Watchdog_Action_Set_History.Get_Count, 1);
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, Init_Dp_Count + 1);
      Test_Action_Data_Product.Buffer (0) := 2#0110_1000#; -- 00 - disabled, 01- warn, 10 - fault
      Data_Product_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get (Init_Dp_Count + 1), Test_Action_Data_Product);

      -- Now send several ticks and make sure we now get a fault with our new updated petter
      T.Tick_T_Send (Input_Tick);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Pet_T_Recv_Sync_History.Get_Count, 4);
      Natural_Assert.Eq (T.Fault_T_Recv_Sync_History.Get_Count, 0);
      T.Tick_T_Send (Input_Tick);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 3);
      Natural_Assert.Eq (T.Component_Exceeded_Pet_Limit_History.Get_Count, 2);
      Natural_Assert.Eq (Natural (T.Component_Exceeded_Pet_Limit_History.Get (2).Index), 3);
      Natural_Assert.Eq (T.Fault_T_Recv_Sync_History.Get_Count, 1);
      Fault_Assert.Eq (T.Fault_T_Recv_Sync_History.Get (1), Test_Create_Fault (2, 3));
      Natural_Assert.Eq (T.Pet_T_Recv_Sync_History.Get_Count, 5);

      -- No more errors from that one even without a pet to reset, but we do get the warn here
      T.Tick_T_Send (Input_Tick);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 4);
      Natural_Assert.Eq (T.Component_Exceeded_Pet_Limit_History.Get_Count, 3);
      Natural_Assert.Eq (Natural (T.Component_Exceeded_Pet_Limit_History.Get (3).Index), 1);
      Natural_Assert.Eq (T.Pet_T_Recv_Sync_History.Get_Count, 6);
      Natural_Assert.Eq (T.Fault_T_Recv_Sync_History.Get_Count, 1);

      -- Get the critical one just to know its working before disabling it
      T.Tick_T_Send (Input_Tick);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 6);
      Natural_Assert.Eq (T.Component_Exceeded_Pet_Limit_History.Get_Count, 4);
      Natural_Assert.Eq (Natural (T.Component_Exceeded_Pet_Limit_History.Get (4).Index), 2);
      Natural_Assert.Eq (T.Critical_Task_Not_Petting_History.Get_Count, 1);
      Natural_Assert.Eq (Natural (T.Critical_Task_Not_Petting_History.Get (1).Index), 2);
      Natural_Assert.Eq (T.Pet_T_Recv_Sync_History.Get_Count, 6);
      Natural_Assert.Eq (T.Fault_T_Recv_Sync_History.Get_Count, 2);
      Fault_Assert.Eq (T.Fault_T_Recv_Sync_History.Get (2), Test_Create_Fault (5, 2));
      T.Tick_T_Send (Input_Tick);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 7);
      Natural_Assert.Eq (T.Critical_Task_Not_Petting_History.Get_Count, 2);
      Natural_Assert.Eq (T.Pet_T_Recv_Sync_History.Get_Count, 6);
      Natural_Assert.Eq (T.Fault_T_Recv_Sync_History.Get_Count, 2);
      T.Tick_T_Send (Input_Tick);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 8);
      Natural_Assert.Eq (T.Critical_Task_Not_Petting_History.Get_Count, 3);
      Natural_Assert.Eq (T.Pet_T_Recv_Sync_History.Get_Count, 6);
      Natural_Assert.Eq (T.Fault_T_Recv_Sync_History.Get_Count, 2);

      -- Change them all to disabled
      T.Command_T_Send (T.Commands.Set_Watchdog_Action ((Index => 3, New_Action => Watchdog_Action_State.Disabled)));
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 2);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (2), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Set_Watchdog_Action_Id, Status => Success));
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 9);
      Natural_Assert.Eq (T.Watchdog_Action_Set_History.Get_Count, 2);
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, Init_Dp_Count + 2);
      Test_Action_Data_Product.Buffer (0) := 2#0110_0000#; -- 00 - disabled, 01- warn, 10 - fault
      Data_Product_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get (Init_Dp_Count + 2), Test_Action_Data_Product);

      T.Command_T_Send (T.Commands.Set_Watchdog_Action ((Index => 2, New_Action => Watchdog_Action_State.Disabled)));
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 3);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (3), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Set_Watchdog_Action_Id, Status => Success));
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 10);
      Natural_Assert.Eq (T.Watchdog_Action_Set_History.Get_Count, 3);
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, Init_Dp_Count + 3);
      Test_Action_Data_Product.Buffer (0) := 2#0100_0000#; -- 00 - disabled, 01- warn, 10 - fault
      Data_Product_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get (Init_Dp_Count + 3), Test_Action_Data_Product);

      T.Command_T_Send (T.Commands.Set_Watchdog_Action ((Index => 1, New_Action => Watchdog_Action_State.Disabled)));
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 4);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (4), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Set_Watchdog_Action_Id, Status => Success));
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 11);
      Natural_Assert.Eq (T.Watchdog_Action_Set_History.Get_Count, 4);
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, Init_Dp_Count + 4);
      Test_Action_Data_Product.Buffer (0) := 2#0000_0000#; -- 00 - disabled, 01- warn, 10 - fault
      Data_Product_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get (Init_Dp_Count + 4), Test_Action_Data_Product);

      -- Pet to reset everything
      T.Pet_T_Send (Connector_Count_Type (1), Input_Pet);
      T.Pet_T_Send (Connector_Count_Type (2), Input_Pet);
      T.Pet_T_Send (Connector_Count_Type (3), Input_Pet);

      -- Now throw at least 5 ticks to make sure no faults are thrown.
      T.Tick_T_Send (Input_Tick);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 11);
      Natural_Assert.Eq (T.Pet_T_Recv_Sync_History.Get_Count, 7);
      Natural_Assert.Eq (T.Fault_T_Recv_Sync_History.Get_Count, 2);
      T.Tick_T_Send (Input_Tick);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 11);
      Natural_Assert.Eq (T.Pet_T_Recv_Sync_History.Get_Count, 8);
      Natural_Assert.Eq (T.Fault_T_Recv_Sync_History.Get_Count, 2);
      T.Tick_T_Send (Input_Tick);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 11);
      Natural_Assert.Eq (T.Pet_T_Recv_Sync_History.Get_Count, 9);
      Natural_Assert.Eq (T.Fault_T_Recv_Sync_History.Get_Count, 2);
      T.Tick_T_Send (Input_Tick);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 11);
      Natural_Assert.Eq (T.Pet_T_Recv_Sync_History.Get_Count, 10);
      Natural_Assert.Eq (T.Fault_T_Recv_Sync_History.Get_Count, 2);

      -- Attempt to change to an invalid action
      T.Command_T_Send (T.Commands.Set_Watchdog_Action ((Index => 1, New_Action => Watchdog_Action_State.Error_Fault)));
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 5);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (5), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Set_Watchdog_Action_Id, Status => Failure));
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 12);
      Natural_Assert.Eq (T.Watchdog_Action_Change_Invalid_Transition_To_Fault_History.Get_Count, 1);
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, Init_Dp_Count + 4);

      -- Now an invalid index
      T.Command_T_Send (T.Commands.Set_Watchdog_Action ((Index => 4, New_Action => Watchdog_Action_State.Warn)));
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 6);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (6), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Set_Watchdog_Action_Id, Status => Failure));
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 13);
      Natural_Assert.Eq (T.Watchdog_Action_Change_Index_Out_Of_Range_History.Get_Count, 1);
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, Init_Dp_Count + 4);

      -- Reset for the next test
      T.Pet_T_Send (Connector_Count_Type (1), Input_Pet);
      T.Pet_T_Send (Connector_Count_Type (2), Input_Pet);
      T.Pet_T_Send (Connector_Count_Type (3), Input_Pet);

   end Test_Watchdog_Action_Command;

   overriding procedure Test_Watchdog_Limit_Command (Self : in out Instance) is
      T : Component.Task_Watchdog.Implementation.Tester.Instance_Access renames Self.Tester;
      Input_Tick : constant Tick.T := ((0, 0), 0);
      Input_Pet : constant Pet.T := (Count => 0);
      Test_Limit_Data_Product : Data_Product.T := (Header => (Id => 2, Time => (0, 0), Buffer_Length => Packed_U16.Serialization.Serialized_Length), Buffer => [others => 0]);
   begin
      Put_Line ("");
      Put_Line ("----------------------------------");
      Put_Line ("Testing Watchdog Limit Change Command:");
      Put_Line ("----------------------------------");

      -- Make sure we get our data products out first 1 per item plus 2
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, Init_Dp_Count);
      Natural_Assert.Eq (Natural (T.Data_Product_T_Recv_Sync_History.Get (1).Header.Id), 0);
      Natural_Assert.Eq (Natural (T.Data_Product_T_Recv_Sync_History.Get (2).Header.Id), 1);
      Natural_Assert.Eq (Natural (T.Data_Product_T_Recv_Sync_History.Get (3).Header.Id), 2);
      Natural_Assert.Eq (Natural (T.Data_Product_T_Recv_Sync_History.Get (4).Header.Id), 3);
      Natural_Assert.Eq (Natural (T.Data_Product_T_Recv_Sync_History.Get (5).Header.Id), 4);
      Data_Product_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get (3), Test_Create_Dp_Limit_Type (2, 2));
      Data_Product_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get (4), Test_Create_Dp_Limit_Type (3, 3));
      Data_Product_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get (5), Test_Create_Dp_Limit_Type (4, 1));
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 0);
      Natural_Assert.Eq (T.Fault_T_Recv_Sync_History.Get_Count, 0);

      -- Set up new limits testing
      T.Tick_T_Send (Input_Tick);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 0);
      Natural_Assert.Eq (T.Pet_T_Recv_Sync_History.Get_Count, 1);
      T.Tick_T_Send (Input_Tick);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 0);
      Natural_Assert.Eq (T.Pet_T_Recv_Sync_History.Get_Count, 2);
      T.Tick_T_Send (Input_Tick);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Component_Exceeded_Pet_Limit_History.Get_Count, 1);
      Natural_Assert.Eq (Natural (T.Component_Exceeded_Pet_Limit_History.Get (1).Index), 1);
      Natural_Assert.Eq (T.Pet_T_Recv_Sync_History.Get_Count, 3);

      -- New limit for the warning case
      T.Command_T_Send (T.Commands.Set_Watchdog_Limit ((Index => 1, New_Limit => 5)));
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 1);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (1), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Set_Watchdog_Limit_Id, Status => Success));
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Watchdog_Limit_Set_History.Get_Count, 1);
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, Init_Dp_Count + 1);
      Test_Limit_Data_Product.Buffer (0 .. 1) := [0, 5];
      Data_Product_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get (Init_Dp_Count + 1), Test_Limit_Data_Product);

      -- Reset the critical petter
      T.Pet_T_Send (Connector_Count_Type (2), Input_Pet);

      T.Tick_T_Send (Input_Tick);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Pet_T_Recv_Sync_History.Get_Count, 4);
      T.Tick_T_Send (Input_Tick);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Pet_T_Recv_Sync_History.Get_Count, 5);
      T.Tick_T_Send (Input_Tick);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Pet_T_Recv_Sync_History.Get_Count, 6);

      T.Pet_T_Send (Connector_Count_Type (2), Input_Pet);

      T.Tick_T_Send (Input_Tick);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Pet_T_Recv_Sync_History.Get_Count, 7);
      T.Tick_T_Send (Input_Tick);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Pet_T_Recv_Sync_History.Get_Count, 8);
      T.Tick_T_Send (Input_Tick);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 3);
      Natural_Assert.Eq (T.Component_Exceeded_Pet_Limit_History.Get_Count, 2);
      Natural_Assert.Eq (Natural (T.Component_Exceeded_Pet_Limit_History.Get (2).Index), 1);
      Natural_Assert.Eq (T.Pet_T_Recv_Sync_History.Get_Count, 9);

      -- Make sure we don't get another event but still a pet
      T.Pet_T_Send (Connector_Count_Type (2), Input_Pet);
      T.Tick_T_Send (Input_Tick);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 3);
      Natural_Assert.Eq (T.Pet_T_Recv_Sync_History.Get_Count, 10);
      T.Tick_T_Send (Input_Tick);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 3);
      Natural_Assert.Eq (T.Pet_T_Recv_Sync_History.Get_Count, 11);

      -- New limit for the critical case
      T.Command_T_Send (T.Commands.Set_Watchdog_Limit ((Index => 2, New_Limit => 4)));
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 2);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (2), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Set_Watchdog_Limit_Id, Status => Success));
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 4);
      Natural_Assert.Eq (T.Watchdog_Limit_Set_History.Get_Count, 2);
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, Init_Dp_Count + 2);
      Test_Limit_Data_Product.Header.Id := 3;
      Test_Limit_Data_Product.Buffer (0 .. 1) := [0, 4];
      Data_Product_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get (Init_Dp_Count + 2), Test_Limit_Data_Product);

      T.Tick_T_Send (Input_Tick);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 4);
      Natural_Assert.Eq (T.Pet_T_Recv_Sync_History.Get_Count, 12);
      T.Tick_T_Send (Input_Tick);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 4);
      Natural_Assert.Eq (T.Pet_T_Recv_Sync_History.Get_Count, 13);
      T.Tick_T_Send (Input_Tick);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 4);
      Natural_Assert.Eq (T.Pet_T_Recv_Sync_History.Get_Count, 14);
      T.Tick_T_Send (Input_Tick);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 4);
      Natural_Assert.Eq (T.Pet_T_Recv_Sync_History.Get_Count, 15);

      T.Tick_T_Send (Input_Tick);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 6);
      Natural_Assert.Eq (T.Component_Exceeded_Pet_Limit_History.Get_Count, 3);
      Natural_Assert.Eq (Natural (T.Component_Exceeded_Pet_Limit_History.Get (3).Index), 2);
      Natural_Assert.Eq (T.Critical_Task_Not_Petting_History.Get_Count, 1);
      Natural_Assert.Eq (Natural (T.Critical_Task_Not_Petting_History.Get (1).Index), 2);
      Natural_Assert.Eq (T.Fault_T_Recv_Sync_History.Get_Count, 1);
      Fault_Assert.Eq (T.Fault_T_Recv_Sync_History.Get (1), Test_Create_Fault (5, 2));
      Natural_Assert.Eq (T.Pet_T_Recv_Sync_History.Get_Count, 15);

      T.Tick_T_Send (Input_Tick);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 7);
      Natural_Assert.Eq (T.Critical_Task_Not_Petting_History.Get_Count, 2);
      Natural_Assert.Eq (Natural (T.Critical_Task_Not_Petting_History.Get (2).Index), 2);
      Natural_Assert.Eq (T.Fault_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Pet_T_Recv_Sync_History.Get_Count, 15);

      -- Now try some failures
      T.Command_T_Send (T.Commands.Set_Watchdog_Limit ((Index => 4, New_Limit => 5)));
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 3);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (3), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Set_Watchdog_Limit_Id, Status => Failure));
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 8);
      Natural_Assert.Eq (T.Watchdog_Limit_Change_Index_Out_Of_Range_History.Get_Count, 1);
      Natural_Assert.Eq (Natural (T.Watchdog_Limit_Change_Index_Out_Of_Range_History.Get (1).Index), 4);
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, Init_Dp_Count + 2);

   end Test_Watchdog_Limit_Command;

   overriding procedure Test_Invalid_Command (Self : in out Instance) is
      T : Component.Task_Watchdog.Implementation.Tester.Instance_Access renames Self.Tester;
      Cmd : Command.T := T.Commands.Set_Watchdog_Limit ((Index => 4, New_Limit => 5));
   begin
      -- Make the command invalid by modifying its length.
      Cmd.Header.Arg_Buffer_Length := 0;

      -- Send bad command and expect bad response:
      T.Command_T_Send (Cmd);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 1);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (1), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Set_Watchdog_Limit_Id, Status => Length_Error));

      -- Make sure some events were thrown:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Invalid_Command_Received_History.Get_Count, 1);
      Invalid_Command_Info_Assert.Eq (T.Invalid_Command_Received_History.Get (1), (Id => T.Commands.Get_Set_Watchdog_Limit_Id, Errant_Field_Number => Interfaces.Unsigned_32'Last, Errant_Field => [0, 0, 0, 0, 0, 0, 0, 0]));
   end Test_Invalid_Command;

end Task_Watchdog_Tests.Implementation;
