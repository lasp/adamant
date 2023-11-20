--------------------------------------------------------------------------------
-- Interrupt_Servicer Tests Body
--------------------------------------------------------------------------------

-- Ada includes:
with Ada.Interrupts;
with Ada.Synchronous_Task_Control;
with Ada.Real_Time; use Ada.Real_Time;

-- System includes:
with Interrupt_Sender;
with Task_Types;
with Basic_Assertions; use Basic_Assertions;
with Tick.Assertion; use Tick.Assertion;
with System;
with Safe_Deallocator;

package body Tests.Implementation is

   -------------------------------------------------------------------------
   -- Fixtures:
   -------------------------------------------------------------------------

   overriding procedure Set_Up_Test (Self : in out Instance) is
   begin
      -- Dynamically allocate the generic component tester:
      Self.Tester := new Component_Tester_Package.Instance (Tick_Interrupt_Handler.Handler'Access);

      -- Set the logger in the component
      Self.Tester.Set_Logger (Self.Logger'Unchecked_Access);

      -- Allocate heap memory to component:
      Self.Tester.Init_Base;

      -- Make necessary connections between tester and component:
      Self.Tester.Connect;

      -- Setup the time for this test
      Self.Tester.System_Time := (14, 7);
   end Set_Up_Test;

   overriding procedure Tear_Down_Test (Self : in out Instance) is
      -- Free the tester component:
      procedure Free_If_Testing is new Safe_Deallocator.Deallocate_If_Testing (Object => Component_Tester_Package.Instance, Name => Component_Tester_Package.Instance_Access);
   begin
      -- Free component heap:
      Self.Tester.Final_Base;

      -- Delete tester:
      Free_If_Testing (Self.Tester);
   end Tear_Down_Test;

   -------------------------------------------------------------------------
   -- Tests:
   -------------------------------------------------------------------------

   overriding procedure Test_Interrupt_Handling (Self : in out Instance) is
      T : Component_Tester_Package.Instance_Access renames Self.Tester;
      Interrupt_Id : Ada.Interrupts.Interrupt_ID renames Component_Tester_Package.Interrupt_Id;

      -- Function to sleep for a while. The test needs to yield the processor to the component task
      -- in order to test its execution. This is the easiest way to do that, since raising an
      -- interrupt does not automatically yield the CPU.
      procedure Sleep_A_Bit is
         Wait_Time : constant Ada.Real_Time.Time_Span := Ada.Real_Time.Microseconds (500_000);
         End_Time : constant Ada.Real_Time.Time := Ada.Real_Time.Clock + Wait_Time;
      begin
         delay until End_Time;
      end Sleep_A_Bit;

      -- Create a local task for the Interrupt Service component to execute:
      Component_Signal : aliased Ada.Synchronous_Task_Control.Suspension_Object;
      Task_Info : aliased Task_Types.Task_Info;
      My_Component_Task : Component.Active_Task (Task_Info'Unchecked_Access, T.Component_Instance'Unrestricted_Access, Component_Signal'Access, System.Priority'Last, 30_000, 3_000);
   begin
      -- Make sure that no ticks have been sent:
      Boolean_Assert.Eq (T.Interrupt_Data_Type_Recv_Sync_History.Is_Empty, True);

      -- Start the task:
      Ada.Synchronous_Task_Control.Set_True (Component_Signal);

      -- Generate 5 interrupts, and yield the cpu after each one.
      Interrupt_Sender.Generate_Interrupt (Interrupt_Id);
      Sleep_A_Bit;
      Interrupt_Sender.Generate_Interrupt (Interrupt_Id);
      Sleep_A_Bit;
      Interrupt_Sender.Generate_Interrupt (Interrupt_Id);
      Sleep_A_Bit;
      Interrupt_Sender.Generate_Interrupt (Interrupt_Id);
      Sleep_A_Bit;
      Interrupt_Sender.Generate_Interrupt (Interrupt_Id);
      Sleep_A_Bit;

      -- Make sure that 5 ticks have been sent:
      Natural_Assert.Eq (T.Interrupt_Data_Type_Recv_Sync_History.Get_Count, 5);

      -- Make sure the ticks are correct:
      Tick_Assert.Eq (T.Interrupt_Data_Type_Recv_Sync_History.Get (1), (T.System_Time, 1));
      Tick_Assert.Eq (T.Interrupt_Data_Type_Recv_Sync_History.Get (2), (T.System_Time, 2));
      Tick_Assert.Eq (T.Interrupt_Data_Type_Recv_Sync_History.Get (3), (T.System_Time, 3));
      Tick_Assert.Eq (T.Interrupt_Data_Type_Recv_Sync_History.Get (4), (T.System_Time, 4));
      Tick_Assert.Eq (T.Interrupt_Data_Type_Recv_Sync_History.Get (5), (T.System_Time, 5));

      -- Terminate the task:
      Ada.Synchronous_Task_Control.Set_True (Component_Signal);
      Sleep_A_Bit;
      Interrupt_Sender.Generate_Interrupt (Interrupt_Id); -- Need to signal one more time to release the task loop.
      Sleep_A_Bit;

   end Test_Interrupt_Handling;

end Tests.Implementation;
