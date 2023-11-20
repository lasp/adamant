--------------------------------------------------------------------------------
-- Interrupt_Listener Tests Body
--------------------------------------------------------------------------------

with Ada.Real_Time; use Ada.Real_Time;
with Ada.Interrupts;

-- System includes:
with Interrupt_Sender;
with Tick.Assertion; use Tick.Assertion;
with Safe_Deallocator;
with Tester_Interrupt_Handler;

package body Tests.Implementation is

   -------------------------------------------------------------------------
   -- Fixtures:
   -------------------------------------------------------------------------

   overriding procedure Set_Up_Test (Self : in out Instance) is
   begin
      -- Dynamically allocate the generic component tester:
      Self.Tester := new Component_Tester_Package.Instance (Tester_Interrupt_Handler.Handler'Access);

      -- Set the logger in the component
      Self.Tester.Set_Logger (Self.Logger'Unchecked_Access);

      -- Make necessary connections between tester and component:
      Self.Tester.Connect;
   end Set_Up_Test;

   overriding procedure Tear_Down_Test (Self : in out Instance) is
      -- Free the tester component:
      procedure Free_If_Testing is new Safe_Deallocator.Deallocate_If_Testing (Object => Component_Tester_Package.Instance, Name => Component_Tester_Package.Instance_Access);
   begin
      -- Delete tester:
      Free_If_Testing (Self.Tester);
   end Tear_Down_Test;

   -------------------------------------------------------------------------
   -- Tests:
   -------------------------------------------------------------------------

   overriding procedure Test_Interrupt_Handling (Self : in out Instance) is
      T : Component_Tester_Package.Instance_Access renames Self.Tester;
      Interrupt_Id : Ada.Interrupts.Interrupt_ID renames Component_Tester_Package.Interrupt_Id;
      The_Tick : Tick.T;

      -- Function to sleep for a while. The test needs to yield the processor to the component task
      -- in order to test its execution. This is the easiest way to do that, since raising an
      -- interrupt does not automatically yield the CPU.
      procedure Sleep_A_Bit is
         Wait_Time : constant Ada.Real_Time.Time_Span := Ada.Real_Time.Microseconds (500_000);
         End_Time : constant Ada.Real_Time.Time := Ada.Real_Time.Clock + Wait_Time;
      begin
         delay until End_Time;
      end Sleep_A_Bit;
   begin
      -- Make sure that no ticks have been sent:
      The_Tick := T.Interrupt_Data_Type_Get;
      Tick_Assert.Eq (The_Tick, ((0, 0), Count => 0));

      -- Send an interrupt and make sure the count goes up:
      Interrupt_Sender.Generate_Interrupt (Interrupt_Id);
      Sleep_A_Bit;
      The_Tick := T.Interrupt_Data_Type_Get;
      Tick_Assert.Eq (The_Tick, ((0, 0), Count => 1));

      -- Make sure the count is still one:
      The_Tick := T.Interrupt_Data_Type_Get;
      Tick_Assert.Eq (The_Tick, ((0, 0), Count => 1));

      -- Send a bunch of interrupts:
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
      The_Tick := T.Interrupt_Data_Type_Get;
      Tick_Assert.Eq (The_Tick, ((0, 0), Count => 6));

      -- Make sure the count is still six:
      The_Tick := T.Interrupt_Data_Type_Get;
      Tick_Assert.Eq (The_Tick, ((0, 0), Count => 6));

   end Test_Interrupt_Handling;

end Tests.Implementation;
