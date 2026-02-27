--------------------------------------------------------------------------------
-- Tick_Listener Tests Body
--------------------------------------------------------------------------------

-- System includes:
with Ada.Real_Time; use Ada.Real_Time;
with Packed_Natural.Assertion; use Packed_Natural.Assertion;
with Tick;

package body Tests.Implementation is

   -------------------------------------------------------------------------
   -- Fixtures:
   -------------------------------------------------------------------------

   overriding procedure Set_Up_Test (Self : in out Instance) is
   begin
      -- Make necessary connections between tester and component:
      Self.Tester.Connect;
   end Set_Up_Test;

   overriding procedure Tear_Down_Test (Self : in out Instance) is
   begin
      null;
   end Tear_Down_Test;

   -------------------------------------------------------------------------
   -- Tests:
   -------------------------------------------------------------------------

   overriding procedure Test_Tick_Handling (Self : in out Instance) is
      T : Component.Tick_Listener.Implementation.Tester.Instance_Access renames Self.Tester;
      The_Count : Packed_Natural.T;
      The_Tick : constant Tick.T := ((0, 0), others => 0);

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
      The_Count := T.Get_Tick_Count_Reciprocal;
      Packed_Natural_Assert.Eq (The_Count, (Value => 0));

      -- Send a tick and make sure the count goes up:
      T.Tick_T_Send (The_Tick);
      Sleep_A_Bit;
      The_Count := T.Get_Tick_Count_Reciprocal;
      Packed_Natural_Assert.Eq (The_Count, (Value => 1));

      -- Make sure the count is now zero:
      The_Count := T.Get_Tick_Count_Reciprocal;
      Packed_Natural_Assert.Eq (The_Count, (Value => 0));

      -- Send a bunch of ticks:
      T.Tick_T_Send (The_Tick);
      Sleep_A_Bit;
      T.Tick_T_Send (The_Tick);
      Sleep_A_Bit;
      T.Tick_T_Send (The_Tick);
      Sleep_A_Bit;
      T.Tick_T_Send (The_Tick);
      Sleep_A_Bit;
      T.Tick_T_Send (The_Tick);
      Sleep_A_Bit;
      The_Count := T.Get_Tick_Count_Reciprocal;
      Packed_Natural_Assert.Eq (The_Count, (Value => 5));

      -- Make sure the count is now zero:
      The_Count := T.Get_Tick_Count_Reciprocal;
      Packed_Natural_Assert.Eq (The_Count, (Value => 0));
   end Test_Tick_Handling;

end Tests.Implementation;
