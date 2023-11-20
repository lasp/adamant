--------------------------------------------------------------------------------
-- Event_Text_Logger Tests Body
--------------------------------------------------------------------------------

with Event_Producer_Events;
with Tick;
with Basic_Assertions; use Basic_Assertions;

package body Tests.Implementation is

   -------------------------------------------------------------------------
   -- Fixtures:
   -------------------------------------------------------------------------

   overriding procedure Set_Up_Test (Self : in out Instance) is
   begin
      -- Allocate heap memory to component:
      Self.Tester.Init_Base (Queue_Size => Self.Tester.Component_Instance.Get_Max_Queue_Element_Size * 10);

      -- Make necessary connections between tester and component:
      Self.Tester.Connect;
   end Set_Up_Test;

   overriding procedure Tear_Down_Test (Self : in out Instance) is
   begin
      -- Free component heap:
      Self.Tester.Final_Base;
   end Tear_Down_Test;

   -------------------------------------------------------------------------
   -- Tests:
   -------------------------------------------------------------------------

   overriding procedure Test_Event_Printing (Self : in out Instance) is
      T : Component.Event_Text_Logger.Implementation.Tester.Instance_Access renames Self.Tester;
      Events : Event_Producer_Events.Instance;
      Tick_1 : constant Tick.T := ((1, 1), (1));
      Tick_2 : constant Tick.T := ((2, 2), (2));
      Tick_3 : constant Tick.T := ((3, 3), (3));
      Cnt : Natural;
   begin
      Events.Set_Id_Base (1);
      T.Event_T_Send (Events.Event_1 (Tick_1.Time, Tick_1));
      T.Event_T_Send (Events.Event_2 (Tick_2.Time, Tick_2));
      T.Event_T_Send (Events.Event_3 (Tick_3.Time, Tick_3));
      Cnt := T.Dispatch_All;
      Natural_Assert.Eq (Cnt, 3);
   end Test_Event_Printing;

end Tests.Implementation;
