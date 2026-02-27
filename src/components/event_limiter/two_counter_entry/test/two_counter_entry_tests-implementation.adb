--------------------------------------------------------------------------------
-- Two_Counter_Entry Tests Body
--------------------------------------------------------------------------------

with Basic_Assertions; use Basic_Assertions;
with Smart_Assert;
with Two_Counter_Entry_Enums;
with Interfaces;
with Event_Types;
with Two_Counter_Entry;

package body Two_Counter_Entry_Tests.Implementation is
   use Two_Counter_Entry_Enums;
   -------------------------------------------------------------------------
   -- Fixtures:
   -------------------------------------------------------------------------

   overriding procedure Set_Up_Test (Self : in out Instance) is
   begin
      null;
   end Set_Up_Test;

   overriding procedure Tear_Down_Test (Self : in out Instance) is
   begin
      null;
   end Tear_Down_Test;

   -------------------------------------------------------------------------
   -- Assertion packages:
   -------------------------------------------------------------------------
   --package Event_Location_Status_Assert is new Smart_Assert.Basic(Two_Counter_Entry.Event_Location, Two_Counter_Entry.Event_Location'Image);
   package Count_Status_Assert is new Smart_Assert.Basic (Two_Counter_Entry.Count_Status, Two_Counter_Entry.Count_Status'Image);
   package Event_State_Status_Assert is new Smart_Assert.Basic (Two_Counter_Entry.Enable_State_Status, Two_Counter_Entry.Enable_State_Status'Image);
   package Event_State_Assert is new Smart_Assert.Basic (Event_State_Type.E, Event_State_Type.E'Image);

   -------------------------------------------------------------------------
   -- Tests:
   -------------------------------------------------------------------------

   overriding procedure Test_Init_List (Self : in out Instance) is
      Ignore_Self : Instance renames Self;
      My_Counter : Two_Counter_Entry.Instance;
      Event_Start_List : constant Two_Counter_Entry.Event_Id_List := [4, 5];
      Event_Start_Empty_List : constant Two_Counter_Entry.Event_Id_List := [1 .. 0 => 0];
      Event_State : Event_State_Type.E;
      State_Return_Status : Two_Counter_Entry.Enable_State_Status;
   begin
      My_Counter.Init (Event_Id_Start => 0, Event_Id_Stop => 5, Event_Disable_List => Event_Start_List, Event_Limit_Persistence => 7);
      -- Get each event's state to determine if the starting disable list worked
      State_Return_Status := My_Counter.Get_Enable_State (0, Event_State);
      Event_State_Status_Assert.Eq (State_Return_Status, Two_Counter_Entry.Success);
      Event_State_Assert.Eq (Event_State, Event_State_Type.Enabled);

      State_Return_Status := My_Counter.Get_Enable_State (1, Event_State);
      Event_State_Status_Assert.Eq (State_Return_Status, Two_Counter_Entry.Success);
      Event_State_Assert.Eq (Event_State, Event_State_Type.Enabled);

      State_Return_Status := My_Counter.Get_Enable_State (2, Event_State);
      Event_State_Status_Assert.Eq (State_Return_Status, Two_Counter_Entry.Success);
      Event_State_Assert.Eq (Event_State, Event_State_Type.Enabled);

      State_Return_Status := My_Counter.Get_Enable_State (3, Event_State);
      Event_State_Status_Assert.Eq (State_Return_Status, Two_Counter_Entry.Success);
      Event_State_Assert.Eq (Event_State, Event_State_Type.Enabled);

      State_Return_Status := My_Counter.Get_Enable_State (4, Event_State);
      Event_State_Status_Assert.Eq (State_Return_Status, Two_Counter_Entry.Success);
      Event_State_Assert.Eq (Event_State, Event_State_Type.Disabled);

      State_Return_Status := My_Counter.Get_Enable_State (5, Event_State);
      Event_State_Status_Assert.Eq (State_Return_Status, Two_Counter_Entry.Success);
      Event_State_Assert.Eq (Event_State, Event_State_Type.Disabled);

      -- Last one out of range
      State_Return_Status := My_Counter.Get_Enable_State (6, Event_State);
      Event_State_Status_Assert.Eq (State_Return_Status, Two_Counter_Entry.Invalid_Id);
      Event_State_Assert.Eq (Event_State, Event_State_Type.Disabled); -- Make sure we don't have something uninitialized
      My_Counter.Destroy;

      -- Single event init, default list
      My_Counter.Init (Event_Id_Start => 0, Event_Id_Stop => 1, Event_Disable_List => Event_Start_Empty_List, Event_Limit_Persistence => 1);
      State_Return_Status := My_Counter.Get_Enable_State (0, Event_State);
      Event_State_Status_Assert.Eq (State_Return_Status, Two_Counter_Entry.Success);
      Event_State_Assert.Eq (Event_State, Event_State_Type.Enabled);

      State_Return_Status := My_Counter.Get_Enable_State (1, Event_State);
      Event_State_Status_Assert.Eq (State_Return_Status, Two_Counter_Entry.Success);
      Event_State_Assert.Eq (Event_State, Event_State_Type.Enabled);

      -- Last one out of range
      State_Return_Status := My_Counter.Get_Enable_State (2, Event_State);
      Event_State_Status_Assert.Eq (State_Return_Status, Two_Counter_Entry.Invalid_Id);
      Event_State_Assert.Eq (Event_State, Event_State_Type.Disabled);
      My_Counter.Destroy;

      -- Test that we can start at a non-zero value but still operate (since its zero based in the code)
      My_Counter.Init (Event_Id_Start => 4, Event_Id_Stop => 5, Event_Disable_List => Event_Start_List, Event_Limit_Persistence => 4);

      State_Return_Status := My_Counter.Get_Enable_State (4, Event_State);
      Event_State_Status_Assert.Eq (State_Return_Status, Two_Counter_Entry.Success);
      Event_State_Assert.Eq (Event_State, Event_State_Type.Disabled);

      State_Return_Status := My_Counter.Get_Enable_State (5, Event_State);
      Event_State_Status_Assert.Eq (State_Return_Status, Two_Counter_Entry.Success);
      Event_State_Assert.Eq (Event_State, Event_State_Type.Disabled);

      -- Last one out of range
      State_Return_Status := My_Counter.Get_Enable_State (3, Event_State);
      Event_State_Status_Assert.Eq (State_Return_Status, Two_Counter_Entry.Invalid_Id);
      Event_State_Assert.Eq (Event_State, Event_State_Type.Disabled);
      State_Return_Status := My_Counter.Get_Enable_State (6, Event_State);
      Event_State_Status_Assert.Eq (State_Return_Status, Two_Counter_Entry.Invalid_Id);
      Event_State_Assert.Eq (Event_State, Event_State_Type.Disabled);
      My_Counter.Destroy;

      -- default no list init
      My_Counter.Init (Event_Id_Start => 3, Event_Id_Stop => 8, Event_Disable_List => Event_Start_Empty_List, Event_Limit_Persistence => 5);
      State_Return_Status := My_Counter.Get_Enable_State (3, Event_State);
      Event_State_Status_Assert.Eq (State_Return_Status, Two_Counter_Entry.Success);
      Event_State_Assert.Eq (Event_State, Event_State_Type.Enabled);

      State_Return_Status := My_Counter.Get_Enable_State (4, Event_State);
      Event_State_Status_Assert.Eq (State_Return_Status, Two_Counter_Entry.Success);
      Event_State_Assert.Eq (Event_State, Event_State_Type.Enabled);

      State_Return_Status := My_Counter.Get_Enable_State (5, Event_State);
      Event_State_Status_Assert.Eq (State_Return_Status, Two_Counter_Entry.Success);
      Event_State_Assert.Eq (Event_State, Event_State_Type.Enabled);

      State_Return_Status := My_Counter.Get_Enable_State (6, Event_State);
      Event_State_Status_Assert.Eq (State_Return_Status, Two_Counter_Entry.Success);
      Event_State_Assert.Eq (Event_State, Event_State_Type.Enabled);

      State_Return_Status := My_Counter.Get_Enable_State (7, Event_State);
      Event_State_Status_Assert.Eq (State_Return_Status, Two_Counter_Entry.Success);
      Event_State_Assert.Eq (Event_State, Event_State_Type.Enabled);

      State_Return_Status := My_Counter.Get_Enable_State (8, Event_State);
      Event_State_Status_Assert.Eq (State_Return_Status, Two_Counter_Entry.Success);
      Event_State_Assert.Eq (Event_State, Event_State_Type.Enabled);

      -- Last one out of range
      State_Return_Status := My_Counter.Get_Enable_State (2, Event_State);
      Event_State_Status_Assert.Eq (State_Return_Status, Two_Counter_Entry.Invalid_Id);
      Event_State_Assert.Eq (Event_State, Event_State_Type.Disabled);
      State_Return_Status := My_Counter.Get_Enable_State (9, Event_State);
      Event_State_Status_Assert.Eq (State_Return_Status, Two_Counter_Entry.Invalid_Id);
      Event_State_Assert.Eq (Event_State, Event_State_Type.Disabled);

      My_Counter.Destroy;
      pragma Unreferenced (My_Counter);
   end Test_Init_List;

   overriding procedure Test_Increment_Count (Self : in out Instance) is
      Ignore_Self : Instance renames Self;
      My_Counter : Two_Counter_Entry.Instance;
      Event_Start_List : constant Two_Counter_Entry.Event_Id_List := [1, 2, 3];
      Return_Status : Two_Counter_Entry.Count_Status;
      Event_State : Event_State_Type.E;
      State_Return_Status : Two_Counter_Entry.Enable_State_Status;
   begin
      My_Counter.Init (Event_Id_Start => 1, Event_Id_Stop => 7, Event_Disable_List => Event_Start_List, Event_Limit_Persistence => 7);

      -- Starting with 6, check the state first
      State_Return_Status := My_Counter.Get_Enable_State (6, Event_State);
      Event_State_Status_Assert.Eq (State_Return_Status, Two_Counter_Entry.Success);
      Event_State_Assert.Eq (Event_State, Event_State_Type.Enabled);
      -- increment the count of an id
      Return_Status := My_Counter.Increment_Counter (6);    -- Count = 0
      Count_Status_Assert.Eq (Return_Status, Two_Counter_Entry.Success);
      Return_Status := My_Counter.Increment_Counter (6);    -- Count = 1
      Count_Status_Assert.Eq (Return_Status, Two_Counter_Entry.Success);
      Return_Status := My_Counter.Increment_Counter (6);    -- Count = 2
      Count_Status_Assert.Eq (Return_Status, Two_Counter_Entry.Success);
      Return_Status := My_Counter.Increment_Counter (6);    -- Count = 3
      Count_Status_Assert.Eq (Return_Status, Two_Counter_Entry.Success);
      Return_Status := My_Counter.Increment_Counter (6);    -- Count = 4
      Count_Status_Assert.Eq (Return_Status, Two_Counter_Entry.Success);
      Return_Status := My_Counter.Increment_Counter (6);    -- Count = 5
      Count_Status_Assert.Eq (Return_Status, Two_Counter_Entry.Success);
      Return_Status := My_Counter.Increment_Counter (6);    -- Count = 6
      Count_Status_Assert.Eq (Return_Status, Two_Counter_Entry.Success);
      -- Maxed out persistence call
      Return_Status := My_Counter.Increment_Counter (6);    -- Count = 7
      Count_Status_Assert.Eq (Return_Status, Two_Counter_Entry.Event_Max_Limit);
      -- Checking that it should still be maxed out
      Return_Status := My_Counter.Increment_Counter (6);   -- Count = 7
      Count_Status_Assert.Eq (Return_Status, Two_Counter_Entry.Event_Max_Limit);

      -- State check for 5 (odd route)
      State_Return_Status := My_Counter.Get_Enable_State (5, Event_State);
      Event_State_Status_Assert.Eq (State_Return_Status, Two_Counter_Entry.Success);
      Event_State_Assert.Eq (Event_State, Event_State_Type.Enabled);

      -- Perform the same with an odd id to hit both the top and bottom paths
      Return_Status := My_Counter.Increment_Counter (5);    -- Count = 0
      Count_Status_Assert.Eq (Return_Status, Two_Counter_Entry.Success);
      Return_Status := My_Counter.Increment_Counter (5);    -- Count = 1
      Count_Status_Assert.Eq (Return_Status, Two_Counter_Entry.Success);
      Return_Status := My_Counter.Increment_Counter (5);    -- Count = 2
      Count_Status_Assert.Eq (Return_Status, Two_Counter_Entry.Success);
      Return_Status := My_Counter.Increment_Counter (5);    -- Count = 3
      Count_Status_Assert.Eq (Return_Status, Two_Counter_Entry.Success);
      Return_Status := My_Counter.Increment_Counter (5);    -- Count = 4
      Count_Status_Assert.Eq (Return_Status, Two_Counter_Entry.Success);
      Return_Status := My_Counter.Increment_Counter (5);    -- Count = 5
      Count_Status_Assert.Eq (Return_Status, Two_Counter_Entry.Success);
      Return_Status := My_Counter.Increment_Counter (5);    -- Count = 6 Total of 7, Count is post incremented to 7
      Count_Status_Assert.Eq (Return_Status, Two_Counter_Entry.Success);
      -- Maxed out persistence call
      Return_Status := My_Counter.Increment_Counter (5);    -- Count = 7
      Count_Status_Assert.Eq (Return_Status, Two_Counter_Entry.Event_Max_Limit);
      -- Checking that it should still be maxed out
      Return_Status := My_Counter.Increment_Counter (5);    -- Count = 7
      Count_Status_Assert.Eq (Return_Status, Two_Counter_Entry.Event_Max_Limit);

      -- State check for 2
      State_Return_Status := My_Counter.Get_Enable_State (2, Event_State);
      Event_State_Status_Assert.Eq (State_Return_Status, Two_Counter_Entry.Success);
      Event_State_Assert.Eq (Event_State, Event_State_Type.Disabled);

      -- Test an increment on disabled events to make sure it bypasses logic and never returns a max limit
      Return_Status := My_Counter.Increment_Counter (2);    -- Count = 0
      Count_Status_Assert.Eq (Return_Status, Two_Counter_Entry.Success);
      Return_Status := My_Counter.Increment_Counter (2);    -- Count = 1
      Count_Status_Assert.Eq (Return_Status, Two_Counter_Entry.Success);
      Return_Status := My_Counter.Increment_Counter (2);    -- Count = 2
      Count_Status_Assert.Eq (Return_Status, Two_Counter_Entry.Success);
      Return_Status := My_Counter.Increment_Counter (2);    -- Count = 3
      Count_Status_Assert.Eq (Return_Status, Two_Counter_Entry.Success);
      Return_Status := My_Counter.Increment_Counter (2);    -- Count = 4
      Count_Status_Assert.Eq (Return_Status, Two_Counter_Entry.Success);
      Return_Status := My_Counter.Increment_Counter (2);    -- Count = 5
      Count_Status_Assert.Eq (Return_Status, Two_Counter_Entry.Success);
      Return_Status := My_Counter.Increment_Counter (2);    -- Count = 6
      Count_Status_Assert.Eq (Return_Status, Two_Counter_Entry.Success);
      Return_Status := My_Counter.Increment_Counter (2);    -- Count = 7
      Count_Status_Assert.Eq (Return_Status, Two_Counter_Entry.Success);
      Return_Status := My_Counter.Increment_Counter (2);    -- Count = 7
      Count_Status_Assert.Eq (Return_Status, Two_Counter_Entry.Success);
      Return_Status := My_Counter.Increment_Counter (2);    -- Count = 7
      Count_Status_Assert.Eq (Return_Status, Two_Counter_Entry.Success);
      Return_Status := My_Counter.Increment_Counter (2);    -- Count = 7
      Count_Status_Assert.Eq (Return_Status, Two_Counter_Entry.Success);

      -- odd disabled case
      -- State check for 3
      State_Return_Status := My_Counter.Get_Enable_State (3, Event_State);
      Event_State_Status_Assert.Eq (State_Return_Status, Two_Counter_Entry.Success);
      Event_State_Assert.Eq (Event_State, Event_State_Type.Disabled);

      Return_Status := My_Counter.Increment_Counter (3);    -- Count = 0
      Count_Status_Assert.Eq (Return_Status, Two_Counter_Entry.Success);
      Return_Status := My_Counter.Increment_Counter (3);    -- Count = 1
      Count_Status_Assert.Eq (Return_Status, Two_Counter_Entry.Success);
      Return_Status := My_Counter.Increment_Counter (3);    -- Count = 2
      Count_Status_Assert.Eq (Return_Status, Two_Counter_Entry.Success);
      Return_Status := My_Counter.Increment_Counter (3);    -- Count = 3
      Count_Status_Assert.Eq (Return_Status, Two_Counter_Entry.Success);
      Return_Status := My_Counter.Increment_Counter (3);    -- Count = 4
      Count_Status_Assert.Eq (Return_Status, Two_Counter_Entry.Success);
      Return_Status := My_Counter.Increment_Counter (3);    -- Count = 5
      Count_Status_Assert.Eq (Return_Status, Two_Counter_Entry.Success);
      Return_Status := My_Counter.Increment_Counter (3);    -- Count = 6
      Count_Status_Assert.Eq (Return_Status, Two_Counter_Entry.Success);
      Return_Status := My_Counter.Increment_Counter (3);    -- Count = 7
      Count_Status_Assert.Eq (Return_Status, Two_Counter_Entry.Success);
      Return_Status := My_Counter.Increment_Counter (3);    -- Count = 7
      Count_Status_Assert.Eq (Return_Status, Two_Counter_Entry.Success);
      Return_Status := My_Counter.Increment_Counter (3);    -- Count = 7
      Count_Status_Assert.Eq (Return_Status, Two_Counter_Entry.Success);
      Return_Status := My_Counter.Increment_Counter (3);    -- Count = 7
      Count_Status_Assert.Eq (Return_Status, Two_Counter_Entry.Success);

      -- Now span the range for the events
      Return_Status := My_Counter.Increment_Counter (1);
      Count_Status_Assert.Eq (Return_Status, Two_Counter_Entry.Success);

      Return_Status := My_Counter.Increment_Counter (7);
      Count_Status_Assert.Eq (Return_Status, Two_Counter_Entry.Success);

      -- Check out of range as well
      Return_Status := My_Counter.Increment_Counter (0);
      Count_Status_Assert.Eq (Return_Status, Two_Counter_Entry.Invalid_Id);
      Return_Status := My_Counter.Increment_Counter (8);
      Count_Status_Assert.Eq (Return_Status, Two_Counter_Entry.Invalid_Id);

      My_Counter.Destroy;
      pragma Unreferenced (My_Counter);
   end Test_Increment_Count;

   overriding procedure Test_Decrement_Count (Self : in out Instance) is
      Ignore_Self : Instance renames Self;
      My_Counter : Two_Counter_Entry.Instance;
      Event_Start_List : constant Two_Counter_Entry.Event_Id_List := [7, 8];
      Return_Status : Two_Counter_Entry.Count_Status;
      Event_State : Event_State_Type.E;
      State_Return_Status : Two_Counter_Entry.Enable_State_Status;
   begin

      My_Counter.Init (Event_Id_Start => 2, Event_Id_Stop => 8, Event_Disable_List => Event_Start_List, Event_Limit_Persistence => 4);

      -- Decrement when the counter is 0 just to make sure nothing bad would happen when enabled
      State_Return_Status := My_Counter.Get_Enable_State (3, Event_State);
      Event_State_Status_Assert.Eq (State_Return_Status, Two_Counter_Entry.Success);
      Event_State_Assert.Eq (Event_State, Event_State_Type.Enabled);

      Return_Status := My_Counter.Decrement_Counter (3);
      Count_Status_Assert.Eq (Return_Status, Two_Counter_Entry.Success);
      Return_Status := My_Counter.Decrement_Counter (3);
      Count_Status_Assert.Eq (Return_Status, Two_Counter_Entry.Success);

      -- Increment and then test the return of the decrement
      Return_Status := My_Counter.Increment_Counter (3);
      Count_Status_Assert.Eq (Return_Status, Two_Counter_Entry.Success);
      Return_Status := My_Counter.Increment_Counter (3);
      Count_Status_Assert.Eq (Return_Status, Two_Counter_Entry.Success);
      Return_Status := My_Counter.Increment_Counter (3);
      Count_Status_Assert.Eq (Return_Status, Two_Counter_Entry.Success);
      Return_Status := My_Counter.Increment_Counter (3);
      Count_Status_Assert.Eq (Return_Status, Two_Counter_Entry.Success);
      Return_Status := My_Counter.Increment_Counter (3);
      Count_Status_Assert.Eq (Return_Status, Two_Counter_Entry.Event_Max_Limit);
      Return_Status := My_Counter.Increment_Counter (3);
      Count_Status_Assert.Eq (Return_Status, Two_Counter_Entry.Event_Max_Limit);

      -- Decrement should give us a max limit first, then back to success even when back to 0
      Return_Status := My_Counter.Decrement_Counter (3);
      Count_Status_Assert.Eq (Return_Status, Two_Counter_Entry.Event_Max_Limit);
      Return_Status := My_Counter.Decrement_Counter (3);
      Count_Status_Assert.Eq (Return_Status, Two_Counter_Entry.Success);
      Return_Status := My_Counter.Decrement_Counter (3);
      Count_Status_Assert.Eq (Return_Status, Two_Counter_Entry.Success);
      Return_Status := My_Counter.Decrement_Counter (3);
      Count_Status_Assert.Eq (Return_Status, Two_Counter_Entry.Success);
      Return_Status := My_Counter.Decrement_Counter (3);
      Count_Status_Assert.Eq (Return_Status, Two_Counter_Entry.Success);
      -- Should have hit 0 at this point so one more for good measure.
      Return_Status := My_Counter.Decrement_Counter (3);
      Count_Status_Assert.Eq (Return_Status, Two_Counter_Entry.Success);

      -- Do the same thing on a even value to cover both the top and bottom
      State_Return_Status := My_Counter.Get_Enable_State (4, Event_State);
      Event_State_Status_Assert.Eq (State_Return_Status, Two_Counter_Entry.Success);
      Event_State_Assert.Eq (Event_State, Event_State_Type.Enabled);

      Return_Status := My_Counter.Decrement_Counter (4);
      Count_Status_Assert.Eq (Return_Status, Two_Counter_Entry.Success);
      Return_Status := My_Counter.Decrement_Counter (4);
      Count_Status_Assert.Eq (Return_Status, Two_Counter_Entry.Success);

      -- Increment to get the max
      Return_Status := My_Counter.Increment_Counter (4);
      Count_Status_Assert.Eq (Return_Status, Two_Counter_Entry.Success);
      Return_Status := My_Counter.Increment_Counter (4);
      Count_Status_Assert.Eq (Return_Status, Two_Counter_Entry.Success);
      Return_Status := My_Counter.Increment_Counter (4);
      Count_Status_Assert.Eq (Return_Status, Two_Counter_Entry.Success);
      Return_Status := My_Counter.Increment_Counter (4);
      Count_Status_Assert.Eq (Return_Status, Two_Counter_Entry.Success);

      -- While there would have been no events dropped yet here, the decrement indicates that it is at the limit.
      Return_Status := My_Counter.Decrement_Counter (4);
      Count_Status_Assert.Eq (Return_Status, Two_Counter_Entry.Event_Max_Limit);
      Return_Status := My_Counter.Decrement_Counter (4);
      Count_Status_Assert.Eq (Return_Status, Two_Counter_Entry.Success);
      Return_Status := My_Counter.Decrement_Counter (4);
      Count_Status_Assert.Eq (Return_Status, Two_Counter_Entry.Success);
      Return_Status := My_Counter.Decrement_Counter (4);
      Count_Status_Assert.Eq (Return_Status, Two_Counter_Entry.Success);
      Return_Status := My_Counter.Decrement_Counter (4);
      Count_Status_Assert.Eq (Return_Status, Two_Counter_Entry.Success);
      Return_Status := My_Counter.Decrement_Counter (4);
      Count_Status_Assert.Eq (Return_Status, Two_Counter_Entry.Success);
      -- should be 0, one more for good measure
      Return_Status := My_Counter.Decrement_Counter (4);
      Count_Status_Assert.Eq (Return_Status, Two_Counter_Entry.Success);

      -- To test that a disabled event doesn't return a max limit, we need to increment, disable the event, then decrement
      State_Return_Status := My_Counter.Get_Enable_State (5, Event_State);
      Event_State_Status_Assert.Eq (State_Return_Status, Two_Counter_Entry.Success);
      Event_State_Assert.Eq (Event_State, Event_State_Type.Enabled);

      Return_Status := My_Counter.Increment_Counter (5);
      Count_Status_Assert.Eq (Return_Status, Two_Counter_Entry.Success);
      Return_Status := My_Counter.Increment_Counter (5);
      Count_Status_Assert.Eq (Return_Status, Two_Counter_Entry.Success);
      Return_Status := My_Counter.Increment_Counter (5);
      Count_Status_Assert.Eq (Return_Status, Two_Counter_Entry.Success);
      Return_Status := My_Counter.Increment_Counter (5);
      Count_Status_Assert.Eq (Return_Status, Two_Counter_Entry.Success);
      Return_Status := My_Counter.Increment_Counter (5);
      Count_Status_Assert.Eq (Return_Status, Two_Counter_Entry.Event_Max_Limit);
      Return_Status := My_Counter.Increment_Counter (5);
      Count_Status_Assert.Eq (Return_Status, Two_Counter_Entry.Event_Max_Limit);
      -- Disable the event
      State_Return_Status := My_Counter.Set_Enable_State (5, Event_State_Type.Disabled);
      Event_State_Status_Assert.Eq (State_Return_Status, Two_Counter_Entry.Success);

      State_Return_Status := My_Counter.Get_Enable_State (5, Event_State);
      Event_State_Status_Assert.Eq (State_Return_Status, Two_Counter_Entry.Success);
      Event_State_Assert.Eq (Event_State, Event_State_Type.Disabled);

      Return_Status := My_Counter.Decrement_Counter (5);
      Count_Status_Assert.Eq (Return_Status, Two_Counter_Entry.Success);
      Return_Status := My_Counter.Decrement_Counter (5);
      Count_Status_Assert.Eq (Return_Status, Two_Counter_Entry.Success);

      -- Even case
      State_Return_Status := My_Counter.Get_Enable_State (6, Event_State);
      Event_State_Status_Assert.Eq (State_Return_Status, Two_Counter_Entry.Success);
      Event_State_Assert.Eq (Event_State, Event_State_Type.Enabled);

      Return_Status := My_Counter.Increment_Counter (6);
      Count_Status_Assert.Eq (Return_Status, Two_Counter_Entry.Success);
      Return_Status := My_Counter.Increment_Counter (6);
      Count_Status_Assert.Eq (Return_Status, Two_Counter_Entry.Success);
      Return_Status := My_Counter.Increment_Counter (6);
      Count_Status_Assert.Eq (Return_Status, Two_Counter_Entry.Success);
      Return_Status := My_Counter.Increment_Counter (6);
      Count_Status_Assert.Eq (Return_Status, Two_Counter_Entry.Success);
      Return_Status := My_Counter.Increment_Counter (6);
      Count_Status_Assert.Eq (Return_Status, Two_Counter_Entry.Event_Max_Limit);
      -- Disable the event
      State_Return_Status := My_Counter.Set_Enable_State (6, Event_State_Type.Disabled);
      Event_State_Status_Assert.Eq (State_Return_Status, Two_Counter_Entry.Success);
      State_Return_Status := My_Counter.Get_Enable_State (6, Event_State);
      Event_State_Status_Assert.Eq (State_Return_Status, Two_Counter_Entry.Success);
      Event_State_Assert.Eq (Event_State, Event_State_Type.Disabled);

      Return_Status := My_Counter.Decrement_Counter (6);
      Count_Status_Assert.Eq (Return_Status, Two_Counter_Entry.Success);
      Return_Status := My_Counter.Decrement_Counter (6);
      Count_Status_Assert.Eq (Return_Status, Two_Counter_Entry.Success);

      -- Lastly just decrement on empty counters of pre-disabled events
      State_Return_Status := My_Counter.Get_Enable_State (7, Event_State);
      Event_State_Status_Assert.Eq (State_Return_Status, Two_Counter_Entry.Success);
      Event_State_Assert.Eq (Event_State, Event_State_Type.Disabled);

      Return_Status := My_Counter.Decrement_Counter (7);
      Count_Status_Assert.Eq (Return_Status, Two_Counter_Entry.Success);
      Return_Status := My_Counter.Decrement_Counter (7);
      Count_Status_Assert.Eq (Return_Status, Two_Counter_Entry.Success);

      State_Return_Status := My_Counter.Get_Enable_State (8, Event_State);
      Event_State_Status_Assert.Eq (State_Return_Status, Two_Counter_Entry.Success);
      Event_State_Assert.Eq (Event_State, Event_State_Type.Disabled);
      -- even
      Return_Status := My_Counter.Decrement_Counter (8);
      Count_Status_Assert.Eq (Return_Status, Two_Counter_Entry.Success);
      Return_Status := My_Counter.Decrement_Counter (8);
      Count_Status_Assert.Eq (Return_Status, Two_Counter_Entry.Success);

      -- Simulate one more test where there is an event sent between each tick
      -- Flood of events that come in before a single tick
      Return_Status := My_Counter.Increment_Counter (2);
      Count_Status_Assert.Eq (Return_Status, Two_Counter_Entry.Success);
      Return_Status := My_Counter.Increment_Counter (2);
      Count_Status_Assert.Eq (Return_Status, Two_Counter_Entry.Success);
      Return_Status := My_Counter.Increment_Counter (2);
      Count_Status_Assert.Eq (Return_Status, Two_Counter_Entry.Success);
      Return_Status := My_Counter.Increment_Counter (2);
      Count_Status_Assert.Eq (Return_Status, Two_Counter_Entry.Success);
      Return_Status := My_Counter.Increment_Counter (2);
      Count_Status_Assert.Eq (Return_Status, Two_Counter_Entry.Event_Max_Limit);

      -- Now "tick"
      Return_Status := My_Counter.Decrement_Counter (2);
      Count_Status_Assert.Eq (Return_Status, Two_Counter_Entry.Event_Max_Limit);

      -- Keep "flooding" before next "Tick" but make sure we still get one event through
      Return_Status := My_Counter.Increment_Counter (2);
      Count_Status_Assert.Eq (Return_Status, Two_Counter_Entry.Success);
      Return_Status := My_Counter.Increment_Counter (2);
      Count_Status_Assert.Eq (Return_Status, Two_Counter_Entry.Event_Max_Limit);
      Return_Status := My_Counter.Increment_Counter (2);
      Count_Status_Assert.Eq (Return_Status, Two_Counter_Entry.Event_Max_Limit);
      Return_Status := My_Counter.Increment_Counter (2);
      Count_Status_Assert.Eq (Return_Status, Two_Counter_Entry.Event_Max_Limit);

      -- repeat the process to make sure
      Return_Status := My_Counter.Decrement_Counter (2);
      Count_Status_Assert.Eq (Return_Status, Two_Counter_Entry.Event_Max_Limit);

      Return_Status := My_Counter.Increment_Counter (2);
      Count_Status_Assert.Eq (Return_Status, Two_Counter_Entry.Success);
      Return_Status := My_Counter.Increment_Counter (2);
      Count_Status_Assert.Eq (Return_Status, Two_Counter_Entry.Event_Max_Limit);
      Return_Status := My_Counter.Increment_Counter (2);
      Count_Status_Assert.Eq (Return_Status, Two_Counter_Entry.Event_Max_Limit);
      Return_Status := My_Counter.Increment_Counter (2);
      Count_Status_Assert.Eq (Return_Status, Two_Counter_Entry.Event_Max_Limit);

      -- Span the range of the array (just did 2 so check 8)
      Return_Status := My_Counter.Decrement_Counter (8);
      Count_Status_Assert.Eq (Return_Status, Two_Counter_Entry.Success);

      -- Check the range here as well
      Return_Status := My_Counter.Decrement_Counter (1);
      Count_Status_Assert.Eq (Return_Status, Two_Counter_Entry.Invalid_Id);
      Return_Status := My_Counter.Decrement_Counter (9);
      Count_Status_Assert.Eq (Return_Status, Two_Counter_Entry.Invalid_Id);

      My_Counter.Destroy;
      pragma Unreferenced (My_Counter);
   end Test_Decrement_Count;

   overriding procedure Test_Set_Enable_State (Self : in out Instance) is
      Ignore_Self : Instance renames Self;
      My_Counter : Two_Counter_Entry.Instance;
      Event_Start_List : constant Two_Counter_Entry.Event_Id_List := [3, 5];
      State_Return_Status : Two_Counter_Entry.Enable_State_Status;
      Event_State : Event_State_Type.E;
   begin
      My_Counter.Init (Event_Id_Start => 3, Event_Id_Stop => 6, Event_Disable_List => Event_Start_List, Event_Limit_Persistence => 5);

      -- 0 was enabled here so set to disabled and verify using the getter
      State_Return_Status := My_Counter.Get_Enable_State (4, Event_State);
      Event_State_Assert.Eq (Event_State, Event_State_Type.Enabled);
      Event_State_Status_Assert.Eq (State_Return_Status, Two_Counter_Entry.Success);
      -- Disable
      State_Return_Status := My_Counter.Set_Enable_State (4, Event_State_Type.Disabled);
      Event_State_Status_Assert.Eq (State_Return_Status, Two_Counter_Entry.Success);
      State_Return_Status := My_Counter.Get_Enable_State (4, Event_State);
      Event_State_Assert.Eq (Event_State, Event_State_Type.Disabled);
      Event_State_Status_Assert.Eq (State_Return_Status, Two_Counter_Entry.Success);

      -- re-enable
      State_Return_Status := My_Counter.Set_Enable_State (4, Event_State_Type.Enabled);
      Event_State_Status_Assert.Eq (State_Return_Status, Two_Counter_Entry.Success);
      State_Return_Status := My_Counter.Get_Enable_State (4, Event_State);
      Event_State_Assert.Eq (Event_State, Event_State_Type.Enabled);
      Event_State_Status_Assert.Eq (State_Return_Status, Two_Counter_Entry.Success);

      -- Event 5 should be disabled so ensure then enable and re-disable
      State_Return_Status := My_Counter.Get_Enable_State (5, Event_State);
      Event_State_Assert.Eq (Event_State, Event_State_Type.Disabled);
      Event_State_Status_Assert.Eq (State_Return_Status, Two_Counter_Entry.Success);
      State_Return_Status := My_Counter.Set_Enable_State (5, Event_State_Type.Enabled);
      Event_State_Status_Assert.Eq (State_Return_Status, Two_Counter_Entry.Success);
      State_Return_Status := My_Counter.Get_Enable_State (5, Event_State);
      Event_State_Assert.Eq (Event_State, Event_State_Type.Enabled);
      Event_State_Status_Assert.Eq (State_Return_Status, Two_Counter_Entry.Success);

      State_Return_Status := My_Counter.Set_Enable_State (5, Event_State_Type.Disabled);
      Event_State_Status_Assert.Eq (State_Return_Status, Two_Counter_Entry.Success);
      State_Return_Status := My_Counter.Get_Enable_State (5, Event_State);
      Event_State_Assert.Eq (Event_State, Event_State_Type.Disabled);
      Event_State_Status_Assert.Eq (State_Return_Status, Two_Counter_Entry.Success);

      -- out of range test
      State_Return_Status := My_Counter.Set_Enable_State (2, Event_State_Type.Disabled);
      Event_State_Status_Assert.Eq (State_Return_Status, Two_Counter_Entry.Invalid_Id);

      State_Return_Status := My_Counter.Get_Enable_State (2, Event_State);
      Event_State_Status_Assert.Eq (State_Return_Status, Two_Counter_Entry.Invalid_Id);
      Event_State_Assert.Eq (Event_State, Event_State_Type.Disabled);

      State_Return_Status := My_Counter.Set_Enable_State (7, Event_State_Type.Disabled);
      Event_State_Status_Assert.Eq (State_Return_Status, Two_Counter_Entry.Invalid_Id);

      State_Return_Status := My_Counter.Get_Enable_State (7, Event_State);
      Event_State_Status_Assert.Eq (State_Return_Status, Two_Counter_Entry.Invalid_Id);
      Event_State_Assert.Eq (Event_State, Event_State_Type.Disabled);

      My_Counter.Destroy;
      pragma Unreferenced (My_Counter);
   end Test_Set_Enable_State;

   overriding procedure Test_Set_Persistence (Self : in out Instance) is
      Ignore_Self : Instance renames Self;
      My_Counter : Two_Counter_Entry.Instance;
      Return_Status : Two_Counter_Entry.Count_Status;
      Event_State : Event_State_Type.E;
      Persistence : Two_Counter_Entry.Persistence_Type;
      State_Return_Status : Two_Counter_Entry.Enable_State_Status;
      Event_Start_Empty_List : constant Two_Counter_Entry.Event_Id_List := [1 .. 0 => 0];
   begin
      My_Counter.Init (Event_Id_Start => 0, Event_Id_Stop => 6, Event_Disable_List => Event_Start_Empty_List, Event_Limit_Persistence => 7);

      -- Test the getter first
      Persistence := My_Counter.Get_Persistence;
      Natural_Assert.Eq (Natural (Persistence), 7);

      -- Make sure we operate on enabled events
      State_Return_Status := My_Counter.Get_Enable_State (0, Event_State);
      Event_State_Status_Assert.Eq (State_Return_Status, Two_Counter_Entry.Success);
      Event_State_Assert.Eq (Event_State, Event_State_Type.Enabled);

      State_Return_Status := My_Counter.Get_Enable_State (1, Event_State);
      Event_State_Status_Assert.Eq (State_Return_Status, Two_Counter_Entry.Success);
      Event_State_Assert.Eq (Event_State, Event_State_Type.Enabled);

      State_Return_Status := My_Counter.Get_Enable_State (2, Event_State);
      Event_State_Status_Assert.Eq (State_Return_Status, Two_Counter_Entry.Success);
      Event_State_Assert.Eq (Event_State, Event_State_Type.Enabled);

      -- Increment until we hit the persistence (do both odd and even to catch both branches when we have to "reset" the counts)
      Return_Status := My_Counter.Increment_Counter (0);
      Count_Status_Assert.Eq (Return_Status, Two_Counter_Entry.Success);
      Return_Status := My_Counter.Increment_Counter (0);
      Count_Status_Assert.Eq (Return_Status, Two_Counter_Entry.Success);
      Return_Status := My_Counter.Increment_Counter (0);
      Count_Status_Assert.Eq (Return_Status, Two_Counter_Entry.Success);
      Return_Status := My_Counter.Increment_Counter (0);
      Count_Status_Assert.Eq (Return_Status, Two_Counter_Entry.Success);
      Return_Status := My_Counter.Increment_Counter (0);
      Count_Status_Assert.Eq (Return_Status, Two_Counter_Entry.Success);
      Return_Status := My_Counter.Increment_Counter (0);
      Count_Status_Assert.Eq (Return_Status, Two_Counter_Entry.Success);
      Return_Status := My_Counter.Increment_Counter (0);
      Count_Status_Assert.Eq (Return_Status, Two_Counter_Entry.Success);
      Return_Status := My_Counter.Increment_Counter (0);
      Count_Status_Assert.Eq (Return_Status, Two_Counter_Entry.Event_Max_Limit);
      Return_Status := My_Counter.Increment_Counter (0);
      Count_Status_Assert.Eq (Return_Status, Two_Counter_Entry.Event_Max_Limit);
      Return_Status := My_Counter.Increment_Counter (0);
      Count_Status_Assert.Eq (Return_Status, Two_Counter_Entry.Event_Max_Limit);

      Return_Status := My_Counter.Increment_Counter (1);
      Count_Status_Assert.Eq (Return_Status, Two_Counter_Entry.Success);
      Return_Status := My_Counter.Increment_Counter (1);
      Count_Status_Assert.Eq (Return_Status, Two_Counter_Entry.Success);
      Return_Status := My_Counter.Increment_Counter (1);
      Count_Status_Assert.Eq (Return_Status, Two_Counter_Entry.Success);
      Return_Status := My_Counter.Increment_Counter (1);
      Count_Status_Assert.Eq (Return_Status, Two_Counter_Entry.Success);
      Return_Status := My_Counter.Increment_Counter (1);
      Count_Status_Assert.Eq (Return_Status, Two_Counter_Entry.Success);
      Return_Status := My_Counter.Increment_Counter (1);
      Count_Status_Assert.Eq (Return_Status, Two_Counter_Entry.Success);
      Return_Status := My_Counter.Increment_Counter (1);
      Count_Status_Assert.Eq (Return_Status, Two_Counter_Entry.Success);
      Return_Status := My_Counter.Increment_Counter (1);
      Count_Status_Assert.Eq (Return_Status, Two_Counter_Entry.Event_Max_Limit);
      Return_Status := My_Counter.Increment_Counter (1);
      Count_Status_Assert.Eq (Return_Status, Two_Counter_Entry.Event_Max_Limit);
      Return_Status := My_Counter.Increment_Counter (1);
      Count_Status_Assert.Eq (Return_Status, Two_Counter_Entry.Event_Max_Limit);

      My_Counter.Set_Persistence (1);
      Persistence := My_Counter.Get_Persistence;
      Natural_Assert.Eq (Natural (Persistence), 1);

      Return_Status := My_Counter.Increment_Counter (2);
      Count_Status_Assert.Eq (Return_Status, Two_Counter_Entry.Success);
      Return_Status := My_Counter.Increment_Counter (2);
      Count_Status_Assert.Eq (Return_Status, Two_Counter_Entry.Event_Max_Limit);
      Return_Status := My_Counter.Increment_Counter (2);
      Count_Status_Assert.Eq (Return_Status, Two_Counter_Entry.Event_Max_Limit);
      Return_Status := My_Counter.Increment_Counter (2);
      Count_Status_Assert.Eq (Return_Status, Two_Counter_Entry.Event_Max_Limit);

      -- Also make sure that the persistence for the last one was reset back to the limit
      Return_Status := My_Counter.Decrement_Counter (0);
      Count_Status_Assert.Eq (Return_Status, Two_Counter_Entry.Event_Max_Limit);

      Return_Status := My_Counter.Increment_Counter (0);
      Count_Status_Assert.Eq (Return_Status, Two_Counter_Entry.Success);
      Return_Status := My_Counter.Increment_Counter (0);
      Count_Status_Assert.Eq (Return_Status, Two_Counter_Entry.Event_Max_Limit);
      Return_Status := My_Counter.Increment_Counter (0);
      Count_Status_Assert.Eq (Return_Status, Two_Counter_Entry.Event_Max_Limit);

      Return_Status := My_Counter.Decrement_Counter (0);
      Count_Status_Assert.Eq (Return_Status, Two_Counter_Entry.Event_Max_Limit);
      Return_Status := My_Counter.Decrement_Counter (0);
      Count_Status_Assert.Eq (Return_Status, Two_Counter_Entry.Success);
      -- Set up the count on 0 to 1 for when the persistence is set higher next
      Return_Status := My_Counter.Increment_Counter (0);
      Count_Status_Assert.Eq (Return_Status, Two_Counter_Entry.Success);

      Return_Status := My_Counter.Increment_Counter (1);
      Count_Status_Assert.Eq (Return_Status, Two_Counter_Entry.Event_Max_Limit);

      Return_Status := My_Counter.Decrement_Counter (1);
      Count_Status_Assert.Eq (Return_Status, Two_Counter_Entry.Event_Max_Limit);
      Return_Status := My_Counter.Decrement_Counter (1);
      Count_Status_Assert.Eq (Return_Status, Two_Counter_Entry.Success);

      -- One more time just for fun
      My_Counter.Set_Persistence (3);
      Persistence := My_Counter.Get_Persistence;
      Natural_Assert.Eq (Natural (Persistence), 3);

      State_Return_Status := My_Counter.Get_Enable_State (6, Event_State);
      Event_State_Status_Assert.Eq (State_Return_Status, Two_Counter_Entry.Success);
      Event_State_Assert.Eq (Event_State, Event_State_Type.Enabled);

      Return_Status := My_Counter.Increment_Counter (6);
      Count_Status_Assert.Eq (Return_Status, Two_Counter_Entry.Success);
      Return_Status := My_Counter.Increment_Counter (6);
      Count_Status_Assert.Eq (Return_Status, Two_Counter_Entry.Success);
      Return_Status := My_Counter.Increment_Counter (6);
      Count_Status_Assert.Eq (Return_Status, Two_Counter_Entry.Success);
      Return_Status := My_Counter.Increment_Counter (6);
      Count_Status_Assert.Eq (Return_Status, Two_Counter_Entry.Event_Max_Limit);
      Return_Status := My_Counter.Increment_Counter (6);
      Count_Status_Assert.Eq (Return_Status, Two_Counter_Entry.Event_Max_Limit);
      Return_Status := My_Counter.Increment_Counter (6);
      Count_Status_Assert.Eq (Return_Status, Two_Counter_Entry.Event_Max_Limit);

      Return_Status := My_Counter.Increment_Counter (0);
      Count_Status_Assert.Eq (Return_Status, Two_Counter_Entry.Success);
      Return_Status := My_Counter.Increment_Counter (0);
      Count_Status_Assert.Eq (Return_Status, Two_Counter_Entry.Success);
      Return_Status := My_Counter.Increment_Counter (0);
      Count_Status_Assert.Eq (Return_Status, Two_Counter_Entry.Event_Max_Limit);

      Return_Status := My_Counter.Increment_Counter (1);
      Count_Status_Assert.Eq (Return_Status, Two_Counter_Entry.Success);
      Return_Status := My_Counter.Increment_Counter (1);
      Count_Status_Assert.Eq (Return_Status, Two_Counter_Entry.Success);
      Return_Status := My_Counter.Increment_Counter (1);
      Count_Status_Assert.Eq (Return_Status, Two_Counter_Entry.Success);
      Return_Status := My_Counter.Increment_Counter (1);
      Count_Status_Assert.Eq (Return_Status, Two_Counter_Entry.Event_Max_Limit);

      My_Counter.Destroy;
      pragma Unreferenced (My_Counter);
   end Test_Set_Persistence;

   overriding procedure Test_Event_Limited_Count (Self : in out Instance) is
      Ignore_Self : Instance renames Self;
      My_Counter : Two_Counter_Entry.Instance;
      Return_Status : Two_Counter_Entry.Count_Status;
      Event_Start_Empty_List : constant Two_Counter_Entry.Event_Id_List := [1 .. 0 => 0];
      Test_Limit_Count : Interfaces.Unsigned_16 := 0;
   begin
      My_Counter.Init (Event_Id_Start => 0, Event_Id_Stop => 6, Event_Disable_List => Event_Start_Empty_List, Event_Limit_Persistence => 2);

      -- All should be enabled so pick an id and increment unit we get a few max limits returned.
      Return_Status := My_Counter.Increment_Counter (3);
      Count_Status_Assert.Eq (Return_Status, Two_Counter_Entry.Success);
      Return_Status := My_Counter.Increment_Counter (3);
      Count_Status_Assert.Eq (Return_Status, Two_Counter_Entry.Success);
      Return_Status := My_Counter.Increment_Counter (3);
      Count_Status_Assert.Eq (Return_Status, Two_Counter_Entry.Event_Max_Limit);
      Return_Status := My_Counter.Increment_Counter (3);
      Count_Status_Assert.Eq (Return_Status, Two_Counter_Entry.Event_Max_Limit);
      Return_Status := My_Counter.Increment_Counter (3);
      Count_Status_Assert.Eq (Return_Status, Two_Counter_Entry.Event_Max_Limit);
      Return_Status := My_Counter.Increment_Counter (3);
      Count_Status_Assert.Eq (Return_Status, Two_Counter_Entry.Event_Max_Limit);
      Return_Status := My_Counter.Increment_Counter (3);
      Count_Status_Assert.Eq (Return_Status, Two_Counter_Entry.Event_Max_Limit);

      Test_Limit_Count := My_Counter.Get_Events_Limited_Count;
      Natural_Assert.Eq (Natural (Test_Limit_Count), 5);

      -- Reset the count
      My_Counter.Reset_Event_Limited_Count;
      Test_Limit_Count := My_Counter.Get_Events_Limited_Count;
      Natural_Assert.Eq (Natural (Test_Limit_Count), 0);
      -- Chose a couple different ID and check that the count is an aggregate of all max limits
      Return_Status := My_Counter.Increment_Counter (1);
      Count_Status_Assert.Eq (Return_Status, Two_Counter_Entry.Success);
      Test_Limit_Count := My_Counter.Get_Events_Limited_Count;
      Natural_Assert.Eq (Natural (Test_Limit_Count), 0);

      Return_Status := My_Counter.Increment_Counter (1);
      Count_Status_Assert.Eq (Return_Status, Two_Counter_Entry.Success);
      Return_Status := My_Counter.Increment_Counter (1);
      Count_Status_Assert.Eq (Return_Status, Two_Counter_Entry.Event_Max_Limit);
      Return_Status := My_Counter.Increment_Counter (1);
      Count_Status_Assert.Eq (Return_Status, Two_Counter_Entry.Event_Max_Limit);
      Return_Status := My_Counter.Increment_Counter (1);
      Count_Status_Assert.Eq (Return_Status, Two_Counter_Entry.Event_Max_Limit);

      Return_Status := My_Counter.Increment_Counter (2);
      Count_Status_Assert.Eq (Return_Status, Two_Counter_Entry.Success);
      Return_Status := My_Counter.Increment_Counter (2);
      Count_Status_Assert.Eq (Return_Status, Two_Counter_Entry.Success);
      Return_Status := My_Counter.Increment_Counter (2);
      Count_Status_Assert.Eq (Return_Status, Two_Counter_Entry.Event_Max_Limit);
      Return_Status := My_Counter.Increment_Counter (2);
      Count_Status_Assert.Eq (Return_Status, Two_Counter_Entry.Event_Max_Limit);
      Return_Status := My_Counter.Increment_Counter (2);
      Count_Status_Assert.Eq (Return_Status, Two_Counter_Entry.Event_Max_Limit);

      Return_Status := My_Counter.Increment_Counter (3);
      Count_Status_Assert.Eq (Return_Status, Two_Counter_Entry.Event_Max_Limit);

      Test_Limit_Count := My_Counter.Get_Events_Limited_Count;
      Natural_Assert.Eq (Natural (Test_Limit_Count), 7);

      My_Counter.Destroy;
      pragma Unreferenced (My_Counter);
   end Test_Event_Limited_Count;

   overriding procedure Test_Get_Event_Range (Self : in out Instance) is
      Ignore_Self : Instance renames Self;
      use Event_Types;
      My_Counter : Two_Counter_Entry.Instance;
      Event_Start_List : constant Two_Counter_Entry.Event_Id_List := [4, 5];
      Start_Id : Event_Id;
      Stop_Id : Event_Id;
   begin
      My_Counter.Init (Event_Id_Start => 0, Event_Id_Stop => 5, Event_Disable_List => Event_Start_List, Event_Limit_Persistence => 7);

      Start_Id := My_Counter.Get_Event_Start_Stop_Range (Stop_Id);
      Natural_Assert.Eq (Natural (Start_Id), 0);
      Natural_Assert.Eq (Natural (Stop_Id), 5);
      My_Counter.Destroy;

      -- Do one more range shifted to make sure.
      My_Counter.Init (Event_Id_Start => 4, Event_Id_Stop => 20, Event_Disable_List => Event_Start_List, Event_Limit_Persistence => 7);

      Start_Id := My_Counter.Get_Event_Start_Stop_Range (Stop_Id);
      Natural_Assert.Eq (Natural (Start_Id), 4);
      Natural_Assert.Eq (Natural (Stop_Id), 20);

      My_Counter.Destroy;
      pragma Unreferenced (My_Counter);
   end Test_Get_Event_Range;

   overriding procedure Test_Master_Enable_Switch (Self : in out Instance) is
      Ignore_Self : Instance renames Self;
      My_Counter : Two_Counter_Entry.Instance;
      Event_Start_List : constant Two_Counter_Entry.Event_Id_List := [4, 5];
      Master_State : Event_State_Type.E;
   begin
      My_Counter.Init (Event_Id_Start => 0, Event_Id_Stop => 5, Event_Disable_List => Event_Start_List, Event_Limit_Persistence => 7);

      -- Make sure we start enabled
      Master_State := My_Counter.Get_Master_Enable_State;
      Event_State_Assert.Eq (Master_State, Event_State_Type.Enabled);

      -- Switch to disabled
      My_Counter.Set_Master_Enable_State (Event_State_Type.Disabled);
      Master_State := My_Counter.Get_Master_Enable_State;
      Event_State_Assert.Eq (Master_State, Event_State_Type.Disabled);

      -- Back to enabled
      My_Counter.Set_Master_Enable_State (Event_State_Type.Enabled);
      Master_State := My_Counter.Get_Master_Enable_State;
      Event_State_Assert.Eq (Master_State, Event_State_Type.Enabled);

      My_Counter.Destroy;
      pragma Unreferenced (My_Counter);
   end Test_Master_Enable_Switch;

end Two_Counter_Entry_Tests.Implementation;
