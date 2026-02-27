--------------------------------------------------------------------------------
-- Event_Filter_Entry Tests Body
--------------------------------------------------------------------------------

with AUnit.Assertions; use AUnit.Assertions;
with Basic_Assertions; use Basic_Assertions;
with Basic_Types;
with Smart_Assert;
with Event_Filter_Entry_Enums;
with Interfaces; use Interfaces;
with Event_Types;
with Event_Filter_Entry.Tester;
with Event_Filter_Entry_Type;

package body Event_Filter_Entry_Tests.Implementation is

   use Event_Filter_Entry;
   use Event_Filter_Entry_Enums;
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
   package Event_Filter_Status_Assert is new Smart_Assert.Basic (Event_Filter_Entry.Filter_Status, Event_Filter_Entry.Filter_Status'Image);
   package Event_Entry_Status_Assert is new Smart_Assert.Basic (Event_Filter_Entry.Event_Entry_Status, Event_Filter_Entry.Event_Entry_Status'Image);
   package Global_State_Assert is new Smart_Assert.Basic (Global_Filter_State.E, Global_Filter_State.E'Image);

   -- Helper function
   function Event_Id_List_To_Byte_Array (Start_Id : in Event_Types.Event_Id; Stop_Id : in Event_Types.Event_Id; Event_Filter_List : in Event_Id_List) return Basic_Types.Byte_Array_Access is
      use Event_Types;
      Event_Array : Basic_Types.Byte_Array_Access := null;
      Num_Events : Natural;
   begin
      Num_Events := (Natural (Stop_Id - Start_Id) + 8) / 8;
      Event_Array := new Basic_Types.Byte_Array (0 .. Num_Events - 1);
      Event_Array.all := [others => Event_Filter_Entry_Type.Serialization.To_Byte_Array ((
         State_0 => Event_Filter_State.Unfiltered,
         State_1 => Event_Filter_State.Unfiltered,
         State_2 => Event_Filter_State.Unfiltered,
         State_3 => Event_Filter_State.Unfiltered,
         State_4 => Event_Filter_State.Unfiltered,
         State_5 => Event_Filter_State.Unfiltered,
         State_6 => Event_Filter_State.Unfiltered,
         State_7 => Event_Filter_State.Unfiltered
      )) (0)];
      for Event_Id_State_Change of Event_Filter_List loop
         declare
            Event_Id_In : constant Natural := Natural (Event_Id_State_Change - Start_Id) / 8;
            Bit_Location : constant Bit_Location_Type := Bit_Location_Type (Natural (Event_Id_State_Change - Start_Id) mod 8);
            Overlay : Event_Filter_Entry_Type.T with
               Import,
               Convention => Ada,
               Address => Event_Array (Event_Id_In)'Address;
         begin
            case Bit_Location is
               when 0 =>
                  Overlay.State_0 := Event_Filter_State.Filtered;
               when 1 =>
                  Overlay.State_1 := Event_Filter_State.Filtered;
               when 2 =>
                  Overlay.State_2 := Event_Filter_State.Filtered;
               when 3 =>
                  Overlay.State_3 := Event_Filter_State.Filtered;
               when 4 =>
                  Overlay.State_4 := Event_Filter_State.Filtered;
               when 5 =>
                  Overlay.State_5 := Event_Filter_State.Filtered;
               when 6 =>
                  Overlay.State_6 := Event_Filter_State.Filtered;
               when 7 =>
                  Overlay.State_7 := Event_Filter_State.Filtered;
            end case;
         end;
      end loop;
      return Event_Array;
   end Event_Id_List_To_Byte_Array;

   -------------------------------------------------------------------------
   -- Tests:
   -------------------------------------------------------------------------

   overriding procedure Test_Init_List (Self : in out Instance) is
      Ignore_Self : Instance renames Self;
      Event_Filter : Event_Filter_Entry.Instance;
      Event_Start_List : constant Event_Id_List := [3, 4];
      Event_Start_Empty_List : constant Event_Id_List := [1 .. 0 => 0];
      State_Return_Status : Filter_Status;

      -- Test setup for catching an invalid input range
      procedure Invalid_Init_Range is
         Start_List : constant Event_Id_List := [2, 5];
      begin
         Event_Filter.Init (Event_Id_Start => 7, Event_Id_Stop => 2, Event_Filter_List => Start_List);
      exception
         -- Expecting the assert to be thrown here:
         when others =>
            Assert (True, "Invalid Event ID Range assert failed!");
      end Invalid_Init_Range;

   begin
      Event_Filter.Init (Event_Id_Start => 0, Event_Id_Stop => 4, Event_Filter_List => Event_Start_List);
      -- Get each event's state to determine if the starting disable list worked
      State_Return_Status := Event_Filter.Filter_Event (0);
      Event_Filter_Status_Assert.Eq (State_Return_Status, Unfiltered);

      State_Return_Status := Event_Filter.Filter_Event (1);
      Event_Filter_Status_Assert.Eq (State_Return_Status, Unfiltered);

      State_Return_Status := Event_Filter.Filter_Event (2);
      Event_Filter_Status_Assert.Eq (State_Return_Status, Unfiltered);

      State_Return_Status := Event_Filter.Filter_Event (3);
      Event_Filter_Status_Assert.Eq (State_Return_Status, Filtered);

      State_Return_Status := Event_Filter.Filter_Event (4);
      Event_Filter_Status_Assert.Eq (State_Return_Status, Filtered);

      -- Out of range check
      State_Return_Status := Event_Filter.Filter_Event (5);
      Event_Filter_Status_Assert.Eq (State_Return_Status, Out_Of_Range);
      Event_Filter.Destroy;

      Event_Filter.Init (Event_Id_Start => 3, Event_Id_Stop => 10, Event_Filter_List => Event_Start_List);

      -- Get each event's state to determine if the starting disable list worked
      State_Return_Status := Event_Filter.Filter_Event (3);
      Event_Filter_Status_Assert.Eq (State_Return_Status, Filtered);

      State_Return_Status := Event_Filter.Filter_Event (4);
      Event_Filter_Status_Assert.Eq (State_Return_Status, Filtered);

      State_Return_Status := Event_Filter.Filter_Event (5);
      Event_Filter_Status_Assert.Eq (State_Return_Status, Unfiltered);

      State_Return_Status := Event_Filter.Filter_Event (6);
      Event_Filter_Status_Assert.Eq (State_Return_Status, Unfiltered);

      State_Return_Status := Event_Filter.Filter_Event (7);
      Event_Filter_Status_Assert.Eq (State_Return_Status, Unfiltered);

      State_Return_Status := Event_Filter.Filter_Event (8);
      Event_Filter_Status_Assert.Eq (State_Return_Status, Unfiltered);

      State_Return_Status := Event_Filter.Filter_Event (9);
      Event_Filter_Status_Assert.Eq (State_Return_Status, Unfiltered);

      State_Return_Status := Event_Filter.Filter_Event (10);
      Event_Filter_Status_Assert.Eq (State_Return_Status, Unfiltered);

      -- Out of range check
      State_Return_Status := Event_Filter.Filter_Event (2);
      Event_Filter_Status_Assert.Eq (State_Return_Status, Out_Of_Range);
      State_Return_Status := Event_Filter.Filter_Event (11);
      Event_Filter_Status_Assert.Eq (State_Return_Status, Out_Of_Range);
      Event_Filter.Destroy;

      -- An empty list test
      Event_Filter.Init (Event_Id_Start => 1, Event_Id_Stop => 6, Event_Filter_List => Event_Start_Empty_List);

      -- Get each event's state to determine if the starting disable list worked
      State_Return_Status := Event_Filter.Filter_Event (1);
      Event_Filter_Status_Assert.Eq (State_Return_Status, Unfiltered);

      State_Return_Status := Event_Filter.Filter_Event (2);
      Event_Filter_Status_Assert.Eq (State_Return_Status, Unfiltered);

      State_Return_Status := Event_Filter.Filter_Event (3);
      Event_Filter_Status_Assert.Eq (State_Return_Status, Unfiltered);

      State_Return_Status := Event_Filter.Filter_Event (4);
      Event_Filter_Status_Assert.Eq (State_Return_Status, Unfiltered);

      State_Return_Status := Event_Filter.Filter_Event (5);
      Event_Filter_Status_Assert.Eq (State_Return_Status, Unfiltered);

      State_Return_Status := Event_Filter.Filter_Event (6);
      Event_Filter_Status_Assert.Eq (State_Return_Status, Unfiltered);

      -- Out of range check
      State_Return_Status := Event_Filter.Filter_Event (0);
      Event_Filter_Status_Assert.Eq (State_Return_Status, Out_Of_Range);
      State_Return_Status := Event_Filter.Filter_Event (7);
      Event_Filter_Status_Assert.Eq (State_Return_Status, Out_Of_Range);
      Event_Filter.Destroy;

      -- Test that a range of 0 ids works
      Event_Filter.Init (Event_Id_Start => 1, Event_Id_Stop => 1, Event_Filter_List => Event_Start_Empty_List);
      -- Get each event's state to determine if the starting disable list worked
      State_Return_Status := Event_Filter.Filter_Event (1);
      Event_Filter_Status_Assert.Eq (State_Return_Status, Unfiltered);

      -- Since there is only one, test the boundary cases
      State_Return_Status := Event_Filter.Filter_Event (0);
      Event_Filter_Status_Assert.Eq (State_Return_Status, Out_Of_Range);

      State_Return_Status := Event_Filter.Filter_Event (2);
      Event_Filter_Status_Assert.Eq (State_Return_Status, Out_Of_Range);

      Event_Filter.Destroy;

      -- Test that an invalid range of ids is caught by our assert
      Invalid_Init_Range;
      pragma Unreferenced (Event_Filter);
   end Test_Init_List;

   overriding procedure Test_Set_Event_State (Self : in out Instance) is
      Ignore_Self : Instance renames Self;
      Event_Filter : Event_Filter_Entry.Instance;
      Event_Start_List : constant Event_Id_List := [3, 4];
      Entry_State : Event_Entry_Status;
      State_Filter_Status : Filter_Status;
   begin
      Event_Filter.Init (Event_Id_Start => 0, Event_Id_Stop => 6, Event_Filter_List => Event_Start_List);

      -- Check that an event is unfiltered, then change the state and check again
      State_Filter_Status := Event_Filter.Filter_Event (0);
      Event_Filter_Status_Assert.Eq (State_Filter_Status, Unfiltered);

      Entry_State := Event_Filter.Set_Filter_State (0, Event_Filter_State.Filtered);
      Event_Entry_Status_Assert.Eq (Entry_State, Success);

      State_Filter_Status := Event_Filter.Filter_Event (0);
      Event_Filter_Status_Assert.Eq (State_Filter_Status, Filtered);

      -- Now reverse back on that same id
      Entry_State := Event_Filter.Set_Filter_State (0, Event_Filter_State.Unfiltered);
      Event_Entry_Status_Assert.Eq (Entry_State, Success);
      State_Filter_Status := Event_Filter.Filter_Event (0);
      Event_Filter_Status_Assert.Eq (State_Filter_Status, Unfiltered);

      -- Same process as before but reversed on one that begins filtered
      State_Filter_Status := Event_Filter.Filter_Event (3);
      Event_Filter_Status_Assert.Eq (State_Filter_Status, Filtered);

      Entry_State := Event_Filter.Set_Filter_State (3, Event_Filter_State.Unfiltered);
      Event_Entry_Status_Assert.Eq (Entry_State, Success);

      State_Filter_Status := Event_Filter.Filter_Event (3);
      Event_Filter_Status_Assert.Eq (State_Filter_Status, Unfiltered);

      Entry_State := Event_Filter.Set_Filter_State (3, Event_Filter_State.Filtered);
      Event_Entry_Status_Assert.Eq (Entry_State, Success);

      State_Filter_Status := Event_Filter.Filter_Event (3);
      Event_Filter_Status_Assert.Eq (State_Filter_Status, Filtered);

      -- One more to cover the full range of the function
      State_Filter_Status := Event_Filter.Filter_Event (6);
      Event_Filter_Status_Assert.Eq (State_Filter_Status, Unfiltered);

      Entry_State := Event_Filter.Set_Filter_State (6, Event_Filter_State.Filtered);
      Event_Entry_Status_Assert.Eq (Entry_State, Success);

      State_Filter_Status := Event_Filter.Filter_Event (6);
      Event_Filter_Status_Assert.Eq (State_Filter_Status, Filtered);

      Entry_State := Event_Filter.Set_Filter_State (6, Event_Filter_State.Unfiltered);
      Event_Entry_Status_Assert.Eq (Entry_State, Success);

      State_Filter_Status := Event_Filter.Filter_Event (6);
      Event_Filter_Status_Assert.Eq (State_Filter_Status, Unfiltered);

      -- Out of range check
      Entry_State := Event_Filter.Set_Filter_State (7, Event_Filter_State.Filtered);
      Event_Entry_Status_Assert.Eq (Entry_State, Invalid_Id);

      Event_Filter.Destroy;
      pragma Unreferenced (Event_Filter);
   end Test_Set_Event_State;

   overriding procedure Test_Event_Filtered_Count (Self : in out Instance) is
      Ignore_Self : Instance renames Self;
      Event_Filter : Event_Filter_Entry.Instance;
      Event_Start_List : constant Event_Id_List := [3, 5];
      State_Filter_Status : Filter_Status;
      Filter_Count : Interfaces.Unsigned_32;
   begin

      Event_Filter.Init (Event_Id_Start => 2, Event_Id_Stop => 11, Event_Filter_List => Event_Start_List);
      -- Start by making sure the count starts at 0
      Filter_Count := Event_Filter.Get_Event_Filtered_Count;
      Unsigned_32_Assert.Eq (Filter_Count, 0);

      -- Send in a few events that should not be filtered
      State_Filter_Status := Event_Filter.Filter_Event (2);
      Event_Filter_Status_Assert.Eq (State_Filter_Status, Unfiltered);
      State_Filter_Status := Event_Filter.Filter_Event (2);
      Event_Filter_Status_Assert.Eq (State_Filter_Status, Unfiltered);
      State_Filter_Status := Event_Filter.Filter_Event (2);
      Event_Filter_Status_Assert.Eq (State_Filter_Status, Unfiltered);
      State_Filter_Status := Event_Filter.Filter_Event (2);
      Event_Filter_Status_Assert.Eq (State_Filter_Status, Unfiltered);

      State_Filter_Status := Event_Filter.Filter_Event (4);
      Event_Filter_Status_Assert.Eq (State_Filter_Status, Unfiltered);
      State_Filter_Status := Event_Filter.Filter_Event (4);
      Event_Filter_Status_Assert.Eq (State_Filter_Status, Unfiltered);
      State_Filter_Status := Event_Filter.Filter_Event (4);
      Event_Filter_Status_Assert.Eq (State_Filter_Status, Unfiltered);

      -- Count should still be 0
      Filter_Count := Event_Filter.Get_Event_Filtered_Count;
      Unsigned_32_Assert.Eq (Filter_Count, 0);

      -- Filtered events should increment the count
      State_Filter_Status := Event_Filter.Filter_Event (3);
      Event_Filter_Status_Assert.Eq (State_Filter_Status, Filtered);
      State_Filter_Status := Event_Filter.Filter_Event (3);
      Event_Filter_Status_Assert.Eq (State_Filter_Status, Filtered);
      State_Filter_Status := Event_Filter.Filter_Event (3);
      Event_Filter_Status_Assert.Eq (State_Filter_Status, Filtered);

      State_Filter_Status := Event_Filter.Filter_Event (5);
      Event_Filter_Status_Assert.Eq (State_Filter_Status, Filtered);
      State_Filter_Status := Event_Filter.Filter_Event (5);
      Event_Filter_Status_Assert.Eq (State_Filter_Status, Filtered);
      State_Filter_Status := Event_Filter.Filter_Event (5);
      Event_Filter_Status_Assert.Eq (State_Filter_Status, Filtered);
      State_Filter_Status := Event_Filter.Filter_Event (5);
      Event_Filter_Status_Assert.Eq (State_Filter_Status, Filtered);

      -- Count should now be 7
      Filter_Count := Event_Filter.Get_Event_Filtered_Count;
      Unsigned_32_Assert.Eq (Filter_Count, 7);

      -- Mix and match a few filtered and unfiltered
      State_Filter_Status := Event_Filter.Filter_Event (6);
      Event_Filter_Status_Assert.Eq (State_Filter_Status, Unfiltered);
      State_Filter_Status := Event_Filter.Filter_Event (6);
      Event_Filter_Status_Assert.Eq (State_Filter_Status, Unfiltered);

      State_Filter_Status := Event_Filter.Filter_Event (5);
      Event_Filter_Status_Assert.Eq (State_Filter_Status, Filtered);
      State_Filter_Status := Event_Filter.Filter_Event (5);
      Event_Filter_Status_Assert.Eq (State_Filter_Status, Filtered);

      State_Filter_Status := Event_Filter.Filter_Event (7);
      Event_Filter_Status_Assert.Eq (State_Filter_Status, Unfiltered);
      State_Filter_Status := Event_Filter.Filter_Event (7);
      Event_Filter_Status_Assert.Eq (State_Filter_Status, Unfiltered);

      State_Filter_Status := Event_Filter.Filter_Event (3);
      Event_Filter_Status_Assert.Eq (State_Filter_Status, Filtered);
      State_Filter_Status := Event_Filter.Filter_Event (3);
      Event_Filter_Status_Assert.Eq (State_Filter_Status, Filtered);

      State_Filter_Status := Event_Filter.Filter_Event (8);
      Event_Filter_Status_Assert.Eq (State_Filter_Status, Unfiltered);
      State_Filter_Status := Event_Filter.Filter_Event (8);
      Event_Filter_Status_Assert.Eq (State_Filter_Status, Unfiltered);

      -- Count should now be 11
      Filter_Count := Event_Filter.Get_Event_Filtered_Count;
      Unsigned_32_Assert.Eq (Filter_Count, 11);

      -- Send a few more out of range event ids to make sure they don't count as filtered
      State_Filter_Status := Event_Filter.Filter_Event (0);
      Event_Filter_Status_Assert.Eq (State_Filter_Status, Out_Of_Range);
      State_Filter_Status := Event_Filter.Filter_Event (0);
      Event_Filter_Status_Assert.Eq (State_Filter_Status, Out_Of_Range);
      State_Filter_Status := Event_Filter.Filter_Event (1);
      Event_Filter_Status_Assert.Eq (State_Filter_Status, Out_Of_Range);
      State_Filter_Status := Event_Filter.Filter_Event (1);
      Event_Filter_Status_Assert.Eq (State_Filter_Status, Out_Of_Range);
      State_Filter_Status := Event_Filter.Filter_Event (12);
      Event_Filter_Status_Assert.Eq (State_Filter_Status, Out_Of_Range);
      State_Filter_Status := Event_Filter.Filter_Event (12);
      Event_Filter_Status_Assert.Eq (State_Filter_Status, Out_Of_Range);
      State_Filter_Status := Event_Filter.Filter_Event (13);
      Event_Filter_Status_Assert.Eq (State_Filter_Status, Out_Of_Range);
      State_Filter_Status := Event_Filter.Filter_Event (14);
      Event_Filter_Status_Assert.Eq (State_Filter_Status, Out_Of_Range);
      -- Should be no change to the count
      Filter_Count := Event_Filter.Get_Event_Filtered_Count;
      Unsigned_32_Assert.Eq (Filter_Count, 11);

      -- Test roll over conditions
      Event_Filter_Entry.Tester.Set_Filtered_Count (Event_Filter, Unsigned_32'Last - 4);
      Unsigned_32_Assert.Eq (Event_Filter_Entry.Tester.Get_Filtered_Count (Event_Filter), Unsigned_32'Last - 4);

      -- Count should now be at what we set it to
      Filter_Count := Event_Filter.Get_Event_Filtered_Count;
      Unsigned_32_Assert.Eq (Filter_Count, Unsigned_32'Last - 4);

      -- Filtered events should increment the count
      State_Filter_Status := Event_Filter.Filter_Event (3);
      Event_Filter_Status_Assert.Eq (State_Filter_Status, Filtered);
      State_Filter_Status := Event_Filter.Filter_Event (3);
      Event_Filter_Status_Assert.Eq (State_Filter_Status, Filtered);
      State_Filter_Status := Event_Filter.Filter_Event (3);
      Event_Filter_Status_Assert.Eq (State_Filter_Status, Filtered);

      State_Filter_Status := Event_Filter.Filter_Event (5);
      Event_Filter_Status_Assert.Eq (State_Filter_Status, Filtered);
      State_Filter_Status := Event_Filter.Filter_Event (5);
      Event_Filter_Status_Assert.Eq (State_Filter_Status, Filtered);

      -- Should be back to 0 here
      Filter_Count := Event_Filter.Get_Event_Filtered_Count;
      Unsigned_32_Assert.Eq (Filter_Count, 0);

      State_Filter_Status := Event_Filter.Filter_Event (5);
      Event_Filter_Status_Assert.Eq (State_Filter_Status, Filtered);
      State_Filter_Status := Event_Filter.Filter_Event (5);
      Event_Filter_Status_Assert.Eq (State_Filter_Status, Filtered);

      -- Count should now be 2
      Filter_Count := Event_Filter.Get_Event_Filtered_Count;
      Unsigned_32_Assert.Eq (Filter_Count, 2);

      Event_Filter.Destroy;
      pragma Unreferenced (Event_Filter);
   end Test_Event_Filtered_Count;

   overriding procedure Test_Event_Unfiltered_Count (Self : in out Instance) is
      Ignore_Self : Instance renames Self;
      Event_Filter : Event_Filter_Entry.Instance;
      Event_Start_List : constant Event_Id_List := [6, 7, 9, 10];
      State_Filter_Status : Filter_Status;
      Unfiltered_Count : Interfaces.Unsigned_32;
   begin

      Event_Filter.Init (Event_Id_Start => 1, Event_Id_Stop => 11, Event_Filter_List => Event_Start_List);
      -- Start by making sure the count starts at 0
      Unfiltered_Count := Event_Filter.Get_Event_Unfiltered_Count;
      Unsigned_32_Assert.Eq (Unfiltered_Count, 0);

      -- Send in a few events that should not be filtered
      State_Filter_Status := Event_Filter.Filter_Event (2);
      Event_Filter_Status_Assert.Eq (State_Filter_Status, Unfiltered);
      State_Filter_Status := Event_Filter.Filter_Event (2);
      Event_Filter_Status_Assert.Eq (State_Filter_Status, Unfiltered);
      State_Filter_Status := Event_Filter.Filter_Event (2);
      Event_Filter_Status_Assert.Eq (State_Filter_Status, Unfiltered);
      State_Filter_Status := Event_Filter.Filter_Event (2);
      Event_Filter_Status_Assert.Eq (State_Filter_Status, Unfiltered);

      State_Filter_Status := Event_Filter.Filter_Event (4);
      Event_Filter_Status_Assert.Eq (State_Filter_Status, Unfiltered);
      State_Filter_Status := Event_Filter.Filter_Event (4);
      Event_Filter_Status_Assert.Eq (State_Filter_Status, Unfiltered);
      State_Filter_Status := Event_Filter.Filter_Event (4);
      Event_Filter_Status_Assert.Eq (State_Filter_Status, Unfiltered);

      -- Count should still be 7
      Unfiltered_Count := Event_Filter.Get_Event_Unfiltered_Count;
      Unsigned_32_Assert.Eq (Unfiltered_Count, 7);

      -- Filtered events should not increment the count
      State_Filter_Status := Event_Filter.Filter_Event (6);
      Event_Filter_Status_Assert.Eq (State_Filter_Status, Filtered);
      State_Filter_Status := Event_Filter.Filter_Event (6);
      Event_Filter_Status_Assert.Eq (State_Filter_Status, Filtered);
      State_Filter_Status := Event_Filter.Filter_Event (6);
      Event_Filter_Status_Assert.Eq (State_Filter_Status, Filtered);

      State_Filter_Status := Event_Filter.Filter_Event (10);
      Event_Filter_Status_Assert.Eq (State_Filter_Status, Filtered);
      State_Filter_Status := Event_Filter.Filter_Event (10);
      Event_Filter_Status_Assert.Eq (State_Filter_Status, Filtered);
      State_Filter_Status := Event_Filter.Filter_Event (10);
      Event_Filter_Status_Assert.Eq (State_Filter_Status, Filtered);
      State_Filter_Status := Event_Filter.Filter_Event (10);
      Event_Filter_Status_Assert.Eq (State_Filter_Status, Filtered);

      -- Count should still be at 7
      Unfiltered_Count := Event_Filter.Get_Event_Unfiltered_Count;
      Unsigned_32_Assert.Eq (Unfiltered_Count, 7);

      -- Mix and match a few filtered and unfiltered
      State_Filter_Status := Event_Filter.Filter_Event (5);
      Event_Filter_Status_Assert.Eq (State_Filter_Status, Unfiltered);
      State_Filter_Status := Event_Filter.Filter_Event (5);
      Event_Filter_Status_Assert.Eq (State_Filter_Status, Unfiltered);

      State_Filter_Status := Event_Filter.Filter_Event (7);
      Event_Filter_Status_Assert.Eq (State_Filter_Status, Filtered);
      State_Filter_Status := Event_Filter.Filter_Event (7);
      Event_Filter_Status_Assert.Eq (State_Filter_Status, Filtered);

      State_Filter_Status := Event_Filter.Filter_Event (8);
      Event_Filter_Status_Assert.Eq (State_Filter_Status, Unfiltered);
      State_Filter_Status := Event_Filter.Filter_Event (8);
      Event_Filter_Status_Assert.Eq (State_Filter_Status, Unfiltered);

      State_Filter_Status := Event_Filter.Filter_Event (9);
      Event_Filter_Status_Assert.Eq (State_Filter_Status, Filtered);
      State_Filter_Status := Event_Filter.Filter_Event (9);
      Event_Filter_Status_Assert.Eq (State_Filter_Status, Filtered);

      State_Filter_Status := Event_Filter.Filter_Event (11);
      Event_Filter_Status_Assert.Eq (State_Filter_Status, Unfiltered);
      State_Filter_Status := Event_Filter.Filter_Event (11);
      Event_Filter_Status_Assert.Eq (State_Filter_Status, Unfiltered);

      -- Count should now be 13
      Unfiltered_Count := Event_Filter.Get_Event_Unfiltered_Count;
      Unsigned_32_Assert.Eq (Unfiltered_Count, 13);

      -- Send a few more out of range event ids to make sure they don't count as filtered
      State_Filter_Status := Event_Filter.Filter_Event (0);
      Event_Filter_Status_Assert.Eq (State_Filter_Status, Out_Of_Range);
      State_Filter_Status := Event_Filter.Filter_Event (0);
      Event_Filter_Status_Assert.Eq (State_Filter_Status, Out_Of_Range);
      State_Filter_Status := Event_Filter.Filter_Event (12);
      Event_Filter_Status_Assert.Eq (State_Filter_Status, Out_Of_Range);
      State_Filter_Status := Event_Filter.Filter_Event (12);
      Event_Filter_Status_Assert.Eq (State_Filter_Status, Out_Of_Range);
      State_Filter_Status := Event_Filter.Filter_Event (13);
      Event_Filter_Status_Assert.Eq (State_Filter_Status, Out_Of_Range);
      -- Should be no change to the count
      Unfiltered_Count := Event_Filter.Get_Event_Unfiltered_Count;
      Unsigned_32_Assert.Eq (Unfiltered_Count, 13);

      Event_Filter.Destroy;

      -- New instance for testing rollover
      Event_Filter.Init (Event_Id_Start => 1, Event_Id_Stop => 11, Event_Filter_List => Event_Start_List);

      Event_Filter_Entry.Tester.Set_Unfiltered_Count (Event_Filter, Unsigned_32'Last - 3);
      Unsigned_32_Assert.Eq (Event_Filter_Entry.Tester.Get_Unfiltered_Count (Event_Filter), Unsigned_32'Last - 3);

      -- Count should now be at what we set it to
      Unfiltered_Count := Event_Filter.Get_Event_Unfiltered_Count;
      Unsigned_32_Assert.Eq (Unfiltered_Count, Unsigned_32'Last - 3);

      -- Filtered events should increment the count
      State_Filter_Status := Event_Filter.Filter_Event (2);
      Event_Filter_Status_Assert.Eq (State_Filter_Status, Unfiltered);
      State_Filter_Status := Event_Filter.Filter_Event (2);
      Event_Filter_Status_Assert.Eq (State_Filter_Status, Unfiltered);
      State_Filter_Status := Event_Filter.Filter_Event (2);
      Event_Filter_Status_Assert.Eq (State_Filter_Status, Unfiltered);

      State_Filter_Status := Event_Filter.Filter_Event (4);
      Event_Filter_Status_Assert.Eq (State_Filter_Status, Unfiltered);
      State_Filter_Status := Event_Filter.Filter_Event (4);
      Event_Filter_Status_Assert.Eq (State_Filter_Status, Unfiltered);
      State_Filter_Status := Event_Filter.Filter_Event (4);
      Event_Filter_Status_Assert.Eq (State_Filter_Status, Unfiltered);
      State_Filter_Status := Event_Filter.Filter_Event (4);
      Event_Filter_Status_Assert.Eq (State_Filter_Status, Unfiltered);

      -- Should have rolled here
      Unfiltered_Count := Event_Filter.Get_Event_Unfiltered_Count;
      Unsigned_32_Assert.Eq (Unfiltered_Count, 3);

      State_Filter_Status := Event_Filter.Filter_Event (11);
      Event_Filter_Status_Assert.Eq (State_Filter_Status, Unfiltered);
      State_Filter_Status := Event_Filter.Filter_Event (11);
      Event_Filter_Status_Assert.Eq (State_Filter_Status, Unfiltered);

      -- Count should now be 5
      Unfiltered_Count := Event_Filter.Get_Event_Unfiltered_Count;
      Unsigned_32_Assert.Eq (Unfiltered_Count, 5);

      Event_Filter.Destroy;
      pragma Unreferenced (Event_Filter);
   end Test_Event_Unfiltered_Count;

   overriding procedure Test_Get_Event_Range (Self : in out Instance) is
      Ignore_Self : Instance renames Self;
      use Event_Types;
      Event_Filter : Event_Filter_Entry.Instance;
      Event_Start_List : constant Event_Id_List := [3, 5];
      Start_Id : Event_Id;
      Stop_Id : Event_Id;
   begin
      Event_Filter.Init (Event_Id_Start => 0, Event_Id_Stop => 5, Event_Filter_List => Event_Start_List);

      Start_Id := Event_Filter.Get_Event_Start_Stop_Range (Stop_Id);
      Natural_Assert.Eq (Natural (Start_Id), 0);
      Natural_Assert.Eq (Natural (Stop_Id), 5);
      Event_Filter.Destroy;

      -- Do one more range shifted to make sure the correct values are returned
      Event_Filter.Init (Event_Id_Start => 3, Event_Id_Stop => 15, Event_Filter_List => Event_Start_List);

      Start_Id := Event_Filter.Get_Event_Start_Stop_Range (Stop_Id);
      Natural_Assert.Eq (Natural (Start_Id), 3);
      Natural_Assert.Eq (Natural (Stop_Id), 15);

      Event_Filter.Destroy;
      pragma Unreferenced (Event_Filter);
   end Test_Get_Event_Range;

   overriding procedure Test_Global_Enable_Switch (Self : in out Instance) is
      Ignore_Self : Instance renames Self;
      Event_Filter : Event_Filter_Entry.Instance;
      Event_Start_List : constant Event_Id_List := [4, 7];
      Global_State : Global_Filter_State.E;
      State_Filter_Status : Filter_Status;
   begin
      Event_Filter.Init (Event_Id_Start => 0, Event_Id_Stop => 9, Event_Filter_List => Event_Start_List);

      -- Make sure we start enabled
      Global_State := Event_Filter.Get_Global_Enable_State;
      Global_State_Assert.Eq (Global_State, Global_Filter_State.Enabled);

      -- Switch to disabled
      Event_Filter.Set_Global_Enable_State (Global_Filter_State.Disabled);
      Global_State := Event_Filter.Get_Global_Enable_State;
      Global_State_Assert.Eq (Global_State, Global_Filter_State.Disabled);

      -- Test by sending in an event that should be filtered if the state was enabled
      State_Filter_Status := Event_Filter.Filter_Event (7);
      Event_Filter_Status_Assert.Eq (State_Filter_Status, Unfiltered);

      -- Back to enabled
      Event_Filter.Set_Global_Enable_State (Global_Filter_State.Enabled);
      Global_State := Event_Filter.Get_Global_Enable_State;
      Global_State_Assert.Eq (Global_State, Global_Filter_State.Enabled);

      State_Filter_Status := Event_Filter.Filter_Event (7);
      Event_Filter_Status_Assert.Eq (State_Filter_Status, Filtered);

      Event_Filter.Destroy;
      pragma Unreferenced (Event_Filter);
   end Test_Global_Enable_Switch;

   overriding procedure Test_Get_Entry_Array (Self : in out Instance) is
      Ignore_Self : Instance renames Self;
      use Event_Types;
      Event_Filter : Event_Filter_Entry.Instance;
      Entry_State : Event_Entry_Status;
      Event_Start_List : constant Event_Id_List := [3, 5];
      Event_End_List : constant Event_Id_List := [0, 3, 5, 10];
      Start_Id : constant Event_Id := 0;
      Stop_Id : constant Event_Id := 10;
      Event_Array : Basic_Types.Byte_Array_Access := null;
      Expected_Array : Basic_Types.Byte_Array_Access := null;
   begin
      Event_Filter.Init (Event_Id_Start => Start_Id, Event_Id_Stop => Stop_Id, Event_Filter_List => Event_Start_List);

      -- From the init info, construct a byte array that should look the same as the event array that will be created in the package
      Expected_Array := Event_Id_List_To_Byte_Array (Start_Id, Stop_Id, Event_Start_List);
      -- Get the array and test it against our known since we know what should or should not be disabled.
      Event_Array := Event_Filter.Get_Entry_Array;
      Byte_Array_Assert.Eq (Event_Array.all, Expected_Array.all);

      -- Now set a few more to enabled and test again
      Entry_State := Event_Filter.Set_Filter_State (0, Event_Filter_State.Filtered);
      Event_Entry_Status_Assert.Eq (Entry_State, Success);
      Entry_State := Event_Filter.Set_Filter_State (10, Event_Filter_State.Filtered);
      Event_Entry_Status_Assert.Eq (Entry_State, Success);

      Expected_Array := Event_Id_List_To_Byte_Array (Start_Id, Stop_Id, Event_End_List);
      Event_Array := Event_Filter.Get_Entry_Array;
      Byte_Array_Assert.Eq (Event_Array.all, Expected_Array.all);

      Event_Filter.Destroy;
      pragma Unreferenced (Event_Filter);
   end Test_Get_Entry_Array;

end Event_Filter_Entry_Tests.Implementation;
