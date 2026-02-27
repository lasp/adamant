--------------------------------------------------------------------------------
-- Event_Limiter Tests Body
--------------------------------------------------------------------------------

with Ada.Text_IO; use Ada.Text_IO;
with Basic_Assertions; use Basic_Assertions;
with Smart_Assert;
with Packed_U16.Assertion; use Packed_U16.Assertion;
with Packed_U32.Assertion; use Packed_U32.Assertion;
with Event;
with Event_Types; use Event_Types;
with Tick;
with Command;
with Packet;
with Command_Response.Assertion; use Command_Response.Assertion;
with Command_Enums; use Command_Enums.Command_Response_Status;
with Event.Assertion; use Event.Assertion;
with Interfaces; use Interfaces;
with Invalid_Command_Info.Assertion; use Invalid_Command_Info.Assertion;
with Event_Limiter_Enums; use Event_Limiter_Enums;
with Event_Id_Limiter_State_Type; use Event_Id_Limiter_State_Type;
with Two_Counter_Entry; use Two_Counter_Entry;
with Two_Counter_Entry_Enums; use Two_Counter_Entry_Enums;

package body Event_Limiter_Tests.Implementation is

   -- Smart assert for event ID types
   package Event_Id_Assert is new Smart_Assert.Discrete (Event_Types.Event_Id, Event_Types.Event_Id'Image);
   package Component_Enabled_Assert is new Smart_Assert.Discrete (Two_Counter_Entry_Enums.Event_State_Type.E, Two_Counter_Entry_Enums.Event_State_Type.E'Image);
   -------------------------------------------------------------------------
   -- Fixtures:
   -------------------------------------------------------------------------

   overriding procedure Set_Up_Test (Self : in out Instance) is
   begin
      -- Allocate heap memory to component:
      Self.Tester.Init_Base;

      -- Make necessary connections between tester and component:
      Self.Tester.Connect;

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

   -- Helper function to test the state packet when issued
   function Check_State_Packet (State_Packet : in Packet.T; Event_Disable_List : in Event_Id_List; Event_Enable_List : in Event_Id_List; Start_Id : in Event_Types.Event_Id) return Boolean is
      use Event_State_Type;
      Byte_Num : Integer;
      Bit_Location : Bit_Num;
      Event_Bitmap : Event_Id_Limiter_State_Type.T;
      Event_State : Event_State_Type.E;
   begin
      for Event_Num of Event_Disable_List loop
         -- One bit per a status so divide by 8 to find the byte number
         Byte_Num := Integer (Event_Num - Start_Id) / 8;
         -- One bit per a status so mod to find which bit in the byte it is
         Bit_Location := Bit_Num (Natural (Event_Num - Start_Id) mod 8);

         Event_Bitmap := Event_Id_Limiter_State_Type.Serialization.From_Byte_Array ([0 => State_Packet.Buffer (Byte_Num)]);

         case Bit_Location is
            when 0 =>
               Event_State := Event_Bitmap.State_0;
            when 1 =>
               Event_State := Event_Bitmap.State_1;
            when 2 =>
               Event_State := Event_Bitmap.State_2;
            when 3 =>
               Event_State := Event_Bitmap.State_3;
            when 4 =>
               Event_State := Event_Bitmap.State_4;
            when 5 =>
               Event_State := Event_Bitmap.State_5;
            when 6 =>
               Event_State := Event_Bitmap.State_6;
            when 7 =>
               Event_State := Event_Bitmap.State_7;
         end case;

         if Event_State /= Event_State_Type.Disabled then
            return False;
         end if;
      end loop;

      for Event_Num of Event_Enable_List loop
         -- One bit per a status so divide by 8 to find the byte number
         Byte_Num := Integer (Event_Num - Start_Id) / 8;
         -- One bit per a status so mod to find which bit in the byte it is
         Bit_Location := Bit_Num (Natural (Event_Num - Start_Id) mod 8);

         Event_Bitmap := Event_Id_Limiter_State_Type.Serialization.From_Byte_Array ([0 => State_Packet.Buffer (Byte_Num)]);

         case Bit_Location is
            when 0 =>
               Event_State := Event_Bitmap.State_0;
            when 1 =>
               Event_State := Event_Bitmap.State_1;
            when 2 =>
               Event_State := Event_Bitmap.State_2;
            when 3 =>
               Event_State := Event_Bitmap.State_3;
            when 4 =>
               Event_State := Event_Bitmap.State_4;
            when 5 =>
               Event_State := Event_Bitmap.State_5;
            when 6 =>
               Event_State := Event_Bitmap.State_6;
            when 7 =>
               Event_State := Event_Bitmap.State_7;
         end case;

         if Event_State /= Event_State_Type.Enabled then
            return False;
         end if;
      end loop;

      return True;
   end Check_State_Packet;

   overriding procedure Test_Received_Event (Self : in out Instance) is
      T : Component.Event_Limiter.Implementation.Tester.Instance_Access renames Self.Tester;
      Incoming_Event : Event.T;
      Input_Tick : constant Tick.T := ((0, 0), 0);
      Event_Start_List : constant Event_Id_List := [3, 6];
   begin
      Put_Line ("");
      Put_Line ("----------------------------------");
      Put_Line ("Testing Received Event:");
      Put_Line ("----------------------------------");

      Self.Tester.Component_Instance.Init (Event_Id_Start => 0, Event_Id_Stop => 10, Event_Disable_List => Event_Start_List, Event_Limit_Persistence => 3);
      -- Make sure all data products were set in the Set_Up procedure, then clear the history for the rest of testing.
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 3);
      Packed_U16_Assert.Eq (T.Limited_Events_Since_Tick_History.Get (1), (Value => 0));
      Packed_U32_Assert.Eq (T.Total_Events_Limited_History.Get (1), (Value => 0));
      Component_Enabled_Assert.Eq (T.Component_Limiting_Enabled_Status_History.Get (1).Component_Enable_State, Two_Counter_Entry_Enums.Event_State_Type.Enabled);
      T.Data_Product_T_Recv_Sync_History.Clear;
      T.Limited_Events_Since_Tick_History.Clear;
      T.Total_Events_Limited_History.Clear;
      T.Component_Limiting_Enabled_Status_History.Clear;

      -- Start with making sure no events have been forwarded yet.
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 0);

      -- Send in a nominal ID. Empty init list should indicate this is enabled and this should be successful.
      Incoming_Event.Header.Id := 0;
      T.Event_T_Send (Incoming_Event);

      -- The first time should be fine so check that we forwarded the event.
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 1);
      Event_Id_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get (1).Header.Id, 0);

      -- Continue until we hit the limit and make sure it was limited
      T.Event_T_Send (Incoming_Event);
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 2);
      T.Event_T_Send (Incoming_Event);
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 3);
      T.Event_T_Send (Incoming_Event);
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 3);
      T.Event_T_Send (Incoming_Event);
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 3);
      T.Event_T_Send (Incoming_Event);
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 3);

      -- Make sure there were no data products sent at this point
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 0);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 0);

      -- Now decrement once to get the data product and make sure we tracked the number of limited events
      T.Tick_T_Send (Input_Tick);

      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 2);
      Packed_U16_Assert.Eq (T.Limited_Events_Since_Tick_History.Get (1), (Value => 3));
      Packed_U32_Assert.Eq (T.Total_Events_Limited_History.Get (1), (Value => 3));

      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Event_Assert.Eq
         (T.Event_T_Recv_Sync_History.Get (1),
          (Header => (Time => (0, 0), Id => T.Events.Get_Events_Limited_Since_Last_Tick_Id, Param_Buffer_Length => 23), Param_Buffer => [0, 3, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]));

      -- Now do it again at the top of the range
      T.Event_Forward_T_Recv_Sync_History.Clear;
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 0);
      Incoming_Event.Header.Id := 10;
      T.Event_T_Send (Incoming_Event);

      -- The first time should be fine so check that we forwarded the event.
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 1);
      Event_Id_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get (1).Header.Id, 10);

      -- Continue until we hit the limit and make sure it was limited
      T.Event_T_Send (Incoming_Event);
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 2);
      T.Event_T_Send (Incoming_Event);
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 3);
      T.Event_T_Send (Incoming_Event);
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 3);
      T.Event_T_Send (Incoming_Event);
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 3);
      T.Event_T_Send (Incoming_Event);
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 3);
      T.Event_T_Send (Incoming_Event);
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 3);
      T.Event_T_Send (Incoming_Event);
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 3);

      -- Make sure there was only the single data product sent previously at this point
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);

      -- Now decrement once to get the data product and make sure we tracked the number of limited events
      T.Tick_T_Send (Input_Tick);

      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 4);
      Packed_U16_Assert.Eq (T.Limited_Events_Since_Tick_History.Get (2), (Value => 5));
      Packed_U32_Assert.Eq (T.Total_Events_Limited_History.Get (2), (Value => 8));
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 2);
      Event_Assert.Eq
         (T.Event_T_Recv_Sync_History.Get (2),
          (Header => (Time => (0, 0), Id => T.Events.Get_Events_Limited_Since_Last_Tick_Id, Param_Buffer_Length => 23), Param_Buffer => [0, 5, 1, 0, 10, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]));

      -- Now perform the operation on a disabled event. Should be no limiting and no data product with it
      T.Event_Forward_T_Recv_Sync_History.Clear;
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 0);
      Incoming_Event.Header.Id := 3;
      T.Event_T_Send (Incoming_Event);

      -- The first time should be fine so check that we forwarded the event.
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 1);
      Event_Id_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get (1).Header.Id, 3);

      -- Continue until we hit the limit and make sure it was limited
      T.Event_T_Send (Incoming_Event);
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 2);
      T.Event_T_Send (Incoming_Event);
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 3);
      T.Event_T_Send (Incoming_Event);
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 4);
      T.Event_T_Send (Incoming_Event);
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 5);
      T.Event_T_Send (Incoming_Event);
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 6);

      -- Again check the data product before we would issue another but the next one shouldn't have any limited messages
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 4);

      -- Now decrement once to get the data product and make sure we tracked the number of limited events
      T.Tick_T_Send (Input_Tick);

      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 5);
      Packed_U16_Assert.Eq (T.Limited_Events_Since_Tick_History.Get (3), (Value => 0));

   end Test_Received_Event;

   overriding procedure Test_Decrement_Event_Count (Self : in out Instance) is
      T : Component.Event_Limiter.Implementation.Tester.Instance_Access renames Self.Tester;
      Input_Tick : constant Tick.T := ((0, 0), 0);
      Event_Start_List : constant Event_Id_List := [2, 8];
      Incoming_Event : Event.T;
   begin
      Put_Line ("");
      Put_Line ("----------------------------------");
      Put_Line ("Testing Decrement all events:");
      Put_Line ("----------------------------------");

      Self.Tester.Component_Instance.Init (Event_Id_Start => 1, Event_Id_Stop => 11, Event_Disable_List => Event_Start_List, Event_Limit_Persistence => 3);
      -- Make sure all data products were set in the Set_Up procedure, then clear the history for the rest of testing.
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 3);
      Packed_U16_Assert.Eq (T.Limited_Events_Since_Tick_History.Get (1), (Value => 0));
      Packed_U32_Assert.Eq (T.Total_Events_Limited_History.Get (1), (Value => 0));
      Component_Enabled_Assert.Eq (T.Component_Limiting_Enabled_Status_History.Get (1).Component_Enable_State, Two_Counter_Entry_Enums.Event_State_Type.Enabled);
      T.Data_Product_T_Recv_Sync_History.Clear;
      T.Limited_Events_Since_Tick_History.Clear;
      T.Total_Events_Limited_History.Clear;
      T.Component_Limiting_Enabled_Status_History.Clear;

      -- Start with making sure no data products have been produced so far
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 0);
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 0);

      -- Perform a tick before anything is sent which helps make sure that decrementing on a 0 count doent produce anything
      T.Tick_T_Send (Input_Tick);
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 1);
      Packed_U16_Assert.Eq (T.Limited_Events_Since_Tick_History.Get (1), (Value => 0));
      -- Event
      Natural_Assert.Eq (T.Events_Limited_Since_Last_Tick_History.Get_Count, 0);

      -- Now increment a few ids for the whole range and one of the disabled, then we will decrement and check that we got the appropriate event and data product out
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 0);
      Incoming_Event.Header.Id := 1;
      T.Event_T_Send (Incoming_Event);
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 1);
      Event_Id_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get (1).Header.Id, 1);
      T.Event_T_Send (Incoming_Event);
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 2);
      T.Event_T_Send (Incoming_Event);
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 3);
      T.Event_T_Send (Incoming_Event);
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 3); -- 1
      T.Event_T_Send (Incoming_Event);
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 3); -- 2

      -- Should have no limiting count
      T.Event_Forward_T_Recv_Sync_History.Clear;
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 0);
      Incoming_Event.Header.Id := 2;
      T.Event_T_Send (Incoming_Event);
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 1);
      Event_Id_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get (1).Header.Id, 2);
      T.Event_T_Send (Incoming_Event);
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 2);

      T.Event_Forward_T_Recv_Sync_History.Clear;
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 0);
      Incoming_Event.Header.Id := 3;
      T.Event_T_Send (Incoming_Event);
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 1);
      Event_Id_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get (1).Header.Id, 3);
      T.Event_T_Send (Incoming_Event);
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 2);

      T.Event_Forward_T_Recv_Sync_History.Clear;
      Incoming_Event.Header.Id := 11;
      T.Event_T_Send (Incoming_Event);
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 1);
      Event_Id_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get (1).Header.Id, 11);
      T.Event_T_Send (Incoming_Event);
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 2);
      T.Event_T_Send (Incoming_Event);
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 3);
      T.Event_T_Send (Incoming_Event);
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 3); -- 3
      T.Event_T_Send (Incoming_Event);
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 3); -- 4

      -- Should have no limiting count, but will be listed as reaching its limit
      T.Event_Forward_T_Recv_Sync_History.Clear;
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 0);
      Incoming_Event.Header.Id := 10;
      T.Event_T_Send (Incoming_Event);
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 1);
      Event_Id_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get (1).Header.Id, 10);
      T.Event_T_Send (Incoming_Event);
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 2);
      T.Event_T_Send (Incoming_Event);
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 3);

      T.Event_Forward_T_Recv_Sync_History.Clear;
      Incoming_Event.Header.Id := 7;
      T.Event_T_Send (Incoming_Event);
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 1);
      Event_Id_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get (1).Header.Id, 7);
      T.Event_T_Send (Incoming_Event);
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 2);
      T.Event_T_Send (Incoming_Event);
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 3);
      T.Event_T_Send (Incoming_Event);
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 3); -- 5
      T.Event_T_Send (Incoming_Event);
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 3); -- 6

      T.Event_Forward_T_Recv_Sync_History.Clear;
      Incoming_Event.Header.Id := 8;
      T.Event_T_Send (Incoming_Event);
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 1);
      Event_Id_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get (1).Header.Id, 8);
      T.Event_T_Send (Incoming_Event);
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 2);
      T.Event_T_Send (Incoming_Event);
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 3);
      T.Event_T_Send (Incoming_Event);
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 4);
      T.Event_T_Send (Incoming_Event);
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 5);

      -- Now call the tick to decrement. Get the event and data product and check the values
      Natural_Assert.Eq (T.Events_Limited_Since_Last_Tick_History.Get_Count, 3);
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 1);
      T.Tick_T_Send (Input_Tick);
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 3);
      Packed_U16_Assert.Eq (T.Limited_Events_Since_Tick_History.Get (2), (Value => 6));
      Packed_U32_Assert.Eq (T.Total_Events_Limited_History.Get (1), (Value => 6));
      -- Event
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Events_Limited_Since_Last_Tick_History.Get_Count, 4);
      Event_Assert.Eq
         (T.Event_T_Recv_Sync_History.Get (1),
          (Header => (Time => (0, 0), Id => T.Events.Get_Events_Limited_Since_Last_Tick_Id, Param_Buffer_Length => 23), Param_Buffer => [0, 6, 4, 0, 1, 0, 7, 0, 10, 0, 11, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]));

      -- Now test if there is an event being limited, that each tick there is one event that we get between each tick
      T.Event_Forward_T_Recv_Sync_History.Clear;
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 0);
      Incoming_Event.Header.Id := 4;
      T.Event_T_Send (Incoming_Event);
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 1);
      Event_Id_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get (1).Header.Id, 4);
      T.Event_T_Send (Incoming_Event);
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 2);
      T.Event_T_Send (Incoming_Event);
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 3);
      T.Event_T_Send (Incoming_Event);
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 3);
      -- Perform a tick now that we are limited
      Natural_Assert.Eq (T.Events_Limited_Since_Last_Tick_History.Get_Count, 4);
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 3);
      T.Tick_T_Send (Input_Tick);
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 5);
      Packed_U16_Assert.Eq (T.Limited_Events_Since_Tick_History.Get (3), (Value => 1));
      Packed_U32_Assert.Eq (T.Total_Events_Limited_History.Get (2), (Value => 7));
      -- Event
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Events_Limited_Since_Last_Tick_History.Get_Count, 5);
      Event_Assert.Eq
         (T.Event_T_Recv_Sync_History.Get (2),
          (Header => (Time => (0, 0), Id => T.Events.Get_Events_Limited_Since_Last_Tick_Id, Param_Buffer_Length => 23), Param_Buffer => [0, 1, 1, 0, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]));
      -- Now make sure we get only one more event
      T.Event_T_Send (Incoming_Event);
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 4);
      T.Event_T_Send (Incoming_Event);
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 4);
      T.Event_T_Send (Incoming_Event);
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 4);
      -- Perform another cycle
      Natural_Assert.Eq (T.Events_Limited_Since_Last_Tick_History.Get_Count, 5);
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 5);
      T.Tick_T_Send (Input_Tick);
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 7);
      Packed_U16_Assert.Eq (T.Limited_Events_Since_Tick_History.Get (4), (Value => 2));
      Packed_U32_Assert.Eq (T.Total_Events_Limited_History.Get (3), (Value => 9));
      -- Event
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 3);
      Natural_Assert.Eq (T.Events_Limited_Since_Last_Tick_History.Get_Count, 6);
      Event_Assert.Eq
         (T.Event_T_Recv_Sync_History.Get (3),
          (Header => (Time => (0, 0), Id => T.Events.Get_Events_Limited_Since_Last_Tick_Id, Param_Buffer_Length => 23), Param_Buffer => [0, 2, 1, 0, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]));

      T.Event_T_Send (Incoming_Event);
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 5);
      T.Event_T_Send (Incoming_Event);
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 5);
      T.Event_T_Send (Incoming_Event);
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 5);

   end Test_Decrement_Event_Count;

   overriding procedure Test_Issue_State_Packet (Self : in out Instance) is
      T : Component.Event_Limiter.Implementation.Tester.Instance_Access renames Self.Tester;
      Input_Tick : constant Tick.T := ((0, 0), 0);
      Event_Start_List : constant Event_Id_List := [2, 4, 6, 8, 10];
      Event_Final_Disable_Range_List : constant Event_Id_List := [2, 4, 6, 8, 10, 14, 15, 16];
      Event_Final_Enable_Range_List : constant Event_Id_List := [0, 1, 3, 5, 7, 9, 11, 12, 13];
      Event_Disable_List_2 : constant Event_Id_List := [2, 4, 6];
      Event_Enable_List_2 : constant Event_Id_List := [0, 1, 3, 5, 7];
      Event_Disable_List_3 : constant Event_Id_List := [5, 9, 13];
      Event_Enable_List_3 : constant Event_Id_List := [6, 7, 8, 10, 11, 12];
      -- Test starting the id at a non-zero value
      Start_Id : Event_Types.Event_Id := 0;
   begin
      Put_Line ("");
      Put_Line ("----------------------------------");
      Put_Line ("Testing Issue State Packet:");
      Put_Line ("----------------------------------");
      -- Setup some events that are enabled and some disabled with a small persistence to quickly test if the state change was successful.
      Self.Tester.Component_Instance.Init (Event_Id_Start => Start_Id, Event_Id_Stop => 13, Event_Disable_List => Event_Start_List, Event_Limit_Persistence => 5);

      -- Just send the packet and check that everything is as expected. There are a mix of enabled and disabled that should be confirmed.
      -- The packet should have a byte at the end where only part of it is valid so we want to make sure that the end is labeled disabled for events not in the list
      T.Command_T_Send (T.Commands.Dump_Event_States);
      -- Command and event info
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 1);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (1), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Dump_Event_States_Id, Status => Success));
      Natural_Assert.Eq (T.Dump_Event_States_Received_History.Get_Count, 1);

      Natural_Assert.Eq (T.Event_Limiter_State_Packet_History.Get_Count, 0);
      T.Tick_T_Send (Input_Tick);
      Natural_Assert.Eq (T.Event_Limiter_State_Packet_History.Get_Count, 1);
      Boolean_Assert.Eq (True, Check_State_Packet (T.Event_Limiter_State_Packet_History.Get (1), Event_Final_Disable_Range_List, Event_Final_Enable_Range_List, Start_Id));

      -- Setup a test where there is not an extra byte
      Self.Tester.Component_Instance.Init (Event_Id_Start => Start_Id, Event_Id_Stop => 7, Event_Disable_List => Event_Disable_List_2, Event_Limit_Persistence => 5);
      -- Packet issue
      T.Command_T_Send (T.Commands.Dump_Event_States);
      -- Command and event info
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 2);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (2), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Dump_Event_States_Id, Status => Success));
      Natural_Assert.Eq (T.Dump_Event_States_Received_History.Get_Count, 2);

      Natural_Assert.Eq (T.Event_Limiter_State_Packet_History.Get_Count, 1);
      T.Tick_T_Send (Input_Tick);
      Natural_Assert.Eq (T.Event_Limiter_State_Packet_History.Get_Count, 2);
      Boolean_Assert.Eq (True, Check_State_Packet (T.Event_Limiter_State_Packet_History.Get (2), Event_Disable_List_2, Event_Enable_List_2, Start_Id));

      -- One more where we have one extra id value. Also offset on this one.
      Start_Id := 5;
      Self.Tester.Component_Instance.Init (Event_Id_Start => Start_Id, Event_Id_Stop => 13, Event_Disable_List => Event_Disable_List_3, Event_Limit_Persistence => 5);
      -- Packet issue
      T.Command_T_Send (T.Commands.Dump_Event_States);
      -- Command and event info
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 3);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (3), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Dump_Event_States_Id, Status => Success));
      Natural_Assert.Eq (T.Dump_Event_States_Received_History.Get_Count, 3);

      Natural_Assert.Eq (T.Event_Limiter_State_Packet_History.Get_Count, 2);
      T.Tick_T_Send (Input_Tick);
      Natural_Assert.Eq (T.Event_Limiter_State_Packet_History.Get_Count, 3);
      Boolean_Assert.Eq (True, Check_State_Packet (T.Event_Limiter_State_Packet_History.Get (3), Event_Disable_List_3, Event_Enable_List_3, Start_Id));

   end Test_Issue_State_Packet;

   overriding procedure Test_Command_Single_State_Change (Self : in out Instance) is
      T : Component.Event_Limiter.Implementation.Tester.Instance_Access renames Self.Tester;
      Incoming_Event : Event.T;
      Input_Tick : constant Tick.T := ((0, 0), 0);
      Event_Start_List : constant Event_Id_List := [3, 6];
      Event_Empty_List : constant Event_Id_List := [1 .. 0 => 0];
      Start_Id : constant Event_Types.Event_Id := 0;
   begin
      Put_Line ("");
      Put_Line ("----------------------------------");
      Put_Line ("Testing Change Single Event State:");
      Put_Line ("----------------------------------");
      -- Setup some events that are enabled and some disabled with a small persistence to quickly test if the state change was successful.
      Self.Tester.Component_Instance.Init (Event_Id_Start => Start_Id, Event_Id_Stop => 10, Event_Disable_List => Event_Start_List, Event_Limit_Persistence => 2);

      -- Start by disabling an enabled event.
      T.Command_T_Send (T.Commands.Disable_Event_Limit ((Event_To_Update => (Id => 1), Issue_State_Packet => Issue_Packet_Type.No_Issue)));
      -- Command and event info
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 1);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (1), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Disable_Event_Limit_Id, Status => Success));
      Natural_Assert.Eq (T.Event_Limit_Disabled_History.Get_Count, 1);
      Event_Id_Assert.Eq (T.Event_Limit_Disabled_History.Get (1).Event_To_Update.Id, 1);

      -- Test the disable by either dumping the packet or until we get to the max from that event but showing it still sends
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 0);
      Incoming_Event.Header.Id := 1;
      T.Event_T_Send (Incoming_Event);

      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 1);
      Event_Id_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get (1).Header.Id, 1);

      T.Event_T_Send (Incoming_Event);
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 2);
      -- Should be limited at this point if enabled (but is not so make sure we got the event back out)
      T.Event_T_Send (Incoming_Event);
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 3);
      T.Event_T_Send (Incoming_Event);
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 4);

      -- Now disable (which should reset the count) and issue the packet this time
      T.Command_T_Send (T.Commands.Enable_Event_Limit ((Event_To_Update => (Id => 1), Issue_State_Packet => Issue_Packet_Type.Issue)));
      -- Command and event info
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 2);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (2), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Enable_Event_Limit_Id, Status => Success));
      Natural_Assert.Eq (T.Event_Limit_Enabled_History.Get_Count, 1);
      Event_Id_Assert.Eq (T.Event_Limit_Enabled_History.Get (1).Event_To_Update.Id, 1);

      -- Have to send the tick to induce the packet. Then check with the helper function that the appropriate ids are disabled
      Natural_Assert.Eq (T.Event_Limiter_State_Packet_History.Get_Count, 0);
      T.Tick_T_Send (Input_Tick);
      Natural_Assert.Eq (T.Event_Limiter_State_Packet_History.Get_Count, 1);
      Boolean_Assert.Eq (True, Check_State_Packet (T.Event_Limiter_State_Packet_History.Get (1), Event_Start_List, Event_Empty_List, Start_Id));

      -- Since we are testing two commands here, we have to perform the same test in reverse so that we hit all the branches
      -- This time we enable a disabled event and send it in until we hit the limit
      T.Event_Forward_T_Recv_Sync_History.Clear;
      T.Event_Limiter_State_Packet_History.Clear;
      -- Send the command
      T.Command_T_Send (T.Commands.Enable_Event_Limit ((Event_To_Update => (Id => 6), Issue_State_Packet => Issue_Packet_Type.No_Issue)));
      -- Command and event info
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 3);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (3), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Enable_Event_Limit_Id, Status => Success));
      Natural_Assert.Eq (T.Event_Limit_Enabled_History.Get_Count, 2);
      Event_Id_Assert.Eq (T.Event_Limit_Enabled_History.Get (2).Event_To_Update.Id, 6);

      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 0);
      Incoming_Event.Header.Id := 6;
      T.Event_T_Send (Incoming_Event);

      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 1);
      Event_Id_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get (1).Header.Id, 6);
      T.Event_T_Send (Incoming_Event);
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 2);
      -- Should be limited at this point if enabled so make sure we don't get the forwarded event after this point
      T.Event_T_Send (Incoming_Event);
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 2);
      T.Event_T_Send (Incoming_Event);
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 2);

      -- Now disable (which should reset the count) and issue the packet this time
      T.Command_T_Send (T.Commands.Disable_Event_Limit ((Event_To_Update => (Id => 6), Issue_State_Packet => Issue_Packet_Type.Issue)));
      -- Command and event info
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 4);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (4), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Disable_Event_Limit_Id, Status => Success));
      Natural_Assert.Eq (T.Event_Limit_Disabled_History.Get_Count, 2);
      Event_Id_Assert.Eq (T.Event_Limit_Disabled_History.Get (2).Event_To_Update.Id, 6);

      -- Have to send the tick to induce the packet. Then check with the helper function that the appropriate ids are disabled
      Natural_Assert.Eq (T.Event_Limiter_State_Packet_History.Get_Count, 0);
      T.Tick_T_Send (Input_Tick);
      Natural_Assert.Eq (T.Event_Limiter_State_Packet_History.Get_Count, 1);
      Boolean_Assert.Eq (True, Check_State_Packet (T.Event_Limiter_State_Packet_History.Get (1), Event_Start_List, Event_Empty_List, Start_Id));

      -- Lastly, make both commands fail due to being out of range

      T.Command_T_Send (T.Commands.Enable_Event_Limit ((Event_To_Update => (Id => 11), Issue_State_Packet => Issue_Packet_Type.No_Issue)));
      -- Command and event info
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 5);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (5), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Enable_Event_Limit_Id, Status => Failure));
      Natural_Assert.Eq (T.Event_Limit_Enable_Invalid_Id_History.Get_Count, 1);
      Event_Id_Assert.Eq (T.Event_Limit_Enable_Invalid_Id_History.Get (1).Event_To_Update.Id, 11);

      T.Command_T_Send (T.Commands.Disable_Event_Limit ((Event_To_Update => (Id => 11), Issue_State_Packet => Issue_Packet_Type.Issue)));
      -- Command and event info
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 6);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (6), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Disable_Event_Limit_Id, Status => Failure));
      Natural_Assert.Eq (T.Event_Limit_Disable_Invalid_Id_History.Get_Count, 1);
      Event_Id_Assert.Eq (T.Event_Limit_Disable_Invalid_Id_History.Get (1).Event_To_Update.Id, 11);

   end Test_Command_Single_State_Change;

   overriding procedure Test_Command_Range_State_Change (Self : in out Instance) is
      T : Component.Event_Limiter.Implementation.Tester.Instance_Access renames Self.Tester;
      Incoming_Event : Event.T;
      Input_Tick : constant Tick.T := ((0, 0), 0);
      Event_Start_List : constant Event_Id_List := [2, 8];
      Event_Final_Disable_Range_List : constant Event_Id_List := [3, 4, 5, 6, 7];
      Event_Final_Enable_Range_List : constant Event_Id_List := [3, 7];
      Event_Empty_List : constant Event_Id_List := [1 .. 0 => 0];
      -- Test starting the id at a non-zero value
      Start_Id : constant Event_Types.Event_Id := 2;
   begin
      Put_Line ("");
      Put_Line ("----------------------------------");
      Put_Line ("Testing Change Range Event State:");
      Put_Line ("----------------------------------");
      -- Setup some events that are enabled and some disabled with a small persistence to quickly test if the state change was successful.
      Self.Tester.Component_Instance.Init (Event_Id_Start => Start_Id, Event_Id_Stop => 10, Event_Disable_List => Event_Start_List, Event_Limit_Persistence => 2);

      -- Start by enabling all events.
      T.Command_T_Send (T.Commands.Enable_Event_Limit_Range ((Start_Event_Id => (Id => 2), Stop_Event_Id => (Id => 8), Issue_State_Packet => Issue_Packet_Type.No_Issue)));
      -- Command and event info
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 1);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (1), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Enable_Event_Limit_Range_Id, Status => Success));
      Natural_Assert.Eq (T.Event_Limit_Range_Enabled_History.Get_Count, 1);
      Event_Id_Assert.Eq (T.Event_Limit_Range_Enabled_History.Get (1).Start_Event_Id.Id, 2);
      Event_Id_Assert.Eq (T.Event_Limit_Range_Enabled_History.Get (1).Stop_Event_Id.Id, 8);

      -- Now use the non-packet method to test that one of the disabled events is now enabled
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 0);
      Incoming_Event.Header.Id := 2;
      T.Event_T_Send (Incoming_Event);

      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 1);
      Event_Id_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get (1).Header.Id, 2);
      T.Event_T_Send (Incoming_Event);
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 2);
      -- Should be limited at this point so make sure we don't get the forwarded event after this point
      T.Event_T_Send (Incoming_Event);
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 2);
      T.Event_T_Send (Incoming_Event);
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 2);

      -- Same with 8 to make sure the range is covered
      T.Event_Forward_T_Recv_Sync_History.Clear;

      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 0);
      Incoming_Event.Header.Id := 8;
      T.Event_T_Send (Incoming_Event);

      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 1);
      Event_Id_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get (1).Header.Id, 8);
      T.Event_T_Send (Incoming_Event);
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 2);
      -- Should be limited at this point so make sure we don't get the forwarded event after this point
      T.Event_T_Send (Incoming_Event);
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 2);
      T.Event_T_Send (Incoming_Event);
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 2);

      -- Disable the range and check with a packet issue.
      T.Command_T_Send (T.Commands.Disable_Event_Limit_Range ((Start_Event_Id => (Id => 3), Stop_Event_Id => (Id => 7), Issue_State_Packet => Issue_Packet_Type.Issue)));
      -- Command and event asserts
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 2);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (2), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Disable_Event_Limit_Range_Id, Status => Success));
      Natural_Assert.Eq (T.Event_Limit_Range_Disabled_History.Get_Count, 1);
      Event_Id_Assert.Eq (T.Event_Limit_Range_Disabled_History.Get (1).Start_Event_Id.Id, 3);
      Event_Id_Assert.Eq (T.Event_Limit_Range_Disabled_History.Get (1).Stop_Event_Id.Id, 7);

      -- Check the state packet this time
      Natural_Assert.Eq (T.Event_Limiter_State_Packet_History.Get_Count, 0);
      T.Tick_T_Send (Input_Tick);
      Natural_Assert.Eq (T.Event_Limiter_State_Packet_History.Get_Count, 1);
      Boolean_Assert.Eq (True, Check_State_Packet (T.Event_Limiter_State_Packet_History.Get (1), Event_Final_Disable_Range_List, Event_Empty_List, Start_Id));

      -- Do the same enable/disable dance because we need to issue the packet from the enable. Start with enabling and issue the packet.
      T.Command_T_Send (T.Commands.Enable_Event_Limit_Range ((Start_Event_Id => (Id => 4), Stop_Event_Id => (Id => 6), Issue_State_Packet => Issue_Packet_Type.Issue)));
      -- Command and event info
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 3);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (3), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Enable_Event_Limit_Range_Id, Status => Success));
      Natural_Assert.Eq (T.Event_Limit_Range_Enabled_History.Get_Count, 2);
      Event_Id_Assert.Eq (T.Event_Limit_Range_Enabled_History.Get (2).Start_Event_Id.Id, 4);
      Event_Id_Assert.Eq (T.Event_Limit_Range_Enabled_History.Get (2).Stop_Event_Id.Id, 6);

      Natural_Assert.Eq (T.Event_Limiter_State_Packet_History.Get_Count, 1);
      T.Tick_T_Send (Input_Tick);
      Natural_Assert.Eq (T.Event_Limiter_State_Packet_History.Get_Count, 2);
      Boolean_Assert.Eq (True, Check_State_Packet (T.Event_Limiter_State_Packet_History.Get (2), Event_Final_Enable_Range_List, Event_Empty_List, Start_Id));

      -- Disable the range and check without a packet issue
      T.Command_T_Send (T.Commands.Disable_Event_Limit_Range ((Start_Event_Id => (Id => 2), Stop_Event_Id => (Id => 10), Issue_State_Packet => Issue_Packet_Type.No_Issue)));
      -- Command and event info
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 4);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (4), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Disable_Event_Limit_Range_Id, Status => Success));
      Natural_Assert.Eq (T.Event_Limit_Range_Disabled_History.Get_Count, 2);
      Event_Id_Assert.Eq (T.Event_Limit_Range_Disabled_History.Get (2).Start_Event_Id.Id, 2);
      Event_Id_Assert.Eq (T.Event_Limit_Range_Disabled_History.Get (2).Stop_Event_Id.Id, 10);

      -- Same with 8 to make sure the range is covered
      T.Event_Forward_T_Recv_Sync_History.Clear;

      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 0);
      Incoming_Event.Header.Id := 5;
      T.Event_T_Send (Incoming_Event);

      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 1);
      Event_Id_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get (1).Header.Id, 5);
      T.Event_T_Send (Incoming_Event);
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 2);
      -- Should NOT be limited at this point so make sure we continue to get events passed through
      T.Event_T_Send (Incoming_Event);
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 3);
      T.Event_T_Send (Incoming_Event);
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 4);

      -- Lastly, check the ranges for out of range failures
      -- Enable
      Natural_Assert.Eq (T.Event_Limit_Range_Enabled_Invalid_Id_History.Get_Count, 0);
      T.Command_T_Send (T.Commands.Enable_Event_Limit_Range ((Start_Event_Id => (Id => 1), Stop_Event_Id => (Id => 6), Issue_State_Packet => Issue_Packet_Type.Issue)));
      -- Command and event info
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 5);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (5), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Enable_Event_Limit_Range_Id, Status => Failure));
      Natural_Assert.Eq (T.Event_Limit_Range_Enabled_Invalid_Id_History.Get_Count, 1);
      Event_Id_Assert.Eq (T.Event_Limit_Range_Enabled_Invalid_Id_History.Get (1).Start_Event_Id.Id, 1);
      Event_Id_Assert.Eq (T.Event_Limit_Range_Enabled_Invalid_Id_History.Get (1).Stop_Event_Id.Id, 6);

      -- Disable
      Natural_Assert.Eq (T.Event_Limit_Range_Disabled_Invalid_Id_History.Get_Count, 0);
      T.Command_T_Send (T.Commands.Disable_Event_Limit_Range ((Start_Event_Id => (Id => 5), Stop_Event_Id => (Id => 11), Issue_State_Packet => Issue_Packet_Type.No_Issue)));
      -- Command and event info
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 6);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (6), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Disable_Event_Limit_Range_Id, Status => Failure));
      Natural_Assert.Eq (T.Event_Limit_Range_Disabled_Invalid_Id_History.Get_Count, 1);
      Event_Id_Assert.Eq (T.Event_Limit_Range_Disabled_Invalid_Id_History.Get (1).Start_Event_Id.Id, 5);
      Event_Id_Assert.Eq (T.Event_Limit_Range_Disabled_Invalid_Id_History.Get (1).Stop_Event_Id.Id, 11);

   end Test_Command_Range_State_Change;

   overriding procedure Test_Command_Component_State_Change (Self : in out Instance) is
      T : Component.Event_Limiter.Implementation.Tester.Instance_Access renames Self.Tester;
      Incoming_Event : Event.T;
      Input_Tick : constant Tick.T := ((0, 0), 0);
      Event_Start_List : constant Event_Id_List := [2, 5, 8, 10];
      Event_Disabled_List : constant Event_Id_List := [2, 5, 8, 10, 12, 13, 14, 15, 16];
      Event_Enabled_List : constant Event_Id_List := [1, 3, 4, 6, 7, 9, 11];
      Component_Enable_State : Two_Counter_Entry_Enums.Event_State_Type.E;
      -- Test starting the id at a non-zero value
      Start_Id : constant Event_Types.Event_Id := 1;
   begin
      Put_Line ("");
      Put_Line ("----------------------------------");
      Put_Line ("Testing Change All Event States:");
      Put_Line ("----------------------------------");
      -- Setup some events that are enabled and some disabled with a small persistence to quickly test if the state change was successful.
      Self.Tester.Component_Instance.Init (Event_Id_Start => Start_Id, Event_Id_Stop => 11, Event_Disable_List => Event_Start_List, Event_Limit_Persistence => 2);
      -- Make sure all data products were set in the Set_Up procedure, then clear the history for the rest of testing.
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 3);
      Packed_U16_Assert.Eq (T.Limited_Events_Since_Tick_History.Get (1), (Value => 0));
      Packed_U32_Assert.Eq (T.Total_Events_Limited_History.Get (1), (Value => 0));
      Component_Enabled_Assert.Eq (T.Component_Limiting_Enabled_Status_History.Get (1).Component_Enable_State, Two_Counter_Entry_Enums.Event_State_Type.Enabled);
      T.Data_Product_T_Recv_Sync_History.Clear;
      T.Limited_Events_Since_Tick_History.Clear;
      T.Total_Events_Limited_History.Clear;
      T.Component_Limiting_Enabled_Status_History.Clear;

      -- Start by enabling all events.
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 0);
      T.Command_T_Send (T.Commands.Enable_Event_Limiting);
      -- Command and event info
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 1);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (1), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Enable_Event_Limiting_Id, Status => Success));
      Natural_Assert.Eq (T.Event_Limiting_Enabled_History.Get_Count, 1);
      -- Data product
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Component_Limiting_Enabled_Status_History.Get_Count, 1);
      Component_Enable_State := T.Component_Limiting_Enabled_Status_History.Get (1).Component_Enable_State;
      Component_Enabled_Assert.Eq (Component_Enable_State, Two_Counter_Entry_Enums.Event_State_Type.Enabled);

      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 0);
      Incoming_Event.Header.Id := 11;
      T.Event_T_Send (Incoming_Event);

      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 1);
      Event_Id_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get (1).Header.Id, 11);
      T.Event_T_Send (Incoming_Event);
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 2);
      T.Event_T_Send (Incoming_Event);
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 2);
      -- Events should be limited at this point
      T.Event_T_Send (Incoming_Event);
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 2);
      T.Event_T_Send (Incoming_Event);
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 2);

      -- Ones that are disabled should be disabled here
      T.Event_Forward_T_Recv_Sync_History.Clear;
      Incoming_Event.Header.Id := 10;
      T.Event_T_Send (Incoming_Event);

      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 1);
      Event_Id_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get (1).Header.Id, 10);
      T.Event_T_Send (Incoming_Event);
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 2);
      T.Event_T_Send (Incoming_Event);
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 3);
      -- Events should continue to be issued at this point
      T.Event_T_Send (Incoming_Event);
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 4);
      T.Event_T_Send (Incoming_Event);
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 5);

      -- Disable all events without issuing a packet
      T.Event_Forward_T_Recv_Sync_History.Clear;

      Natural_Assert.Eq (T.Event_Limiting_Disabled_History.Get_Count, 0);
      T.Command_T_Send (T.Commands.Disable_Event_Limiting);
      -- Command and event info
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 2);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (2), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Disable_Event_Limiting_Id, Status => Success));
      Natural_Assert.Eq (T.Event_Limiting_Disabled_History.Get_Count, 1);
      -- data product
      Natural_Assert.Eq (T.Component_Limiting_Enabled_Status_History.Get_Count, 2);
      Component_Enable_State := T.Component_Limiting_Enabled_Status_History.Get (2).Component_Enable_State;
      Component_Enabled_Assert.Eq (Component_Enable_State, Two_Counter_Entry_Enums.Event_State_Type.Disabled);

      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 0);
      Incoming_Event.Header.Id := 11;
      T.Event_T_Send (Incoming_Event);

      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 1);
      Event_Id_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get (1).Header.Id, 11);
      T.Event_T_Send (Incoming_Event);
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 2);
      T.Event_T_Send (Incoming_Event);
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 3);
      -- Events should continue to be issued at this point
      T.Event_T_Send (Incoming_Event);
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 4);
      T.Event_T_Send (Incoming_Event);
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 5);

      -- Go back to enabled and pick an event that should be disabled to make sure we maintined our state through the global change
      -- Enable
      T.Command_T_Send (T.Commands.Enable_Event_Limiting);
      -- Command and event info
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 3);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (3), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Enable_Event_Limiting_Id, Status => Success));
      Natural_Assert.Eq (T.Event_Limiting_Enabled_History.Get_Count, 2);
      -- data product
      Natural_Assert.Eq (T.Component_Limiting_Enabled_Status_History.Get_Count, 3);
      Component_Enable_State := T.Component_Limiting_Enabled_Status_History.Get (3).Component_Enable_State;
      Component_Enabled_Assert.Eq (Component_Enable_State, Two_Counter_Entry_Enums.Event_State_Type.Enabled);
      -- Issue packet
      T.Command_T_Send (T.Commands.Dump_Event_States);
      -- Command and event info
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 4);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (4), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Dump_Event_States_Id, Status => Success));
      Natural_Assert.Eq (T.Dump_Event_States_Received_History.Get_Count, 1);

      -- Packet check
      Natural_Assert.Eq (T.Event_Limiter_State_Packet_History.Get_Count, 0);
      T.Tick_T_Send (Input_Tick);
      Natural_Assert.Eq (T.Event_Limiter_State_Packet_History.Get_Count, 1);
      Boolean_Assert.Eq (True, Check_State_Packet (T.Event_Limiter_State_Packet_History.Get (1), Event_Disabled_List, Event_Enabled_List, Start_Id));

      T.Event_Forward_T_Recv_Sync_History.Clear;
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 0);
      Incoming_Event.Header.Id := 10;
      T.Event_T_Send (Incoming_Event);

      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 1);
      Event_Id_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get (1).Header.Id, 10);
      T.Event_T_Send (Incoming_Event);
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 2);
      T.Event_T_Send (Incoming_Event);
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 3);
      -- Events should continue to be issued at this point
      T.Event_T_Send (Incoming_Event);
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 4);
      T.Event_T_Send (Incoming_Event);
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 5);

      -- Lastly, disable and then send the packet. Should still reflect the internal state
      Natural_Assert.Eq (T.Event_Limiting_Disabled_History.Get_Count, 1);
      T.Command_T_Send (T.Commands.Disable_Event_Limiting);
      -- Command and event info
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 5);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (5), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Disable_Event_Limiting_Id, Status => Success));
      Natural_Assert.Eq (T.Event_Limiting_Disabled_History.Get_Count, 2);
      -- data product
      Natural_Assert.Eq (T.Component_Limiting_Enabled_Status_History.Get_Count, 4);
      Component_Enable_State := T.Component_Limiting_Enabled_Status_History.Get (4).Component_Enable_State;
      Component_Enabled_Assert.Eq (Component_Enable_State, Two_Counter_Entry_Enums.Event_State_Type.Disabled);

      -- Issue packet
      T.Command_T_Send (T.Commands.Dump_Event_States);
      -- Command and event info
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 6);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (6), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Dump_Event_States_Id, Status => Success));
      Natural_Assert.Eq (T.Dump_Event_States_Received_History.Get_Count, 2);

      -- Packet check
      Natural_Assert.Eq (T.Event_Limiter_State_Packet_History.Get_Count, 1);
      T.Tick_T_Send (Input_Tick);
      Natural_Assert.Eq (T.Event_Limiter_State_Packet_History.Get_Count, 2);
      Boolean_Assert.Eq (True, Check_State_Packet (T.Event_Limiter_State_Packet_History.Get (2), Event_Disabled_List, Event_Enabled_List, Start_Id));

      -- Cannot make these commands fail so to failure cases
   end Test_Command_Component_State_Change;

   overriding procedure Test_Persistence_Change (Self : in out Instance) is
      T : Component.Event_Limiter.Implementation.Tester.Instance_Access renames Self.Tester;
      Incoming_Event : Event.T;
   begin
      Put_Line ("");
      Put_Line ("----------------------------------");
      Put_Line ("Testing Changing the persistence:");
      Put_Line ("----------------------------------");
      Self.Tester.Component_Instance.Init (Event_Id_Start => 0, Event_Id_Stop => 10, Event_Limit_Persistence => 2);

      -- Make sure we first hit the persistence that is set
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 0);
      Incoming_Event.Header.Id := 1;
      T.Event_T_Send (Incoming_Event);
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 1);
      Event_Id_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get (1).Header.Id, 1);
      T.Event_T_Send (Incoming_Event);
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 2);
      T.Event_T_Send (Incoming_Event);
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 2);
      T.Event_T_Send (Incoming_Event);
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 2);

      -- Change the persistence
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 0);
      T.Command_T_Send (T.Commands.Set_Event_Limit_Persistence ((Persistence => 4)));
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 1);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (1), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Set_Event_Limit_Persistence_Id, Status => Success));
      Natural_Assert.Eq (T.Set_New_Persistence_History.Get_Count, 1);

      -- Test that the persistence took effect
      T.Event_Forward_T_Recv_Sync_History.Clear;
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 0);
      Incoming_Event.Header.Id := 3;
      T.Event_T_Send (Incoming_Event);
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 1);
      Event_Id_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get (1).Header.Id, 3);
      T.Event_T_Send (Incoming_Event);
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 2);
      T.Event_T_Send (Incoming_Event);
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 3);
      T.Event_T_Send (Incoming_Event);
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 4);
      T.Event_T_Send (Incoming_Event);
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 4);
      T.Event_T_Send (Incoming_Event);
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 4);

   end Test_Persistence_Change;

   overriding procedure Test_Invalid_Command (Self : in out Instance) is
      T : Component.Event_Limiter.Implementation.Tester.Instance_Access renames Self.Tester;
      Cmd : Command.T := T.Commands.Disable_Event_Limit_Range ((Start_Event_Id => (Id => 5), Stop_Event_Id => (Id => 6), Issue_State_Packet => Issue_Packet_Type.No_Issue));
   begin
      Put_Line ("");
      Put_Line ("----------------------------------");
      Put_Line ("Testing Invalid Command:");
      Put_Line ("----------------------------------");

      Self.Tester.Component_Instance.Init (Event_Id_Start => 0, Event_Id_Stop => 10, Event_Limit_Persistence => 6);

      -- Make the command invalid by modifying its length.
      Cmd.Header.Arg_Buffer_Length := 0;

      -- Send bad command and expect bad response:
      T.Command_T_Send (Cmd);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 1);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (1), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Disable_Event_Limit_Range_Id, Status => Length_Error));

      -- Make sure some events were thrown:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Invalid_Command_Received_History.Get_Count, 1);
      Invalid_Command_Info_Assert.Eq (T.Invalid_Command_Received_History.Get (1), (Id => T.Commands.Get_Disable_Event_Limit_Range_Id, Errant_Field_Number => Interfaces.Unsigned_32'Last, Errant_Field => [0, 0, 0, 0, 0, 0, 0, 0]));
   end Test_Invalid_Command;

end Event_Limiter_Tests.Implementation;
