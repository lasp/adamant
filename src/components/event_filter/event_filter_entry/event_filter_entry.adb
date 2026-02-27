with Safe_Deallocator;
with Event_Filter_Entry_Type;

package body Event_Filter_Entry is

   procedure Init (Self : in out Instance; Event_Id_Start : in Event_Id; Event_Id_Stop : in Event_Id; Event_Filter_List : in Event_Id_List) is
      Num_Event_Bytes : Natural;
      Status : Event_Entry_Status;
   begin
      -- First make sure that we are not given a range that is invalid
      pragma Assert (Event_Id_Start <= Event_Id_Stop, "Invalid Event ID range. Start ID must be less than or equal to Stop ID");
      -- Now determine how many bytes we need to store the events. Add two to compensate for an odd number of events since we will need an extra byte in that case. (two events = 1 byte)
      Num_Event_Bytes := Natural (Event_Id_Stop - Event_Id_Start + 8) / 8;
      Self.Start_Id := Event_Id_Start;
      Self.End_Id := Event_Id_Stop;

      Self.Events := new Basic_Types.Byte_Array (0 .. Num_Event_Bytes - 1);

      -- Init the array so that all events are enabled by default and the counts are set to 0.
      Self.Events.all := [others => Event_Filter_Entry_Type.Serialization.To_Byte_Array ((
         State_0 => Event_Filter_State.Unfiltered,
         State_1 => Event_Filter_State.Unfiltered,
         State_2 => Event_Filter_State.Unfiltered,
         State_3 => Event_Filter_State.Unfiltered,
         State_4 => Event_Filter_State.Unfiltered,
         State_5 => Event_Filter_State.Unfiltered,
         State_6 => Event_Filter_State.Unfiltered,
         State_7 => Event_Filter_State.Unfiltered
      )) (0)];

      -- Then disable the event based on our disable list
      for Event_Id_To_Filter of Event_Filter_List loop
         Status := Set_Filter_State (Self, Event_Id_To_Filter, Event_Filter_State.Filtered);
         -- Assert here on status
         pragma Assert (Status /= Invalid_Id, "Event ID in the filtered list is out of range");
      end loop;

   end Init;

   procedure Destroy (Self : in out Instance) is
      use Basic_Types;
      procedure Free_If_Testing is new Safe_Deallocator.Deallocate_If_Testing (
         Object => Basic_Types.Byte_Array,
         Name => Basic_Types.Byte_Array_Access
      );
   begin
      -- Reset all defaults for the record
      if Self.Events /= null then
         Free_If_Testing (Self.Events);
         Self.Events := null;
      end if;
      Self.Start_Id := Event_Id'First;
      Self.End_Id := Event_Id'First;
      Self.Num_Events_Filtered := Interfaces.Unsigned_32'First;
      Self.Num_Events_Unfiltered := Interfaces.Unsigned_32'First;
   end Destroy;

   -- Set the status for a given event id
   function Set_Filter_State (Self : in out Instance; Id : in Event_Id; New_State : in Event_Filter_State.E) return Event_Entry_Status is
   begin
      -- Check that the event is in range
      if Id >= Self.Start_Id and then Id <= Self.End_Id then
         declare
            Event_Id_In : constant Natural := Natural (Id - Self.Start_Id) / 8;
            Bit_Location : constant Bit_Location_Type := Bit_Location_Type (Natural (Id - Self.Start_Id) mod 8);
            Overlay : Event_Filter_Entry_Type.T with
               Import,
               Convention => Ada,
               Address => Self.Events (Event_Id_In)'Address;
         begin
            -- Since we found a valid id, we need to find the bit we are interested in and return that state
            case Bit_Location is
               when 0 =>
                  Overlay.State_0 := New_State;
               when 1 =>
                  Overlay.State_1 := New_State;
               when 2 =>
                  Overlay.State_2 := New_State;
               when 3 =>
                  Overlay.State_3 := New_State;
               when 4 =>
                  Overlay.State_4 := New_State;
               when 5 =>
                  Overlay.State_5 := New_State;
               when 6 =>
                  Overlay.State_6 := New_State;
               when 7 =>
                  Overlay.State_7 := New_State;
            end case;
         end;
         return Success;
      else
         return Invalid_Id;
      end if;
   end Set_Filter_State;

   -- Set the status for a given event id
   function Filter_Event (Self : in out Instance; Id : in Event_Id) return Filter_Status is
      use Global_Filter_State;
      use Event_Filter_State;
      use Interfaces;
   begin
      -- Check the component state. If its disabled then skip the logic altogether and just pass out that we are not filtered
      if Self.Global_Enable_State = Global_Filter_State.Disabled then
         return Unfiltered;
      end if;

      -- Check that the event is in range
      if Id >= Self.Start_Id and then Id <= Self.End_Id then
         declare
            Event_Id_In : constant Natural := Natural (Id - Self.Start_Id) / 8;
            Bit_Location : constant Bit_Location_Type := Bit_Location_Type (Natural (Id - Self.Start_Id) mod 8);
            Event_State : Event_Filter_State.E;
            Overlay : Event_Filter_Entry_Type.T with
               Import,
               Convention => Ada,
               Address => Self.Events (Event_Id_In)'Address;
         begin
            -- Since we found a valid id, we need to find the bit we are interested in and return that state
            case Bit_Location is
               when 0 =>
                  Event_State := Overlay.State_0;
               when 1 =>
                  Event_State := Overlay.State_1;
               when 2 =>
                  Event_State := Overlay.State_2;
               when 3 =>
                  Event_State := Overlay.State_3;
               when 4 =>
                  Event_State := Overlay.State_4;
               when 5 =>
                  Event_State := Overlay.State_5;
               when 6 =>
                  Event_State := Overlay.State_6;
               when 7 =>
                  Event_State := Overlay.State_7;
            end case;

            if Event_State = Event_Filter_State.Filtered then
               Self.Num_Events_Filtered := @ + 1;
               return Filtered;
            else
               Self.Num_Events_Unfiltered := @ + 1;
               return Unfiltered;
            end if;
         end;
         -- Otherwise the id is not in our list and return an invalid ID status.
      else
         return Out_Of_Range;
      end if;
   end Filter_Event;

   procedure Set_Global_Enable_State (Self : in out Instance; New_Global_State : in Global_Filter_State.E) is
   begin
      Self.Global_Enable_State := New_Global_State;
   end Set_Global_Enable_State;

   function Get_Global_Enable_State (Self : in Instance) return Global_Filter_State.E is
   begin
      return Self.Global_Enable_State;
   end Get_Global_Enable_State;

   function Get_Event_Filtered_Count (Self : in Instance) return Interfaces.Unsigned_32 is
   begin
      return Self.Num_Events_Filtered;
   end Get_Event_Filtered_Count;

   function Get_Event_Unfiltered_Count (Self : in Instance) return Interfaces.Unsigned_32 is
   begin
      return Self.Num_Events_Unfiltered;
   end Get_Event_Unfiltered_Count;

   function Get_Event_Start_Stop_Range (Self : in Instance; Event_Stop_Id : out Event_Id) return Event_Id is
   begin
      Event_Stop_Id := Self.End_Id;
      return Self.Start_Id;
   end Get_Event_Start_Stop_Range;

   function Get_Entry_Array (Self : in Instance) return Basic_Types.Byte_Array_Access is
   begin
      return Self.Events;
   end Get_Entry_Array;

end Event_Filter_Entry;
