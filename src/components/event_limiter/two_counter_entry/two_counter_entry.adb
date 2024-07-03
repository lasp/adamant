with Safe_Deallocator;

package body Two_Counter_Entry is

   procedure Init (Self : in out Instance; Event_Id_Start : in Event_Id; Event_Id_Stop : in Event_Id; Event_Disable_List : in Event_Id_List; Event_Limit_Persistence : in Persistence_Type) is
      Num_Event_Bytes : Natural;
      Status : Enable_State_Status;
   begin
      -- First make sure that we are not given a range that is invalid
      pragma Assert (Event_Id_Start <= Event_Id_Stop, "Invalid Event ID range. Start ID must be less than or equal to Stop ID");
      -- Now determine how many bytes we need to store the events. Add two to compensate for an odd number of events since we will need an extra byte in that case. (two events = 1 byte)
      Num_Event_Bytes := Natural (Event_Id_Stop - Event_Id_Start + 2) / 2;
      Self.Start_Id := Event_Id_Start;
      Self.End_Id := Event_Id_Stop;
      Self.Persistence := Event_Limit_Persistence;
      Self.Master_Enable_State := Event_State_Type.Enabled;

      Self.Bytes := new Basic_Types.Byte_Array (0 .. Num_Event_Bytes - 1);

      -- Init the array so that all events are enabled by default and the counts are set to 0.
      Self.Bytes.all := [others => Two_Counter_Entry_Type.Serialization.To_Byte_Array ((Top_Event_Enabled_State => Event_State_Type.Enabled, Top_Event_Count => 0, Bottom_Event_Enabled_State => Event_State_Type.Enabled, Bottom_Event_Count => 0)) (0)];

      -- Then disable the event based on our disable list
      for Event_Id_To_Disable of Event_Disable_List loop
         Status := Set_Enable_State (Self, Event_Id_To_Disable, Event_State_Type.Disabled);
         -- Assert here on status
         pragma Assert (Status /= Invalid_Id, "Event ID in the disable list is out of range");
      end loop;

   end Init;

   procedure Destroy (Self : in out Instance) is
      use Basic_Types;
      procedure Free_If_Testing is new Safe_Deallocator.Deallocate_If_Testing (Object => Basic_Types.Byte_Array, Name => Basic_Types.Byte_Array_Access);
   begin
      -- Reset all defaults for the record
      if Self.Bytes /= null then
         Free_If_Testing (Self.Bytes);
         Self.Bytes := null;
      end if;
      Self.Start_Id := Event_Id'First;
      Self.End_Id := Event_Id'First;
      Self.Persistence := Persistence_Type'Last;
      Self.Num_Events_Limited := Interfaces.Unsigned_16'First;
   end Destroy;

   -- increment the counter for a given id
   function Increment_Counter (Self : in out Instance; Id : in Event_Id) return Count_Status is
      use Event_State_Type;
      use Interfaces;
      Event_Info : Two_Counter_Entry_Type.T;
      Status : Event_Location;
      Increment_Status : Count_Status := Success;
   begin
      -- Check the master state. If it is disabled then bypass all the logic for efficiency
      if Self.Master_Enable_State = Enabled then
         -- Start by range checking. If this fails we shouldn't access anything after
         Status := Get_Entry (Self, Id, Event_Info);

         case Status is
            when Invalid_Id =>
               return Invalid_Id;

            when Top =>
               declare
                  Event_State : constant Event_State_Type.E := Event_Info.Top_Event_Enabled_State;
               begin
                  -- If the state is disabled, then just skip all the logic for better performance
                  case Event_State is
                     when Event_State_Type.Enabled =>

                        -- Check if the event was at the max persistence and should be limited
                        if Event_Info.Top_Event_Count >= Self.Persistence then
                           -- Increment our number of limited event counter if the event is enabled here (so that its protected in the component)
                           Self.Num_Events_Limited := Self.Num_Events_Limited + 1;
                           Increment_Status := Event_Max_Limit;
                           -- Increment if we are less than the persistence
                        else
                           Event_Info.Top_Event_Count := Event_Info.Top_Event_Count + 1;
                        end if;
                        -- Nothing to do if disabled
                     when Event_State_Type.Disabled =>
                        null;
                  end case;
               end;

            when Bottom =>
               declare
                  Event_State : constant Event_State_Type.E := Event_Info.Bottom_Event_Enabled_State;
               begin
                  -- If the state is disabled, then just skip all the logic for better performance
                  case Event_State is
                     when Event_State_Type.Enabled =>

                        -- Check if the event was at the max persistence and limited
                        if Event_Info.Bottom_Event_Count >= Self.Persistence then
                           -- Increment our number of limited event counter if the event is enabled here (so that its protected in the component)
                           Self.Num_Events_Limited := Self.Num_Events_Limited + 1;
                           Increment_Status := Event_Max_Limit;
                           -- Increment if we are less than the persistence
                        else
                           Event_Info.Bottom_Event_Count := Event_Info.Bottom_Event_Count + 1;
                        end if;
                        -- Nothing to do if disabled
                     when Event_State_Type.Disabled =>
                        null;
                  end case;
               end;
         end case;

         -- Write the new values back into our structure
         Set_Entry (Self, Id, Event_Info);
      end if;

      return Increment_Status;
   end Increment_Counter;

   -- Decrement a counter for a given id
   function Decrement_Counter (Self : in out Instance; Id : in Event_Id) return Count_Status is
      use Event_State_Type;
      Event_Info : Two_Counter_Entry_Type.T;
      Status : Event_Location;
      Decrement_Status : Count_Status := Success;
   begin
      -- If the master state is disabled, then bypass all the logic and just return success
      if Self.Master_Enable_State = Enabled then
         -- Start by range checking. If this fails we shouldn't access anything after
         Status := Get_Entry (Self, Id, Event_Info);
         case Status is
            when Invalid_Id =>
               return Invalid_Id;

            when Top =>
               declare
                  Event_State : constant Event_State_Type.E := Event_Info.Top_Event_Enabled_State;
               begin
                  -- If the state is disabled, then just skip all the logic for better performance
                  case Event_State is
                     when Enabled =>
                        -- Check if the event was limited. Not necessarily a bad status but used in the component.
                        if Event_Info.Top_Event_Count >= Self.Persistence then
                           Decrement_Status := Event_Max_Limit;
                        end if;
                        -- Decrement the count if needed
                        if Event_Info.Top_Event_Count > 0 then
                           Event_Info.Top_Event_Count := Event_Info.Top_Event_Count - 1;
                        end if;
                        -- Nothing to do if disabled
                     when Disabled =>
                        null;
                  end case;
               end;

            when Bottom =>
               declare
                  Event_State : constant Event_State_Type.E := Event_Info.Bottom_Event_Enabled_State;
               begin
                  -- If the state is disabled, then just skip all the logic for better performance
                  case Event_State is
                     when Enabled =>
                        -- Check if the event was limited
                        if Event_Info.Bottom_Event_Count >= Self.Persistence then
                           Decrement_Status := Event_Max_Limit;
                        end if;
                        -- Decrement count if needed
                        if Event_Info.Bottom_Event_Count > 0 then
                           Event_Info.Bottom_Event_Count := Event_Info.Bottom_Event_Count - 1;
                        end if;
                        -- Nothing to do if disabled
                     when Disabled =>
                        null;
                  end case;
               end;
         end case;

         -- Write the new values back into our structure
         Set_Entry (Self, Id, Event_Info);
      end if;

      return Decrement_Status;

   end Decrement_Counter;

   -- Set the status for a given event id
   function Set_Enable_State (Self : in out Instance; Id : in Event_Id; New_State : in Event_State_Type.E) return Enable_State_Status is
      Event_Info : Two_Counter_Entry_Type.T;
      Status : Event_Location;
   begin
      -- Start by range checking. If this fails we shouldn't access anything after
      Status := Get_Entry (Self, Id, Event_Info);

      case Status is
         when Invalid_Id =>
            return Invalid_Id;

         when Top =>
            -- When we change the state, we will reset the count as well
            Event_Info.Top_Event_Enabled_State := New_State;
            Event_Info.Top_Event_Count := 0;

         when Bottom =>
            -- When we change the state, we will reset the count as well
            Event_Info.Bottom_Event_Enabled_State := New_State;
            Event_Info.Bottom_Event_Count := 0;

      end case;

      -- Write the new values back into our structure
      Set_Entry (Self, Id, Event_Info);

      return Success;
   end Set_Enable_State;

   -- Set the status for a given event id
   function Get_Enable_State (Self : in Instance; Id : in Event_Id; Event_State : out Event_State_Type.E) return Enable_State_Status is
      Event_Info : Two_Counter_Entry_Type.T;
      Status : Event_Location;
   begin
      -- Set our event state just in case we have a bad Id
      Event_State := Event_State_Type.Disabled;
      -- Start by range checking. If this fails we shouldn't access anything after
      Status := Get_Entry (Self, Id, Event_Info);

      case Status is
         when Invalid_Id =>
            return Invalid_Id;
         when Top =>
            Event_State := Event_Info.Top_Event_Enabled_State;
         when Bottom =>
            Event_State := Event_Info.Bottom_Event_Enabled_State;
      end case;

      return Success;
   end Get_Enable_State;

   procedure Set_Persistence (Self : in out Instance; New_Persistence : in Persistence_Type) is
   begin
      -- If the new persistence decreases, then we need to loop through all the events and make sure the counts are less than or equal to the new persistence
      if New_Persistence < Self.Persistence then
         -- No need to check the id since we assume it should be in range from our init values
         for Idx in Self.Bytes'Range loop
            declare
               Event_Info : Two_Counter_Entry_Type.T with
                  Import,
                  Convention => Ada,
                  Address => Self.Bytes (Idx)'Address;
            begin
               -- If the top half to the byte has a count greater than the persistence, then set it to the new persistence value
               if Event_Info.Top_Event_Count > New_Persistence then
                  Event_Info.Top_Event_Count := New_Persistence;
               end if;
               -- If the bottom half to the byte has a count greater than the persistence, then set it to the new persistence value
               if Event_Info.Bottom_Event_Count > New_Persistence then
                  Event_Info.Bottom_Event_Count := New_Persistence;
               end if;
            end;
         end loop;
      end if;
      -- At the end capture the new persistence
      Self.Persistence := New_Persistence;
   end Set_Persistence;

   function Get_Persistence (Self : in Instance) return Persistence_Type is
   begin
      return Self.Persistence;
   end Get_Persistence;

   procedure Set_Master_Enable_State (Self : in out Instance; New_Master_State : in Event_State_Type.E) is
   begin
      Self.Master_Enable_State := New_Master_State;
   end Set_Master_Enable_State;

   function Get_Master_Enable_State (Self : in Instance) return Event_State_Type.E is
   begin
      return Self.Master_Enable_State;
   end Get_Master_Enable_State;

   function Get_Events_Limited_Count (Self : in Instance) return Interfaces.Unsigned_16 is
   begin
      return Self.Num_Events_Limited;
   end Get_Events_Limited_Count;

   procedure Reset_Event_Limited_Count (Self : in out Instance) is
   begin
      Self.Num_Events_Limited := 0;
   end Reset_Event_Limited_Count;

   function Get_Event_Start_Stop_Range (Self : in Instance; Event_Stop_Id : out Event_Id) return Event_Id is
   begin
      Event_Stop_Id := Self.End_Id;
      return Self.Start_Id;
   end Get_Event_Start_Stop_Range;

   function Get_Entry (Self : in Instance; Id : in Event_Id; Event_Info : out Two_Counter_Entry_Type.T) return Event_Location is
   begin
      -- Initialize event info to be safe:
      Event_Info := (
         Top_Event_Enabled_State => Event_State_Type.Disabled,
         Top_Event_Count => 0,
         Bottom_Event_Enabled_State => Event_State_Type.Disabled,
         Bottom_Event_Count => 0
      );

      -- Check that the event is in range
      if Id >= Self.Start_Id and then Id <= Self.End_Id then
         declare
            Event_Id_In : constant Natural := Natural (Id - Self.Start_Id) / 2;
            Top_Or_Bottom : constant Natural := Natural (Id) mod 2;
            Overlay : Two_Counter_Entry_Type.T with
               Import,
               Convention => Ada,
               Address => Self.Bytes (Event_Id_In)'Address;
         begin
            -- Give the caller the overlaid byte so that it can be operated on and copied back in.
            Event_Info := Overlay;
            -- Return a Top or Bottom status so that the caller doesn't have to perform the math on the id again and knows which half of the byte to operate on.
            if Top_Or_Bottom = 0 then
               return Top;
            else
               return Bottom;
            end if;
         end;

      else
         return Invalid_Id;
      end if;
   end Get_Entry;

   procedure Set_Entry (Self : in out Instance; Id : in Event_Id; Event_New_Info : in Two_Counter_Entry_Type.T) is
      Event_Id_In : constant Natural := Natural (Id - Self.Start_Id) / 2;
      Event_Info : Two_Counter_Entry_Type.T with
         Import,
         Convention => Ada,
         Address => Self.Bytes (Event_Id_In)'Address;
   begin
      -- No range checking here since we always call get then set. Up to the caller to be responsible here.
      Event_Info := Event_New_Info;
   end Set_Entry;

end Two_Counter_Entry;
