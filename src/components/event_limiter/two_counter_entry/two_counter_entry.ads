-- This is a two-counter entry data structure for event limiting.
-- Each entry packs two event counters and their enable states into a single byte.
with Basic_Types;
with Event_Types;
with Two_Counter_Entry_Enums;
with Two_Counter_Entry_Type;
with Interfaces;

package Two_Counter_Entry is
   use Event_Types;
   use Two_Counter_Entry_Enums;
   use Two_Counter_Entry_Type;
   type Instance is tagged private;

   -- Defined return type
   type Count_Status is (Success, Event_Max_Limit, Invalid_Id);
   type Enable_State_Status is (Success, Invalid_Id);
   type Event_Location is (Top, Bottom, Invalid_Id);

   -- The type of data stored on the statistic array.
   type Event_Id_List is array (Natural range <>) of Event_Types.Event_Id;
   type Event_Id_List_Access is access Event_Id_List;

   -- Persistence type
   subtype Persistence_Type is Event_Count_Type range 1 .. Event_Count_Type'Last;

   --
   -- Initialization/destruction functions:
   --
   -- Init Parameters:
   -- Event_Id_Start : Event_Id - The starting event ID for the range of events to be managed by this limiter.
   -- Event_Id_Stop : Event_Id - The ending event ID for the range of events to be managed by this limiter (inclusive).
   -- Event_Disable_List : Event_Id_List - A list of event IDs that are disabled by default.
   --
   procedure Init (Self : in out Instance; Event_Id_Start : in Event_Id; Event_Id_Stop : in Event_Id; Event_Disable_List : in Event_Id_List; Event_Limit_Persistence : in Persistence_Type);
   procedure Destroy (Self : in out Instance);

   -- Function to increment the count of a given event ID. This will not increment if the counter is at the maximum value.
   function Increment_Counter (Self : in out Instance; Id : in Event_Id) return Count_Status;
   -- Function to decrement the count of a given event ID. This will not decrement if the counter is at 0.
   function Decrement_Counter (Self : in out Instance; Id : in Event_Id) return Count_Status;
   -- Function to set the status of a given event ID to the given status.
   function Set_Enable_State (Self : in out Instance; Id : in Event_Id; New_State : in Event_State_Type.E) return Enable_State_Status;
   -- Function that will retrieve the event state (enabled/disabled)
   function Get_Enable_State (Self : in Instance; Id : in Event_Id; Event_State : out Event_State_Type.E) return Enable_State_Status;
   -- procedure to change the persistence
   procedure Set_Persistence (Self : in out Instance; New_Persistence : in Persistence_Type);
   -- Function that will fetch the persistence (for commanding verification)
   function Get_Persistence (Self : in Instance) return Persistence_Type;
   -- Procedure to set the master state to enabled/disabled. Used exclusively by the component
   procedure Set_Master_Enable_State (Self : in out Instance; New_Master_State : in Event_State_Type.E);
   -- Function to get the master state for the component.
   function Get_Master_Enable_State (Self : in Instance) return Event_State_Type.E;
   -- Function to get the count of the number of events that have been limited
   function Get_Events_Limited_Count (Self : in Instance) return Interfaces.Unsigned_16;
   -- Procedure to reset the number of events limited
   procedure Reset_Event_Limited_Count (Self : in out Instance);
   -- Procedure to fetch the event range. This helps keep the component in sync with the package
   function Get_Event_Start_Stop_Range (Self : in Instance; Event_Stop_Id : out Event_Id) return Event_Id;

private
   -- Helper functions to perform range checking on the array, pass the counter data, and to rewrite the values back in. Returns the entry if a valid one is found and otherwise fails with the Invalid_Id status
   function Get_Entry (Self : in Instance; Id : in Event_Id; Event_Info : out Two_Counter_Entry_Type.T) return Event_Location;
   procedure Set_Entry (Self : in out Instance; Id : in Event_Id; Event_New_Info : in Two_Counter_Entry_Type.T);

   type Instance is tagged record
      -- Array to hold the two counter entry. Two counters and a state for each byte.
      Bytes : Basic_Types.Byte_Array_Access := null;
      -- Keep track of the event range for range checking
      Start_Id : Event_Id := Event_Id'First;
      End_Id : Event_Id := Event_Id'First;
      -- Persistence value that determines how many events to increment before it should be limited
      Persistence : Persistence_Type := Persistence_Type'Last;
      -- Track the number of events limited (hit the max persistence and is enabled)
      Num_Events_Limited : Interfaces.Unsigned_16 := Interfaces.Unsigned_16'First;
      -- Value to indicate if the entire package/component should be enabled/disabled. Note that this uses the same enumeration as the states but is used global switch.
      -- It is also only used in the component, but stored here to be protected with the rest of the internal data structures
      Master_Enable_State : Event_State_Type.E := Event_State_Type.Enabled;
   end record;

end Two_Counter_Entry;
