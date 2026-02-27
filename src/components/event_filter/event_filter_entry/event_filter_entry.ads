-- This is a generic, unprotected statistics data structure.
-- The user can instantiate this class with any type that they choose.
with Basic_Types;
with Event_Types;
with Event_Filter_Entry_Enums;
with Interfaces;

package Event_Filter_Entry is
   use Event_Types;
   use Event_Filter_Entry_Enums;
   type Instance is tagged private;

   -- Defined return type
   type Filter_Status is (Filtered, Unfiltered, Out_Of_Range);
   type Event_Entry_Status is (Success, Invalid_Id);

   -- The type of data stored on the statistic array.
   type Event_Id_List is array (Natural range <>) of Event_Types.Event_Id;
   type Event_Id_List_Access is access Event_Id_List;

   -- Type used to limit the number to 8 for the bit location of the event we are searching for.
   type Bit_Location_Type is mod 8;

   --
   -- Initialization/destruction functions:
   --
   -- Init Parameters:
   -- Event_Id_Start : Event_Id - The event ID that begins the range of ids that the component will include for filtering of events.
   -- Event_Id_Stop : Event_Id - The event ID that ends the range of ids that the component will include for filtering of events.
   -- Event_Filter_List : Event_Id_List - A list of event IDs that are enabled for filtering by default
   --
   procedure Init (Self : in out Instance; Event_Id_Start : in Event_Id; Event_Id_Stop : in Event_Id; Event_Filter_List : in Event_Id_List);
   procedure Destroy (Self : in out Instance);

   -- Function to set the status of a given event ID to the given status.
   function Set_Filter_State (Self : in out Instance; Id : in Event_Id; New_State : in Event_Filter_State.E) return Event_Entry_Status;
   -- Function that will determine if the state for a given event is set to be filtered. It will track the number of filtered events if the state is set to filtered
   function Filter_Event (Self : in out Instance; Id : in Event_Id) return Filter_Status;
   -- Procedure to set the Global state to enabled/disabled. Used exclusively by the component
   procedure Set_Global_Enable_State (Self : in out Instance; New_Global_State : in Global_Filter_State.E);
   -- Function to get the Global state for the component.
   function Get_Global_Enable_State (Self : in Instance) return Global_Filter_State.E;
   -- Function to get the count of the number of events that have been filtered
   function Get_Event_Filtered_Count (Self : in Instance) return Interfaces.Unsigned_32;
   -- Function to get the count of the number of events that have been unfiltered
   function Get_Event_Unfiltered_Count (Self : in Instance) return Interfaces.Unsigned_32;
   -- Function to fetch the event range. This helps keep the component in sync with the package
   function Get_Event_Start_Stop_Range (Self : in Instance; Event_Stop_Id : out Event_Id) return Event_Id;
   -- Function to get the pointer for the array. This is so that we can quickly copy the whole thing into the state packet
   function Get_Entry_Array (Self : in Instance) return Basic_Types.Byte_Array_Access;

private

   type Instance is tagged record
      -- Array to hold the two counter entry. Two counters and a state for each byte.
      Events : Basic_Types.Byte_Array_Access := null;
      -- Keep track of the event range for range checking
      Start_Id : Event_Id := Event_Id'First;
      End_Id : Event_Id := Event_Id'First;
      -- Track the number of events filtered and unfiltered
      Num_Events_Filtered : Interfaces.Unsigned_32 := Interfaces.Unsigned_32'First;
      Num_Events_Unfiltered : Interfaces.Unsigned_32 := Interfaces.Unsigned_32'First;
      -- Value to indicate if the entire package/component should be enabled/disabled. Note that this uses the same enumeration as the states but is used as a global switch.
      -- It is also only used in the component, but stored here to be protected with the rest of the internal data structures
      Global_Enable_State : Global_Filter_State.E := Global_Filter_State.Enabled;
   end record;

end Event_Filter_Entry;
