-- Standard includes:
with Event;

-- Package which provides function which transforms an event
-- into a human readable string.
package {{ name }}_Event_To_Text is
   -----------------------------------
   -- Public Subprograms:
   -----------------------------------
   function Event_To_Text (The_Event : in Event.T) return String;

end {{ name }}_Event_To_Text;
