with Interfaces; use Interfaces;

package Event_Filter_Entry.Tester is

   -- Function to set the filtered and unfiltered count for testing rollovers
   procedure Set_Filtered_Count (Self : in out Instance; Count : in Unsigned_32);
   function Get_Filtered_Count (Self : in Instance) return Unsigned_32;
   procedure Set_Unfiltered_Count (Self : in out Instance; Count : in Unsigned_32);
   function Get_Unfiltered_Count (Self : in Instance) return Unsigned_32;

end Event_Filter_Entry.Tester;
