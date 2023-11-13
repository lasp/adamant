package body Event_Filter_Entry.Tester is

   procedure Set_Filtered_Count (Self : in out Instance; Count : in Unsigned_32) is
   begin
      Self.Num_Events_Filtered := Count;
   end Set_Filtered_Count;

   function Get_Filtered_Count (Self : in Instance) return Unsigned_32 is
   begin
      return Self.Num_Events_Filtered;
   end Get_Filtered_Count;

   procedure Set_Unfiltered_Count (Self : in out Instance; Count : in Unsigned_32) is
   begin
      Self.Num_Events_Unfiltered := Count;
   end Set_Unfiltered_Count;

   function Get_Unfiltered_Count (Self : in Instance) return Unsigned_32 is
   begin
      return Self.Num_Events_Unfiltered;
   end Get_Unfiltered_Count;

end Event_Filter_Entry.Tester;
