with Safe_Deallocator;

package body Database is

   procedure Init (Self : in out Instance; Max_Num_Values : Positive; Minimum_Id : Id_Type; Maximum_Id : Id_Type) is
   begin
      Self.Db_Table := new Database_Table (Database_Index'First .. Database_Index'First + Max_Num_Values - 1);
      Self.Index_Table := new Index_Lookup_Table (Minimum_Id .. Maximum_Id);
      -- Initialize this table to zero, to signify unallocated entries:
      Self.Index_Table.all := [others => 0];
   end Init;

   procedure Destroy (Self : in out Instance) is
      procedure Free_Table_If_Testing is new Safe_Deallocator.Deallocate_If_Testing (Object => Database_Table, Name => Database_Table_Access);
      procedure Free_Index_Table_If_Testing is new Safe_Deallocator.Deallocate_If_Testing (Object => Index_Lookup_Table, Name => Index_Lookup_Table_Access);
   begin
      Self.Head := Positive'First;
      Free_Table_If_Testing (Self.Db_Table);
      Free_Index_Table_If_Testing (Self.Index_Table);
   end Destroy;

   function Is_Full (Self : in Instance) return Boolean is
   begin
      return Self.Head > Self.Db_Table'Last;
   end Is_Full;

   function Is_Empty (Self : in Instance) return Boolean is
   begin
      return Self.Head = Positive'First;
   end Is_Empty;

   function Get_Count (Self : in Instance) return Natural is
   begin
      return Self.Head - 1;
   end Get_Count;

   function Update (Self : in out Instance; Id : in Id_Type; Value : in Value_Type) return Update_Status is
      Db_Index : Natural;
   begin
      -- Check the Id:
      if Id < Self.Index_Table'First or else Id > Self.Index_Table'Last then
         return Id_Out_Of_Range;
      end if;

      -- Look up the database index:
      Db_Index := Self.Index_Table (Id);

      -- Has data been stored at this index before? If not
      -- then assign a database index.
      if Db_Index = 0 then
         -- Make sure we have room in the database table to store the
         -- new element:
         if Self.Head > Self.Db_Table'Last then
            return Not_Enough_Memory;
         end if;

         -- Ok there is space, lets allocate:
         Db_Index := Self.Head;
         Self.Index_Table (Id) := Db_Index;

         -- Increment the head:
         Self.Head := Self.Head + 1;
      end if;

      -- The index should ALWAYS be less than head.
      pragma Assert (Db_Index < Self.Head);

      -- Fill in the database entry:
      Self.Db_Table (Db_Index) := Value;

      return Success;
   end Update;

   function Fetch (Self : in Instance; Id : in Id_Type; Value : out Value_Type) return Fetch_Status is
      Db_Index : Natural;
   begin
      if Id < Self.Index_Table'First or else Id > Self.Index_Table'Last then
         return Id_Out_Of_Range;
      end if;

      -- Look up the database index:
      Db_Index := Self.Index_Table (Id);

      -- Has data been stored at this index before? If not
      -- then return error.
      if Db_Index = 0 then
         return Data_Not_Available;
      end if;

      -- The index should ALWAYS be less than head.
      pragma Assert (Db_Index < Self.Head);

      -- Index is valid, lets fetch the data:
      Value := Self.Db_Table (Db_Index);

      return Success;
   end Fetch;

end Database;
