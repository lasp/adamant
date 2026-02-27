--
-- This package implements a simple constant time access database data structure. To instantiate
-- the database you must provide two generic parameters, a discrete Id type which will be the key
-- used to access a generic value type.
--
-- To initialize the component 3 parameters must be passed
--    1) The maximum number of values for the database to hold
--    2) The minimum Id that the database should be able to accommodate
--    3) The maximum Id that the database should be able to accommodate
--
-- The data structure will allocate two tables. The first table will be as large as the value
-- provided in #1 and will hold the database values. The second table will as large as the
-- difference between the value provided in #3 and the value provided in #2, and will map
-- the user Id space to an internal Id space used to index into the first table.
--
-- Note: You should NOT use a sparse ID set when using this database data structure or you
-- will be wasting a lot of memory on the second table. This database is designed for
-- compact Id space to Value mapping.
--
generic
   type Id_Type is (<>);
   type Value_Type is private;
package Database is
   -- Object type:
   type Instance is tagged private;

   -- Return types:
   type Update_Status is (Success, Id_Out_Of_Range, Not_Enough_Memory);
   type Fetch_Status is (Success, Id_Out_Of_Range, Data_Not_Available);

   -- Object primitives:
   procedure Init (Self : in out Instance; Max_Num_Values : Positive; Minimum_Id : Id_Type; Maximum_Id : Id_Type);
   procedure Destroy (Self : in out Instance);
   function Is_Full (Self : in Instance) return Boolean;
   function Is_Empty (Self : in Instance) return Boolean;
   function Get_Count (Self : in Instance) return Natural;
   function Update (Self : in out Instance; Id : in Id_Type; Value : in Value_Type) return Update_Status;
   function Fetch (Self : in Instance; Id : in Id_Type; Value : out Value_Type) return Fetch_Status;

private
   -- Database table type which maps a positive index to a database
   -- element of generic type T:
   subtype Database_Index is Positive;
   type Database_Table is array (Database_Index range <>) of Value_Type;
   type Database_Table_Access is access Database_Table;

   -- Index table type that maps the index type to an array of positives
   -- which are the indexes into the database table. A value of 0
   -- is reserved for entries that have not yet been allocated in the
   -- database table.
   type Index_Lookup_Table is array (Id_Type range <>) of Natural;
   type Index_Lookup_Table_Access is access Index_Lookup_Table;

   -- The object instance record:
   type Instance is tagged record
      Head : Database_Index := Database_Index'First;
      Db_Table : Database_Table_Access;
      Index_Table : Index_Lookup_Table_Access;
   end record;

end Database;
