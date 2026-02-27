with Basic_Types;
with Variable_Serializer;
with Serializer_Types;

--
-- This package implements a constant time access database data structure meant to be used with
-- Adamant variable length packed types. To instantiate the database you must provide four generic parameters:
--    1) Id_Type - a discrete Id type which will be the database key
--    2) T - a type to store on the database (usually a packed record)
--    3) Serialized_Length - a function that when passed T will return the serialized length of T (number of bytes)
--    4) Serialized_Length - a function that when passed a serialized version of T will return the length of T (number of bytes)
--
-- To initialize the component 3 parameters must be passed
--    1) The minimum Id that the database should be able to accommodate
--    2) The maximum Id that the database should be able to accommodate
--
-- The database will be sized to have entries for each possible Id between the minimum and maximum.
--
-- Note: You should NOT use a sparse ID set when using this database data structure or you
-- will be wasting a lot of memory. This database is designed for a compact contiguous Id space to Value mapping.
--
generic
   type Id_Type is (<>); -- Any discrete type: integer, modular, or enumeration.
   type T is private;
   with function Serialized_Length (Src : in T; Num_Bytes_Serialized : out Natural) return Serializer_Types.Serialization_Status;
   with function Serialized_Length (Src : in Basic_Types.Byte_Array; Num_Bytes_Serialized : out Natural) return Serializer_Types.Serialization_Status;
package Variable_Database is

   -- Object type:
   type Instance is tagged private;

   -- Return types:
   type Update_Status is (Success, Id_Out_Of_Range, Serialization_Failure);
   type Fetch_Status is (Success, Id_Out_Of_Range, Data_Not_Available);
   type Clear_Override_Status is (Success, Id_Out_Of_Range);

   -- Object primitives:
   procedure Init (Self : in out Instance; Minimum_Id : in Id_Type; Maximum_Id : in Id_Type);
   procedure Destroy (Self : in out Instance);
   function Update (Self : in out Instance; Id : in Id_Type; Value : in T) return Update_Status;
   function Fetch (Self : in Instance; Id : in Id_Type; Value : out T) return Fetch_Status;

   -- Backdoor features:
   -- Same as update, but prevents any future updates from changing the underlying value. This
   -- state can be reversed using Clear_Override.
   function Override (Self : in out Instance; Id : in Id_Type; Value : in T) return Update_Status;
   -- Allow future updates to take effect again.
   function Clear_Override (Self : in out Instance; Id : in Id_Type) return Clear_Override_Status;
   -- Clear_Override for all entries in the database.
   procedure Clear_Override_All (Self : in out Instance);
   -- Returns True if any entries are currently being overridden.
   function Any_Overridden (Self : in Instance) return Boolean;

private

   -- Instantiate the variable serializer for our type:
   package T_Serializer is new Variable_Serializer (T, Serialized_Length, Serialized_Length);

   -- State of a data base entry:
   -- Empty - The database entry has not been stored to yet.
   -- Filled - The database entry has been stored to.
   -- Override - The database entry has been overridden.
   type Entry_State is (Empty, Filled, Override);

   -- An entry into the database. It stores the value as well as the
   -- valid/invalid status. A value is valid if it has been successfully
   -- stored.
   type Database_Entry is record
      State : Entry_State := Empty;
      Data : T_Serializer.Byte_Array := [others => 0];
   end record;

   -- Database table type which maps the index type (unconstrained)
   -- to a database entry type:
   type Database_Table is array (Id_Type range <>) of Database_Entry;
   type Database_Table_Access is access Database_Table;

   -- The object instance record:
   type Instance is tagged record
      Db_Table : Database_Table_Access := null;
   end record;

end Variable_Database;
