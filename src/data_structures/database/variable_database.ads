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
package Variable_Database with SPARK_Mode => On is

   -- The contracts and ghost code in this package exist for proof with
   -- GNATprove only. The assertion policy below disables them at runtime, so
   -- the generated code and the runtime behavior of this package is
   -- identical to what it was before the SPARK conversion. The defensive
   -- pragma Assert statements in the package body are not affected by this
   -- policy. They remain compiled in and enabled under the project wide
   -- assertion policy, and they are also proved.
   pragma Assertion_Policy
      (Pre => Ignore,
       Pre'Class => Ignore,
       Post => Ignore,
       Post'Class => Ignore,
       Contract_Cases => Ignore,
       Ghost => Ignore,
       Loop_Invariant => Ignore,
       Loop_Variant => Ignore,
       Assert_And_Cut => Ignore,
       Assume => Ignore,
       Subprogram_Variant => Ignore);
   pragma Unevaluated_Use_Of_Old (Allow);

   -- Object type:
   type Instance is tagged private;

   -- Ghost predicate stating that the database is in a valid, initialized
   -- state: its table has been allocated by Init. This is the precondition
   -- of every operation that touches the table. It takes a class-wide
   -- parameter so that it is not a dispatching operation, which would force
   -- every derived type to override each operation mentioning it
   -- (SPARK RM 6.1.1).
   function Is_Valid (Self : in Instance'Class) return Boolean
      with Ghost;

   -- Ghost functions giving the range of Ids the database was initialized to
   -- hold, and whether an Id is within it:
   function First_Id (Self : in Instance'Class) return Id_Type
      with Ghost,
           Pre => Is_Valid (Self);
   function Last_Id (Self : in Instance'Class) return Id_Type
      with Ghost,
           Pre => Is_Valid (Self);
   function Contains (Self : in Instance'Class; Id : in Id_Type) return Boolean is
      (Id in First_Id (Self) .. Last_Id (Self))
      with Ghost,
           Pre => Is_Valid (Self);

   -- Return types:
   type Update_Status is (Success, Id_Out_Of_Range, Serialization_Failure);
   type Fetch_Status is (Success, Id_Out_Of_Range, Data_Not_Available);
   type Clear_Override_Status is (Success, Id_Out_Of_Range);

   -- Object primitives:
   procedure Init (Self : in out Instance; Minimum_Id : in Id_Type; Maximum_Id : in Id_Type)
      with
         -- The database is valid and holds exactly the Ids from Minimum_Id to Maximum_Id.
         Post => Is_Valid (Self) and then (for all Id in Id_Type => Contains (Self, Id) = (Id in Minimum_Id .. Maximum_Id));
   procedure Destroy (Self : in out Instance);
   function Update (Self : in out Instance; Id : in Id_Type; Value : in T) return Update_Status
      with
         Side_Effects,
         -- The database is valid.
         Pre'Class => Is_Valid (Self),
         -- The database is still valid, holds the same Ids, and the update fails with Id_Out_Of_Range exactly when the Id is not held.
         Post => Is_Valid (Self)
            and then First_Id (Self) = First_Id (Self)'Old and then Last_Id (Self) = Last_Id (Self)'Old
            and then (Update'Result = Id_Out_Of_Range) = (not Contains (Self, Id));
   function Fetch (Self : in Instance; Id : in Id_Type; Value : out T) return Fetch_Status
      with
         Side_Effects,
         -- The database is valid.
         Pre'Class => Is_Valid (Self),
         -- The fetch fails with Id_Out_Of_Range exactly when the Id is not held.
         Post => (Fetch'Result = Id_Out_Of_Range) = (not Contains (Self, Id));
   pragma Annotate (GNATprove, Intentional, "might not be set", "Value is only meaningful when Fetch returns Success, and callers check the status before using it. There is no value of the private type T to write on the failure paths.");

   -- Backdoor features:
   -- Same as update, but prevents any future updates from changing the underlying value. This
   -- state can be reversed using Clear_Override.
   function Override (Self : in out Instance; Id : in Id_Type; Value : in T) return Update_Status
      with
         Side_Effects,
         -- The database is valid.
         Pre'Class => Is_Valid (Self),
         -- The database is still valid, holds the same Ids, and the override fails with Id_Out_Of_Range exactly when the Id is not held.
         Post => Is_Valid (Self)
            and then First_Id (Self) = First_Id (Self)'Old and then Last_Id (Self) = Last_Id (Self)'Old
            and then (Override'Result = Id_Out_Of_Range) = (not Contains (Self, Id));
   -- Allow future updates to take effect again.
   function Clear_Override (Self : in out Instance; Id : in Id_Type) return Clear_Override_Status
      with
         Side_Effects,
         -- The database is valid.
         Pre'Class => Is_Valid (Self),
         -- The database is still valid, holds the same Ids, and the clear succeeds exactly when the Id is held.
         Post => Is_Valid (Self)
            and then First_Id (Self) = First_Id (Self)'Old and then Last_Id (Self) = Last_Id (Self)'Old
            and then (Clear_Override'Result = Success) = Contains (Self, Id);
   -- Clear_Override for all entries in the database.
   procedure Clear_Override_All (Self : in out Instance)
      with
         -- The database is valid.
         Pre'Class => Is_Valid (Self),
         -- The database is still valid and holds the same Ids.
         Post => Is_Valid (Self) and then First_Id (Self) = First_Id (Self)'Old and then Last_Id (Self) = Last_Id (Self)'Old;
   -- Returns True if any entries are currently being overridden.
   function Any_Overridden (Self : in Instance) return Boolean
      with
         -- The database is valid.
         Pre'Class => Is_Valid (Self);

private

   -- Instantiate the variable serializer for our type. Its declarations are in
   -- SPARK and its body, which overlays byte arrays by address, is not
   -- analyzed, so SPARK treats its operations as trusted boundary operations.
   package T_Serializer is new Variable_Serializer (T, Serialized_Length, Serialized_Length);
   -- The storage for one serialized value, sized to hold the largest T:
   subtype Entry_Bytes is T_Serializer.Byte_Array;

   -- Serialize a value into an entry's storage:
   function Store (Data : out Entry_Bytes; Value : in T) return Serializer_Types.Serialization_Status
      with Side_Effects;
   -- Deserialize a value from an entry's storage:
   function Load (Value : out T; Data : in Entry_Bytes) return Serializer_Types.Serialization_Status
      with Side_Effects;

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
      Data : Entry_Bytes := [others => 0];
   end record;

   -- Database table type which maps the index type (unconstrained)
   -- to a database entry type:
   type Database_Table is array (Id_Type range <>) of Database_Entry;
   type Database_Table_Access is access Database_Table;

   -- The object instance record:
   type Instance is tagged record
      Db_Table : Database_Table_Access := null;
   end record;

   -- Ghost model completions:
   function Is_Valid (Self : in Instance'Class) return Boolean is
      (Self.Db_Table /= null and then Self.Db_Table'First in Id_Type and then Self.Db_Table'Last in Id_Type);
   function First_Id (Self : in Instance'Class) return Id_Type is (Self.Db_Table'First);
   function Last_Id (Self : in Instance'Class) return Id_Type is (Self.Db_Table'Last);

end Variable_Database;
