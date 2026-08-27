with Safe_Deallocator;

package body Variable_Database with SPARK_Mode => On is

   -- See the note in the package specification. All contracts and ghost code
   -- are for proof only and are disabled at runtime:
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

   function Store (Data : out Entry_Bytes; Value : in T) return Serializer_Types.Serialization_Status is
      Status : Serializer_Types.Serialization_Status;
   begin
      Status := T_Serializer.To_Byte_Array (Data, Value);
      return Status;
   end Store;

   function Load (Value : out T; Data : in Entry_Bytes) return Serializer_Types.Serialization_Status is
      Status : Serializer_Types.Serialization_Status;
   begin
      Status := T_Serializer.From_Byte_Array (Value, Data);
      return Status;
   end Load;

   -- The body is not analyzed by SPARK since it performs the heap allocation
   -- that owns the table for the rest of the database's life. The
   -- postcondition holds because the allocation below has exactly the
   -- requested bounds.
   procedure Init (Self : in out Instance; Minimum_Id : in Id_Type; Maximum_Id : in Id_Type) with SPARK_Mode => Off is
   begin
      Self.Db_Table := new Database_Table (Minimum_Id .. Maximum_Id);
   end Init;

   -- The body is not analyzed by SPARK since conditional deallocation is
   -- outside the SPARK ownership model.
   procedure Destroy (Self : in out Instance) with SPARK_Mode => Off is
      procedure Free_If_Testing is new Safe_Deallocator.Deallocate_If_Testing (Object => Database_Table, Name => Database_Table_Access);
   begin
      Free_If_Testing (Self.Db_Table);
   end Destroy;

   function Update (Self : in out Instance; Id : in Id_Type; Value : in T) return Update_Status is
      use Serializer_Types;
      Stat : Serializer_Types.Serialization_Status;
   begin
      -- Check the Id:
      if Id < Self.Db_Table'First or else Id > Self.Db_Table'Last then
         return Id_Out_Of_Range;
      end if;

      -- Check the state of the entry:
      case Self.Db_Table (Id).State is
         -- If not in override mode then allow the entry to be updated:
         when Empty | Filled =>
            -- Try to serialize the value into the database:
            Stat := Store (Self.Db_Table (Id).Data, Value);
            if Stat /= Success then
               return Serialization_Failure;
            end if;
            -- Set the state:
            Self.Db_Table (Id).State := Filled;
         when Override => null; -- Nothing to do
      end case;

      return Success;
   end Update;

   function Fetch (Self : in Instance; Id : in Id_Type; Value : out T) return Fetch_Status is
      use Serializer_Types;
      Stat : Serializer_Types.Serialization_Status;
   begin
      -- Check the Id:
      if Id < Self.Db_Table'First or else Id > Self.Db_Table'Last then
         return Id_Out_Of_Range;
      end if;

      -- Make sure the database entry is valid:
      case Self.Db_Table (Id).State is
         when Filled | Override => null;
         when Empty => return Data_Not_Available;
      end case;

      -- OK, we are good, grab the data:
      Stat := Load (Value, Self.Db_Table (Id).Data);

      -- The status should always be successful here. Since the data was serialized
      -- into the database, there is no way it can deserialize with error unless a
      -- bit was flipped or there is a software bug in this package.
      pragma Assert (Stat = Success, "Deserialization of database item failed!");
      pragma Annotate (GNATprove, Intentional, "assertion might fail", "The bytes were written by Store, so Load can only fail if the storage has since been corrupted. That is a fault to report, not a case to handle, and no proof can rule it out.");

      return Success;
   end Fetch;

   function Override (Self : in out Instance; Id : in Id_Type; Value : in T) return Update_Status is
      use Serializer_Types;
      Stat : Serializer_Types.Serialization_Status;
   begin
      -- Check the Id:
      if Id < Self.Db_Table'First or else Id > Self.Db_Table'Last then
         return Id_Out_Of_Range;
      end if;

      -- Try to serialize the value into the database:
      Stat := Store (Self.Db_Table (Id).Data, Value);
      if Stat /= Success then
         return Serialization_Failure;
      end if;
      -- Set the state:
      Self.Db_Table (Id).State := Override;

      return Success;
   end Override;

   function Clear_Override (Self : in out Instance; Id : in Id_Type) return Clear_Override_Status is
   begin
      -- Check the Id:
      if Id < Self.Db_Table'First or else Id > Self.Db_Table'Last then
         return Id_Out_Of_Range;
      end if;

      -- Set the state:
      case Self.Db_Table (Id).State is
         when Filled | Override =>
            -- Set the state:
            Self.Db_Table (Id).State := Filled;
         when Empty => null; -- Nothing to do.
      end case;

      return Success;
   end Clear_Override;

   procedure Clear_Override_All (Self : in out Instance) is
      Stat : Clear_Override_Status;
   begin
      for Id in Self.Db_Table'Range loop
         -- The database stays valid and keeps its range, so every Id of the range is still held.
         pragma Loop_Invariant (Is_Valid (Self));
         pragma Loop_Invariant (First_Id (Self) = First_Id (Self)'Loop_Entry and then Last_Id (Self) = Last_Id (Self)'Loop_Entry);
         Stat := Self.Clear_Override (Id);
         pragma Assert (Stat = Success);
      end loop;
   end Clear_Override_All;

   function Any_Overridden (Self : in Instance) return Boolean is
   begin
      -- Go through each entry and return True if any are overridden.
      for Id in Self.Db_Table'Range loop
         if Self.Db_Table (Id).State = Override then
            return True;
         end if;
      end loop;
      return False;
   end Any_Overridden;

end Variable_Database;
