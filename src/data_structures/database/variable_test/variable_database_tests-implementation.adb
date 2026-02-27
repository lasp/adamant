--------------------------------------------------------------------------------
-- Variable_Database Tests Body
--------------------------------------------------------------------------------

with Smart_Assert;
with Simple_Variable.Representation;
with Simple_Variable.Assertion; use Simple_Variable.Assertion;
with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;

package body Variable_Database_Tests.Implementation is

   -------------------------------------------------------------------------
   -- Global definitions:
   -------------------------------------------------------------------------
   Start_Id : constant Id_Type := 17;
   Stop_Id : constant Id_Type := 25;

   -------------------------------------------------------------------------
   -- Fixtures:
   -------------------------------------------------------------------------

   overriding procedure Set_Up_Test (Self : in out Instance) is
   begin
      -- Initialize the database:
      Self.Db.Init (Start_Id, Stop_Id);
   end Set_Up_Test;

   overriding procedure Tear_Down_Test (Self : in out Instance) is
   begin
      -- Destroy the database:
      Self.Db.Destroy;
   end Tear_Down_Test;

   -------------------------------------------------------------------------
   -- Assertion packages:
   -------------------------------------------------------------------------

   package Update_Status_Assert is new Smart_Assert.Basic (My_Database.Update_Status, My_Database.Update_Status'Image);
   package Fetch_Status_Assert is new Smart_Assert.Basic (My_Database.Fetch_Status, My_Database.Fetch_Status'Image);
   package Clear_Status_Assert is new Smart_Assert.Basic (My_Database.Clear_Override_Status, My_Database.Clear_Override_Status'Image);

   -------------------------------------------------------------------------
   -- Tests:
   -------------------------------------------------------------------------

   overriding procedure Test_Nominal_Update_Fetch (Self : in out Instance) is
      Value : Simple_Variable.T := (0, [others => 0]);
      Cnt : Unsigned_8 := 0;
      U_Status : My_Database.Update_Status;
      F_Status : My_Database.Fetch_Status;
   begin
      -- Add items to database:
      for Id in Start_Id .. Stop_Id loop
         Value.Length := Cnt;
         Value.Buffer := [others => Cnt + 7];
         Cnt := @ + 1;
         Put_Line ("inserting " & Natural'Image (Natural (Id)) & " => " & Simple_Variable.Representation.To_Tuple_String (Value));
         U_Status := Self.Db.Update (Id, Value);
         Update_Status_Assert.Eq (U_Status, My_Database.Success);
      end loop;

      -- Fetch items from database and check values:
      Cnt := 0;
      for Id in Start_Id .. Stop_Id loop
         Value := (0, [others => 0]);
         F_Status := Self.Db.Fetch (Id, Value);
         Fetch_Status_Assert.Eq (F_Status, My_Database.Success);
         Simple_Variable_Assert.Eq (Value, (Cnt, [others => Cnt + 7]));
         Put_Line ("fetching " & Natural'Image (Natural (Id)) & " => " & Simple_Variable.Representation.To_Tuple_String (Value));
         Cnt := @ + 1;
      end loop;

      -- Add items to database:
      Cnt := 0;
      for Id in reverse Start_Id .. Stop_Id loop
         Value.Length := Cnt;
         Value.Buffer := [others => Cnt + 11];
         Cnt := @ + 1;
         Put_Line ("inserting " & Natural'Image (Natural (Id)) & " => " & Simple_Variable.Representation.To_Tuple_String (Value));
         U_Status := Self.Db.Update (Id, Value);
         Update_Status_Assert.Eq (U_Status, My_Database.Success);
      end loop;

      -- Fetch items from database and check values:
      Cnt := 0;
      for Id in reverse Start_Id .. Stop_Id loop
         Value := (0, [others => 0]);
         F_Status := Self.Db.Fetch (Id, Value);
         Fetch_Status_Assert.Eq (F_Status, My_Database.Success);
         Simple_Variable_Assert.Eq (Value, (Cnt, [others => Cnt + 11]));
         Put_Line ("fetching " & Natural'Image (Natural (Id)) & " => " & Simple_Variable.Representation.To_Tuple_String (Value));
         Cnt := @ + 1;
      end loop;
   end Test_Nominal_Update_Fetch;

   overriding procedure Test_Id_Out_Of_Range (Self : in out Instance) is
      Value : Simple_Variable.T := (8, [others => 7]);
      U_Status : My_Database.Update_Status;
      F_Status : My_Database.Fetch_Status;
   begin
      -- Try to update a too high and too low value:
      U_Status := Self.Db.Update (Id_Type'Last, Value);
      Update_Status_Assert.Eq (U_Status, My_Database.Id_Out_Of_Range);
      Simple_Variable_Assert.Eq (Value, (8, [others => 7]));
      U_Status := Self.Db.Update (Id_Type'First, Value);
      Update_Status_Assert.Eq (U_Status, My_Database.Id_Out_Of_Range);
      Simple_Variable_Assert.Eq (Value, (8, [others => 7]));

      -- Fetch with bad id:
      F_Status := Self.Db.Fetch (Id_Type'Last, Value);
      Fetch_Status_Assert.Eq (F_Status, My_Database.Id_Out_Of_Range);
      Simple_Variable_Assert.Eq (Value, (8, [others => 7]));
      F_Status := Self.Db.Fetch (Id_Type'First, Value);
      Fetch_Status_Assert.Eq (F_Status, My_Database.Id_Out_Of_Range);
      Simple_Variable_Assert.Eq (Value, (8, [others => 7]));
   end Test_Id_Out_Of_Range;

   overriding procedure Test_Data_Not_Available (Self : in out Instance) is
      Value : Simple_Variable.T := (5, [others => 1]);
      U_Status : My_Database.Update_Status;
      F_Status : My_Database.Fetch_Status;
      Cnt : Unsigned_8 := 0;
   begin
      -- Nothing should be available on startup:
      for Id in Start_Id .. Stop_Id loop
         F_Status := Self.Db.Fetch (Id, Value);
         Fetch_Status_Assert.Eq (F_Status, My_Database.Data_Not_Available);
         Simple_Variable_Assert.Eq (Value, (5, [others => 1]));
      end loop;

      -- Add values and they should now be available:
      for Id in Start_Id .. Stop_Id - 1 loop
         Value.Length := Cnt;
         Value.Buffer := [others => Cnt + 12];
         Cnt := @ + 1;
         U_Status := Self.Db.Update (Id, Value);
         Update_Status_Assert.Eq (U_Status, My_Database.Success);
      end loop;

      -- Fetch items from database and check values:
      Cnt := 0;
      for Id in Start_Id .. Stop_Id - 1 loop
         Value := (0, [others => 0]);
         F_Status := Self.Db.Fetch (Id, Value);
         Fetch_Status_Assert.Eq (F_Status, My_Database.Success);
         Simple_Variable_Assert.Eq (Value, (Cnt, [others => Cnt + 12]));
         Cnt := @ + 1;
      end loop;

      -- Fetching the last item should still be unavailable:
      Value := (5, [others => 1]);
      F_Status := Self.Db.Fetch (Stop_Id, Value);
      Fetch_Status_Assert.Eq (F_Status, My_Database.Data_Not_Available);
      Simple_Variable_Assert.Eq (Value, (5, [others => 1]));
   end Test_Data_Not_Available;

   overriding procedure Test_Serialization_Failure (Self : in out Instance) is
      Value : Simple_Variable.T := (5, [others => 1]);
      Cnt : Unsigned_8 := 0;
      U_Status : My_Database.Update_Status;
      F_Status : My_Database.Fetch_Status;
   begin
      -- Seed database:
      for Id in Start_Id .. Stop_Id loop
         Value.Length := Cnt;
         Value.Buffer := [others => Cnt + 7];
         Cnt := @ + 1;
         U_Status := Self.Db.Update (Id, Value);
         Update_Status_Assert.Eq (U_Status, My_Database.Success);
      end loop;

      -- Try to update with a value that has a length that is too large:
      Value := (80, [others => 1]);
      U_Status := Self.Db.Update (Start_Id, Value);
      Update_Status_Assert.Eq (U_Status, My_Database.Serialization_Failure);
      Value := (11, [others => 1]);
      U_Status := Self.Db.Override (Stop_Id, Value);
      Update_Status_Assert.Eq (U_Status, My_Database.Serialization_Failure);

      -- Make sure the database values were not overwritten in anyway:
      Cnt := 0;
      for Id in Start_Id .. Stop_Id loop
         Value := (0, [others => 0]);
         F_Status := Self.Db.Fetch (Id, Value);
         Fetch_Status_Assert.Eq (F_Status, My_Database.Success);
         Simple_Variable_Assert.Eq (Value, (Cnt, [others => Cnt + 7]));
         Cnt := @ + 1;
      end loop;
   end Test_Serialization_Failure;

   overriding procedure Test_Override (Self : in out Instance) is
      Value : Simple_Variable.T := (0, [others => 0]);
      Cnt : Unsigned_8 := 0;
      U_Status : My_Database.Update_Status;
      C_Status : My_Database.Clear_Override_Status;
      F_Status : My_Database.Fetch_Status;
   begin
      -- Add items to database:
      for Id in Start_Id .. Stop_Id loop
         Value.Length := Cnt;
         Value.Buffer := [others => Cnt + 7];
         Cnt := @ + 1;
         Put_Line ("inserting " & Natural'Image (Natural (Id)) & " => " & Simple_Variable.Representation.To_Tuple_String (Value));
         U_Status := Self.Db.Update (Id, Value);
         Update_Status_Assert.Eq (U_Status, My_Database.Success);
      end loop;

      -- Fetch items from database and check values:
      Cnt := 0;
      for Id in Start_Id .. Stop_Id loop
         Value := (0, [others => 0]);
         F_Status := Self.Db.Fetch (Id, Value);
         Fetch_Status_Assert.Eq (F_Status, My_Database.Success);
         Simple_Variable_Assert.Eq (Value, (Cnt, [others => Cnt + 7]));
         Put_Line ("fetching " & Natural'Image (Natural (Id)) & " => " & Simple_Variable.Representation.To_Tuple_String (Value));
         Cnt := @ + 1;
      end loop;

      -- Override items in database:
      pragma Assert (Self.Db.Any_Overridden = False);
      Cnt := 0;
      for Id in Start_Id .. Stop_Id loop
         Value.Length := Cnt;
         Value.Buffer := [others => Cnt];
         Cnt := @ + 1;
         Put_Line ("overriding " & Natural'Image (Natural (Id)) & " => " & Simple_Variable.Representation.To_Tuple_String (Value));
         U_Status := Self.Db.Override (Id, Value);
         Update_Status_Assert.Eq (U_Status, My_Database.Success);
         pragma Assert (Self.Db.Any_Overridden = True);
      end loop;

      -- Fetch items from database and check values:
      Cnt := 0;
      for Id in Start_Id .. Stop_Id loop
         Value := (0, [others => 0]);
         F_Status := Self.Db.Fetch (Id, Value);
         Fetch_Status_Assert.Eq (F_Status, My_Database.Success);
         Simple_Variable_Assert.Eq (Value, (Cnt, [others => Cnt]));
         Put_Line ("fetching " & Natural'Image (Natural (Id)) & " => " & Simple_Variable.Representation.To_Tuple_String (Value));
         Cnt := @ + 1;
         pragma Assert (Self.Db.Any_Overridden = True);
      end loop;

      -- Update, but values should not change.
      Cnt := 0;
      for Id in Start_Id .. Stop_Id loop
         Value.Length := Cnt;
         Value.Buffer := [others => Cnt + 7];
         Cnt := @ + 1;
         Put_Line ("inserting " & Natural'Image (Natural (Id)) & " => " & Simple_Variable.Representation.To_Tuple_String (Value));
         U_Status := Self.Db.Update (Id, Value);
         Update_Status_Assert.Eq (U_Status, My_Database.Success);
      end loop;

      -- Fetch items from database and check values:
      Cnt := 0;
      for Id in Start_Id .. Stop_Id loop
         Value := (0, [others => 0]);
         F_Status := Self.Db.Fetch (Id, Value);
         Fetch_Status_Assert.Eq (F_Status, My_Database.Success);
         Simple_Variable_Assert.Eq (Value, (Cnt, [others => Cnt]));
         Put_Line ("fetching " & Natural'Image (Natural (Id)) & " => " & Simple_Variable.Representation.To_Tuple_String (Value));
         Cnt := @ + 1;
         pragma Assert (Self.Db.Any_Overridden = True);
      end loop;

      -- Clear the override on even valued items.
      Cnt := 0;
      for Id in Start_Id .. Stop_Id loop
         if Id mod 2 = 0 then
            C_Status := Self.Db.Clear_Override (Id);
            Clear_Status_Assert.Eq (C_Status, My_Database.Success);
         end if;
      end loop;

      -- Update, some values should not change.
      Cnt := 0;
      for Id in Start_Id .. Stop_Id loop
         Value.Length := Cnt;
         Value.Buffer := [others => Cnt + 7];
         Cnt := @ + 1;
         Put_Line ("inserting " & Natural'Image (Natural (Id)) & " => " & Simple_Variable.Representation.To_Tuple_String (Value));
         U_Status := Self.Db.Update (Id, Value);
         Update_Status_Assert.Eq (U_Status, My_Database.Success);
      end loop;

      -- Fetch items from database and check values:
      Cnt := 0;
      for Id in Start_Id .. Stop_Id loop
         Value := (0, [others => 0]);
         F_Status := Self.Db.Fetch (Id, Value);
         Fetch_Status_Assert.Eq (F_Status, My_Database.Success);
         if Id mod 2 = 0 then
            Simple_Variable_Assert.Eq (Value, (Cnt, [others => Cnt + 7]));
         else
            Simple_Variable_Assert.Eq (Value, (Cnt, [others => Cnt]));
         end if;
         Put_Line ("fetching " & Natural'Image (Natural (Id)) & " => " & Simple_Variable.Representation.To_Tuple_String (Value));
         Cnt := @ + 1;
         pragma Assert (Self.Db.Any_Overridden = True);
      end loop;

      -- Clear all override
      Self.Db.Clear_Override_All;
      pragma Assert (Self.Db.Any_Overridden = False);

      -- Update, all values should not change.
      Cnt := 0;
      for Id in Start_Id .. Stop_Id loop
         Value.Length := Cnt;
         Value.Buffer := [others => Cnt + 10];
         Cnt := @ + 1;
         Put_Line ("inserting " & Natural'Image (Natural (Id)) & " => " & Simple_Variable.Representation.To_Tuple_String (Value));
         U_Status := Self.Db.Update (Id, Value);
         Update_Status_Assert.Eq (U_Status, My_Database.Success);
      end loop;

      -- Fetch items from database and check values:
      Cnt := 0;
      for Id in Start_Id .. Stop_Id loop
         Value := (0, [others => 0]);
         F_Status := Self.Db.Fetch (Id, Value);
         Fetch_Status_Assert.Eq (F_Status, My_Database.Success);
         Simple_Variable_Assert.Eq (Value, (Cnt, [others => Cnt + 10]));
         Put_Line ("fetching " & Natural'Image (Natural (Id)) & " => " & Simple_Variable.Representation.To_Tuple_String (Value));
         Cnt := @ + 1;
         pragma Assert (Self.Db.Any_Overridden = False);
      end loop;
   end Test_Override;

end Variable_Database_Tests.Implementation;
