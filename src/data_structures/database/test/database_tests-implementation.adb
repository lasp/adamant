--------------------------------------------------------------------------------
-- Database Tests Body
--------------------------------------------------------------------------------

with Smart_Assert;
with Ada.Text_IO; use Ada.Text_IO;
with Basic_Assertions; use Basic_Assertions;

package body Database_Tests.Implementation is

   -------------------------------------------------------------------------
   -- Global definitions:
   -------------------------------------------------------------------------
   Database_Size : constant Natural := 10; -- bytes
   Start_Id : constant Id_Type := Id_Type (Id_Range'First);
   Stop_Id : constant Id_Type := Id_Type ((Natural (Id_Range'First) + Database_Size - 1));

   -------------------------------------------------------------------------
   -- Fixtures:
   -------------------------------------------------------------------------

   overriding procedure Set_Up_Test (Self : in out Instance) is
   begin
      -- Initialize the database:
      Self.Db.Init (Database_Size, Id_Type (Id_Range'First), Id_Type (Id_Range'Last));
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

   -------------------------------------------------------------------------
   -- Tests:
   -------------------------------------------------------------------------

   overriding procedure Test_Nominal_Update_Fetch (Self : in out Instance) is
      Cnt : Positive := 1;
      Value : Natural;
      U_Status : My_Database.Update_Status;
      F_Status : My_Database.Fetch_Status;
   begin
      -- Make sure database is empty:
      Boolean_Assert.Eq (Self.Db.Is_Empty, True);
      Boolean_Assert.Eq (Self.Db.Is_Full, False);
      Natural_Assert.Eq (Self.Db.Get_Count, 0);

      -- Add items to database:
      for Id in Start_Id .. Stop_Id loop
         Put_Line ("inserting " & Natural'Image (Natural (Id)) & " => " & Natural'Image (Natural (Id) + 60));
         U_Status := Self.Db.Update (Id, Natural (Id) + 60);
         Update_Status_Assert.Eq (U_Status, My_Database.Success);
         Natural_Assert.Eq (Self.Db.Get_Count, Cnt);
         Cnt := @ + 1;
         Boolean_Assert.Eq (Self.Db.Is_Empty, False);
         if Id /= Id_Type ((Natural (Id_Range'First) + Database_Size - 1)) then
            Boolean_Assert.Eq (Self.Db.Is_Full, False);
         end if;
      end loop;

      -- Make sure database is full:
      Boolean_Assert.Eq (Self.Db.Is_Empty, False);
      Boolean_Assert.Eq (Self.Db.Is_Full, True);
      Natural_Assert.Eq (Self.Db.Get_Count, Database_Size);

      -- Fetch items from database and check values:
      for Id in Start_Id .. Stop_Id loop
         F_Status := Self.Db.Fetch (Id, Value);
         Fetch_Status_Assert.Eq (F_Status, My_Database.Success);
         Natural_Assert.Eq (Value, Natural (Id) + 60);
         Put_Line ("fetching " & Natural'Image (Natural (Id)) & " => " & Natural'Image (Value));
      end loop;

      -- Update items in database to new values:
      for Id in Start_Id .. Stop_Id loop
         Put_Line ("inserting " & Natural'Image (Natural (Id)) & " => " & Natural'Image (Natural (Id) + 60));
         U_Status := Self.Db.Update (Id, Natural (Id) + 199);
         Update_Status_Assert.Eq (U_Status, My_Database.Success);
      end loop;

      -- Fetch items from database and check values:
      for Id in Start_Id .. Stop_Id loop
         F_Status := Self.Db.Fetch (Id, Value);
         Fetch_Status_Assert.Eq (F_Status, My_Database.Success);
         Natural_Assert.Eq (Value, Natural (Id) + 199);
         Put_Line ("fetching " & Natural'Image (Natural (Id)) & " => " & Natural'Image (Value));
      end loop;

      -- Make sure database is still full:
      Boolean_Assert.Eq (Self.Db.Is_Empty, False);
      Boolean_Assert.Eq (Self.Db.Is_Full, True);
      Natural_Assert.Eq (Self.Db.Get_Count, Database_Size);
   end Test_Nominal_Update_Fetch;

   overriding procedure Test_Id_Out_Of_Range (Self : in out Instance) is
      Value : Natural := 22;
      U_Status : My_Database.Update_Status;
      F_Status : My_Database.Fetch_Status;
   begin
      -- Update with bad id:
      U_Status := Self.Db.Update (Id_Type'Last, 17);
      Update_Status_Assert.Eq (U_Status, My_Database.Id_Out_Of_Range);
      Natural_Assert.Eq (Self.Db.Get_Count, 0);
      U_Status := Self.Db.Update (Id_Type'First, 18);
      Update_Status_Assert.Eq (U_Status, My_Database.Id_Out_Of_Range);
      Natural_Assert.Eq (Self.Db.Get_Count, 0);

      -- Update with good id:
      U_Status := Self.Db.Update (Start_Id, 19);
      Update_Status_Assert.Eq (U_Status, My_Database.Success);
      Natural_Assert.Eq (Self.Db.Get_Count, 1);

      -- Fetch with bad id:
      F_Status := Self.Db.Fetch (Id_Type'Last, Value);
      Fetch_Status_Assert.Eq (F_Status, My_Database.Id_Out_Of_Range);
      F_Status := Self.Db.Fetch (Id_Type'First, Value);
      Fetch_Status_Assert.Eq (F_Status, My_Database.Id_Out_Of_Range);

      -- Fetch with good id:
      F_Status := Self.Db.Fetch (Start_Id, Value);
      Fetch_Status_Assert.Eq (F_Status, My_Database.Success);
      Natural_Assert.Eq (Value, 19);
   end Test_Id_Out_Of_Range;

   overriding procedure Test_Not_Enough_Memory (Self : in out Instance) is
      U_Status : My_Database.Update_Status;
   begin
      -- Make sure database is empty:
      Boolean_Assert.Eq (Self.Db.Is_Empty, True);
      Boolean_Assert.Eq (Self.Db.Is_Full, False);
      Natural_Assert.Eq (Self.Db.Get_Count, 0);

      -- Fill up database:
      for Id in Start_Id .. Stop_Id loop
         U_Status := Self.Db.Update (Id, Natural (Id) + 60);
         Update_Status_Assert.Eq (U_Status, My_Database.Success);
      end loop;

      -- Make sure database is full:
      Boolean_Assert.Eq (Self.Db.Is_Empty, False);
      Boolean_Assert.Eq (Self.Db.Is_Full, True);
      Natural_Assert.Eq (Self.Db.Get_Count, Database_Size);

      -- Update new expect out of memory:
      U_Status := Self.Db.Update (Id_Type (Id_Range'Last), 19);
      Update_Status_Assert.Eq (U_Status, My_Database.Not_Enough_Memory);
      U_Status := Self.Db.Update (Id_Type (Id_Range'Last), 19);
      Update_Status_Assert.Eq (U_Status, My_Database.Not_Enough_Memory);
      U_Status := Self.Db.Update (Id_Type (Id_Range'Last), 19);
      Update_Status_Assert.Eq (U_Status, My_Database.Not_Enough_Memory);

      -- Update old, expect success:
      U_Status := Self.Db.Update (Start_Id, 19);
      Update_Status_Assert.Eq (U_Status, My_Database.Success);
      U_Status := Self.Db.Update (Start_Id, 19);
      Update_Status_Assert.Eq (U_Status, My_Database.Success);
      U_Status := Self.Db.Update (Start_Id, 19);
      Update_Status_Assert.Eq (U_Status, My_Database.Success);

      -- Update new expect out of memory:
      U_Status := Self.Db.Update (Id_Type (Id_Range'Last), 19);
      Update_Status_Assert.Eq (U_Status, My_Database.Not_Enough_Memory);
      U_Status := Self.Db.Update (Id_Type (Id_Range'Last), 19);
      Update_Status_Assert.Eq (U_Status, My_Database.Not_Enough_Memory);
      U_Status := Self.Db.Update (Id_Type (Id_Range'Last), 19);
      Update_Status_Assert.Eq (U_Status, My_Database.Not_Enough_Memory);

      -- Make sure database is full:
      Boolean_Assert.Eq (Self.Db.Is_Empty, False);
      Boolean_Assert.Eq (Self.Db.Is_Full, True);
      Natural_Assert.Eq (Self.Db.Get_Count, Database_Size);
   end Test_Not_Enough_Memory;

   overriding procedure Test_Data_Not_Available (Self : in out Instance) is
      Value : Natural;
      U_Status : My_Database.Update_Status;
      F_Status : My_Database.Fetch_Status;
   begin
      -- Retrieve value that doesn't exist yet:
      F_Status := Self.Db.Fetch (Start_Id, Value);
      Fetch_Status_Assert.Eq (F_Status, My_Database.Data_Not_Available);
      F_Status := Self.Db.Fetch (Start_Id, Value);
      Fetch_Status_Assert.Eq (F_Status, My_Database.Data_Not_Available);

      -- Update value:
      U_Status := Self.Db.Update (Start_Id, 19);
      Update_Status_Assert.Eq (U_Status, My_Database.Success);
      F_Status := Self.Db.Fetch (Start_Id, Value);
      Fetch_Status_Assert.Eq (F_Status, My_Database.Success);
      Natural_Assert.Eq (Value, 19);

      -- Retrieve value that doesn't exist yet:
      F_Status := Self.Db.Fetch (Stop_Id, Value);
      Fetch_Status_Assert.Eq (F_Status, My_Database.Data_Not_Available);
      F_Status := Self.Db.Fetch (Stop_Id, Value);
      Fetch_Status_Assert.Eq (F_Status, My_Database.Data_Not_Available);

      -- Update value:
      U_Status := Self.Db.Update (Stop_Id, 22);
      Update_Status_Assert.Eq (U_Status, My_Database.Success);
      F_Status := Self.Db.Fetch (Stop_Id, Value);
      Fetch_Status_Assert.Eq (F_Status, My_Database.Success);
      Natural_Assert.Eq (Value, 22);
   end Test_Data_Not_Available;

end Database_Tests.Implementation;
