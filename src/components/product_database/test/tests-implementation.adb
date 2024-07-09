--------------------------------------------------------------------------------
-- Product_Database Tests Body
--------------------------------------------------------------------------------

with Data_Product_Types;
with Data_Product_Return;
with Basic_Types;
with Basic_Assertions; use Basic_Assertions;
with Data_Product_Id.Assertion; use Data_Product_Id.Assertion;
with Data_Product.Assertion; use Data_Product.Assertion;
with Data_Product_Header.Assertion; use Data_Product_Header.Assertion;
with Data_Product_Return.Assertion; use Data_Product_Return.Assertion;
with Data_Product_Enums; use Data_Product_Enums;
with Command_Response.Assertion; use Command_Response.Assertion;
with Command_Enums; use Command_Enums.Command_Response_Status;
with Invalid_Command_Info.Assertion; use Invalid_Command_Info.Assertion;
with Command;
with Serializer_Types;
with Packed_Enable_Disable_Type.Assertion; use Packed_Enable_Disable_Type.Assertion;
with Interfaces; use Interfaces;
with Data_Product_Poly_Extract.Assertion; use Data_Product_Poly_Extract.Assertion;
with Data_Product_Poly_Event.Assertion; use Data_Product_Poly_Event.Assertion;
with Data_Product_Poly_Type.Assertion; use Data_Product_Poly_Type.Assertion;
with Basic_Enums; use Basic_Enums.Enable_Disable_Type;

package body Tests.Implementation is

   -------------------------------------------------------------------------
   -- Global definitions:
   -------------------------------------------------------------------------
   Min_Id : constant Data_Product_Types.Data_Product_Id := 17;
   Max_Id : constant Data_Product_Types.Data_Product_Id := 26;
   Max_Max_Id : constant Data_Product_Types.Data_Product_Id := 27;

   -------------------------------------------------------------------------
   -- Fixtures:
   -------------------------------------------------------------------------

   overriding procedure Set_Up_Test (Self : in out Instance) is
   begin
      -- Allocate heap memory to component:
      Self.Tester.Init_Base;

      -- Initialize the buffer manager component:
      Self.Tester.Component_Instance.Init (Minimum_Data_Product_Id => Min_Id, Maximum_Data_Product_Id => Max_Max_Id);

      -- Make necessary connections between tester and component:
      Self.Tester.Connect;
   end Set_Up_Test;

   overriding procedure Tear_Down_Test (Self : in out Instance) is
   begin
      -- Finalize the component:
      Self.Tester.Component_Instance.Final;

      -- Free component heap:
      Self.Tester.Final_Base;
   end Tear_Down_Test;

   -------------------------------------------------------------------------
   -- Tests:
   -------------------------------------------------------------------------

   overriding procedure Test_Nominal_Scenario (Self : in out Instance) is
      T : Component.Product_Database.Implementation.Tester.Instance_Access renames Self.Tester;
      D_Prod : Data_Product.T := (Header => (Time => (1, 17), Id => 1, Buffer_Length => 4), Buffer => [others => 3]);
      D_Prod_Return : Data_Product_Return.T;
   begin
      -- Store some data:
      for Id in Min_Id .. Max_Id loop
         D_Prod.Header.Id := Id;
         D_Prod.Buffer := [others => Basic_Types.Byte (Id)];
         T.Data_Product_T_Send (D_Prod);
      end loop;

      -- Make sure no events were thrown:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 0);

      -- Fetch some data:
      for Id in Min_Id .. Max_Id loop
         D_Prod.Header.Id := Id;
         D_Prod.Buffer := [others => Basic_Types.Byte (Id)];
         D_Prod_Return := T.Data_Product_Fetch_T_Request ((Id => Id));
         -- Check return data:
         The_Status_Assert.Eq (D_Prod_Return.The_Status, Fetch_Status.Success);
         Data_Product_Assert.Eq (D_Prod_Return.The_Data_Product, D_Prod);
      end loop;

      -- Make sure no events were thrown:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 0);
   end Test_Nominal_Scenario;

   overriding procedure Test_Nominal_Override (Self : in out Instance) is
      use Serializer_Types;
      use Data_Product_Types;
      T : Component.Product_Database.Implementation.Tester.Instance_Access renames Self.Tester;
      Cmd : Command.T;
      D_Prod_Return : Data_Product_Return.T;
      D_Prod : Data_Product.T := (Header => (Time => (1, 17), Id => Min_Id, Buffer_Length => 4), Buffer => [others => 3]);
   begin
      -- Setup the component:
      Self.Tester.Component_Instance.Set_Up;

      -- Check data product:
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Database_Override_History.Get_Count, 1);
      Packed_Enable_Disable_Type_Assert.Eq (T.Database_Override_History.Get (1), (State => Disabled));

      ---------------------------------------
      -- Simple single data product test:
      ---------------------------------------

      -- Send command to override a data product:
      pragma Assert (T.Commands.Override ((
         Header => (
            Time => (1, 13),
            Id => Min_Id,
            Buffer_Length => 9
         ),
         Buffer => [
            others => 99
         ]), Cmd) = Success);
      T.Command_T_Send (Cmd);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 1);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (1), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Override_Id, Status => Success));

      -- Check event.
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Data_Product_Overridden_History.Get_Count, 1);
      Data_Product_Header_Assert.Eq (T.Data_Product_Overridden_History.Get (1), (
         Time => (1, 13),
         Id => Min_Id,
         Buffer_Length => 9
      ));

      -- Check data product:
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Database_Override_History.Get_Count, 2);
      Packed_Enable_Disable_Type_Assert.Eq (T.Database_Override_History.Get (2), (State => Enabled));

      -- Fetch and check the overridden data product:
      D_Prod_Return := T.Data_Product_Fetch_T_Request ((Id => Min_Id));
      The_Status_Assert.Eq (D_Prod_Return.The_Status, Fetch_Status.Success);
      Data_Product_Assert.Eq (D_Prod_Return.The_Data_Product, (
         Header => (
            Time => (1, 13),
            Id => Min_Id,
            Buffer_Length => 9
         ),
         Buffer => [
            others => 99
         ]
      ));

      -- Update the overridden data product, expect no change in fetch.
      T.Data_Product_T_Send (D_Prod);

      -- Fetch and check the overridden data product:
      D_Prod_Return := T.Data_Product_Fetch_T_Request ((Id => Min_Id));
      The_Status_Assert.Eq (D_Prod_Return.The_Status, Fetch_Status.Success);
      Data_Product_Assert.Eq (D_Prod_Return.The_Data_Product, (
         Header => (
            Time => (1, 13),
            Id => Min_Id,
            Buffer_Length => 9
         ),
         Buffer => [
            others => 99
         ]
      ));

      -- Clear the override status for this ID.
      T.Command_T_Send (T.Commands.Clear_Override ((Id => Min_Id)));
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 2);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (2), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Clear_Override_Id, Status => Success));

      -- Check event.
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Override_Cleared_History.Get_Count, 1);
      Data_Product_Id_Assert.Eq (T.Override_Cleared_History.Get (1), (Id => Min_Id));

      -- Check data product:
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 3);
      Natural_Assert.Eq (T.Database_Override_History.Get_Count, 3);
      Packed_Enable_Disable_Type_Assert.Eq (T.Database_Override_History.Get (3), (State => Disabled));

      -- Update the overridden data product, expect change in fetch.
      T.Data_Product_T_Send (D_Prod);

      -- Fetch and check the overridden data product:
      D_Prod_Return := T.Data_Product_Fetch_T_Request ((Id => Min_Id));
      The_Status_Assert.Eq (D_Prod_Return.The_Status, Fetch_Status.Success);
      Data_Product_Assert.Eq (D_Prod_Return.The_Data_Product, D_Prod);

      ---------------------------------------
      -- Multi data product test:
      ---------------------------------------

      -- Send command to override a data product:
      pragma Assert (T.Commands.Override ((
         Header => (
            Time => (1, 13),
            Id => Min_Id + 1,
            Buffer_Length => 12
         ),
         Buffer => [
            others => 88
         ]), Cmd) = Success);
      T.Command_T_Send (Cmd);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 3);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (3), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Override_Id, Status => Success));

      -- Check event.
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 3);
      Natural_Assert.Eq (T.Data_Product_Overridden_History.Get_Count, 2);
      Data_Product_Header_Assert.Eq (T.Data_Product_Overridden_History.Get (2), (
         Time => (1, 13),
         Id => Min_Id + 1,
         Buffer_Length => 12
      ));

      -- Check data product:
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 4);
      Natural_Assert.Eq (T.Database_Override_History.Get_Count, 4);
      Packed_Enable_Disable_Type_Assert.Eq (T.Database_Override_History.Get (4), (State => Enabled));

      -- Fetch and check the overridden data product:
      D_Prod_Return := T.Data_Product_Fetch_T_Request ((Id => Min_Id + 1));
      The_Status_Assert.Eq (D_Prod_Return.The_Status, Fetch_Status.Success);
      Data_Product_Assert.Eq (D_Prod_Return.The_Data_Product, (
         Header => (
            Time => (1, 13),
            Id => Min_Id + 1,
            Buffer_Length => 12
         ),
         Buffer => [
            others => 88
         ]
      ));

      -- Send command to override another data product:
      pragma Assert (T.Commands.Override ((
         Header => (
            Time => (1, 13),
            Id => Min_Id + 2,
            Buffer_Length => 11
         ),
         Buffer => [
            others => 77
         ]), Cmd) = Success);
      T.Command_T_Send (Cmd);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 4);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (4), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Override_Id, Status => Success));

      -- Check event.
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 4);
      Natural_Assert.Eq (T.Data_Product_Overridden_History.Get_Count, 3);
      Data_Product_Header_Assert.Eq (T.Data_Product_Overridden_History.Get (3), (
         Time => (1, 13),
         Id => Min_Id + 2,
         Buffer_Length => 11
      ));

      -- Check data product:
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 5);
      Natural_Assert.Eq (T.Database_Override_History.Get_Count, 5);
      Packed_Enable_Disable_Type_Assert.Eq (T.Database_Override_History.Get (5), (State => Enabled));

      -- Fetch and check the overridden data product:
      D_Prod_Return := T.Data_Product_Fetch_T_Request ((Id => Min_Id + 2));
      The_Status_Assert.Eq (D_Prod_Return.The_Status, Fetch_Status.Success);
      Data_Product_Assert.Eq (D_Prod_Return.The_Data_Product, (
         Header => (
            Time => (1, 13),
            Id => Min_Id + 2,
            Buffer_Length => 11
         ),
         Buffer => [
            others => 77
         ]
      ));

      -- Update three data products (last two which are overridden):
      D_Prod.Header.Id := Min_Id;
      T.Data_Product_T_Send (D_Prod);
      D_Prod.Header.Id := Min_Id + 1;
      T.Data_Product_T_Send (D_Prod);
      D_Prod.Header.Id := Min_Id + 2;
      T.Data_Product_T_Send (D_Prod);

      -- Fetch and check the NOT overridden data product:
      D_Prod_Return := T.Data_Product_Fetch_T_Request ((Id => Min_Id));
      The_Status_Assert.Eq (D_Prod_Return.The_Status, Fetch_Status.Success);
      D_Prod.Header.Id := Min_Id;
      Data_Product_Assert.Eq (D_Prod_Return.The_Data_Product, D_Prod);

      -- Fetch and check the overridden data product:
      D_Prod_Return := T.Data_Product_Fetch_T_Request ((Id => Min_Id + 1));
      The_Status_Assert.Eq (D_Prod_Return.The_Status, Fetch_Status.Success);
      Data_Product_Assert.Eq (D_Prod_Return.The_Data_Product, (
         Header => (
            Time => (1, 13),
            Id => Min_Id + 1,
            Buffer_Length => 12
         ),
         Buffer => [
            others => 88
         ]
      ));

      -- Fetch and check the overridden data product:
      D_Prod_Return := T.Data_Product_Fetch_T_Request ((Id => Min_Id + 2));
      The_Status_Assert.Eq (D_Prod_Return.The_Status, Fetch_Status.Success);
      Data_Product_Assert.Eq (D_Prod_Return.The_Data_Product, (
         Header => (
            Time => (1, 13),
            Id => Min_Id + 2,
            Buffer_Length => 11
         ),
         Buffer => [
            others => 77
         ]
      ));

      -- Clear the override condition for all.
      T.Command_T_Send (T.Commands.Clear_Override_For_All);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 5);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (5), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Clear_Override_For_All_Id, Status => Success));

      -- Check event:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 5);
      Natural_Assert.Eq (T.Override_Cleared_For_All_History.Get_Count, 1);

      -- Check data product:
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 6);
      Natural_Assert.Eq (T.Database_Override_History.Get_Count, 6);
      Packed_Enable_Disable_Type_Assert.Eq (T.Database_Override_History.Get (6), (State => Disabled));

      -- Update three data products (last two which are overridden):
      D_Prod.Header.Id := Min_Id;
      T.Data_Product_T_Send (D_Prod);
      D_Prod.Header.Id := Min_Id + 1;
      T.Data_Product_T_Send (D_Prod);
      D_Prod.Header.Id := Min_Id + 2;
      T.Data_Product_T_Send (D_Prod);

      -- Fetch and check the NOT overridden data products:
      D_Prod_Return := T.Data_Product_Fetch_T_Request ((Id => Min_Id));
      The_Status_Assert.Eq (D_Prod_Return.The_Status, Fetch_Status.Success);
      D_Prod.Header.Id := Min_Id;
      Data_Product_Assert.Eq (D_Prod_Return.The_Data_Product, D_Prod);

      D_Prod_Return := T.Data_Product_Fetch_T_Request ((Id => Min_Id + 1));
      The_Status_Assert.Eq (D_Prod_Return.The_Status, Fetch_Status.Success);
      D_Prod.Header.Id := Min_Id + 1;
      Data_Product_Assert.Eq (D_Prod_Return.The_Data_Product, D_Prod);

      D_Prod_Return := T.Data_Product_Fetch_T_Request ((Id => Min_Id + 2));
      The_Status_Assert.Eq (D_Prod_Return.The_Status, Fetch_Status.Success);
      D_Prod.Header.Id := Min_Id + 2;
      Data_Product_Assert.Eq (D_Prod_Return.The_Data_Product, D_Prod);

      -- Check no more data products:
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 6);
   end Test_Nominal_Override;

   overriding procedure Test_Nominal_Dump (Self : in out Instance) is
      T : Component.Product_Database.Implementation.Tester.Instance_Access renames Self.Tester;
      D_Prod : Data_Product.T := (Header => (Time => (1, 17), Id => 1, Buffer_Length => 4), Buffer => [others => 3]);
      Cnt : Natural := 1;
   begin
      -- Store some data:
      for Id in Min_Id .. Max_Id loop
         D_Prod.Header.Id := Id;
         D_Prod.Buffer := [others => Basic_Types.Byte (Id)];
         T.Data_Product_T_Send (D_Prod);
      end loop;

      -- Make sure no events were thrown:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 0);

      -- Dump some data:
      for Id in Min_Id .. Max_Id loop
         D_Prod.Header.Id := Id;
         D_Prod.Buffer := [others => Basic_Types.Byte (Id)];

         -- Dump data product:
         T.Command_T_Send (T.Commands.Dump ((Id => Id)));
         Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, Cnt);
         Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (Cnt), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Dump_Id, Status => Success));

         -- Make sure events were thrown:
         Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, Cnt);
         Natural_Assert.Eq (T.Data_Product_Dumped_History.Get_Count, Cnt);
         Data_Product_Header_Assert.Eq (T.Data_Product_Dumped_History.Get (Cnt), D_Prod.Header);

         -- Check packet:
         Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, Cnt);
         Natural_Assert.Eq (T.Dump_Packet_History.Get_Count, Cnt);
         Data_Product_Assert.Eq (T.Dump_Packet_History.Get (Cnt), D_Prod);
         Cnt := Cnt + 1;
      end loop;
   end Test_Nominal_Dump;

   overriding procedure Test_Nominal_Dump_Poly (Self : in out Instance) is
      T : Component.Product_Database.Implementation.Tester.Instance_Access renames Self.Tester;
      D_Prod : Data_Product.T := (Header => (Time => (1, 17), Id => 1, Buffer_Length => 4), Buffer => [others => 3]);
      Cnt : Natural := 1;
   begin
      -- Store some data:
      for Id in Min_Id .. Max_Id loop
         D_Prod.Header.Id := Id;
         D_Prod.Buffer := [others => Basic_Types.Byte (Id)];
         T.Data_Product_T_Send (D_Prod);
      end loop;

      -- Make sure no events were thrown:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 0);

      -- Dump some data:
      for Id in Min_Id .. Max_Id loop
         D_Prod.Header.Id := Id;
         D_Prod.Buffer := [others => Basic_Types.Byte (Id)];

         -- Dump data product:
         T.Command_T_Send (T.Commands.Dump_Poly_Type ((Id => Id, Offset => 8, Size => 8)));
         Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, Cnt);
         Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (Cnt), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Dump_Poly_Type_Id, Status => Success));

         -- Make sure events were thrown:
         Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, Cnt * 2);
         Natural_Assert.Eq (T.Dumping_Data_Product_Poly_Type_History.Get_Count, Cnt);
         Data_Product_Poly_Extract_Assert.Eq (T.Dumping_Data_Product_Poly_Type_History.Get (Cnt), (Id => Id, Offset => 8, Size => 8));
         Natural_Assert.Eq (T.Dumped_Data_Product_Poly_Type_History.Get_Count, Cnt);
         Data_Product_Poly_Event_Assert.Eq (T.Dumped_Data_Product_Poly_Type_History.Get (Cnt), (Header => D_Prod.Header, Data => [0, 0, 0, Basic_Types.Byte (Id)]));

         -- Check data product:
         Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, Cnt);
         Natural_Assert.Eq (T.Data_Product_Poly_Type_Dump_History.Get_Count, Cnt);
         Data_Product_Poly_Type_Assert.Eq (T.Data_Product_Poly_Type_Dump_History.Get (Cnt), (Time => D_Prod.Header.Time, Id => Id, Data => [0, 0, 0, Basic_Types.Byte (Id)]));
         Cnt := Cnt + 1;
      end loop;

      -- Induce an extraction failure.
      T.Command_T_Send (T.Commands.Dump_Poly_Type ((Id => Min_Id, Offset => 31, Size => 2)));
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 11);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (11), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Dump_Poly_Type_Id, Status => Failure));

      -- Make sure events were thrown:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 22);
      Natural_Assert.Eq (T.Dumping_Data_Product_Poly_Type_History.Get_Count, 11);
      Data_Product_Poly_Extract_Assert.Eq (T.Dumping_Data_Product_Poly_Type_History.Get (11), (Id => Min_Id, Offset => 31, Size => 2));
      Natural_Assert.Eq (T.Data_Product_Poly_Type_Extraction_Failed_History.Get_Count, 1);
      D_Prod.Header.Id := Min_Id;
      Data_Product_Header_Assert.Eq (T.Data_Product_Poly_Type_Extraction_Failed_History.Get (1), D_Prod.Header);
   end Test_Nominal_Dump_Poly;

   overriding procedure Test_Data_Not_Available (Self : in out Instance) is
      T : Component.Product_Database.Implementation.Tester.Instance_Access renames Self.Tester;
      D_Prod : Data_Product.T := (Header => (Time => (1, 17), Id => 1, Buffer_Length => 4), Buffer => [others => 3]);
      D_Prod_Return : Data_Product_Return.T;
      Cnt : Natural := 1;
   begin
      -- Make sure no events were thrown:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 0);

      -- Fetch some data, expect error:
      for Id in Min_Id .. Max_Id loop
         D_Prod.Header.Id := Id;
         D_Prod.Buffer := [others => Basic_Types.Byte (Id)];
         D_Prod_Return := T.Data_Product_Fetch_T_Request ((Id => Id));
         -- Check return data:
         The_Status_Assert.Eq (D_Prod_Return.The_Status, Fetch_Status.Not_Available);
         -- Make sure events were thrown:
         Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, Cnt);
         Natural_Assert.Eq (T.Data_Product_Fetch_Id_Not_Available_History.Get_Count, Cnt);
         Data_Product_Id_Assert.Eq (T.Data_Product_Fetch_Id_Not_Available_History.Get (Cnt), (Id => Id));
         Cnt := Cnt + 1;
      end loop;

      -- Make sure events were thrown:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 10);

      -- Try to dump some data products that are not available:
      Cnt := 1;
      for Id in Min_Id .. Max_Id loop
         T.Command_T_Send (T.Commands.Dump ((Id => Id)));
         Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, Cnt);
         Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (Cnt), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Dump_Id, Status => Failure));

         -- Make sure events were thrown:
         Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, Cnt + 10);
         Natural_Assert.Eq (T.Data_Product_Dump_Id_Not_Available_History.Get_Count, Cnt);
         Data_Product_Id_Assert.Eq (T.Data_Product_Dump_Id_Not_Available_History.Get (Cnt), (Id => Id));
         Cnt := Cnt + 1;
      end loop;

      -- Try to dump some data products poly types that are not available:
      Cnt := 1;
      for Id in Min_Id .. Max_Id loop
         T.Command_T_Send (T.Commands.Dump_Poly_Type ((Id => Id, Offset => 0, Size => 1)));
         Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, Cnt + 10);
         Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (Cnt + 10), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Dump_Poly_Type_Id, Status => Failure));

         -- Make sure events were thrown:
         Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, Cnt * 2 + 20);
         -- Poly info event ignored here, checked in nominal test.
         Natural_Assert.Eq (T.Data_Product_Dump_Poly_Id_Not_Available_History.Get_Count, Cnt);
         Data_Product_Id_Assert.Eq (T.Data_Product_Dump_Poly_Id_Not_Available_History.Get (Cnt), (Id => Id));
         Cnt := Cnt + 1;
      end loop;

      -- Store some data:
      for Id in Min_Id .. Max_Id loop
         D_Prod.Header.Id := Id;
         D_Prod.Buffer := [others => Basic_Types.Byte (Id)];
         T.Data_Product_T_Send (D_Prod);
      end loop;

      -- Make sure no events were thrown:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 40);

      -- Fetch some data and expect to get valid data:
      for Id in Min_Id .. Max_Id loop
         D_Prod.Header.Id := Id;
         D_Prod.Buffer := [others => Basic_Types.Byte (Id)];
         D_Prod_Return := T.Data_Product_Fetch_T_Request ((Id => Id));
         -- Check return data:
         The_Status_Assert.Eq (D_Prod_Return.The_Status, Fetch_Status.Success);
         Data_Product_Assert.Eq (D_Prod_Return.The_Data_Product, D_Prod);
      end loop;

      -- Make sure no events were thrown:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 40);
   end Test_Data_Not_Available;

   overriding procedure Test_Id_Out_Of_Range (Self : in out Instance) is
      use Data_Product_Types;
      use Serializer_Types;
      T : Component.Product_Database.Implementation.Tester.Instance_Access renames Self.Tester;
      D_Prod : Data_Product.T := (Header => (Time => (1, 17), Id => 1, Buffer_Length => 4), Buffer => [others => 3]);
      D_Prod_Return : Data_Product_Return.T;
      Cmd : Command.T;
   begin
      -- Try to store data with bad id:
      D_Prod.Header.Id := Max_Max_Id + Data_Product_Types.Data_Product_Id (1);
      D_Prod.Buffer := [others => Basic_Types.Byte (2)];
      T.Data_Product_T_Send (D_Prod);
      T.Data_Product_T_Send (D_Prod);
      T.Data_Product_T_Send (D_Prod);

      -- Expect 3 events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 3);
      Natural_Assert.Eq (T.Data_Product_Update_Id_Out_Of_Range_History.Get_Count, 3);
      Data_Product_Id_Assert.Eq (T.Data_Product_Update_Id_Out_Of_Range_History.Get (1), (Id => D_Prod.Header.Id));
      Data_Product_Id_Assert.Eq (T.Data_Product_Update_Id_Out_Of_Range_History.Get (2), (Id => D_Prod.Header.Id));
      Data_Product_Id_Assert.Eq (T.Data_Product_Update_Id_Out_Of_Range_History.Get (3), (Id => D_Prod.Header.Id));

      -- Try to fetch data with bad id:
      D_Prod_Return := T.Data_Product_Fetch_T_Request ((Id => D_Prod.Header.Id));
      The_Status_Assert.Eq (D_Prod_Return.The_Status, Fetch_Status.Id_Out_Of_Range);
      D_Prod_Return := T.Data_Product_Fetch_T_Request ((Id => D_Prod.Header.Id));
      The_Status_Assert.Eq (D_Prod_Return.The_Status, Fetch_Status.Id_Out_Of_Range);
      D_Prod_Return := T.Data_Product_Fetch_T_Request ((Id => D_Prod.Header.Id));
      The_Status_Assert.Eq (D_Prod_Return.The_Status, Fetch_Status.Id_Out_Of_Range);

      -- Expect 3 more events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 6);
      Natural_Assert.Eq (T.Data_Product_Fetch_Id_Out_Of_Range_History.Get_Count, 3);
      Data_Product_Id_Assert.Eq (T.Data_Product_Fetch_Id_Out_Of_Range_History.Get (1), (Id => D_Prod.Header.Id));
      Data_Product_Id_Assert.Eq (T.Data_Product_Fetch_Id_Out_Of_Range_History.Get (2), (Id => D_Prod.Header.Id));
      Data_Product_Id_Assert.Eq (T.Data_Product_Fetch_Id_Out_Of_Range_History.Get (3), (Id => D_Prod.Header.Id));

      -- Try to dump data product with bad id:
      T.Command_T_Send (T.Commands.Dump ((Id => 1)));
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 1);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (1), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Dump_Id, Status => Failure));
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 7);
      Natural_Assert.Eq (T.Data_Product_Dump_Id_Out_Of_Range_History.Get_Count, 1);
      Data_Product_Id_Assert.Eq (T.Data_Product_Dump_Id_Out_Of_Range_History.Get (1), (Id => 1));

      -- Try to dump data product poly type with bad id:
      T.Command_T_Send (T.Commands.Dump_Poly_Type ((Id => 1, Offset => 1, Size => 1)));
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 2);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (2), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Dump_Poly_Type_Id, Status => Failure));
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 9);
      -- Poly info event ignored here, checked in nominal test.
      Natural_Assert.Eq (T.Data_Product_Dump_Poly_Id_Out_Of_Range_History.Get_Count, 1);
      Data_Product_Id_Assert.Eq (T.Data_Product_Dump_Poly_Id_Out_Of_Range_History.Get (1), (Id => 1));

      -- Try to clear override data product with bad id:
      T.Command_T_Send (T.Commands.Clear_Override ((Id => 1)));
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 3);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (3), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Clear_Override_Id, Status => Failure));
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 10);
      Natural_Assert.Eq (T.Data_Product_Clear_Override_Id_Out_Of_Range_History.Get_Count, 1);
      Data_Product_Id_Assert.Eq (T.Data_Product_Clear_Override_Id_Out_Of_Range_History.Get (1), (Id => 1));

      -- Try to override data with bad id:
      D_Prod.Header.Id := 1;
      D_Prod.Buffer := [others => Basic_Types.Byte (2)];
      pragma Assert (T.Commands.Override (D_Prod, Cmd) = Success);
      T.Command_T_Send (Cmd);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 4);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (4), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Override_Id, Status => Failure));
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 11);
      Natural_Assert.Eq (T.Data_Product_Override_Id_Out_Of_Range_History.Get_Count, 1);
      Data_Product_Id_Assert.Eq (T.Data_Product_Override_Id_Out_Of_Range_History.Get (1), (Id => 1));
   end Test_Id_Out_Of_Range;

   overriding procedure Test_Invalid_Command (Self : in out Instance) is
      T : Component.Product_Database.Implementation.Tester.Instance_Access renames Self.Tester;
      Cmd : Command.T := T.Commands.Dump ((Id => 1));
   begin
      -- Make the command invalid by modifying its length.
      Cmd.Header.Arg_Buffer_Length := 0;

      -- Send bad command and expect bad response:
      T.Command_T_Send (Cmd);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 1);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (1), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Dump_Id, Status => Length_Error));

      -- Make sure some events were thrown:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Invalid_Command_Received_History.Get_Count, 1);
      Invalid_Command_Info_Assert.Eq (T.Invalid_Command_Received_History.Get (1), (Id => T.Commands.Get_Dump_Id, Errant_Field_Number => Interfaces.Unsigned_32'Last, Errant_Field => [0, 0, 0, 0, 0, 0, 0, 0]));
   end Test_Invalid_Command;

end Tests.Implementation;
