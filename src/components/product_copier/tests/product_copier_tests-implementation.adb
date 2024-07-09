--------------------------------------------------------------------------------
-- Product_Copier Tests Body
--------------------------------------------------------------------------------

with AUnit.Assertions; use AUnit.Assertions;
with Basic_Assertions; use Basic_Assertions;
with Component.Product_Copier;
with Ada.Assertions;
with Interfaces;

package body Product_Copier_Tests.Implementation is

   -------------------------------------------------------------------------
   -- Fixtures:
   -------------------------------------------------------------------------
   use Component.Product_Copier;

   Init_Products : aliased Product_Mapping_Array := [
      (Source_Id => 1, Destination_Id => 1),
      (Source_Id => 2, Destination_Id => 2),
      (Source_Id => 3, Destination_Id => 3),
      (Source_Id => 4, Destination_Id => 4),
      (Source_Id => 5, Destination_Id => 5),
      (Source_Id => 100, Destination_Id => 6)
   ];

   overriding procedure Set_Up_Test (Self : in out Instance) is
   begin
      -- Allocate heap memory to component:
      Self.Tester.Init_Base;

      -- Make necessary connections between tester and component:
      Self.Tester.Connect;

      -- TODO Call component init here.
      Self.Tester.Component_Instance.Init (Products_To_Copy => Init_Products'Access);

      -- Call the component set up method that the assembly would normally call.
      Self.Tester.Component_Instance.Set_Up;

      -- TODO Insert custom set up code here.
      null;
   end Set_Up_Test;

   overriding procedure Tear_Down_Test (Self : in out Instance) is
   begin
      -- TODO Insert custom cleanup code here.
      null;
      -- Free component heap:
      Self.Tester.Final_Base;
   end Tear_Down_Test;

   -------------------------------------------------------------------------
   -- Tests:
   -------------------------------------------------------------------------

   No_Conflict_Products_1 : aliased Product_Mapping_Array := [];
   No_Conflict_Products_2 : aliased Product_Mapping_Array := [
      (Source_Id => 1, Destination_Id => 1)
   ];
   No_Conflict_Products_3 : aliased Product_Mapping_Array := [
      (Source_Id => 2, Destination_Id => 0),
      (Source_Id => 0, Destination_Id => 1),
      (Source_Id => 0, Destination_Id => 2),
      (Source_Id => 3, Destination_Id => 3)
   ];
   No_Conflict_Products_4 : aliased Product_Mapping_Array := [
      (Source_Id => 0, Destination_Id => 0),
      (Source_Id => 0, Destination_Id => 1),
      (Source_Id => 0, Destination_Id => 2),
      (Source_Id => 0, Destination_Id => 3),
      (Source_Id => 0, Destination_Id => 4)
   ];

   Conflict_Products_1 : aliased Product_Mapping_Array := [
      (Source_Id => 0, Destination_Id => 0),
      (Source_Id => 0, Destination_Id => 1),
      (Source_Id => 0, Destination_Id => 2),
      (Source_Id => 0, Destination_Id => 0)
   ];
   Conflict_Products_2 : aliased Product_Mapping_Array := [
      (Source_Id => 1, Destination_Id => 1),
      (Source_Id => 3, Destination_Id => 1)
   ];

   -- Tests whether two conflicting destinations raise an error.
   overriding procedure Test_Dest_Conflict (Self : in out Instance) is
   begin
      -- TODO is Init'ing many times / after Set_Up_Test okay?

      -- these should not raise an assertion error
      Self.Tester.Component_Instance.Init (Products_To_Copy => No_Conflict_Products_1'Access);
      Self.Tester.Component_Instance.Init (Products_To_Copy => No_Conflict_Products_2'Access);
      Self.Tester.Component_Instance.Init (Products_To_Copy => No_Conflict_Products_3'Access);
      Self.Tester.Component_Instance.Init (Products_To_Copy => No_Conflict_Products_4'Access);

      -- these should raise an assertion error
      declare -- TODO named declare?
      begin
         Self.Tester.Component_Instance.Init (Products_To_Copy => Conflict_Products_1'Access);
         -- this should be unreachable
         Assert (False, "Conflicting destinations, but did not raise error");
      exception
         -- this is what gets raised by `pragma Assert (...)`
         when Ada.Assertions.Assertion_Error =>
            null;
      end;

      declare
      begin
         Self.Tester.Component_Instance.Init (Products_To_Copy => Conflict_Products_2'Access);
         -- this should be unreachable
         Assert (False, "Conflicting destinations, but did not raise error");
      exception
         when Ada.Assertions.Assertion_Error =>
            null;
      end;
   end Test_Dest_Conflict;

   Non_Error_Products : aliased Product_Mapping_Array := [(Source_Id => 1, Destination_Id => 1)];

   -- Tests the fetch and send operations caused by a tick.
   overriding procedure Test_Nominal_Tick (Self : in out Instance) is
      -- TODO declarations
      use Interfaces;
   begin
      -- see the reciprocal component
      -- fetching source id 1 always results in success
      Self.Tester.Component_Instance.Init (Products_To_Copy => Non_Error_Products'Access);

      Self.Tester.Tick_T_Send ((Time => (0, 0), Count => 0));

      -- data_product_fetch_t_request should have been invoked
      Natural_Assert.Eq (Self.Tester.Data_Product_Fetch_T_Service_History.Get_Count, 1);

      -- data_product_t_send should have been invoked
      Natural_Assert.Eq (Self.Tester.Data_Product_T_Recv_Sync_History.Get_Count, 1);

      -- no events should have been sent
      Natural_Assert.Eq (Self.Tester.Source_Not_Available_History.Get_Count, 0);
      Natural_Assert.Eq (Self.Tester.Source_Id_Out_Of_Range_History.Get_Count, 0);

      -- send 5 more ticks
      for I in 1 .. 5 loop
         Self.Tester.Tick_T_Send ((Time => (Unsigned_32 (I), 0), Count => Unsigned_32 (I)));
      end loop;

      -- same checks, plus 5
      Natural_Assert.Eq (Self.Tester.Data_Product_Fetch_T_Service_History.Get_Count, 6);
      Natural_Assert.Eq (Self.Tester.Data_Product_T_Recv_Sync_History.Get_Count, 6);
      Natural_Assert.Eq (Self.Tester.Source_Not_Available_History.Get_Count, 0);
      Natural_Assert.Eq (Self.Tester.Source_Id_Out_Of_Range_History.Get_Count, 0);

      -- check contents of successful copies
      declare
         Previous_Id : Integer := -1;
         Int_Id : Integer;
      begin
         for I in 1 .. Self.Tester.Data_Product_T_Recv_Sync_History.Get_Count loop
            Int_Id := Integer (
               Self.Tester.Data_Product_T_Recv_Sync_History.Get (Natural (I)).Header.Id
            );
            Assert (
               Int_Id mod 10 = 1,
               "Tick copied the incorrect Data_Product at position " & I'Image & ". (Possibly an error in the reciprocal component.)"
            );
            -- we care about the order across multiple ticks
            Assert (
               Previous_Id < Int_Id,
               "Tick copied incorrect data products -- IDs should be in ascending order per the reciprocal tester component."
            );
            Previous_Id := Int_Id;
         end loop;
      end;

   end Test_Nominal_Tick;

   -- do not change numbers/order; see body of Test_Fetch_Fail_Behavior
   -- TODO make more descriptive names, i.e. One_Failing_Product
   Fetch_Fail_Products_1 : aliased Product_Mapping_Array := [
      (Source_Id => 2, Destination_Id => 2)
   ];
   Fetch_Fail_Products_2 : aliased Product_Mapping_Array := [
      (Source_Id => 5, Destination_Id => 5),
      (Source_Id => 1, Destination_Id => 1),
      (Source_Id => 4, Destination_Id => 4),
      (Source_Id => 2, Destination_Id => 2),
      (Source_Id => 3, Destination_Id => 3)
   ];

   -- Tests that no data products are sent to the destination when a fetch fails.
   overriding procedure Test_Fetch_Fail_Behavior (Self : in out Instance) is
   begin
      -- see reciprocal component for which values of Source_Id cause errors
      Self.Tester.Component_Instance.Init (Products_To_Copy => Fetch_Fail_Products_1'Access);

      -- send tick with bad Source_Id
      Self.Tester.Tick_T_Send ((Time => (0, 0), Count => 0));
      -- fetch, but no send
      Natural_Assert.Eq (Self.Tester.Data_Product_Fetch_T_Service_History.Get_Count, 1);
      Natural_Assert.Eq (Self.Tester.Data_Product_T_Recv_Sync_History.Get_Count, 0);

      Self.Tester.Tick_T_Send ((Time => (1, 0), Count => 1));
      Self.Tester.Tick_T_Send ((Time => (2, 0), Count => 2));
      Self.Tester.Tick_T_Send ((Time => (3, 0), Count => 3));
      Self.Tester.Tick_T_Send ((Time => (4, 0), Count => 4));

      Natural_Assert.Eq (Self.Tester.Data_Product_Fetch_T_Service_History.Get_Count, 5);
      Natural_Assert.Eq (Self.Tester.Data_Product_T_Recv_Sync_History.Get_Count, 0);

      -- should work with variety of ids
      -- note new init
      Self.Tester.Component_Instance.Init (Products_To_Copy => Fetch_Fail_Products_2'Access);
      Self.Tester.Data_Product_T_Recv_Sync_History.Clear;

      Self.Tester.Tick_T_Send ((Time => (5, 0), Count => 5)); -- S,F,F,S,S

      Natural_Assert.Eq (Self.Tester.Data_Product_Fetch_T_Service_History.Get_Count, 10);
      Natural_Assert.Eq (Self.Tester.Data_Product_T_Recv_Sync_History.Get_Count, 3);

      Self.Tester.Tick_T_Send ((Time => (6, 0), Count => 6)); -- S,F,F,S,F

      Natural_Assert.Eq (Self.Tester.Data_Product_Fetch_T_Service_History.Get_Count, 15);
      Natural_Assert.Eq (Self.Tester.Data_Product_T_Recv_Sync_History.Get_Count, 5);

      Self.Tester.Tick_T_Send ((Time => (7, 0), Count => 7)); -- S,F,S,S,S

      Natural_Assert.Eq (Self.Tester.Data_Product_Fetch_T_Service_History.Get_Count, 20);
      Natural_Assert.Eq (Self.Tester.Data_Product_T_Recv_Sync_History.Get_Count, 9);

      Self.Tester.Tick_T_Send ((Time => (8, 0), Count => 8)); -- S,F,S,S,F

      Natural_Assert.Eq (Self.Tester.Data_Product_Fetch_T_Service_History.Get_Count, 25);
      Natural_Assert.Eq (Self.Tester.Data_Product_T_Recv_Sync_History.Get_Count, 12);

      -- test that the 12 products are: 5 1 4 1 4 5 1 4 3 1 4 3
      -- TODO do we care about the order within ticks?
      Check_Product_Ids : declare
         Counts : array (1 .. 5) of Natural := [others => 0];
         Id : Natural;
         Mod_Id : Natural;
      begin
         for I in 1 .. Self.Tester.Data_Product_T_Recv_Sync_History.Get_Count loop
            Id := Natural (
                Self.Tester.Data_Product_T_Recv_Sync_History.Get (Natural (I)).Header.Id
            );
            Mod_Id := Id mod 10;
            -- Ada.Text_IO.Put_Line (Mod_Id'Image);
            -- TODO convert Count_X's to array, match Mod_Id with 1-5 and increment counts
            case Mod_Id is
               when Counts'Range =>
                  Counts (Mod_Id) := @ + 1;
               when others =>
                  Assert (False, "Unexpected ID found");
            end case;
         end loop;
         Natural_Assert.Eq (Counts (1), 4);
         Natural_Assert.Eq (Counts (2), 0);
         Natural_Assert.Eq (Counts (3), 2);
         Natural_Assert.Eq (Counts (4), 4);
         Natural_Assert.Eq (Counts (5), 2);
      end Check_Product_Ids;

   end Test_Fetch_Fail_Behavior;

   -- these three values of Source_Id will result in success, not available, and ID out of range respectively
   Every_Error_Products : aliased Product_Mapping_Array := [
      (Source_Id => 1, Destination_Id => 1),
      (Source_Id => 2, Destination_Id => 2),
      (Source_Id => 99, Destination_Id => 3)
   ];
   Out_Of_Range_Products : aliased Product_Mapping_Array := [
      (Source_Id => 1, Destination_Id => 1),
      (Source_Id => 99, Destination_Id => 3)
   ];
   Not_Available_Products : aliased Product_Mapping_Array := [
      (Source_Id => 1, Destination_Id => 1),
      (Source_Id => 2, Destination_Id => 2)
   ];
   Okay_Products : aliased Product_Mapping_Array := [
      (Source_Id => 1, Destination_Id => 1)
   ];
   -- Makes sure an event is raised when a fetch operation fails and the
   -- corresponding init flag is set.
   overriding procedure Test_Fetch_Fail_Event (Self : in out Instance) is
   begin
      -- all events turned off
      Self.Tester.Component_Instance.Init (
         Products_To_Copy => Every_Error_Products'Access,
         Send_Event_On_Source_Id_Out_Of_Range => False,
         Send_Event_On_Source_Not_Available => False
      );
      Self.Tester.Tick_T_Send ((Time => (0, 0), Count => 0));
      Natural_Assert.Eq (Self.Tester.Event_T_Recv_Sync_History.Get_Count, 0);

      -- relevant events turned off
      Self.Tester.Component_Instance.Init (
         Products_To_Copy => Not_Available_Products'Access,
         Send_Event_On_Source_Id_Out_Of_Range => True,
         Send_Event_On_Source_Not_Available => False
      );
      Self.Tester.Tick_T_Send ((Time => (0, 0), Count => 0));
      Natural_Assert.Eq (Self.Tester.Event_T_Recv_Sync_History.Get_Count, 0);

      Self.Tester.Component_Instance.Init (
         Products_To_Copy => Out_Of_Range_Products'Access,
         Send_Event_On_Source_Id_Out_Of_Range => False,
         Send_Event_On_Source_Not_Available => True
      );
      Self.Tester.Tick_T_Send ((Time => (0, 0), Count => 0));
      Natural_Assert.Eq (Self.Tester.Event_T_Recv_Sync_History.Get_Count, 0);

      -- events turned on, but no event should raise
      Self.Tester.Component_Instance.Init (
         Products_To_Copy => Okay_Products'Access,
         Send_Event_On_Source_Id_Out_Of_Range => True,
         Send_Event_On_Source_Not_Available => True
      );
      Self.Tester.Tick_T_Send ((Time => (0, 0), Count => 0));
      Natural_Assert.Eq (Self.Tester.Event_T_Recv_Sync_History.Get_Count, 0);

      -- now check correct events are raised

      -- check not_available events
      Self.Tester.Component_Instance.Init (
         Products_To_Copy => Not_Available_Products'Access,
         Send_Event_On_Source_Id_Out_Of_Range => True,
         Send_Event_On_Source_Not_Available => True
      );
      Self.Tester.Tick_T_Send ((Time => (0, 0), Count => 0));
      Self.Tester.Tick_T_Send ((Time => (1, 0), Count => 1));
      Self.Tester.Tick_T_Send ((Time => (2, 0), Count => 2));

      Natural_Assert.Eq (Self.Tester.Source_Not_Available_History.Get_Count, 3);
      Natural_Assert.Eq (Self.Tester.Source_Id_Out_Of_Range_History.Get_Count, 0);

      -- TODO also check specific contents of event?

      -- clear history
      Self.Tester.Source_Not_Available_History.Clear;

      -- check id_out_of_range events
      Self.Tester.Component_Instance.Init (
         Products_To_Copy => Out_Of_Range_Products'Access,
         Send_Event_On_Source_Id_Out_Of_Range => True,
         Send_Event_On_Source_Not_Available => True
      );
      Self.Tester.Tick_T_Send ((Time => (0, 0), Count => 0));
      Self.Tester.Tick_T_Send ((Time => (1, 0), Count => 1));
      Self.Tester.Tick_T_Send ((Time => (2, 0), Count => 2));

      Natural_Assert.Eq (Self.Tester.Source_Not_Available_History.Get_Count, 0);
      Natural_Assert.Eq (Self.Tester.Source_Id_Out_Of_Range_History.Get_Count, 3);

      -- clear
      Self.Tester.Source_Id_Out_Of_Range_History.Clear;

   end Test_Fetch_Fail_Event;

end Product_Copier_Tests.Implementation;
