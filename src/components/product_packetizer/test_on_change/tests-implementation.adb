--------------------------------------------------------------------------------
-- Product_Packetizer On-Change Tests Body
--------------------------------------------------------------------------------

with Tick;
with Packet_Types;
with Interfaces;
with Product_Packetizer_Commands;
with Basic_Assertions; use Basic_Assertions;
with Packet_Period.Assertion; use Packet_Period.Assertion;
with Command_Response.Assertion; use Command_Response.Assertion;
with Command_Enums; use Command_Enums.Command_Response_Status;
with Data_Product_Enums; use Data_Product_Enums;

package body Tests.Implementation is

   -------------------------------------------------------------------------
   -- Helper functions
   -------------------------------------------------------------------------

   function Packet_Id_Count (
      Tester : Component.Product_Packetizer.Implementation.Tester.Instance_Access;
      Packet_Id : Packet_Types.Packet_Id
   ) return Natural is
      use type Packet_Types.Packet_Id;

      Count : Natural := 0;
   begin
      for Idx in 1 .. Tester.Packet_T_Recv_Sync_History.Get_Count loop
         if Tester.Packet_T_Recv_Sync_History.Get (Idx).Header.Id = Packet_Id then
            Count := Count + 1;
         end if;
      end loop;
      return Count;
   end Packet_Id_Count;

   -------------------------------------------------------------------------
   -- Fixtures:
   -------------------------------------------------------------------------

   overriding procedure Set_Up_Test (Self : in out Instance) is
   begin
      -- Allocate heap memory to component:
      Self.Tester.Init_Base (Queue_Size => Self.Tester.Component_Instance.Get_Max_Queue_Element_Size * 3);

      -- Make necessary connections between tester and component:
      Self.Tester.Connect;

      -- Initialize the component:
      Self.Tester.Component_Instance.Init;

      -- Set the desired time for the tests
      Self.Tester.System_Time := (3, 17);

      -- Set count to zero:
      Self.Tester.Reset_Count;
      Self.Tester.Use_Data_Product_Return_Value := False;
   end Set_Up_Test;

   overriding procedure Tear_Down_Test (Self : in out Instance) is
   begin
      -- Free component heap:
      Self.Tester.Final_Base;
   end Tear_Down_Test;

   -------------------------------------------------------------------------
   -- Tests:
   -------------------------------------------------------------------------

   overriding procedure Test_On_Change_Nominal (Self : in out Instance) is
      T : Component.Product_Packetizer.Implementation.Tester.Instance_Access renames Self.Tester;
      The_Tick : Tick.T := ((0, 0), 1);
   begin
      -- Packet 6 starts enabled in On_Change mode via YAML, but until a tracked DP beats the last emission time we expect silence.
      -- Initial tick should not produce a packet because no DP has changed yet.
      T.Tick_T_Send (The_Tick);
      Natural_Assert.Eq (Packet_Id_Count (T, 16), 0);

      The_Tick.Time := (1, 0);
      T.Data_Product_Return_Target_Id := 4;
      -- Inject a DP_D change by overriding the fetch return. DP_D is the only `used_for_on_change => True` entry for this packet.
      -- We also leave DP_B untouched to prove that helper fields excluded from change detection do not cause emissions.
      T.Data_Product_Return_Value := (
         The_Status => Data_Product_Enums.Fetch_Status.Success,
         The_Data_Product => (
            Header => (Time => (1, 0), Id => 4, Buffer_Length => 2),
            Buffer => [others => 11]
         )
      );
      T.Use_Data_Product_Return_Value := True;

      -- Expect the packet to be sent exactly once when the DP timestamp advances.
      T.Tick_T_Send (The_Tick);
      Natural_Assert.Eq (Packet_Id_Count (T, 16), 1);
      T.Packet_T_Recv_Sync_History.Clear;

      -- No additional change -> no packet.
      The_Tick.Time := (2, 0);
      T.Tick_T_Send (The_Tick);
      Natural_Assert.Eq (Packet_Id_Count (T, 16), 0);
      T.Packet_T_Recv_Sync_History.Clear;

      -- Bump the DP timestamp again and ensure another packet is emitted.
      T.Data_Product_Return_Value.The_Data_Product.Header.Time := (3, 0);
      The_Tick.Time := (3, 0);
      T.Tick_T_Send (The_Tick);
      Natural_Assert.Eq (Packet_Id_Count (T, 16), 1);
      T.Use_Data_Product_Return_Value := False;
   end Test_On_Change_Nominal;

   overriding procedure Test_On_Change_Used_For_On_Change_False (Self : in out Instance) is
      T : Component.Product_Packetizer.Implementation.Tester.Instance_Access renames Self.Tester;
      The_Tick : Tick.T := ((0, 0), 1);
   begin
      -- First, update DP_B which has `used_for_on_change => False` and make sure it does not trigger an emission.
      The_Tick.Time := (1, 0);
      T.Data_Product_Return_Target_Id := 2;
      T.Data_Product_Return_Value := (
         The_Status => Data_Product_Enums.Fetch_Status.Success,
         The_Data_Product => (
            Header => (Time => (1, 0), Id => 2, Buffer_Length => 10),
            Buffer => [others => 22]
         )
      );
      T.Use_Data_Product_Return_Value := True;

      T.Tick_T_Send (The_Tick);
      Natural_Assert.Eq (Packet_Id_Count (T, 16), 0);
      T.Packet_T_Recv_Sync_History.Clear;

      -- Now toggle DP_D which is flagged for on-change; the packet should send once.
      T.Data_Product_Return_Target_Id := 4;
      T.Data_Product_Return_Value.The_Data_Product.Header := (Time => (2, 0), Id => 4, Buffer_Length => 2);
      The_Tick.Time := (2, 0);

      T.Tick_T_Send (The_Tick);
      Natural_Assert.Eq (Packet_Id_Count (T, 16), 1);
      T.Packet_T_Recv_Sync_History.Clear;
      T.Use_Data_Product_Return_Value := False;
   end Test_On_Change_Used_For_On_Change_False;

   overriding procedure Test_Enable_Packet_On_Change_Command (Self : in out Instance) is
      T : Component.Product_Packetizer.Implementation.Tester.Instance_Access renames Self.Tester;
      The_Tick : constant Tick.T := ((0, 0), 1);
      Commands : Product_Packetizer_Commands.Instance;
   begin
      -- First, run packet 1 in periodic mode to establish the baseline send.
      T.Command_T_Send (Commands.Enable_Packet ((Id => 7)));
      T.Tick_T_Send (The_Tick);
      T.Command_Response_T_Recv_Sync_History.Clear;
      T.Event_T_Recv_Sync_History.Clear;
      T.Packet_T_Recv_Sync_History.Clear;

      T.Tick_T_Send (The_Tick);
      T.Tick_T_Send (The_Tick);
      T.Tick_T_Send (The_Tick);
      Natural_Assert.Eq (Packet_Id_Count (T, 7), 1);
      T.Packet_T_Recv_Sync_History.Clear;

      -- Switch packet 1 into on-change mode and ensure we emit the correct event.
      T.Command_T_Send (Commands.Enable_Packet_On_Change ((Id => 7)));
      T.Tick_T_Send (The_Tick);

      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 1);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (1),
         (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Enable_Packet_On_Change_Id, Status => Success));

      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Packet_Enabled_On_Change_History.Get_Count, 1);
      Packet_Period_Assert.Eq (T.Packet_Enabled_On_Change_History.Get (1), (Id => 7, Period => 3));
      T.Packet_T_Recv_Sync_History.Clear;
      T.Dp_Time := (0, 0);

      -- Any further ticks without DP changes should not send packets.
      T.Tick_T_Send (The_Tick);
      T.Tick_T_Send (The_Tick);
      T.Tick_T_Send (The_Tick);
      Natural_Assert.Eq (Packet_Id_Count (T, 7), 0);
   end Test_Enable_Packet_On_Change_Command;

   overriding procedure Test_On_Change_Multiple_Changes (Self : in out Instance) is
      T : Component.Product_Packetizer.Implementation.Tester.Instance_Access renames Self.Tester;
      The_Tick : Tick.T := ((0, 0), 1);
   begin
      -- Exercise a sequence of DP_D updates to verify we send exactly once per timestamp change and stay quiet otherwise.
      -- Each timestamp update below is separated by an intervening tick with no change so we can confirm we never double-send.
      The_Tick.Time := (6, 0);
      T.Data_Product_Return_Target_Id := 4;
      T.Data_Product_Return_Value := (
         The_Status => Data_Product_Enums.Fetch_Status.Success,
         The_Data_Product => (
            Header => (Time => (6, 0), Id => 4, Buffer_Length => 2),
            Buffer => [others => 11]
         )
      );
      T.Use_Data_Product_Return_Value := True;
      T.Tick_T_Send (The_Tick);
      Natural_Assert.Eq (Packet_Id_Count (T, 16), 1);
      T.Packet_T_Recv_Sync_History.Clear;

      The_Tick.Time := (7, 0);
      T.Tick_T_Send (The_Tick);
      Natural_Assert.Eq (Packet_Id_Count (T, 16), 0);

      -- Advance DP_D again, this time also tweaking its payload to show that data bytes are copied whenever a change occurs.
      The_Tick.Time := (8, 0);
      T.Data_Product_Return_Value.The_Data_Product.Header.Time := (8, 0);
      T.Data_Product_Return_Value.The_Data_Product.Buffer (0) := 99;
      T.Tick_T_Send (The_Tick);
      Natural_Assert.Eq (Packet_Id_Count (T, 16), 1);
      T.Packet_T_Recv_Sync_History.Clear;

      The_Tick.Time := (9, 0);
      T.Tick_T_Send (The_Tick);
      Natural_Assert.Eq (Packet_Id_Count (T, 16), 0);

      -- Final timestamp bump confirms we can detect multiple deltas in succession without leaving stale state set.
      The_Tick.Time := (10, 0);
      T.Data_Product_Return_Value.The_Data_Product.Header.Time := (10, 0);
      T.Tick_T_Send (The_Tick);
      Natural_Assert.Eq (Packet_Id_Count (T, 16), 1);
      T.Use_Data_Product_Return_Value := False;
   end Test_On_Change_Multiple_Changes;

   overriding procedure Test_On_Change_With_Period (Self : in out Instance) is
      T : Component.Product_Packetizer.Implementation.Tester.Instance_Access renames Self.Tester;
      The_Tick : Tick.T := ((0, 0), 1);
      Commands : Product_Packetizer_Commands.Instance;
      Previous_Count : Natural := 0;
   begin
      T.Command_T_Send (Commands.Enable_Packet_On_Change ((Id => 7)));
      T.Tick_T_Send (The_Tick);
      T.Use_Data_Product_Return_Value := False;
      T.Data_Product_Return_Target_Id := 1;
      T.Data_Product_Return_Value := (
         The_Status => Data_Product_Enums.Fetch_Status.Success,
         The_Data_Product => (
            Header => (Time => (6, 0), Id => 1, Buffer_Length => 4),
            Buffer => [others => 11]
         )
      );
      T.Use_Data_Product_Return_Value := True;

      -- Drive packet 1 through the on-change command path. The pre-evaluation tick occurs while we are still accumulating history.
      -- Pre-evaluation tick should not send.
      The_Tick.Time := (6, 0);
      T.Tick_T_Send (The_Tick);
      Natural_Assert.Eq (Packet_Id_Count (T, 7), 0);

      -- Evaluation tick detects data change and sends once.
      The_Tick.Time := (7, 0);
      T.Tick_T_Send (The_Tick);
      Natural_Assert.Eq (Packet_Id_Count (T, 7), 1);
      Previous_Count := Packet_Id_Count (T, 7);

      -- Remove override and ensure no additional packets when data is unchanged.
      T.Use_Data_Product_Return_Value := False;
      T.Dp_Time := (0, 0);
      -- Walk through an entire additional period with no changes to ensure nothing fires spuriously.
      for Time in 8 .. 10 loop
         The_Tick.Time := (Interfaces.Unsigned_32 (Time), 0);
         T.Tick_T_Send (The_Tick);
         Natural_Assert.Eq (Packet_Id_Count (T, 7), Previous_Count);
      end loop;
   end Test_On_Change_With_Period;

end Tests.Implementation;
