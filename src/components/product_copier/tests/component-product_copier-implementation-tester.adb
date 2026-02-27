--------------------------------------------------------------------------------
-- Product_Copier Component Tester Body
--------------------------------------------------------------------------------

with Data_Product_Enums;
with Data_Product_Types;
with Packed_U32;
with Packed_U16;

package body Component.Product_Copier.Implementation.Tester is

   ---------------------------------------
   -- Initialize heap variables:
   ---------------------------------------
   procedure Init_Base (Self : in out Instance) is
   begin
      -- Initialize tester heap:
      -- Connector histories:
      Self.Data_Product_T_Recv_Sync_History.Init (Depth => 100);
      Self.Data_Product_Fetch_T_Service_History.Init (Depth => 100);
      Self.Event_T_Recv_Sync_History.Init (Depth => 100);
      Self.Sys_Time_T_Return_History.Init (Depth => 100);
      -- Event histories:
      Self.Source_Not_Available_History.Init (Depth => 100);
      Self.Source_Id_Out_Of_Range_History.Init (Depth => 100);
   end Init_Base;

   procedure Final_Base (Self : in out Instance) is
   begin
      -- Destroy tester heap:
      -- Connector histories:
      Self.Data_Product_T_Recv_Sync_History.Destroy;
      Self.Data_Product_Fetch_T_Service_History.Destroy;
      Self.Event_T_Recv_Sync_History.Destroy;
      Self.Sys_Time_T_Return_History.Destroy;
      -- Event histories:
      Self.Source_Not_Available_History.Destroy;
      Self.Source_Id_Out_Of_Range_History.Destroy;
   end Final_Base;

   ---------------------------------------
   -- Test initialization functions:
   ---------------------------------------
   procedure Connect (Self : in out Instance) is
   begin
      Self.Component_Instance.Attach_Data_Product_T_Send (To_Component => Self'Unchecked_Access, Hook => Self.Data_Product_T_Recv_Sync_Access);
      Self.Component_Instance.Attach_Data_Product_Fetch_T_Request (To_Component => Self'Unchecked_Access, Hook => Self.Data_Product_Fetch_T_Service_Access);
      Self.Component_Instance.Attach_Event_T_Send (To_Component => Self'Unchecked_Access, Hook => Self.Event_T_Recv_Sync_Access);
      Self.Component_Instance.Attach_Sys_Time_T_Get (To_Component => Self'Unchecked_Access, Hook => Self.Sys_Time_T_Return_Access);
      Self.Attach_Tick_T_Send (To_Component => Self.Component_Instance'Unchecked_Access, Hook => Self.Component_Instance.Tick_T_Recv_Sync_Access);
   end Connect;

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   overriding procedure Data_Product_T_Recv_Sync (Self : in out Instance; Arg : in Data_Product.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Data_Product_T_Recv_Sync_History.Push (Arg);
   end Data_Product_T_Recv_Sync;

   overriding function Data_Product_Fetch_T_Service (Self : in out Instance; Arg : in Data_Product_Fetch.T) return Data_Product_Return.T is
      use Data_Product_Enums;
      use Data_Product_Types;
      Dp : Data_Product.T;
      DP_Return : Data_Product_Return.T;
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Data_Product_Fetch_T_Service_History.Push (Arg);

      -- simulate a database, with different indices having different behavior
      case Arg.Id is
         when 1 =>
            -- always succeeds
            Dp.Header.Id := 1 + 10 * Data_Product_Id (Self.Case_1_Ctr);
            Dp.Header.Buffer_Length := Packed_U32.Serialization.Byte_Array'Length;
            Dp.Buffer (Dp.Buffer'First .. Dp.Buffer'First + Dp.Header.Buffer_Length - 1) := Packed_U32.Serialization.To_Byte_Array ((Value => 23));

            DP_Return.The_Status := Fetch_Status.Success;
            DP_Return.The_Data_Product := Dp;

            Self.Case_1_Ctr := @ + 1;
         when 2 =>
            -- always fails
            DP_Return.The_Status := Fetch_Status.Not_Available;
         when 3 =>
            -- fails first 2 attempts, then succeeds
            if Self.Case_3_Ctr < 2 then
               DP_Return.The_Status := Fetch_Status.Not_Available;
            else
               Dp.Header.Id := 3 + 10 * Data_Product_Id (Self.Case_3_Ctr);
               Dp.Header.Buffer_Length := Tick.Serialization.Byte_Array'Length;
               Dp.Buffer (Dp.Buffer'First .. Dp.Buffer'First + Dp.Header.Buffer_Length - 1) := Tick.Serialization.To_Byte_Array ((Dp.Header.Time, 14));

               DP_Return.The_Status := Fetch_Status.Success;
               DP_Return.The_Data_Product := Dp;
            end if;
            Self.Case_3_Ctr := @ + 1;
         when 4 =>
            -- succeeds first 2 attempts, then returns same DP repeatedly
            -- always has success status
            if Self.Case_4_Ctr < 2 then
               Self.Case_4_Ctr := @ + 1;
            end if;

            Dp.Header.Id := 4 + 10 * Data_Product_Id (Self.Case_4_Ctr);
            Dp.Header.Buffer_Length := Packed_U16.Serialization.Byte_Array'Length;
            Dp.Buffer (Dp.Buffer'First .. Dp.Buffer'First + Dp.Header.Buffer_Length - 1) := Packed_U16.Serialization.To_Byte_Array ((Value => 33));

            DP_Return.The_Status := Fetch_Status.Success;
            DP_Return.The_Data_Product := Dp;
         when 5 =>
            -- alternates success and failure
            if Self.Case_5_Ctr rem 2 = 0 then
               Dp.Header.Id := 5 + 10 * Data_Product_Id (Self.Case_5_Ctr);
               Dp.Header.Buffer_Length := Packed_U16.Serialization.Byte_Array'Length;
               Dp.Buffer (Dp.Buffer'First .. Dp.Buffer'First + Dp.Header.Buffer_Length - 1) := Packed_U16.Serialization.To_Byte_Array ((Value => 0));

               DP_Return.The_Status := Fetch_Status.Success;
               DP_Return.The_Data_Product := Dp;
            else
               DP_Return.The_Status := Fetch_Status.Not_Available;
            end if;
            Self.Case_5_Ctr := @ + 1;
         -- adding a 6th branch will not break any tests
         when others =>
            -- pass any other id to get this status
            DP_Return.The_Status := Fetch_Status.Id_Out_Of_Range;
      end case;

      return DP_Return;
   end Data_Product_Fetch_T_Service;

   overriding procedure Event_T_Recv_Sync (Self : in out Instance; Arg : in Event.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Event_T_Recv_Sync_History.Push (Arg);
      -- Dispatch the event to the correct handler:
      Self.Dispatch_Event (Arg);
   end Event_T_Recv_Sync;

   overriding function Sys_Time_T_Return (Self : in out Instance) return Sys_Time.T is
      -- Return the system time:
      To_Return : constant Sys_Time.T := Self.System_Time;
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Sys_Time_T_Return_History.Push (To_Return);
      return To_Return;
   end Sys_Time_T_Return;

   -----------------------------------------------
   -- Event handler primitive:
   -----------------------------------------------
   -- A data product fetch resulted in an a Not_Available status, and was not read
   -- from the source.
   overriding procedure Source_Not_Available (Self : in out Instance; Arg : in Product_Copier_Error_Info.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Source_Not_Available_History.Push (Arg);
   end Source_Not_Available;

   -- A data product fetch resulted in an Id_Out_Of_Range status, and was not read
   -- from the source.
   overriding procedure Source_Id_Out_Of_Range (Self : in out Instance; Arg : in Product_Copier_Error_Info.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Source_Id_Out_Of_Range_History.Push (Arg);
   end Source_Id_Out_Of_Range;

end Component.Product_Copier.Implementation.Tester;
