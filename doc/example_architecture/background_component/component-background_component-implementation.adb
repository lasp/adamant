--------------------------------------------------------------------------------
-- Background_Component Component Implementation Body
--------------------------------------------------------------------------------

package body Component.Background_Component.Implementation is

   ---------------------------------------
   -- Protected type definition:
   ---------------------------------------
   protected body Protected_Data is
      procedure Set_Data (New_Data : in Two_Byte_Array) is
      begin
         Data := New_Data;
      end Set_Data;

      function Get_Data return Two_Byte_Array is
      begin
         return Data;
      end Get_Data;
   end Protected_Data;

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- This connector receives a 16-bit number synchronously that is used to populate the outgoing packet.
   overriding procedure Packed_U16_T_Recv_Sync (Self : in out Instance; Arg : in Packed_U16.T) is
   begin
      -- Set the data safely using the protected object:
      Self.P_Data.Set_Data (Packed_U16.Serialization.To_Byte_Array (Arg));
   end Packed_U16_T_Recv_Sync;

   ---------------------------------------------------
   -- Definition of cycle function for task execution:
   ---------------------------------------------------
   -- This is an active component with no queue, so the
   -- cycle function for the component's task must be
   -- implemented here in the implementation class as
   -- a user defined custom function.
   overriding procedure Cycle (Self : in out Instance) is
   begin
      -- Delay until the wake up time period:
      delay until Self.Wake_Up_Time;

      -- Let's create a packet.
      declare
         -- Fill in the packet length with the stored packet length.
         Pkt : Packet.T := (
            Header => (
               Time => Self.Sys_Time_T_Get,
               Id => 0,
               Sequence_Count => 0,
               Buffer_Length => 2
            ),
            Buffer => [others => 0]
         );
      begin
         -- Set the packet data:
         Pkt.Buffer (0 .. 1) := Self.P_Data.Get_Data;

         -- Send the packet.
         Self.Packet_T_Send_If_Connected (Pkt);
      end;

      -- Calculate the next wake up time as 500ms after this wake up time.
      Self.Wake_Up_Time := Self.Wake_Up_Time + Ada.Real_Time.Microseconds (500_000);
   end Cycle;

end Component.Background_Component.Implementation;
