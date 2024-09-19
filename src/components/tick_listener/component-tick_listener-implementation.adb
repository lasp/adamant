--------------------------------------------------------------------------------
-- Tick_Listener Component Implementation Body
--------------------------------------------------------------------------------

package body Component.Tick_Listener.Implementation is

   -- Protected tick counter type:
   protected body Tick_Counter is

      function Get_Count return Natural is
      begin
         -- Return the count without modifying its value:
         return Count;
      end Get_Count;

      procedure Get_Count_And_Reset (The_Count : out Natural) is
      begin
         -- Return the count to the user and reset it to 0:
         The_Count := Count;
         Count := Natural'First;
      end Get_Count_And_Reset;

      procedure Reset_Count is
      begin
         -- Reset the count to 0:
         Count := Natural'First;
      end Reset_Count;

      procedure Increment_Count is
      begin
         -- Increment the count:
         if Count /= Natural'Last then
            Count := @ + 1;
         end if;
      end Increment_Count;

   end Tick_Counter;

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- The interrupt counter get connection.
   overriding function Get_Tick_Count (Self : in out Instance) return Packed_Natural.T is
      The_Count : Natural := Natural'First;
   begin
      Self.The_Counter.Get_Count_And_Reset (The_Count);
      return (Value => The_Count);
   end Get_Tick_Count;

   -- The tick receive connection.
   overriding procedure Tick_T_Recv_Sync (Self : in out Instance; Arg : in Tick.T) is
      Ignore : Tick.T renames Arg;
   begin
      Self.The_Counter.Increment_Count;
   end Tick_T_Recv_Sync;

end Component.Tick_Listener.Implementation;
