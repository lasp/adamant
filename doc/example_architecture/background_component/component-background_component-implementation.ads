--------------------------------------------------------------------------------
-- Background_Component Component Implementation Spec
--------------------------------------------------------------------------------

-- Includes:
with Packed_U16;
with Ada.Real_Time; use Ada.Real_Time;

-- This is the background component.
package Component.Background_Component.Implementation is

   -- The component class instance record:
   type Instance is new Background_Component.Base_Instance with private;

private
   -- Declare a two byte array type for convenience.
   subtype Two_Byte_Array is Basic_Types.Byte_Array (0 .. 1);

   -- A protected type used for ensuring mutual exclusion around the
   -- bytes of data put into the packet. Since this data can be set by
   -- multiple external tasks simultaneously, and could be being copied
   -- into a packet at the same time, we need to protect access to the
   -- data to ensure there is no race condition and that the data never
   -- gets corrupted.
   protected type Protected_Data is
      -- Set the data.
      procedure Set_Data (New_Data : in Two_Byte_Array);
      -- Fetch the data.
      function Get_Data return Two_Byte_Array;
   private
      Data : Basic_Types.Byte_Array (0 .. 1) := [0, 0];
   end Protected_Data;

   -- The component class instance record:
   type Instance is new Background_Component.Base_Instance with record
      P_Data : Protected_Data;
      Wake_Up_Time : Ada.Real_Time.Time := Ada.Real_Time.Clock;
   end record;

   ---------------------------------------
   -- Set Up Procedure
   ---------------------------------------
   -- Null method which can be implemented to provide some component
   -- set up code. This method is generally called by the assembly
   -- main.adb after all component initialization and tasks have been started.
   -- Some activities need to only be run once at startup, but cannot be run
   -- safely until everything is up and running, i.e. command registration, initial
   -- data product updates. This procedure should be implemented to do these things
   -- if necessary.
   overriding procedure Set_Up (Self : in out Instance) is null;

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- This connector receives a 16-bit number synchronously that is used to populate the outgoing packet.
   overriding procedure Packed_U16_T_Recv_Sync (Self : in out Instance; Arg : in Packed_U16.T);

   ---------------------------------------
   -- Invoker connector primitives:
   ---------------------------------------
   -- This procedure is called when a Packet_T_Send message is dropped due to a full queue.
   overriding procedure Packet_T_Send_Dropped (Self : in out Instance; Arg : in Packet.T) is null;

   ---------------------------------------------------
   -- Definition of cycle function for task execution:
   ---------------------------------------------------
   -- This is an active component with no queue, so the
   -- cycle function for the component's task must be
   -- implemented here in the implementation class as
   -- a user defined custom function.
   overriding procedure Cycle (Self : in out Instance);

end Component.Background_Component.Implementation;
