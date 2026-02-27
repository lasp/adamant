--------------------------------------------------------------------------------
-- Ccsds_Serial_Interface Component Implementation Spec
--------------------------------------------------------------------------------

-- Includes:
with Ccsds_Space_Packet;
with Ada.Task_Identification;

-- This component is meant to be a backdoor serial component which uses Ada.Text_IO to send and receive data over a serial port. On Linux, this will send/recv data to/from the terminal, but Ada.Text_IO is attached to a diagnostic uart on most embedded systems. This means that this component can be used as a quick and dirty serial interface without implementing hardware specific uart drivers.
--
package Component.Ccsds_Serial_Interface.Implementation is

   -- Sync pattern for serial. Made public so it can be used elsewhere.
   Sync_Pattern : constant Basic_Types.Byte_Array := [0 => 16#FE#, 1 => 16#D4#, 2 => 16#AF#, 3 => 16#EE#];

   -- The component class instance record:
   type Instance is new Ccsds_Serial_Interface.Base_Instance with private;

   --------------------------------------------------
   -- Subprogram for implementation init method:
   --------------------------------------------------
   -- Init to provide gap between packets if necessary
   --
   -- Init Parameters:
   -- Interpacket_Gap_Ms : Natural - Amount of time in milliseconds to wait in
   -- between transmission of each CCSDS packet. Some UART protocols rely on a gap to
   -- differentiate between packets, and this can be used to enforce that.
   --
   overriding procedure Init (Self : in out Instance; Interpacket_Gap_Ms : in Natural := 0);

private

   -- The component class instance record:
   type Instance is new Ccsds_Serial_Interface.Base_Instance with record
      Listener_Task_Id : Ada.Task_Identification.Task_Id;
      Task_Id_Set : Boolean := False;
      Cpu_Usage : Float;
      Count : Natural := 0;
      Interpacket_Gap_Ms : Natural := 0;
   end record;

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- On this connector the Serial Interface Component receives data and sends it out of the serial port.
   overriding procedure Ccsds_Space_Packet_T_Recv_Async (Self : in out Instance; Arg : in Ccsds_Space_Packet.T);
   -- This procedure is called when a Ccsds_Space_Packet_T_Recv_Async message is dropped due to a full queue.
   overriding procedure Ccsds_Space_Packet_T_Recv_Async_Dropped (Self : in out Instance; Arg : in Ccsds_Space_Packet.T) is null;

   ---------------------------------------
   -- Invoker connector primitives:
   ---------------------------------------
   -- This procedure is called when a Ccsds_Space_Packet_T_Send message is dropped due to a full queue.
   overriding procedure Ccsds_Space_Packet_T_Send_Dropped (Self : in out Instance; Arg : in Ccsds_Space_Packet.T) is null;
   -- This procedure is called when a Event_T_Send message is dropped due to a full queue.
   overriding procedure Event_T_Send_Dropped (Self : in out Instance; Arg : in Event.T) is null;

   -------------------------------------------------------
   -- Definition of subtasks functions for task execution:
   -------------------------------------------------------
   -- This internal task is used to listen on the serial port for incoming packets.
   -- IMPORTANT: This component needs an inner task to run the
   -- following function as if it were the "cycle" method of
   -- a normal component. The "cycle" method of this component
   -- acts in the normal way, blocking on the component
   -- queue, waiting for messages to send through the serial port.
   -- The special cycle method below should be run on another
   -- task to block on the serial port, waiting for incoming messages
   -- which it will then send out to the rest of the assembly.
   -- This task should be set to the LOWEST priority, since it spin
   -- locks while waiting for data on the serial port in many
   -- bare board implementation.
   overriding procedure Listener (Self : in out Instance);

end Component.Ccsds_Serial_Interface.Implementation;
