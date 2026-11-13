--------------------------------------------------------------------------------
-- Ccsds_Command_Forwarder Component Implementation Spec
--------------------------------------------------------------------------------

-- Includes:
with Command;

-- The CCSDS command forwarder is a component that forwards CCSDS packets received
-- as command arguments out of a CCSDS space packet send connector. The purpose of
-- this component is to allow CCSDS packets to be injected into a CCSDS packet
-- processing chain (e.g. into a CCSDS router) via command, which is useful for
-- testing and debugging the software. The packet is forwarded exactly as provided
-- in the command argument, with no validation or modification of its contents.
-- Note that the maximum size packet that can be forwarded by this component is
-- limited by the size of the command argument buffer, as configured by the
-- command_buffer_size configuration parameter.
package Component.Ccsds_Command_Forwarder.Implementation is

   -- The component class instance record:
   type Instance is new Ccsds_Command_Forwarder.Base_Instance with private;

private

   -- The component class instance record:
   type Instance is new Ccsds_Command_Forwarder.Base_Instance with record
      -- The number of packets forwarded by this component since startup.
      Packet_Count : Unsigned_32 := 0;
   end record;

   ---------------------------------------
   -- Set Up Procedure
   ---------------------------------------
   -- This procedure sends out the initial value of the Packets_Forwarded_Count
   -- data product.
   overriding procedure Set_Up (Self : in out Instance);

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- This is the command receive connector.
   overriding procedure Command_T_Recv_Sync (Self : in out Instance; Arg : in Command.T);

   ---------------------------------------
   -- Invoker connector primitives:
   ---------------------------------------
   -- This procedure is called when a Command_Response_T_Send message is dropped due to a full queue.
   overriding procedure Command_Response_T_Send_Dropped (Self : in out Instance; Arg : in Command_Response.T) is null;
   -- This procedure is called when a Ccsds_Space_Packet_T_Send message is dropped due to a full queue.
   overriding procedure Ccsds_Space_Packet_T_Send_Dropped (Self : in out Instance; Arg : in Ccsds_Space_Packet.T) is null;
   -- This procedure is called when a Event_T_Send message is dropped due to a full queue.
   overriding procedure Event_T_Send_Dropped (Self : in out Instance; Arg : in Event.T) is null;
   -- This procedure is called when a Data_Product_T_Send message is dropped due to a full queue.
   overriding procedure Data_Product_T_Send_Dropped (Self : in out Instance; Arg : in Data_Product.T) is null;

   -----------------------------------------------
   -- Command handler primitives:
   -----------------------------------------------
   -- Description:
   --    These are the commands for the CCSDS command forwarder component.
   -- Forward the provided CCSDS packet out of the CCSDS space packet send connector.
   overriding function Forward_Packet (Self : in out Instance; Arg : in Ccsds_Command_Space_Packet.T) return Command_Execution_Status.E;

   -- Invalid command handler. This procedure is called when a command's arguments are found to be invalid:
   overriding procedure Invalid_Command (Self : in out Instance; Cmd : in Command.T; Errant_Field_Number : in Unsigned_32; Errant_Field : in Basic_Types.Poly_Type);

end Component.Ccsds_Command_Forwarder.Implementation;
