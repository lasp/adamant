--------------------------------------------------------------------------------
-- Zero_Divider Component Tester Spec
--------------------------------------------------------------------------------

-- Includes:
with Component.Zero_Divider_Reciprocal;
with Printable_History;
with Command_Response.Representation;
with Sys_Time.Representation;
with Event.Representation;
with Event;
with Packed_Natural.Representation;
with Packed_U32.Representation;
with Invalid_Command_Info.Representation;
with Packed_Exception_Occurrence.Representation;

-- The purpose of this component is to provide a safe, commandable way to cause the Ada Last Chance Handler to be called. To accomplish this, this component provides a Divide_By_Zero command which divides an integer by zero, which causes an Ada exception to be thrown, which is purposely not handled. The Divide_By_Zero command must be passed a magic number as an argument. If the magic number does not match the number that this component is instantiated with at initialization, then the Divide_By_Zero is not executed. This feature prevents inadvertent execution of this command. This component also supplies the packet definition for the assembly for a Last Chance Handler (LCH) packet that is created by the last chance handler itself (which is not usually implemented as an Adamant component). This provides the ground system the LCH packet definition so it can be parsed and stored. The component does not contain a Packet.T send connector, so will not send out this packet itself. Your Last Chance Handler should produce a packet with this packet definition.
package Component.Zero_Divider.Implementation.Tester is

   use Component.Zero_Divider_Reciprocal;
   -- Invoker connector history packages:
   package Command_Response_T_Recv_Sync_History_Package is new Printable_History (Command_Response.T, Command_Response.Representation.Image);
   package Sys_Time_T_Return_History_Package is new Printable_History (Sys_Time.T, Sys_Time.Representation.Image);
   package Event_T_Recv_Sync_History_Package is new Printable_History (Event.T, Event.Representation.Image);

   -- Event history packages:
   package Dividing_By_Zero_History_Package is new Printable_History (Packed_Natural.T, Packed_Natural.Representation.Image);
   package Invalid_Magic_Number_History_Package is new Printable_History (Packed_U32.T, Packed_U32.Representation.Image);
   package Invalid_Command_Received_History_Package is new Printable_History (Invalid_Command_Info.T, Invalid_Command_Info.Representation.Image);

   -- Packet history packages:
   package Last_Chance_Handler_Packet_History_Package is new Printable_History (Packed_Exception_Occurrence.T, Packed_Exception_Occurrence.Representation.Image);

   -- Component class instance:
   type Instance is new Component.Zero_Divider_Reciprocal.Base_Instance with record
      -- The component instance under test:
      Component_Instance : aliased Component.Zero_Divider.Implementation.Instance;
      -- Connector histories:
      Command_Response_T_Recv_Sync_History : Command_Response_T_Recv_Sync_History_Package.Instance;
      Sys_Time_T_Return_History : Sys_Time_T_Return_History_Package.Instance;
      Event_T_Recv_Sync_History : Event_T_Recv_Sync_History_Package.Instance;
      -- Event histories:
      Dividing_By_Zero_History : Dividing_By_Zero_History_Package.Instance;
      Invalid_Magic_Number_History : Invalid_Magic_Number_History_Package.Instance;
      Invalid_Command_Received_History : Invalid_Command_Received_History_Package.Instance;
      -- Packet histories:
      Last_Chance_Handler_Packet_History : Last_Chance_Handler_Packet_History_Package.Instance;
   end record;
   type Instance_Access is access all Instance;

   ---------------------------------------
   -- Initialize component heap variables:
   ---------------------------------------
   procedure Init_Base (Self : in out Instance);
   procedure Final_Base (Self : in out Instance);

   ---------------------------------------
   -- Test initialization functions:
   ---------------------------------------
   procedure Connect (Self : in out Instance);

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- This connector is used to register and respond to the component's commands.
   overriding procedure Command_Response_T_Recv_Sync (Self : in out Instance; Arg : in Command_Response.T);
   -- The system time is retrieved via this connector.
   overriding function Sys_Time_T_Return (Self : in out Instance) return Sys_Time.T;
   -- Events are sent out of this connector.
   overriding procedure Event_T_Recv_Sync (Self : in out Instance; Arg : in Event.T);

   -----------------------------------------------
   -- Event handler primitive:
   -----------------------------------------------
   -- A divide by zero command was received, and the magic number was correct. The division will occur in N milliseconds, where N is provided as the event parameter.
   overriding procedure Dividing_By_Zero (Self : in out Instance; Arg : in Packed_Natural.T);
   -- A divide by zero command was received, but the magic number was incorrect. The division will not occur.
   overriding procedure Invalid_Magic_Number (Self : in out Instance; Arg : in Packed_U32.T);
   -- A command was received with invalid parameters.
   overriding procedure Invalid_Command_Received (Self : in out Instance; Arg : in Invalid_Command_Info.T);

   -----------------------------------------------
   -- Packet handler primitives:
   -----------------------------------------------
   -- Description:
   --    The second packet listed here is not actually produced by the Last Chance Manager component, but instead should be produced by the implementation of the Last\_Chance\_Handler. This packet definition exists to ensure that the packet gets reflected in the documentation and ground system definitions.
   -- This packet contains information regarding an exception occurrence that triggers the Last\_Chance\_Handler to get invoked. This packet is not produced directly by this component, and should be produced by the last chance handler implementation. This packet definition exists to ensure that the packet gets reflected in the documentation and ground system definitions.
   overriding procedure Last_Chance_Handler_Packet (Self : in out Instance; Arg : in Packed_Exception_Occurrence.T);

end Component.Zero_Divider.Implementation.Tester;
