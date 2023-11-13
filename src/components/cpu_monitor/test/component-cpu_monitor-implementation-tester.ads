--------------------------------------------------------------------------------
-- Cpu_Monitor Component Tester Spec
--------------------------------------------------------------------------------

-- Includes:
with Component.Cpu_Monitor_Reciprocal;
with Sys_Time;
with Printable_History;
with Packet.Representation;
with Sys_Time.Representation;
with Command_Response.Representation;
with Data_Product.Representation;
with Event.Representation;
with Data_Product;
with Packed_U16.Representation;
with Event;
with Invalid_Command_Info.Representation;
with History;

-- This component produces a packet holding the CPU execution time for all tasks and interrupts configured for a particular assembly. It is provided an autocoded data structure upon initialization that contains the tasks and interrupts which it is to monitor. The packet produced contains 3 CPU execution numbers (1 bytes in size ranging from 0 - 100) for each task/interrupt, corresponding to different length time periods. The length of these time periods is also specified at initialization as multiples of the master tick driving the component.
--
-- Note that this component monitors CPU utilization by calling the Ada runtime Ada.Execution_Time.Clock subprogram which returns the amount of time since startup that a task or interrupt has been running on the CPU. The input to this subprogram is a Ada.Task_Identification id, which is provided by Adamant in an autocoded global variable for every modeled task which is passed into this component upon initialization. This interface is nonstandard, in that it exchanges information without the use of a connector. However, the use of this nonstandard interface improves efficiency and avoids having to include task identification connectors for every active component, which would be overly cumbersome.
package Component.Cpu_Monitor.Implementation.Tester is

   use Component.Cpu_Monitor_Reciprocal;
   -- Invoker connector history packages:
   package Packet_T_Recv_Sync_History_Package is new Printable_History (Packet.T, Packet.Representation.Image);
   package Sys_Time_T_Return_History_Package is new Printable_History (Sys_Time.T, Sys_Time.Representation.Image);
   package Command_Response_T_Recv_Sync_History_Package is new Printable_History (Command_Response.T, Command_Response.Representation.Image);
   package Data_Product_T_Recv_Sync_History_Package is new Printable_History (Data_Product.T, Data_Product.Representation.Image);
   package Event_T_Recv_Sync_History_Package is new Printable_History (Event.T, Event.Representation.Image);

   -- Event history packages:
   package Packet_Period_Set_History_Package is new Printable_History (Packed_U16.T, Packed_U16.Representation.Image);
   package Invalid_Command_Received_History_Package is new Printable_History (Invalid_Command_Info.T, Invalid_Command_Info.Representation.Image);

   -- Data product history packages:
   package Packet_Period_History_Package is new Printable_History (Packed_U16.T, Packed_U16.Representation.Image);

   -- Packet history packages:
   package Cpu_Usage_Packet_History_Package is new History (Packet.T);

   -- Component class instance:
   type Instance is new Component.Cpu_Monitor_Reciprocal.Base_Instance with record
      -- The component instance under test:
      Component_Instance : aliased Component.Cpu_Monitor.Implementation.Instance;
      -- Connector histories:
      Packet_T_Recv_Sync_History : Packet_T_Recv_Sync_History_Package.Instance;
      Sys_Time_T_Return_History : Sys_Time_T_Return_History_Package.Instance;
      Command_Response_T_Recv_Sync_History : Command_Response_T_Recv_Sync_History_Package.Instance;
      Data_Product_T_Recv_Sync_History : Data_Product_T_Recv_Sync_History_Package.Instance;
      Event_T_Recv_Sync_History : Event_T_Recv_Sync_History_Package.Instance;
      -- Event histories:
      Packet_Period_Set_History : Packet_Period_Set_History_Package.Instance;
      Invalid_Command_Received_History : Invalid_Command_Received_History_Package.Instance;
      -- Data product histories:
      Packet_Period_History : Packet_Period_History_Package.Instance;
      -- Packet histories:
      Cpu_Usage_Packet_History : Cpu_Usage_Packet_History_Package.Instance;
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
   -- Send a packet of cpu execution times.
   overriding procedure Packet_T_Recv_Sync (Self : in out Instance; Arg : in Packet.T);
   -- The system time is retrieved via this connector.
   overriding function Sys_Time_T_Return (Self : in out Instance) return Sys_Time.T;
   -- This connector is used to register and respond to the component's commands.
   overriding procedure Command_Response_T_Recv_Sync (Self : in out Instance; Arg : in Command_Response.T);
   -- Data products are sent out of this connector.
   overriding procedure Data_Product_T_Recv_Sync (Self : in out Instance; Arg : in Data_Product.T);
   -- Events are sent out of this connector.
   overriding procedure Event_T_Recv_Sync (Self : in out Instance; Arg : in Event.T);

   -----------------------------------------------
   -- Event handler primitive:
   -----------------------------------------------
   -- A command was received to change the packet period.
   overriding procedure Packet_Period_Set (Self : in out Instance; Arg : in Packed_U16.T);
   -- A command was received with invalid parameters.
   overriding procedure Invalid_Command_Received (Self : in out Instance; Arg : in Invalid_Command_Info.T);

   -----------------------------------------------
   -- Data product handler primitives:
   -----------------------------------------------
   -- Description:
   --    Data products for the CPU Monitor component.
   -- The current packet period.
   overriding procedure Packet_Period (Self : in out Instance; Arg : in Packed_U16.T);

   -----------------------------------------------
   -- Packet handler primitives:
   -----------------------------------------------
   -- Description:
   --    Packets for the cpu monitor.
   -- This packet contains cpu usage numbers for tasks and interrupts in the system.
   overriding procedure Cpu_Usage_Packet (Self : in out Instance; Arg : in Packet.T);

end Component.Cpu_Monitor.Implementation.Tester;
