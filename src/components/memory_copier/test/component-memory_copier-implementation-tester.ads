--------------------------------------------------------------------------------
-- Memory_Copier Component Tester Spec
--------------------------------------------------------------------------------

-- Includes:
with Component.Memory_Copier_Reciprocal;
with Sys_Time;
with Printable_History;
with Command_Response.Representation;
with Memory_Region_Copy.Representation;
with Memory_Region_Request.Representation;
with Ided_Memory_Region.Representation;
with Event.Representation;
with Sys_Time.Representation;
with Event;
with Invalid_Memory_Region_Length.Representation;
with Virtual_Memory_Region_Copy.Representation;
with Invalid_Command_Info.Representation;
with Memory_Region_Release.Representation;
with Command_Header.Representation;
with Memory_Manager_Enums;

-- This component services a command to copy from one memory region to another. The to/from destination of the copy command is determined by how it is connected in the assembly. The component will wait a configurable timeout for the copy command to complete before failing the command and reporting a timeout error.
package Component.Memory_Copier.Implementation.Tester is

   use Component.Memory_Copier_Reciprocal;
   -- Invoker connector history packages:
   package Command_Response_T_Recv_Sync_History_Package is new Printable_History (Command_Response.T, Command_Response.Representation.Image);
   package Memory_Region_Copy_T_Recv_Sync_History_Package is new Printable_History (Memory_Region_Copy.T, Memory_Region_Copy.Representation.Image);
   package Memory_Region_Request_T_Return_History_Package is new Printable_History (Memory_Region_Request.T, Memory_Region_Request.Representation.Image);
   package Ided_Memory_Region_Release_Reciprocal_History_Package is new Printable_History (Ided_Memory_Region.T, Ided_Memory_Region.Representation.Image);
   package Event_T_Recv_Sync_History_Package is new Printable_History (Event.T, Event.Representation.Image);
   package Sys_Time_T_Return_History_Package is new Printable_History (Sys_Time.T, Sys_Time.Representation.Image);

   -- Event history packages:
   package Memory_Region_Length_Mismatch_History_Package is new Printable_History (Invalid_Memory_Region_Length.T, Invalid_Memory_Region_Length.Representation.Image);
   package Memory_Region_Unavailable_History_Package is new Printable_History (Natural, Natural'Image);
   package Starting_Copy_History_Package is new Printable_History (Virtual_Memory_Region_Copy.T, Virtual_Memory_Region_Copy.Representation.Image);
   package Finished_Copy_History_Package is new Printable_History (Virtual_Memory_Region_Copy.T, Virtual_Memory_Region_Copy.Representation.Image);
   package Invalid_Command_Received_History_Package is new Printable_History (Invalid_Command_Info.T, Invalid_Command_Info.Representation.Image);
   package Copy_Timeout_History_Package is new Printable_History (Natural, Natural'Image);
   package Copy_Failure_History_Package is new Printable_History (Memory_Region_Release.T, Memory_Region_Release.Representation.Image);
   package Command_Dropped_History_Package is new Printable_History (Command_Header.T, Command_Header.Representation.Image);

   -- Component class instance:
   type Instance is new Component.Memory_Copier_Reciprocal.Base_Instance with record
      -- The component instance under test:
      Component_Instance : aliased Component.Memory_Copier.Implementation.Instance;
      -- Connector histories:
      Command_Response_T_Recv_Sync_History : Command_Response_T_Recv_Sync_History_Package.Instance;
      Memory_Region_Copy_T_Recv_Sync_History : Memory_Region_Copy_T_Recv_Sync_History_Package.Instance;
      Memory_Region_Request_T_Return_History : Memory_Region_Request_T_Return_History_Package.Instance;
      Ided_Memory_Region_Release_Reciprocal_History : Ided_Memory_Region_Release_Reciprocal_History_Package.Instance;
      Event_T_Recv_Sync_History : Event_T_Recv_Sync_History_Package.Instance;
      Sys_Time_T_Return_History : Sys_Time_T_Return_History_Package.Instance;
      -- Event histories:
      Memory_Region_Length_Mismatch_History : Memory_Region_Length_Mismatch_History_Package.Instance;
      Memory_Region_Unavailable_History : Memory_Region_Unavailable_History_Package.Instance;
      Starting_Copy_History : Starting_Copy_History_Package.Instance;
      Finished_Copy_History : Finished_Copy_History_Package.Instance;
      Invalid_Command_Received_History : Invalid_Command_Received_History_Package.Instance;
      Copy_Timeout_History : Copy_Timeout_History_Package.Instance;
      Copy_Failure_History : Copy_Failure_History_Package.Instance;
      Command_Dropped_History : Command_Dropped_History_Package.Instance;
      -- Booleans to control assertion if message is dropped on async queue:
      Expect_Command_T_Send_Dropped : Boolean := False;
      Command_T_Send_Dropped_Count : Natural := 0;
      -- Memory regions for simulation:
      Scratch : Basic_Types.Byte_Array (0 .. 99) := [others => 15];
      Scratch_Return_Status : Memory_Manager_Enums.Memory_Request_Status.E := Memory_Manager_Enums.Memory_Request_Status.Success;
   end record;
   type Instance_Access is access all Instance;

   ---------------------------------------
   -- Initialize component heap variables:
   ---------------------------------------
   procedure Init_Base (Self : in out Instance; Queue_Size : in Natural);
   procedure Final_Base (Self : in out Instance);

   ---------------------------------------
   -- Test initialization functions:
   ---------------------------------------
   procedure Connect (Self : in out Instance);

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- This connector is used to send the command response back to the command router.
   overriding procedure Command_Response_T_Recv_Sync (Self : in out Instance; Arg : in Command_Response.T);
   -- A memory region is sent on this connector for copy.
   overriding procedure Memory_Region_Copy_T_Recv_Sync (Self : in out Instance; Arg : in Memory_Region_Copy.T);
   -- The scratch memory region is requested on this connector.
   overriding function Memory_Region_Request_T_Return (Self : in out Instance) return Memory_Region_Request.T;
   -- The memory region is released (returned) to scratch on this connector.
   overriding procedure Ided_Memory_Region_Release_Reciprocal (Self : in out Instance; Arg : in Ided_Memory_Region.T);
   -- The event send connector
   overriding procedure Event_T_Recv_Sync (Self : in out Instance; Arg : in Event.T);
   -- The system time is retrieved via this connector.
   overriding function Sys_Time_T_Return (Self : in out Instance) return Sys_Time.T;

   ---------------------------------------
   -- Invoker connector primitives:
   ---------------------------------------
   -- This procedure is called when a Command_T_Send message is dropped due to a full queue.
   overriding procedure Command_T_Send_Dropped (Self : in out Instance; Arg : in Command.T);

   -----------------------------------------------
   -- Event handler primitive:
   -----------------------------------------------
   -- Description:
   --    Events for the Memory Copier component.
   -- A memory region was received with a length less than that specified in the copy command. The length of the region requested must be the same size or greater than the length specified in the copy command.
   overriding procedure Memory_Region_Length_Mismatch (Self : in out Instance; Arg : in Invalid_Memory_Region_Length.T);
   -- Requesting the memory region was denied because it is currently in use by another component.
   overriding procedure Memory_Region_Unavailable (Self : in out Instance);
   -- Starting copy from source to destination.
   overriding procedure Starting_Copy (Self : in out Instance; Arg : in Virtual_Memory_Region_Copy.T);
   -- Finished copy from source to destination, without errors.
   overriding procedure Finished_Copy (Self : in out Instance; Arg : in Virtual_Memory_Region_Copy.T);
   -- A command was received with invalid parameters.
   overriding procedure Invalid_Command_Received (Self : in out Instance; Arg : in Invalid_Command_Info.T);
   -- A timeout occurred while waiting for a copy operation to complete.
   overriding procedure Copy_Timeout (Self : in out Instance);
   -- A copy failed.
   overriding procedure Copy_Failure (Self : in out Instance; Arg : in Memory_Region_Release.T);
   -- A command was dropped due to a full queue.
   overriding procedure Command_Dropped (Self : in out Instance; Arg : in Command_Header.T);

   -----------------------------------------------
   -- Special primitives for activating component
   -- queue:
   -----------------------------------------------
   -- Tell the component to dispatch all items off of its queue:
   not overriding function Dispatch_All (Self : in out Instance) return Natural;
   -- Tell the component to dispatch n items off of its queue:
   not overriding function Dispatch_N (Self : in out Instance; N : in Positive := 1) return Natural;

end Component.Memory_Copier.Implementation.Tester;
