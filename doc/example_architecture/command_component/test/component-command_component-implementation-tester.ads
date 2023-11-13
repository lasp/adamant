--------------------------------------------------------------------------------
-- Command_Component Component Tester Spec
--------------------------------------------------------------------------------

-- Includes:
with Component.Command_Component_Reciprocal;
with Printable_History;
with Command_Response.Representation;

-- This is the command component, which executes commands.
package Component.Command_Component.Implementation.Tester is

   -- Invoker connector history packages:
   package Command_Response_T_Recv_Sync_History_Package is new Printable_History (Command_Response.T, Command_Response.Representation.Image);

   -- Component class instance:
   type Instance is new Component.Command_Component_Reciprocal.Base_Instance with record
      -- The component instance under test:
      Component_Instance : aliased Component.Command_Component.Implementation.Instance;
      -- Connector histories:
      Command_Response_T_Recv_Sync_History : Command_Response_T_Recv_Sync_History_Package.Instance;
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
   -- This connector is used to register the components commands at initialization, and send command responses after execution.
   overriding procedure Command_Response_T_Recv_Sync (Self : in out Instance; Arg : in Command_Response.T);

end Component.Command_Component.Implementation.Tester;
