--------------------------------------------------------------------------------
-- Fault_Component Component Tester Spec
--------------------------------------------------------------------------------

-- Includes:
with Component.Fault_Component_Reciprocal;
with Sys_Time;
with Printable_History;
with Sys_Time.Representation;
with Fault.Representation;
with Fault;
with Packed_U32.Representation;

-- This is the fault component, which sends faults.
package Component.Fault_Component.Implementation.Tester is

   use Component.Fault_Component_Reciprocal;
   -- Invoker connector history packages:
   package Sys_Time_T_Return_History_Package is new Printable_History (Sys_Time.T, Sys_Time.Representation.Image);
   package Fault_T_Recv_Sync_History_Package is new Printable_History (Fault.T, Fault.Representation.Image);

   -- Fault history packages:
   package Discontinuous_Time_Fault_History_Package is new Printable_History (Packed_U32.T, Packed_U32.Representation.Image);
   package Zero_Time_Fault_History_Package is new Printable_History (Natural, Natural'Image);

   -- Component class instance:
   type Instance is new Component.Fault_Component_Reciprocal.Base_Instance with record
      -- The component instance under test:
      Component_Instance : aliased Component.Fault_Component.Implementation.Instance;
      -- Connector histories:
      Sys_Time_T_Return_History : Sys_Time_T_Return_History_Package.Instance;
      Fault_T_Recv_Sync_History : Fault_T_Recv_Sync_History_Package.Instance;
      -- Fault histories:
      Discontinuous_Time_Fault_History : Discontinuous_Time_Fault_History_Package.Instance;
      Zero_Time_Fault_History : Zero_Time_Fault_History_Package.Instance;
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
   -- This connector is used to fetch the current system time.
   overriding function Sys_Time_T_Return (Self : in out Instance) return Sys_Time.T;
   -- This connector is used to send out a fault.
   overriding procedure Fault_T_Recv_Sync (Self : in out Instance; Arg : in Fault.T);

   -----------------------------------------------
   -- Fault handler primitive:
   -----------------------------------------------
   -- Description:
   --    A set of faults for the Fault Component.
   -- A discontinuous time was detected by the component.
   overriding procedure Discontinuous_Time_Fault (Self : in out Instance; Arg : in Packed_U32.T);
   -- A time restart at zero seconds epoch was detected by the component.
   overriding procedure Zero_Time_Fault (Self : in out Instance);

end Component.Fault_Component.Implementation.Tester;
