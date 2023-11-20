--------------------------------------------------------------------------------
-- Parameter_Component Component Tester Spec
--------------------------------------------------------------------------------

-- Includes:
with Component.Parameter_Component_Reciprocal;

-- This is the parameter component, which is configured via parameters.
package Component.Parameter_Component.Implementation.Tester is

   use Component.Parameter_Component_Reciprocal;
   -- Component class instance:
   type Instance is new Component.Parameter_Component_Reciprocal.Base_Instance with record
      -- The component instance under test:
      Component_Instance : aliased Component.Parameter_Component.Implementation.Instance;
   end record;
   type Instance_Access is access all Instance;

   ---------------------------------------
   -- Test initialization functions:
   ---------------------------------------
   procedure Connect (Self : in out Instance);

   -----------------------------------------------
   -- Special primitives for aiding in the staging,
   -- fetching, and updating of parameters
   -----------------------------------------------
   -- Stage a parameter value within the component
   not overriding function Stage_Parameter (Self : in out Instance; Par : in Parameter.T) return Parameter_Update_Status.E;
   -- Fetch the value of a parameter with the component
   not overriding function Fetch_Parameter (Self : in out Instance; Id : in Parameter_Types.Parameter_Id; Par : out Parameter.T) return Parameter_Update_Status.E;
   -- Tell the component it is OK to atomically update all of its
   -- working parameter values with the staged values.
   not overriding function Update_Parameters (Self : in out Instance) return Parameter_Update_Status.E;

end Component.Parameter_Component.Implementation.Tester;
