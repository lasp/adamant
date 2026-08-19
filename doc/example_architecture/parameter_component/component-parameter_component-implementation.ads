--------------------------------------------------------------------------------
-- Parameter_Component Component Implementation Spec
--------------------------------------------------------------------------------

-- Includes:
with Tick;
with Parameter;

-- This is the parameter component, which is configured via parameters.
package Component.Parameter_Component.Implementation is

   -- The component class instance record:
   type Instance is new Parameter_Component.Base_Instance with private;

private

   -- The component class instance record:
   type Instance is new Parameter_Component.Base_Instance with record
      Count : Unsigned_16 := 0;
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
   -- This connector provides the schedule tick for the component.
   overriding procedure Tick_T_Recv_Sync (Self : in out Instance; Arg : in Tick.T);
   -- The parameter update connector.
   overriding procedure Parameter_Update_T_Modify (Self : in out Instance; Arg : in out Parameter_Update.T);

   -----------------------------------------------
   -- Parameter primitives:
   -----------------------------------------------
   -- Description:
   --    A set of parameters for the Parameter Component.

   -- Invalid parameter handler. This procedure is called when a parameter's type is found to be invalid:
   overriding procedure Invalid_Parameter (Self : in out Instance; Par : in Parameter.T; Errant_Field_Number : in Unsigned_32; Errant_Field : in Basic_Types.Poly_Type);
   -- This procedure is called when the parameters of a component have been updated. The default implementation of this
   -- subprogram in the implementation package is a null procedure. However, this procedure can, and should be implemented if
   -- something special needs to happen after a parameter update. Examples of this might be copying certain parameters to
   -- hardware registers, or performing other special functionality that only needs to be performed after parameters have
   -- been updated.
   overriding procedure Update_Parameters_Action (Self : in out Instance);
   -- This function is called when the parameter operation type is "Validate", and once at startup by
   -- Validate_Parameter_Defaults to check the compiled-in default parameter values. The default implementation of this
   -- subprogram in the implementation package is a function that returns "Valid". However, this function can, and should be
   -- overridden if a parameter requires validation beyond its individual type range, such as enforcing a relationship
   -- between parameters. Note that range checking is performed during staging, and does not need to be implemented here.
   -- The startup call runs before components are connected or initialized, so an override must be a pure function of its
   -- parameter arguments - it must not invoke connectors or rely on state established during component initialization.
   overriding function Validate_Parameters (
      Self : in out Instance;
      Start_Count : in Packed_U16.U;
      Hello_World_Value : in Packed_U16.U
   ) return Parameter_Validation_Status.E is (Parameter_Validation_Status.Valid);

end Component.Parameter_Component.Implementation;
