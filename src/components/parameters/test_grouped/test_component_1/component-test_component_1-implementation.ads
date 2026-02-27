--------------------------------------------------------------------------------
-- Test_Component_1 Component Implementation Spec
--------------------------------------------------------------------------------

-- Includes:
with Parameter_Update;
with Parameter_Enums; use Parameter_Enums.Parameter_Update_Status;

-- A simple component which has a parameter receive connector.
package Component.Test_Component_1.Implementation is

   -- The component class instance record:
   type Instance is new Test_Component_1.Base_Instance with private;

   -- Public subprograms to help simulate error conditions during unit testing.
   procedure Override_Parameter_Return (Self : in out Instance; Status : in Parameter_Enums.Parameter_Update_Status.E; Length : in Natural; Only_On_Update : in Boolean := False; Only_On_Fetch : in Boolean := False);
   procedure Disable_Parameter_Return_Override (Self : in out Instance);
   procedure Override_Parameter_U16 (Self : in out Instance; Value : in Packed_U16.U);
   procedure Override_Parameter_I32 (Self : in out Instance; Value : in Packed_I32.U);

private

   -- The component class instance record:
   type Instance is new Test_Component_1.Base_Instance with record
      Do_Override : Boolean := False;
      Only_On_Update : Boolean := False;
      Only_On_Fetch : Boolean := False;
      Override_Parameter_Update_Status : Parameter_Enums.Parameter_Update_Status.E := Success;
      Override_Parameter_Length : Natural := 0;
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
   -- Parameter updates are received on this connector.
   overriding procedure Parameter_Update_T_Modify (Self : in out Instance; Arg : in out Parameter_Update.T);

   -----------------------------------------------
   -- Parameter primitives:
   -----------------------------------------------
   -- Description:
   --    Parameters for the test component.

   -- Invalid parameter handler. This procedure is called when a parameter's type is found to be invalid:
   overriding procedure Invalid_Parameter (Self : in out Instance; Par : in Parameter.T; Errant_Field_Number : in Unsigned_32; Errant_Field : in Basic_Types.Poly_Type);
   -- This procedure is called when the parameters of a component have been updated. The default implementation of this
   -- subprogram in the implementation package is a null procedure. However, this procedure can, and should be implemented if
   -- something special needs to happen after a parameter update. Examples of this might be copying certain parameters to
   -- hardware registers, or performing other special functionality that only needs to be performed after parameters have
   -- been updated.
   overriding procedure Update_Parameters_Action (Self : in out Instance) is null;
   -- This function is called when the parameter operation type is "Validate". The default implementation of this
   -- subprogram in the implementation package is a function that returns "Valid". However, this function can, and should be
   -- overridden if something special needs to happen to further validate a parameter. Examples of this might be validation of
   -- certain parameters beyond individual type ranges, or performing other special functionality that only needs to be
   -- performed after parameters have been validated. Note that range checking is performed during staging, and does not need
   -- to be implemented here.
   overriding function Validate_Parameters (
      Self : in out Instance;
      Parameter_U16 : in Packed_U16.U;
      Parameter_I32 : in Packed_I32.U
   ) return Parameter_Validation_Status.E is (Parameter_Validation_Status.Valid);

end Component.Test_Component_1.Implementation;
