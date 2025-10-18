--------------------------------------------------------------------------------
-- Test_Component_2 Component Implementation Body
--------------------------------------------------------------------------------

package body Component.Test_Component_2.Implementation is

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- Parameter updates are received on this connector.
   overriding procedure Parameter_Update_T_Modify (Self : in out Instance; Arg : in out Parameter_Update.T) is
   begin
      -- Process the parameter update, staging or fetching parameters as requested.
      Self.Process_Parameter_Update (Arg);
   end Parameter_Update_T_Modify;

   -----------------------------------------------
   -- Parameter handlers:
   -----------------------------------------------
   -- Description:
   --    Parameters for the test component.
   -- Invalid Parameter handler. This procedure is called when a parameter's type is found to be invalid:
   overriding procedure Invalid_Parameter (Self : in out Instance; Par : in Parameter.T; Errant_Field_Number : in Unsigned_32; Errant_Field : in Basic_Types.Poly_Type) is
   begin
      null;
   end Invalid_Parameter;

end Component.Test_Component_2.Implementation;
