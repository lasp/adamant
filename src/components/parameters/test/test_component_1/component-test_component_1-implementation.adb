--------------------------------------------------------------------------------
-- Test_Component_1 Component Implementation Body
--------------------------------------------------------------------------------

package body Component.Test_Component_1.Implementation is

   procedure Override_Parameter_Return (Self : in out Instance; Status : in Parameter_Enums.Parameter_Update_Status.E; Length : in Natural; Only_On_Update : in Boolean := False; Only_On_Fetch : in Boolean := False) is
   begin
      Self.Do_Override := True;
      Self.Override_Parameter_Update_Status := Status;
      Self.Override_Parameter_Length := Length;
      Self.Only_On_Update := Only_On_Update;
      Self.Only_On_Fetch := Only_On_Fetch;
   end Override_Parameter_Return;

   procedure Disable_Parameter_Return_Override (Self : in out Instance) is
   begin
      Self.Do_Override := False;
      Self.Only_On_Update := False;
   end Disable_Parameter_Return_Override;

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- Parameter updates are received on this connector.
   overriding procedure Parameter_Update_T_Modify (Self : in out Instance; Arg : in out Parameter_Update.T) is
      use Parameter_Enums.Parameter_Operation_Type;
   begin
      -- If in override mode, then don't update the parameter, instead override the return value
      -- to simulate failure.
      if Self.Do_Override and then
         (not Self.Only_On_Update or else (Self.Only_On_Update and then Arg.Operation = Update)) and then
         (not Self.Only_On_Fetch or else (Self.Only_On_Fetch and then Arg.Operation = Fetch))
      then
         Arg.Status := Self.Override_Parameter_Update_Status;
         Arg.Param.Header.Buffer_Length := Self.Override_Parameter_Length;
         pragma Annotate (GNATSAS, False_Positive, "range check", "Override_Parameter_Length set by test configuration, within expected range");
      else
         -- Process the parameter update, staging or fetching parameters as requested.
         Self.Process_Parameter_Update (Arg);
      end if;
      pragma Annotate (GNATSAS, Intentional, "condition predetermined", "Test component flags may be set to constant values for specific test scenarios");
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

end Component.Test_Component_1.Implementation;
