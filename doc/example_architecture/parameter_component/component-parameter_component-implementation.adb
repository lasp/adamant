--------------------------------------------------------------------------------
-- Parameter_Component Component Implementation Body
--------------------------------------------------------------------------------

with Ada.Text_IO; use Ada.Text_IO;

package body Component.Parameter_Component.Implementation is

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- The parameter update connector.
   overriding procedure Parameter_Update_T_Modify (Self : in out Instance; Arg : in out Parameter_Update.T) is
   begin
      -- Process the parameter update, staging or fetching parameters as requested.
      Self.Process_Parameter_Update (Arg);
   end Parameter_Update_T_Modify;

   -- This connector provides the schedule tick for the component.
   overriding procedure Tick_T_Recv_Sync (Self : in out Instance; Arg : in Tick.T) is
      Ignore : Tick.T renames Arg;
   begin
      -- If there are any staged updates to our parameters, then perform the copy now to update
      -- our local parameter values.
      Self.Update_Parameters;

      -- Print hello world if our count has reached the parameter
      -- value.
      if Self.Count = Self.Hello_World_Value.Value then
         Put_Line ("Hello, World!");
      end if;

      -- Increment the count:
      Self.Count := @ + 1;
   end Tick_T_Recv_Sync;

   -- This procedure is called when the parameters of a component have been updated. The default implementation of this
   -- subprogram in the implementation package is a null procedure. However, this procedure can, and should be implemented if
   -- something special needs to happen after a parameter update. Examples of this might be copying certain parameters to
   -- hardware registers, or performing other special functionality that only needs to be performed after parameters have
   -- been updated.
   overriding procedure Update_Parameters_Action (Self : in out Instance) is
   begin
      -- Set the current count to the start count value every time the component's parameters
      -- are updated.
      Self.Count := Self.Start_Count.Value;
   end Update_Parameters_Action;

   -----------------------------------------------
   -- Parameter handlers:
   -----------------------------------------------
   -- Description:
   --    A set of parameters for the Parameter Component.
   -- Invalid Parameter handler. This procedure is called when a parameter's type is found to be invalid:
   overriding procedure Invalid_Parameter (Self : in out Instance; Par : in Parameter.T; Errant_Field_Number : in Unsigned_32; Errant_Field : in Basic_Types.Poly_Type) is
   begin
      Put_Line ("Oh, no! A parameter was sent that was malformed.");
   end Invalid_Parameter;

end Component.Parameter_Component.Implementation;
