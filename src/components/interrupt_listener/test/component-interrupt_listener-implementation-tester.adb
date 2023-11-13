--------------------------------------------------------------------------------
-- Interrupt_Listener Component Tester Body
--------------------------------------------------------------------------------

package body Component.Interrupt_Listener.Implementation.Tester is

   ---------------------------------------
   -- Test initialization functions:
   ---------------------------------------
   procedure Connect (Self : in out Instance) is
   begin
      Self.Attach_Interrupt_Data_Type_Get (Self.Component_Instance'Unchecked_Access, Self.Component_Instance.Interrupt_Data_Type_Return_Access);
   end Connect;

end Component.Interrupt_Listener.Implementation.Tester;
