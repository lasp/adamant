package body In_Out_Connector is

   procedure Attach (Self : in out Instance; To_Component : in not null Component.Class_Access; Hook : in not null Invokee_Hook; To_Index : in Connector_Index_Type := Connector_Index_Type'First) is
   begin
      -- Connect the component access.
      Self.Component_Access := To_Component;
      -- Save the connector hook subprogram
      Self.Hook := Hook;
      -- Save the index:
      Self.Index := To_Index;
      -- Save the connected state.
      Self.Connected := True;
   end Attach;

   procedure Call (Self : in Instance; Input : in out T) is
   begin
      pragma Assert (Self.Connected);
      Self.Hook.all (Self.Component_Access.all, Input, Self.Index);
   end Call;

   function Is_Connected (Self : in Instance) return Boolean is
   begin
      return Self.Connected;
   end Is_Connected;

end In_Out_Connector;
