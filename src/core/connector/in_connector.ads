with Component;
with Connector_Types; use Connector_Types;

generic
   type Connector_Type is private;
package In_Connector is
   -- Make specified type visible
   subtype T is Connector_Type;

   -- Instance variable:
   type Instance is tagged private;
   type Ptr is access all Instance;

   -- This is the subprogram definition for the invokee side of this connector.
   type Invokee_Hook is access function (A_Component : in out Component.Core_Instance'Class; Input : in T; Index : in Connector_Index_Type; Full_Queue_Behavior : in Full_Queue_Action) return Connector_Status;

   -- Public function to register the component attached
   -- to the receiving end of the connector:
   procedure Attach (Self : in out Instance; To_Component : in not null Component.Class_Access; Hook : in not null Invokee_Hook; To_Index : in Connector_Index_Type := Connector_Index_Type'First);

   -- Public function to call the function attached
   -- to the connector:
   function Call (Self : in Instance; Input : in T; Full_Queue_Behavior : in Full_Queue_Action := Drop) return Connector_Status
      with Inline => True;
   function Is_Connected (Self : in Instance) return Boolean
      with Inline => True;
private

   -- Connector instance object:
   type Instance is tagged record
      Component_Access : Component.Class_Access := null;
      Hook : Invokee_Hook := null;
      Index : Connector_Index_Type := Connector_Index_Type'First;
      Connected : Boolean := False;
   end record;

end In_Connector;
