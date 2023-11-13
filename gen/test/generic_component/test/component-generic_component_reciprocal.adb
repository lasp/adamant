package body Component.Generic_Component_Reciprocal is
   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   not overriding function Generic_Type_1_Recv_Sync_Access (Self : in Base_Instance; Index : in Connector_Index_Type := Connector_Index_Type'First) return not null Generic_Type_1_Recv_Sync_Connector.Invokee_Hook is
      Ignore : Base_Instance renames Self;
      subtype Index_Type is Generic_Type_1_Recv_Sync_Index;
   begin
      pragma Assert (Index = Index_Type'Last and then Index = Index_Type'First,
         "Cannot connect to invokee connector Generic_Type_1_Recv_Sync at index " & Connector_Index_Type'Image (Index) & ". " &
         "Index must be between " & Index_Type'Image (Index_Type'First) & " and " & Index_Type'Image (Index_Type'Last) & ".");
      return Generic_Type_1_Recv_Sync_Hook'Access;
   end Generic_Type_1_Recv_Sync_Access;

   function Generic_Type_1_Recv_Sync_Hook (Class_Self : in out Component.Core_Instance'Class; Arg : in Generic_Type_1; Index : in Connector_Index_Type := Connector_Index_Type'First; Ignore : in Full_Queue_Action := Drop) return Connector_Types.Connector_Status is
      Self : Base_Instance renames Base_Instance (Class_Self);
      -- Ignore the index, since we don't allocate arrayed connectors in the tester.
      Ignore_Index : Connector_Index_Type renames Index;
   begin
      -- Call up to the derived class' for execution:
      Base_Instance'Class (Self).Generic_Type_1_Recv_Sync (Arg);
      return Self.Connector_Generic_Type_1_Recv_Sync_Status;
   end Generic_Type_1_Recv_Sync_Hook;

   ---------------------------------------
   -- Invoker connector primitives:
   ---------------------------------------
   not overriding procedure Attach_Generic_Type_2_Send (Self : in out Base_Instance; To_Component : in not null Component.Class_Access; Hook : in not null Generic_Type_2_Send_Connector.Invokee_Hook; To_Index : in Connector_Index_Type := Connector_Index_Type'First) is
   begin
      Self.Connector_Generic_Type_2_Send.Attach (To_Component, Hook, To_Index);
   end Attach_Generic_Type_2_Send;

   not overriding procedure Generic_Type_2_Send (Self : in out Base_Instance; Arg : in Generic_Type_2; Full_Queue_Behavior : in Full_Queue_Action := Drop) is
      Ret : Connector_Status;
   begin
      Ret := Generic_Type_2_Send_Connector.Call (Self.Connector_Generic_Type_2_Send, Arg, Full_Queue_Behavior);
      case Ret is
         -- Send worked fine, just continue on.
         when Success => null;
         when Message_Dropped =>
            pragma Assert (False, "A message should never be dropped from a synchronous connection.");
      end case;
   end Generic_Type_2_Send;

   ---------------------------------------------------
   -- Definition of cycle function for task execution:
   ---------------------------------------------------
   -- Passive component queue implementation for cycle.
   -- Tester components NEVER have threads.
   -- This method is implemented, but if called will throw an assertion.
   overriding procedure Cycle (Self : in out Base_Instance) is
      Ignore : Base_Instance renames Self;
   begin
      -- This is a passive component, meaning it CANNOT be tasked. If the component
      -- is given a task we quit.
      pragma Assert (False);
   end Cycle;

end Component.Generic_Component_Reciprocal;
