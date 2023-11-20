-- Includes:
with Connector_Types;
use Connector_Types;
with In_Connector;
with Aa;
with Serializer_Types;

-- This is a queued test component with generic connectors.
generic
   -- Ignore warnings having to do with "not referenced" generic types. These are often
   -- referenced in the implementation package instead. But we want to include them here
   -- to make sure the model matches the implementation.
   pragma Warnings (Off);
   type Generic_Type_1 is private;
   type Generic_Type_2 is private;
   -- A method that returns the serialized length of an item of type Generic_Type_2. This is useful for serializing variable length packed types onto the queue.
   with function Serialized_Length (Src : in Generic_Type_2; Num_Bytes_Serialized : out Natural) return Serializer_Types.Serialization_Status;
   -- Turn warnings back on.
   pragma Warnings (On);
package Component.Generic_Queued_Reciprocal is

   -- Define invoker connector packages:
   package Generic_Type_2_Send_Connector is new In_Connector (Generic_Type_2);
   package Aa_T_Send_Connector is new In_Connector (Aa.T);

   -- Define invokee connector packages:
   package Generic_Type_1_Recv_Sync_Connector is new In_Connector (Generic_Type_1);
   -- Define index type for invokee connector. Range is always 1 .. 1 for tester components since testers do not have arrayed invokee connectors.
   subtype Generic_Type_1_Recv_Sync_Index is Connector_Index_Type range Connector_Index_Type'First .. Connector_Index_Type'First;

   -- Base class instance record:
   type Base_Instance is abstract new Component.Core_Instance with record
      -- Invoker connector objects:
      Connector_Generic_Type_2_Send : Generic_Type_2_Send_Connector.Instance;
      Connector_Aa_T_Send : Aa_T_Send_Connector.Instance;
      -- Invokee connector status return values. You can change these during test to simulate
      -- different connector status return values:
      Connector_Generic_Type_1_Recv_Sync_Status : Connector_Types.Connector_Status := Connector_Types.Success;
   end record;

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- The output connector
   -- Abstract connector procedure to be overridden by child:
   not overriding procedure Generic_Type_1_Recv_Sync (Self : in out Base_Instance; Arg : in Generic_Type_1) is abstract;
   -- Function which returns the hook for this connector. Used when attaching this connector to an invoker connector:
   not overriding function Generic_Type_1_Recv_Sync_Access (Self : in Base_Instance; Index : in Connector_Index_Type := Connector_Index_Type'First) return not null Generic_Type_1_Recv_Sync_Connector.Invokee_Hook;

   ---------------------------------------
   -- Invoker connector primitives:
   ---------------------------------------
   -- The input connector
   -- Function to attach this invoker connector to an invokee connector:
   not overriding procedure Attach_Generic_Type_2_Send (Self : in out Base_Instance; To_Component : in not null Component.Class_Access; Hook : in not null Generic_Type_2_Send_Connector.Invokee_Hook; To_Index : in Connector_Index_Type := Connector_Index_Type'First);

   -- The sync input connector
   -- Function to attach this invoker connector to an invokee connector:
   not overriding procedure Attach_Aa_T_Send (Self : in out Base_Instance; To_Component : in not null Component.Class_Access; Hook : in not null Aa_T_Send_Connector.Invokee_Hook; To_Index : in Connector_Index_Type := Connector_Index_Type'First);

   ---------------------------------------------------
   -- Definition of cycle function for task execution:
   ---------------------------------------------------
   -- Passive component queue implementation for cycle.
   -- Tester components NEVER have threads.
   -- This method is implemented, but if called will throw an assertion.
   overriding procedure Cycle (Self : in out Base_Instance);

   ---------------------------------------
   -- Invokee connector hooks which
   -- dispatch invokee calls to the correct
   -- abstract function defined in the
   -- child package:
   ---------------------------------------
   -- The output connector
   function Generic_Type_1_Recv_Sync_Hook (Class_Self : in out Component.Core_Instance'Class; Arg : in Generic_Type_1; Index : in Connector_Index_Type := Connector_Index_Type'First; Ignore : in Full_Queue_Action := Drop) return Connector_Types.Connector_Status;

   ---------------------------------------
   -- Invoker connector functions
   -- for use in the child package:
   ---------------------------------------
   -- The input connector
   not overriding procedure Generic_Type_2_Send (Self : in out Base_Instance; Arg : in Generic_Type_2; Full_Queue_Behavior : in Full_Queue_Action := Drop);
   -- This abstract procedure must be overridden in the child package to specify the behavior when a Generic_Type_2_Send message is dropped due to a full queue.
   not overriding procedure Generic_Type_2_Send_Dropped (Self : in out Base_Instance; Arg : in Generic_Type_2) is abstract;
   -- The sync input connector
   not overriding procedure Aa_T_Send (Self : in out Base_Instance; Arg : in Aa.T; Full_Queue_Behavior : in Full_Queue_Action := Drop);

end Component.Generic_Queued_Reciprocal;
