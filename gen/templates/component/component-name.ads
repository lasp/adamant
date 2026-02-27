--------------------------------------------------------------------------------
-- {{ formatType(model_name) }} {{ formatType(model_type) }} Base Spec
--
-- Generated from {{ filename }} on {{ time }}.
--------------------------------------------------------------------------------

{% if base_ads_includes %}
-- Includes:
{% for include in base_ads_includes %}
with {{ include }};
{% if include in ["Interfaces", "Connector_Types"] %}
use {{ include }};
{% endif %}
{% endfor %}

{% endif %}
{% if description %}
{{ printMultiLine(description, '-- ') }}
{% endif %}
{% if generic %}
generic
   -- Ignore warnings having to do with "not referenced" generic types. These are often
   -- referenced in the implementation package instead. But we want to include them here
   -- to make sure the model matches the implementation.
   pragma Warnings (Off);
{% if generic.description %}
{{ printMultiLine(generic.description, '   -- ') }}
   --
{% endif %}
{% for formal_parameter in generic.formal_parameters %}
{% if formal_parameter.description %}
{{ printMultiLine(formal_parameter.description, '   -- ') }}
{% endif %}
   {{ formal_parameter.formal_type }}
{% endfor %}
   -- Turn warnings back on.
   pragma Warnings (On);
{% endif %}
package Component.{{ name }} is
{% if preamble %}

   -- Preamble code:
{{ printMultiLine(preamble, "   ", 10000) }}
{% endif %}

   -- Base class instance:
   type Base_Instance is abstract new Component.Core_Instance with private;
   type Base_Class_Access is access all Base_Instance'Class;

{% if connectors.requires_queue() %}
   -- The maximum size of an element (in bytes) that can be put onto the queue, including any bytes used for overhead
   -- when storing the element. This value can be used to aid in sizing the queue for this component inside the assembly.
   Max_Queue_Element_Size : constant Natural;
   -- Primitive to return the value above, sometimes this is more convenient to use.
   not overriding function Get_Max_Queue_Element_Size (Self : in Base_Instance) return Natural
      with Inline => True;

   -- Get the current percent usage of the component's internal queue, if there
   -- is one, else assert. This function is used by the queue monitor component
   -- as a backdoor convenience to bypass the connector system, reducing complexity
   -- of reporting queue usage for every component.
   overriding function Get_Queue_Current_Percent_Used (Self : in out Base_Instance) return Basic_Types.Byte
      with Inline => True;

   -- Get the maximum percent usage of the component's internal queue, if there
   -- is one, else assert. This function is used by the queue monitor component
   -- as a backdoor convenience to bypass the connector system, reducing complexity
   -- of reporting queue usage for every component. i.e. "high water mark"
   overriding function Get_Queue_Maximum_Percent_Used (Self : in out Base_Instance) return Basic_Types.Byte
      with Inline => True;

{% endif %}
{% if init %}
   --------------------------------------------------
   -- Abstract subprogram for implementation init method:
   --------------------------------------------------
{% if init.description %}
{{ printMultiLine(init.description, '   -- ') }}
{% endif %}
{% if init.parameters %}
   --
{{ printMultiLine("Init Parameters:", '   -- ') }}
{% for p in init.parameters %}
{{ printMultiLine(p.name + " : " + p.type + ((" - " + p.description) if (p.description) else ""), '   -- ') }}
{% endfor %}
   --
{% endif %}
   not overriding procedure Init (Self : in out Base_Instance{% if init.parameters %}; {{ init.parameter_declaration_string() }}{% endif %}) is abstract;

{% endif %}
{% if init_base %}
   ------------------------------------------
   -- Initialize and finalize heap variables:
   ------------------------------------------
{% if init_base.description %}
{{ printMultiLine(init_base.description, '   -- ') }}
{% endif %}
{% if init_base.parameters %}
   --
{{ printMultiLine("Init Base Parameters:", '   -- ') }}
{% for p in init_base.parameters %}
{{ printMultiLine(p.name + " : " + p.type + ((" - " + p.description) if (p.description) else ""), '   -- ') }}
{% endfor %}
   --
{% endif %}
   not overriding procedure Init_Base (Self : in out Base_Instance{% if init_base.parameters %}; {{ init_base.parameter_declaration_string() }}{% endif %});
{% if connectors.of_kind("recv_async") or connectors.n_arrayed().invoker() %}
   not overriding procedure Final_Base (Self : in out Base_Instance);
{% endif %}

{% endif %}
{% if set_id_bases %}
   -----------------------------------------------------------------------
   -- Initialize the Id_Bases for any commands, data products, or events:
   -----------------------------------------------------------------------
{% if set_id_bases.description %}
{{ printMultiLine(set_id_bases.description, '   -- ') }}
{% endif %}
{% if set_id_bases.parameters %}
   --
{{ printMultiLine("Set Id Bases Parameters:", '   -- ') }}
{% for p in set_id_bases.parameters %}
{{ printMultiLine(p.name + " : " + p.type + ((" - " + p.description) if (p.description) else ""), '   -- ') }}
{% endfor %}
   --
{% endif %}
   not overriding procedure Set_Id_Bases (Self : in out Base_Instance{% if set_id_bases.parameters %}; {{ set_id_bases.parameter_declaration_string() }}{% endif %});

{% endif %}
{% if map_data_dependencies %}
   -----------------------------------------------------------------------
   -- Initialize the IDs for the component's data dependencies
   -----------------------------------------------------------------------
{% if map_data_dependencies.description %}
{{ printMultiLine(map_data_dependencies.description, '   -- ') }}
{% endif %}
{% if map_data_dependencies.parameters %}
   --
{{ printMultiLine("Set Data Dependency Ids Parameters:", '   -- ') }}
{% for p in map_data_dependencies.parameters %}
{{ printMultiLine(p.name + " : " + p.type + ((" - " + p.description) if (p.description) else ""), '   -- ') }}
{% endfor %}
   --
{% endif %}
   not overriding procedure Map_Data_Dependencies (Self : in out Base_Instance{% if map_data_dependencies.parameters %}; {{ map_data_dependencies.parameter_declaration_string() }}{% endif %});

{% endif %}
{% if connectors.invokee() %}
   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
{% for connector in connectors.invokee() %}
{% if connector.description %}
{{ printMultiLine(connector.description, '   -- ') }}
{% endif %}
   -- Define connector package:
{% if connector.common and not generic %}
   package {{ connector.name }}_Connector renames Common_Connectors.{{ connector.common_connector_package }};
{% elif connector.kind == "return" %}
   package {{ connector.name }}_Connector is new {{ connector.connector_package }} ({{ connector.return_type }});
{% elif connector.kind == "service" %}
   package {{ connector.name }}_Connector is new {{ connector.connector_package }} ({{ connector.type }}, {{ connector.return_type }});
{% else %}
   package {{ connector.name }}_Connector is new {{ connector.connector_package }} ({{ connector.type }});
{% endif %}
   -- Define index type for invokee connector.
{% if connector.count == 0   %}
   subtype {{ connector.name }}_Index is Connector_Index_Type;
{% elif connector.count > 1 %}
   subtype {{ connector.name }}_Index is Connector_Index_Type range Connector_Index_Type'First .. Connector_Index_Type'First + {{ connector.count }} - 1;
{% else %}
   subtype {{ connector.name }}_Index is Connector_Index_Type range Connector_Index_Type'First .. Connector_Index_Type'First;
{% endif %}
   -- Abstract connector procedure to be overridden by child:
{% if connector.kind == "return" %}
   not overriding function {{ connector.name }} (Self : in out Base_Instance{% if connector.count == 0 or connector.count > 1 %}; Index : in {{ connector.name }}_Index{% endif %}) return {{ connector.return_type }} is abstract;
{% elif connector.kind == "service" %}
   not overriding function {{ connector.name }} (Self : in out Base_Instance{% if connector.count == 0 or connector.count > 1 %}; Index : in {{ connector.name }}_Index{% endif %}; Arg : {{ connector.mode }} {{ connector.type }}) return {{ connector.return_type }} is abstract;
{% else %}
   not overriding procedure {{ connector.name }} (Self : in out Base_Instance{% if connector.count == 0 or connector.count > 1 %}; Index : in {{ connector.name }}_Index{% endif %}; Arg : {{ connector.mode }} {{ connector.type }}) is abstract;
{% endif %}
{% if connector.kind == "recv_async" %}
   -- This abstract procedure must be overridden in the child package to specify the behavior when a {{ connector.name }} message is dropped due to a full queue.
   not overriding procedure {{ connector.name }}_Dropped (Self : in out Base_Instance{% if connector.type %}{% if connector.count == 0 or connector.count > 1 %}; Index : in {{ connector.name }}_Index{% endif %}; Arg : in {{ connector.type }}{% endif %}) is abstract;
{% endif %}
   -- Function which returns the hook for this connector. Used when attaching this connector to an invoker connector:
   not overriding function {{ connector.name }}_Access (Self : in Base_Instance; Index : in Connector_Index_Type{% if connector.count == 1 %} := Connector_Index_Type'First{% endif %}) return not null {{ connector.name }}_Connector.Invokee_Hook
      with Inline => True;

{% endfor %}
{% endif %}
{% if connectors.invoker() %}
   ---------------------------------------
   -- Invoker connector primitives:
   ---------------------------------------
{% for connector in connectors.invoker() %}
{% if connector.description %}
{{ printMultiLine(connector.description, '   -- ') }}
{% endif %}
   -- Define connector package:
{% if connector.common and not generic %}
   package {{ connector.name }}_Connector renames Common_Connectors.{{ connector.common_connector_package }};
{% elif connector.kind == "get" %}
   package {{ connector.name }}_Connector is new {{ connector.connector_package }} ({{ connector.return_type }});
{% elif connector.kind == "request" %}
   package {{ connector.name }}_Connector is new {{ connector.connector_package }} ({{ connector.type }}, {{ connector.return_type }});
{% else %}
   package {{ connector.name }}_Connector is new {{ connector.connector_package }} ({{ connector.type }});
{% endif %}
   -- Function to attach this invoker connector to an invokee connector:
{% if connector.count == 0   %}
   subtype {{ connector.name }}_Index is Connector_Index_Type;
   type {{ connector.name }}_Array is array ({{ connector.name }}_Index range <>) of {{ connector.name }}_Connector.Instance;
   type {{ connector.name }}_Array_Access is access {{ connector.name }}_Array;
{% elif connector.count > 1 %}
   subtype {{ connector.name }}_Index is Connector_Index_Type range Connector_Index_Type'First .. Connector_Index_Type'First + {{ connector.count }} - 1;
   type {{ connector.name }}_Array is array ({{ connector.name }}_Index) of {{ connector.name }}_Connector.Instance;
{% endif %}
   not overriding procedure Attach_{{ connector.name }} (Self : in out Base_Instance{% if connector.count == 0 or connector.count > 1 %}; From_Index : in {{ connector.name }}_Index{% endif %}; To_Component : in not null Component.Class_Access; Hook : in not null {{ connector.name }}_Connector.Invokee_Hook; To_Index : in Connector_Index_Type := Connector_Index_Type'First)
      with Inline => True;
{% if connector.kind == "send" %}
   -- This abstract procedure must be overridden in the child package to specify the behavior when a {{ connector.name }} message is dropped due to a full queue.
   not overriding procedure {{ connector.name }}_Dropped (Self : in out Base_Instance{% if connector.count == 0 or connector.count > 1 %}; Index : in {{ connector.name }}_Index{% endif %}; Arg : in {{ connector.type }}) is abstract;
{% endif %}

{% endfor %}
{% endif %}
{% if execution in ["active", "either"] and connectors.requires_queue() %}
   -- Public function to gracefully end the task, if active.
   not overriding procedure Stop_Task (Self : in out Base_Instance);

{% endif %}
{% if commands %}
   -----------------------------------------------
   -- Abstract command handlers to be overridden:
   -----------------------------------------------
{% if commands.description %}
   -- Description:
{{ printMultiLine(commands.description, '   --    ') }}

{% endif %}
   use Command_Enums;
{% for command in commands %}
{% if command.description %}
{{ printMultiLine(command.description, '   -- ') }}
{% endif %}
   not overriding function {{ command.name }} (Self : in out Base_Instance{% if command.type %}; Arg : in {{ command.type }}{% endif %}) return Command_Execution_Status.E is abstract;
{% endfor %}
   -- Invalid command handler. This procedure is called when a command's arguments are found to be invalid:
   not overriding procedure Invalid_Command (Self : in out Base_Instance; Cmd : in Command.T; Errant_Field_Number : in Unsigned_32; Errant_Field : in Basic_Types.Poly_Type) is abstract;

{% endif %}
{% if parameters %}
   use Parameter_Enums;
   -----------------------------------------------
   -- Abstract parameter procedures to be overridden:
   -----------------------------------------------
{% if parameters.description %}
   -- Description:
{{ printMultiLine(parameters.description, '   --    ') }}
{% endif %}
   -- Invalid parameter handler. This procedure is called when a parameter's arguments are found to be invalid:
   not overriding procedure Invalid_Parameter (Self : in out Base_Instance; Par : in Parameter.T; Errant_Field_Number : in Unsigned_32; Errant_Field : in Basic_Types.Poly_Type) is abstract;

   -- This procedure is called when the parameters of a component have been updated. The default implementation of this
   -- subprogram in the implementation package is a null procedure. However, this procedure can, and should be overridden if
   -- something special needs to happen after a parameter update. Examples of this might be copying certain parameters to
   -- hardware registers, or performing other special functionality that only needs to be performed after parameters have
   -- been updated.
   not overriding procedure Update_Parameters_Action (Self : in out Base_Instance) is abstract;

   -- This function is called when the parameter operation type is "Validate". The default implementation of this
   -- subprogram in the implementation package is a function that returns "Valid". However, this function can, and should be
   -- overridden if something special needs to happen to further validate a parameter. Examples of this might be validation of
   -- certain parameters beyond individual type ranges, or performing other special functionality that only needs to be
   -- performed after parameters have been validated. Note that range checking is performed during staging, and does not need
   -- to be implemented here.
   not overriding function Validate_Parameters (
      Self : in out Base_Instance;
{% for par in parameters %}
      {{ par.name }} : in {% if par.type_package %}{{ par.type_package }}.U{% else %}{{ par.type }}{% endif %}{{ ";" if not loop.last }}
{% endfor %}
   ) return Parameter_Validation_Status.E is abstract;

{% endif %}
{% if data_dependencies %}
   -----------------------------------------------
   -- Abstract data dependency procedures to be overridden:
   -----------------------------------------------
{% if data_dependencies.description %}
   -- Description:
{{ printMultiLine(data_dependencies.description, '   --    ') }}
{% endif %}
   -- Function which retrieves a data dependency. This should be overridden by the implementation to call the correct connector.
   not overriding function Get_Data_Dependency (Self : in out Base_Instance; Id : in Data_Product_Types.Data_Product_Id) return Data_Product_Return.T is abstract;

   -- Invalid data dependency handler. This procedure is called when a data dependency's id or length are found to be invalid:
   not overriding procedure Invalid_Data_Dependency (Self : in out Base_Instance; Id : in Data_Product_Types.Data_Product_Id; Ret : in Data_Product_Return.T) is abstract;

{% endif %}
{% if tasks.has_subtasks %}
   -------------------------------------------------------
   -- Definition of subtasks functions for task execution:
   -------------------------------------------------------
{% for subtask in tasks.subtask_list %}
{% if subtask.description %}
{{ printMultiLine(subtask.description, '   -- ') }}
{% endif %}
   procedure {{ subtask.name }} (Self : in out Base_Instance) is abstract;

   -- Task type, which calls the {{ subtask.name }} procedure above:
   task type {{ subtask.name }}_Task (
      Task_Data : Task_Types.Task_Info_Access;
      Class_Self : Component.{{ name }}.Base_Class_Access;
      Signal : not null access Ada.Synchronous_Task_Control.Suspension_Object;
      Pri : System.Priority;
      Stack_Size : Natural;
      Secondary_Stack_Size : Natural
   ) -- Set the priority and stack size for the task:
      with Priority => Pri,
             Storage_Size => Stack_Size,
             Secondary_Stack_Size => Secondary_Stack_Size;

{% endfor %}
{% endif %}
private
{% if connectors.requires_queue() %}
   -------------------------------------------
   -- Types for handling asynchronous messages
   -- on the queue:
   -------------------------------------------
   -- Define the connector identifier enum:
   type Connector_Identifier_Enum is (
      Quit,
{% for connector in connectors.of_kind('recv_async') %}
      {{ connector.name }}{{ "," if not loop.last }}
{% endfor %}
   );
   for Connector_Identifier_Enum use (
      Quit => 0,
{% for connector in connectors.of_kind('recv_async') %}
      {{ connector.name }} => {{ loop.index }}{{ "," if not loop.last }}
{% endfor %}
   );

{% if connectors.arrayed_invokee() %}
   -- Define connector identifier record, which also includes
   -- room for the index that the connector is being invoked on.
   type Connector_Identifier_Type is record
      Id : Connector_Identifier_Enum;
      Index : Connector_Index_Type;
   end record
      with Size => 24,
             Object_Size => 24,
             Value_Size => 24,
             Alignment => 1,
             Volatile => False;

   -- Define the size of the record to be compact to save space on
   -- the queue (24 bits).
   for Connector_Identifier_Type use record
      Id at 0 range 0 .. 7;
      Index at 0 range 8 .. 23;
   end record;
{% else %}
   -- Define packed connector identifier record.
   type Connector_Identifier_Type is record
      Id : Connector_Identifier_Enum;
   end record
      with Size => 8,
           Object_Size => 8,
           Value_Size => 8,
           Alignment => 1,
           Volatile => False;

   -- Define the size of the record to be compact to save space on
   -- the queue (24 bits).
   for Connector_Identifier_Type use record
      Id at 0 range 0 .. 7;
   end record;
{% endif %}

{% if connectors.requires_priority_queue() %}
   -- Define the queue priority type:
   type Queue_Priority_Type is new Natural range 0 .. 255;

   -- Define the element that will go on the queue to track the enqueued
   -- element's priority and type:
   type Priority_Element is record
      Priority : Queue_Priority_Type;
      Id_Record : Connector_Identifier_Type;
   end record;

   -- Define the comparison functions required for the priority queue:
   function Priority_Element_Greater_Than (A, B : in Priority_Element) return Boolean is (A.Priority > B.Priority);
   function Priority_Element_Equal_To (A, B : in Priority_Element) return Boolean is (A.Priority = B.Priority);

   -- Create the queue package for this component:
   package Queue_Package is new Protected_Priority_Queue (
      Priority_Type => Priority_Element,
      Priority_Greater_Than => Priority_Element_Greater_Than,
      Priority_Equal_To => Priority_Element_Equal_To
   );
{% else %}
   -- Create the queue package for this component:
   pragma Warnings (Off, "overlay changes scalar storage order");
   package Queue_Package is new Labeled_Queue (Label_Type => Connector_Identifier_Type);
   pragma Warnings (On, "overlay changes scalar storage order");
{% endif %}

   -- Calculate the maximum message size that will be
   -- stored inside of the component queue:
   Max_Message_Size : constant Natural :=
{% for connector in connectors.of_kind('recv_async') %}
{% if connector.generic %}
      Integer'Max (({{ connector.parameter.type }}'Object_Size - 1) / 8 + 1,
{% elif connector.type_model %}
      Integer'Max ({{ connector.parameter.type_package }}.Size_In_Bytes,
{% else %}
      Integer'Max (({{ connector.parameter.type }}'Object_Size - 1) / 8 + 1,
{% endif %}
{% endfor %}
      0{% for connector in connectors.of_kind('recv_async') %}){% endfor %};

   -- Declare byte array type of the maximum message size:
   subtype Queue_Byte_Array is Basic_Types.Byte_Array (0 .. Max_Message_Size - 1);

   -- Resolve public maximum element size as the maximum data message size + the queue overhead size (in bytes):
   Max_Queue_Element_Size : constant Natural := Max_Message_Size + Queue_Package.Element_Storage_Overhead;

   -- Private invokee connector dispatch functions:
   not overriding procedure Dispatch_Quit (Ignore : in out Base_Instance;{% if connectors.arrayed_invokee() %} Ignore_Idx : in Connector_Index_Type;{% endif %} Ignore_Bytes : in Basic_Types.Byte_Array);
{% for connector in connectors.of_kind('recv_async') %}
   not overriding procedure Dispatch_{{ connector.name }} (Self : in out Base_Instance;{% if connectors.arrayed_invokee() %} Index : in Connector_Index_Type;{% endif %} Bytes : in Basic_Types.Byte_Array);
{% endfor %}

   -- Procedure lookup table for dispatching to correct connector handler:
   type Dispatch_Procedure is not null access procedure (Self : in out Base_Instance;{% if connectors.arrayed_invokee() %} Index : in Connector_Index_Type;{% endif %} Bytes : in Basic_Types.Byte_Array);
   type Dispatch_Table_T is array (Connector_Identifier_Enum) of Dispatch_Procedure;
   Dispatch_Table : constant Dispatch_Table_T := [
      Quit =>   Dispatch_Quit'Access,
{% for connector in connectors.of_kind('recv_async') %}
      {{ connector.name }} => Dispatch_{{ connector.name }}'Access{{ "," if not loop.last }}
{% endfor %}
   ];

   ---------------------------------------
   -- Private methods for a component queue
   ---------------------------------------
   -- Function which dispatches up to "n" messages from the queue, or drains the queue,
   -- whichever happens first. It returns the number of messages dispatched:
   not overriding function Dispatch_N (Self : in out Base_Instance; N : in Natural := 1) return Natural;
   -- Function which tries to drain the queue. It returns the number of messages dispatched:
   not overriding function Dispatch_All (Self : in out Base_Instance) return Natural;
   -- Function which waits on queue for message and then dispatches it.
   procedure Dispatch_Block (Self : in out Base_Instance);
   -- Function dispatches message from queue if available, otherwise returns False.
   function Dispatch_Nonblock (Self : in out Base_Instance) return Boolean;

   ------------------------------------------------------------
   -- Private enqueue methods for different types on the queue
   ------------------------------------------------------------
   -- Enqueue a specific type for each connector onto the queue.
{% for connector in connectors.of_kind('recv_async') %}
   not overriding function Enqueue_{{ connector.name }} (Self : in out Base_Instance{% if connectors.arrayed_invokee() %}; Index : in {{ connector.name }}_Index{% endif %}; Arg : in {{ connector.type }}; Full_Queue_Behavior : in Full_Queue_Action := Drop) return Connector_Types.Connector_Status;
{% endfor %}

{% endif %}
   ---------------------------------------------------
   -- Definition of cycle function for task execution:
   ---------------------------------------------------
{% if execution in ["active", "either"] %}
{% if connectors.requires_queue() %}
   -- Active component queue implementation for cycle.
   -- Component continuously waits for items to arrive on queue
   -- and then executes when an item is received.
   overriding procedure Cycle (Self : in out Base_Instance);
{% else %}
   -- "cycle" method is abstract, and should be overridden in implementation.
{% endif %}
{% else %}
   -- Passive component queue implementation for cycle.
   -- This method is implemented, but if called will throw an assertion.
   overriding procedure Cycle (Self : in out Base_Instance);
{% endif %}

{% if connectors.invokee() %}
   ---------------------------------------
   -- Private invokee connector hooks which
   -- dispatch invokee calls to the correct
   -- abstract function defined in the
   -- child package:
   ---------------------------------------
{% for connector in connectors.invokee() %}
{% if connector.description %}
{{ printMultiLine(connector.description, '   -- ') }}
{% endif %}
{% if connector.kind == "return" %}
   function {{ connector.name }}_Hook (Class_Self : in out Component.Core_Instance'Class; Index : in Connector_Index_Type := Connector_Index_Type'First) return {{ connector.return_type }};
{% elif connector.kind == "service" %}
   function {{ connector.name }}_Hook (Class_Self : in out Component.Core_Instance'Class; Arg : {{ connector.mode }} {{ connector.type }}; Index : in Connector_Index_Type := Connector_Index_Type'First) return {{ connector.return_type }};
{% elif connector.kind == "modify" %}
   procedure {{ connector.name }}_Hook (Class_Self : in out Component.Core_Instance'Class; Arg : {{ connector.mode }} {{ connector.type }}; Index : in Connector_Index_Type := Connector_Index_Type'First);
{% else %}
   function {{ connector.name }}_Hook (Class_Self : in out Component.Core_Instance'Class; Arg : {{ connector.mode }} {{ connector.type }}; Index : in Connector_Index_Type := Connector_Index_Type'First; Full_Queue_Behavior : in Full_Queue_Action := Drop) return Connector_Types.Connector_Status;
{% endif %}
{% endfor %}

{% endif %}
{% if connectors.invoker() %}
   ---------------------------------------
   -- Private invoker connector functions
   -- for use in the child package:
   ---------------------------------------
{% for connector in connectors.invoker() %}
{% if connector.description %}
{{ printMultiLine(connector.description, '   -- ') }}
{% endif %}
{% if connector.kind == "send" %}
   not overriding procedure {{ connector.name }}_If_Connected (Self : in out Base_Instance{% if connector.count == 0 or connector.count > 1 %}; Index : in {{ connector.name }}_Index{% endif %}; Arg : {{ connector.mode }} {{ connector.type }}; Full_Queue_Behavior : in Full_Queue_Action := Drop);
   not overriding procedure {{ connector.name }} (Self : in out Base_Instance{% if connector.count == 0 or connector.count > 1 %}; Index : in {{ connector.name }}_Index{% endif %}; Arg : {{ connector.mode }} {{ connector.type }}; Full_Queue_Behavior : in Full_Queue_Action := Drop)
{% elif connector.kind == "provide" %}
   not overriding procedure {{ connector.name }} (Self : in Base_Instance{% if connector.count == 0 or connector.count > 1 %}; Index : in {{ connector.name }}_Index{% endif %}; Arg : {{ connector.mode }} {{ connector.type }})
{% elif connector.kind == "get" %}
   not overriding function {{ connector.name }} (Self : in Base_Instance{% if connector.count == 0 or connector.count > 1 %}; Index : in {{ connector.name }}_Index{% endif %}) return {{ connector.return_type }}
{% elif connector.kind == "request" %}
   not overriding function {{ connector.name }} (Self : in Base_Instance{% if connector.count == 0 or connector.count > 1 %}; Index : in {{ connector.name }}_Index{% endif %}; Arg : {{ connector.mode }} {{ connector.type }}) return {{ connector.return_type }}
{% endif %}
      with Inline => True;
   not overriding function Is_{{ connector.name }}_Connected (Self : in Base_Instance{% if connector.count == 0 or connector.count > 1 %}; Index : in {{ connector.name }}_Index{% endif %}) return Boolean
      with Inline => True;
{% endfor %}

{% endif %}
{% if commands %}
   -------------------------------------------
   -- Private subprograms for command handling
   -------------------------------------------
   -- Dispatch command to correct handler:
   not overriding function Execute_Command (Self : in out Base_Instance; Cmd : in Command.T) return Command_Response_Status.E;

   -- Default implementation of register commands. Override if you want different behavior.
   not overriding procedure Register_Commands (Self : in out Base_Instance; Arg : in Command_Registration_Request.T);
   not overriding procedure Execute_Register_Commands (Self : in out Base_Instance; Cmd : in Command.T);
   not overriding procedure Handle_Command_Length_Error (Self : in out Base_Instance; Cmd : in Command.T);
{% if "Command_Response" not in connectors.invoker().includes %}

   -- Dummy command response function that does not do anything. This is to allow the component to compile without command response
   -- capability, if a command response connector is not included.
   not overriding procedure Command_Response_T_Send (Self : in out Base_Instance; Arg : in Command_Response.T; Full_Queue_Behavior : in Full_Queue_Action := Drop) is null;
   not overriding procedure Command_Response_T_Send_If_Connected (Self : in out Base_Instance; Arg : in Command_Response.T; Full_Queue_Behavior : in Full_Queue_Action := Drop) is null;
{% endif %}

   -- Private command dispatching procedures:
{% for command in commands %}
   not overriding function Execute_{{ command.name }} (Self : in out Base_Instance; Cmd : in Command.T) return Command_Response_Status.E;
{% endfor %}

   -- Procedure lookup table for dispatching to correct command handler:
   type Execute_Function is not null access function (Self : in out Base_Instance; Cmd : in Command.T) return Command_Response_Status.E;
   type Command_Table_T is array ({{ commands.name }}.Local_Command_Id_Type) of Execute_Function;
   Command_Id_Table : constant Command_Table_T := [
{% for command in commands %}
      {{ commands.name }}.{{ command.name }}_Id => Execute_{{ command.name }}'Access{{ "," if not loop.last }}
{% endfor %}
   ];

{% endif %}
{% if parameters %}
   -------------------------------------------
   -- Private subprograms for parameter handling
   -------------------------------------------
   -- Procedure meant to be called by implementation package which updates
   -- the working copy of the parameters if they have all been successfully
   -- staged:
   not overriding procedure Update_Parameters (Self : in out Base_Instance);

   -- Procedure meant to be called by implementation package to process an
   -- incoming Parameter_Update record. This call will stage or fetch
   -- a parameter according to the operation set.
   not overriding procedure Process_Parameter_Update (Self : in out Base_Instance; Par_Update : in out Parameter_Update.T);

   -- Dispatch parameter to correct staging procedure:
   not overriding function Stage_Parameter (
      Self : in out Base_Instance;
      Table_Id : in Parameter_Types.Parameter_Table_Id;
      Par : in Parameter.T
   ) return Parameter_Update_Status.E;

   -- Fetch a requested parameter value:
   not overriding function Fetch_Parameter (Self : in out Base_Instance; Par : in out Parameter.T) return Parameter_Update_Status.E;

   -- Handle and invalid parameter length:
   not overriding procedure Handle_Parameter_Length_Error (Self : in out Base_Instance; Par : in Parameter.T);

   -- Private parameter staging procedures:
{% for par in parameters %}
   not overriding function Stage_{{ par.name }} (
      Self : in out Base_Instance;
      Table_Id : in Parameter_Types.Parameter_Table_Id;
      Par : in Parameter.T
   ) return Parameter_Update_Status.E;
   not overriding function Fetch_{{ par.name }} (Self : in out Base_Instance; Par : in out Parameter.T) return Parameter_Update_Status.E;
{% endfor %}

   -- Procedure lookup table for dispatching to parameter stage handler:
   type Stage_Function is not null access function (
      Self : in out Base_Instance;
      Table_Id : in Parameter_Types.Parameter_Table_Id;
      Par : in Parameter.T
   ) return Parameter_Update_Status.E;
   type Parameter_Table_T is array ({{ parameters.name }}.Local_Parameter_Id_Type) of Stage_Function;
   Parameter_Id_Table : constant Parameter_Table_T := [
{% for par in parameters %}
      {{ parameters.name }}.{{ par.name }}_Id => Stage_{{ par.name }}'Access{{ "," if not loop.last }}
{% endfor %}
   ];

   -- Procedure lookup table for dispatching to parameter fetch handler:
   type Fetch_Function is not null access function (Self : in out Base_Instance; Par : in out Parameter.T) return Parameter_Update_Status.E;
   type Parameter_Fetch_Table_T is array ({{ parameters.name }}.Local_Parameter_Id_Type) of Fetch_Function;
   Parameter_Id_Fetch_Table : constant Parameter_Fetch_Table_T := [
{% for par in parameters %}
      {{ parameters.name }}.{{ par.name }}_Id => Fetch_{{ par.name }}'Access{{ "," if not loop.last }}
{% endfor %}
   ];

   -- A protected object is used to store the component's staged parameters. This is because
   -- the staged parameters are accessed by both the execution thread of the component and the
   -- execution thread of the parameter component responsible for updating the parameters.
   -- The design of this protected object is to optimize the speed at which the copying of parameters
   -- from the staged to the working variables is as fast as possible.
   protected type Protected_Staged_Parameters is
      -- Set all parameters staged by Table_Id as ready to be updated by the component:
      procedure Set_Ready_To_Update (Table_Id : in Parameter_Types.Parameter_Table_Id);

      -- Returns True if the parameters are ready to update:
      function Is_Ready_To_Update return Boolean;

      -- Staging functions for each parameter:
{% for par in parameters %}
      procedure Stage_{{ par.name }} (
         Table_Id : in Parameter_Types.Parameter_Table_Id;
         Par : in {% if par.type_package %}{{ par.type_package }}.U{% else %}{{ par.type }}{% endif %});
{% endfor %}

      -- Fetching functions for each parameter:
{% for par in parameters %}
      function Get_{{ par.name }}_For_Fetch (Active_Value : in {% if par.type_package %}{{ par.type_package }}.U{% else %}{{ par.type }}{% endif %}) return {% if par.type_package %}{{ par.type_package }}.U{% else %}{{ par.type }}{% endif %};
      function Get_{{ par.name }}_For_Validate (Table_Id : in Parameter_Types.Parameter_Table_Id; Active_Value : in {% if par.type_package %}{{ par.type_package }}.U{% else %}{{ par.type }}{% endif %}) return {% if par.type_package %}{{ par.type_package }}.U{% else %}{{ par.type }}{% endif %};
{% endfor %}

      -- Single update function to copy all the parameters from the
      -- staged versions to the working copy passed in if the parameters
      -- are in the Ready_To_Update state. This function also resets
      -- the global Params_Ready_To_Update boolean to False.
      procedure Copy_From_Staged (
{% for par in parameters %}
         {{ par.name }} : in out {% if par.type_package %}{{ par.type_package }}.U{% else %}{{ par.type }}{% endif %}{{ ";" if not loop.last }}
{% endfor %}
      );
   private
      -- Global boolean to signify one or more parameters is ready for
      -- update by the component.
      Params_Ready_To_Update : Boolean := False;
      -- Staged parameter store:
{% for par in parameters %}
      {{ par.name }}_Status : Parameter_Types.Parameter_Status := (State => Parameter_Types.Updated, Staged_By => 0);
      {{ par.name }}_Staged : {% if par.type_package %}{{ par.type_package }}.U{% else %}{{ par.type }}{% endif %}{% if par.default_value %} := {{ par.default_value }}{% endif %};
{% endfor %}
   end Protected_Staged_Parameters;

{% endif %}
{% if data_dependencies %}
   -------------------------------------------
   -- Private subprograms for data dependencies
   -------------------------------------------
   -- Private helper functions to get data dependencies:
{% for dd in data_dependencies %}
   not overriding function Get_{{ dd.name }} (Self : in out Base_Instance; Stale_Reference : in Sys_Time.T; Timestamp : out Sys_Time.T; Value : out {{ dd.type }}) return Data_Product_Enums.Data_Dependency_Status.E;
   not overriding function Get_{{ dd.name }} (Self : in out Base_Instance; Stale_Reference : in Sys_Time.T; Value : out {{ dd.type }}) return Data_Product_Enums.Data_Dependency_Status.E
      with Inline => True;
{% endfor %}

{% endif %}
   -------------------------------------------
   -- The base class instance record:
   -------------------------------------------
   type Base_Instance is abstract new Component.Core_Instance with record
{% if connectors.requires_queue() or connectors.invoker() or connectors.invokee().n_arrayed() or data_products or commands or events or packets or parameters or faults %}
{% if connectors.requires_queue() %}
      -- Internal asynchronous connector queue:
      Queue : Queue_Package.Instance;
{% endif %}
{% if connectors.invoker() %}
      -- Invoker connector objects:
{% for connector in connectors.invoker() %}
{% if connector.count == 0   %}
      Connector_{{ connector.name }} : {{ connector.name }}_Array_Access := null;
{% elif connector.count > 1 %}
      Connector_{{ connector.name }} : {{ connector.name }}_Array;
{% else %}
      Connector_{{ connector.name }} : {{ connector.name }}_Connector.Instance;
{% endif %}
{% endfor %}
{% endif %}
{% if connectors.invokee() %}
{% for connector in connectors.invokee() %}
{% if connector.count == 0 %}
      -- Store the dynamically allocated count for the unconstrained {{ connector.name }} connector:
      {{ connector.name }}_Count : Connector_Count_Type := 0;
{% endif %}
{% endfor %}
{% endif %}
{% if events %}
      -- Event instance:
      Events : {{ events.name }}.Instance;
{% endif %}
{% if data_products %}
      -- Data product instance:
      Data_Products : {{ data_products.name }}.Instance;
{% endif %}
{% if data_dependencies %}
      -- Data dependencies instance:
      Data_Dependencies : {{ data_dependencies.name }}.Instance;
{% endif %}
{% if packets %}
      -- Packet instance:
      Packets : {{ packets.name }}.Instance;
{% endif %}
{% if faults %}
      -- Fault instance:
      Faults : {{ faults.name }}.Instance;
{% endif %}
{% if commands %}
      -- Command variables:
      Command_Id_Base : Command_Types.Command_Id := 1; -- Should not be 0 since, command registration is always 0
      Command_Reg_Id : Command_Types.Command_Registration_Id := Command_Types.Command_Registration_Id'First;
{% endif %}
{% if parameters %}
      -- Parameter variables:
      Parameter_Id_Base : Parameter_Types.Parameter_Id := 1; -- Should not be 0 since, "parameters set" is always 0
      -- Staged parameters:
      Staged_Parameters : Protected_Staged_Parameters;
      -- Working parameters:
{% for par in parameters %}
      {{ par.name }} : {% if par.type_package %}{{ par.type_package }}.U{% else %}par.type{% endif %}{% if par.default_value %} := {{ par.default_value }}{% endif %};
{% endfor %}
{% endif %}
{% else %}
      null;
{% endif %}
   end record;

end Component.{{ name }};
