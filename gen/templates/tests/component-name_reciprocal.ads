--------------------------------------------------------------------------------
-- {{ formatType(model_name) }} {{ formatType(model_type) }} Reciprocal Base Spec
--
-- Generated from {{ filename }} on {{ time }}.
--------------------------------------------------------------------------------

-- Includes:
with File_Logger;
{% if tester_base_ads_includes %}
{% for include in tester_base_ads_includes %}
{% if include in includes %}
pragma Warnings (Off, "unit ""{{ include }}"" is not referenced");
{% endif %}
with {{ include }};
{% if include in includes %}
pragma Warnings (On, "unit ""{{ include }}"" is not referenced");
{% endif %}
{% if include in ["Connector_Types"] %}
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
package Component.{{ name }}_Reciprocal is

{% if connectors.invokee() %}
   -- Define invoker connector packages:
{% for connector in connectors.invokee() %}
{% if connector.common and not generic %}
   package {{ connector.tester_name }}_Connector renames Common_Connectors.{{ connector.common_connector_package }};
{% elif connector.kind == "return" %}
   package {{ connector.tester_name }}_Connector is new {{ connector.connector_package }} ({{ connector.return_type }});
{% elif connector.kind == "service" %}
   package {{ connector.tester_name }}_Connector is new {{ connector.connector_package }} ({{ connector.type }}, {{ connector.return_type }});
{% else %}
   package {{ connector.tester_name }}_Connector is new {{ connector.connector_package }} ({{ connector.type }});
{% endif %}
{% if connector.count == 0   %}
   subtype {{ connector.tester_name }}_Index is Connector_Index_Type;
   type {{ connector.tester_name }}_Array is array ({{ connector.tester_name }}_Index range <>) of {{ connector.tester_name }}_Connector.Instance;
   type {{ connector.tester_name }}_Array_Access is access {{ connector.tester_name }}_Array;
{% elif connector.count > 1 %}
   subtype {{ connector.tester_name }}_Index is Connector_Index_Type range Connector_Index_Type'First .. Connector_Index_Type'First + {{ connector.count }} - 1;
   type {{ connector.tester_name }}_Array is array ({{ connector.tester_name }}_Index) of {{ connector.tester_name }}_Connector.Instance;
{% endif %}
{% endfor %}

{% endif %}
{% if connectors.invoker() %}
   -- Define invokee connector packages:
{% for connector in connectors.invoker() %}
{% if connector.common and not generic %}
   package {{ connector.tester_name }}_Connector renames Common_Connectors.{{ connector.common_connector_package }};
{% elif connector.kind == "get" %}
   package {{ connector.tester_name }}_Connector is new {{ connector.connector_package }} ({{ connector.return_type }});
{% elif connector.kind == "request" %}
   package {{ connector.tester_name }}_Connector is new {{ connector.connector_package }} ({{ connector.type }}, {{ connector.return_type }});
{% else %}
   package {{ connector.tester_name }}_Connector is new {{ connector.connector_package }} ({{ connector.type }});
{% endif %}
   -- Define index type for invokee connector. Range is always 1 .. 1 for tester components since testers do not have arrayed invokee connectors.
   subtype {{ connector.tester_name }}_Index is Connector_Index_Type range Connector_Index_Type'First .. Connector_Index_Type'First;
{% endfor %}

{% endif %}
   -- Base class instance record:
   type Base_Instance is abstract new Component.Core_Instance with record
{% if connectors.invokee() %}
      -- Invoker connector objects:
{% for connector in connectors.invokee() %}
{% if connector.count == 0   %}
      Connector_{{ connector.tester_name }} : {{ connector.tester_name }}_Array_Access := null;
{% elif connector.count > 1 %}
      Connector_{{ connector.tester_name }} : {{ connector.tester_name }}_Array;
{% else %}
      Connector_{{ connector.tester_name }} : {{ connector.tester_name }}_Connector.Instance;
{% endif %}
{% endfor %}
{% endif %}
{% if connectors.of_kind("send") %}
      -- Invokee connector status return values. You can change these during test to simulate
      -- different connector status return values:
{% for connector in connectors.of_kind("send") %}
      Connector_{{ connector.tester_name }}_Status : Connector_Types.Connector_Status := Connector_Types.Success;
{% endfor %}
{% endif %}
{% if commands %}
      -- Command suite object instance:
      Commands : {{ commands.name }}.Instance;
{% endif %}
{% if parameters %}
      -- Parameter suite object instance:
      Parameters : {{ parameters.name }}.Instance;
{% endif %}
{% if packets %}
      -- Packet suite object instance:
      Packets : {{ packets.name }}.Instance;
{% endif %}
{% if events %}
      -- Event suite object instance:
      Events : {{ events.name }}.Instance;
{% endif %}
{% if faults %}
      -- Fault suite object instance:
      Faults : {{ faults.name }}.Instance;
{% endif %}
{% if data_products %}
      -- Data product suite object instance:
      Data_Products : {{ data_products.name }}.Instance;
{% endif %}
{% if data_dependencies %}
      -- Data dependency suite object instance:
      Data_Dependencies : {{ data_dependencies.name }}.Instance;
{% endif %}
{% if not connectors.invokee() and not connectors.invoker() and not commands and not parameters and not data_products and not events and not packets and not faults %}
      null;
{% endif %}
      -- File for logging
      Logger : File_Logger.Instance_Access := null;
{% if data_dependencies %}
      -- System time for test. Make this non-zero to data dependencies don't report as stale.
      System_Time : Sys_Time.T := (10000, 0);
{% else %}
      -- System time for test:
      System_Time : Sys_Time.T := (0, 0);
{% endif %}
      -- Flags for different log outputs that are available for this component. There are switches for each connector and each item of each connector.
      Log_Verbose : Boolean := False;
{% if connectors %}
{% for connector in connectors %}
      Log_{{ connector.name }} : Boolean := True;
{% endfor %}
{% endif %}
{% if events %}
      -- Switches for logging events
      Log_Events : Boolean := True;
{% for event in events %}
      Log_{{ event.name }} : Boolean := True;
{% endfor %}
{% endif %}
{% if data_products %}
      -- Switches for logging data products
      Log_Data_Products : Boolean := True;
{% for dp in data_products %}
      Log_{{ dp.name }} : Boolean := True;
{% endfor %}
{% endif %}
{% if faults %}
      -- Switches for logging faults
      Log_Faults : Boolean := True;
{% for fault in faults %}
      Log_{{ fault.name }} : Boolean := True;
{% endfor %}
{% endif %}
{% if packets %}
      -- Switches for logging packets
      Log_Packets : Boolean := True;
{% for pkt in packets %}
      Log_{{ pkt.name }} : Boolean := True;
{% endfor %}
{% endif %}
{% if parameters and parameters.parameters %}
      -- Switches for logging parameters
      Log_Parameters : Boolean := True;
{% for p in parameters %}
      Log_{{ p.name }} : Boolean := True;
{% endfor %}
{% endif %}
{% if commands and commands.commands %}
      -- Switches for logging commands
      Log_Commands : Boolean := True;
{% for command in commands %}
      Log_{{ command.name }} : Boolean := True;
{% endfor %}
{% endif %}
   end record;

{% if connectors.invoker() %}
   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
{% for connector in connectors.invoker() %}
{% if connector.description %}
{{ printMultiLine(connector.description, '   -- ') }}
{% endif %}
   -- Abstract connector procedure to be overridden by child:
{% if connector.kind == "get" %}
   not overriding function {{ connector.tester_name }} (Self : in out Base_Instance) return {{ connector.return_type }} is abstract;
{% elif connector.kind == "request" %}
   not overriding function {{ connector.tester_name }} (Self : in out Base_Instance; Arg : {{ connector.mode }} {{ connector.type }}) return {{ connector.return_type }} is abstract;
{% else %}
   not overriding procedure {{ connector.tester_name }} (Self : in out Base_Instance; Arg : {{ connector.mode }} {{ connector.type }}) is abstract;
{% endif %}
   -- Function which returns the hook for this connector. Used when attaching this connector to an invoker connector:
   not overriding function {{ connector.tester_name }}_Access (Self : in out Base_Instance; Index : in Connector_Index_Type := Connector_Index_Type'First) return not null {{ connector.tester_name }}_Connector.Invokee_Hook;

{% endfor %}
{% endif %}
{% if connectors.invokee() %}
   ---------------------------------------
   -- Invoker connector primitives:
   ---------------------------------------
{% for connector in connectors.invokee() %}
{% if connector.description %}
{{ printMultiLine(connector.description, '   -- ') }}
{% endif %}
   -- Function to attach this invoker connector to an invokee connector:
   not overriding procedure Attach_{{ connector.tester_name }} (Self : in out Base_Instance{% if connector.count == 0 or connector.count > 1 %}; From_Index : in {{ connector.tester_name }}_Index{% endif %}; To_Component : in not null Component.Class_Access; Hook : in not null {{ connector.tester_name }}_Connector.Invokee_Hook; To_Index : in Connector_Index_Type := Connector_Index_Type'First);

{% endfor %}
{% endif %}
   ---------------------------------------------------
   -- Definition of cycle function for task execution:
   ---------------------------------------------------
   -- Passive component queue implementation for cycle.
   -- Tester components NEVER have threads.
   -- This method is implemented, but if called will throw an assertion.
   overriding procedure Cycle (Self : in out Base_Instance);

{% if connectors.invoker() %}
   ---------------------------------------
   -- Invokee connector hooks which
   -- dispatch invokee calls to the correct
   -- abstract function defined in the
   -- child package:
   ---------------------------------------
{% for connector in connectors.invoker() %}
{% if connector.description %}
{{ printMultiLine(connector.description, '   -- ') }}
{% endif %}
{% if connector.kind == "get" %}
   function {{ connector.tester_name }}_Hook (Class_Self : in out Component.Core_Instance'Class; Index : in Connector_Index_Type := Connector_Index_Type'First) return {{ connector.return_type }};
{% elif connector.kind == "request" %}
   function {{ connector.tester_name }}_Hook (Class_Self : in out Component.Core_Instance'Class; Arg : {{ connector.mode }} {{ connector.type }}; Index : in Connector_Index_Type := Connector_Index_Type'First) return {{ connector.return_type }};
{% elif connector.kind == "provide" %}
   procedure {{ connector.tester_name }}_Hook (Class_Self : in out Component.Core_Instance'Class; Arg : {{ connector.mode }} {{ connector.type }}; Index : in Connector_Index_Type := Connector_Index_Type'First);
{% else %}
   function {{ connector.tester_name }}_Hook (Class_Self : in out Component.Core_Instance'Class; Arg : {{ connector.mode }} {{ connector.type }}; Index : in Connector_Index_Type := Connector_Index_Type'First; Ignore : in Full_Queue_Action := Drop) return Connector_Types.Connector_Status;
{% endif %}
{% endfor %}

{% endif %}
{% if connectors.invokee() %}
   ---------------------------------------
   -- Invoker connector functions
   -- for use in the child package:
   ---------------------------------------
{% for connector in connectors.invokee() %}
{% if connector.description %}
{{ printMultiLine(connector.description, '   -- ') }}
{% endif %}
{% if connector.kind == "return" %}
   not overriding function {{ connector.tester_name }} (Self : in out Base_Instance{% if connector.count == 0 or connector.count > 1 %}; Index : in {{ connector.tester_name }}_Index{% endif %}) return {{ connector.return_type }};
{% elif connector.kind == "service" %}
   not overriding function {{ connector.tester_name }} (Self : in out Base_Instance{% if connector.count == 0 or connector.count > 1 %}; Index : in {{ connector.tester_name }}_Index{% endif %}; Arg : {{ connector.mode }} {{ connector.type }}) return {{ connector.return_type }};
{% elif connector.kind == "modify" %}
   not overriding procedure {{ connector.tester_name }} (Self : in out Base_Instance{% if connector.count == 0 or connector.count > 1 %}; Index : in {{ connector.tester_name }}_Index{% endif %}; Arg : {{ connector.mode }} {{ connector.type }});
{% else %}
   not overriding procedure {{ connector.tester_name }} (Self : in out Base_Instance{% if connector.count == 0 or connector.count > 1 %}; Index : in {{ connector.tester_name }}_Index{% endif %}; Arg : {{ connector.mode }} {{ connector.type }}; Full_Queue_Behavior : in Full_Queue_Action := Drop);
{% if connector.kind == "recv_async" %}
   -- This abstract procedure must be overridden in the child package to specify the behavior when a {{ connector.tester_name }} message is dropped due to a full queue.
   not overriding procedure {{ connector.tester_name }}_Dropped (Self : in out Base_Instance{% if connector.count == 0 or connector.count > 1 %}; Index : in {{ connector.tester_name }}_Index{% endif %}; Arg : in {{ connector.type }}) is abstract;
{% endif %}
{% endif %}
{% endfor %}

{% endif %}
{% if parameters and ("Parameter.T" in connectors.of_kind("recv_sync").types) %}
   -----------------------------------------------
   -- Helper functions for dealing with parameters.
   -----------------------------------------------
   -- Send special zero-id parameter to component signaling it to update its staged
   -- parameters to its local parameters whenever it is ready.
   procedure Update_Local_Parameters (Self : in out Base_Instance);

{% endif %}
{% if events %}
   -----------------------------------------------
   -- Abstract event handlers to be overridden:
   -----------------------------------------------
{% if events.description %}
   -- Description:
{{ printMultiLine(events.description, '   --    ') }}
{% endif %}
{% for event in events %}
{% if event.description %}
{{ printMultiLine(event.description, '   -- ') }}
{% endif %}
   procedure {{ event.name }} (Self : in out Base_Instance{% if event.type %}; Arg : in {{ event.type }}{% endif %}) is abstract;
{% endfor %}

   -------------------------------------------
   -- Subprograms for event handling
   -------------------------------------------
   -- Dispatch event to correct handler:
   not overriding procedure Dispatch_Event (Self : in out Base_Instance; Evnt : in Event.T);

   -- Private event dispatching procedures:
{% for event in events %}
   not overriding procedure Dispatch_{{ event.name }} (Self : in out Base_Instance; Evnt : in Event.T);
{% endfor %}

   -- Procedure lookup table for dispatching to correct connector handler:
   type Dispatch_Event_Procedure is access procedure (Self : in out Base_Instance; Evnt : in Event.T);
   type Event_Table_T is array ({{ events.name }}.Local_Event_Id_Type) of Dispatch_Event_Procedure;
   Event_Id_Table : Event_Table_T := [
{% for event in events %}
      {{ events.name }}.{{ event.name }}_Id => Dispatch_{{ event.name }}'Access{{ "," if not loop.last }}
{% endfor %}
   ];

{% endif %}
{% if data_products %}
   -----------------------------------------------
   -- Abstract data product handlers to be overridden:
   -----------------------------------------------
{% if data_products.description %}
   -- Description:
{{ printMultiLine(data_products.description, '   --    ') }}
{% endif %}
{% for dp in data_products.data_products %}
{% if dp.description %}
{{ printMultiLine(dp.description, '   -- ') }}
{% endif %}
   procedure {{ dp.name }} (Self : in out Base_Instance; Arg : in {{ dp.type }}) is abstract;
{% endfor %}

   -------------------------------------------
   -- Subprograms for data product handling
   -------------------------------------------
   -- Dispatch data product to correct handler:
   not overriding procedure Dispatch_Data_Product (Self : in out Base_Instance; Dp : in Data_Product.T);

   -- Private data product dispatching procedures:
{% for dp in data_products %}
   not overriding procedure Dispatch_{{ dp.name }} (Self : in out Base_Instance; Dp : in Data_Product.T);
{% endfor %}

   -- Procedure lookup table for dispatching to correct connector handler:
   type Dispatch_Data_Product_Procedure is access procedure (Self : in out Base_Instance; Dp : in Data_Product.T);
   type Data_Product_Table_T is array ({{ data_products.name }}.Local_Data_Product_Id_Type) of Dispatch_Data_Product_Procedure;
   Data_Product_Id_Table : Data_Product_Table_T := [
{% for dp in data_products %}
      {{ data_products.name }}.{{ dp.name }}_Id => Dispatch_{{ dp.name }}'Access{{ "," if not loop.last }}
{% endfor %}
   ];

{% endif %}
{% if packets %}
   -----------------------------------------------
   -- Abstract packet handlers to be overridden:
   -----------------------------------------------
{% if packets.description %}
   -- Description:
{{ printMultiLine(packets.description, '   --    ') }}
{% endif %}
{% for p in packets %}
{% if p.description %}
{{ printMultiLine(p.description, '   -- ') }}
{% endif %}
{% if p.type %}
   procedure {{ p.name }} (Self : in out Base_Instance; Arg : in {{ p.type }}) is abstract;
{% else %}
   procedure {{ p.name }} (Self : in out Base_Instance; Arg : in Packet.T) is abstract;
{% endif %}
{% endfor %}

   -------------------------------------------
   -- Subprograms for packet handling
   -------------------------------------------
   -- Dispatch packet to correct handler:
   not overriding procedure Dispatch_Packet (Self : in out Base_Instance; P : in Packet.T);

   -- Private packet dispatching procedures:
{% for p in packets %}
   not overriding procedure Dispatch_{{ p.name }} (Self : in out Base_Instance; P : in Packet.T);
{% endfor %}

{% endif %}
{% if faults %}
   -----------------------------------------------
   -- Abstract fault handlers to be overridden:
   -----------------------------------------------
{% if faults.description %}
   -- Description:
{{ printMultiLine(faults.description, '   --    ') }}
{% endif %}
{% for fault in faults %}
{% if fault.description %}
{{ printMultiLine(fault.description, '   -- ') }}
{% endif %}
   procedure {{ fault.name }} (Self : in out Base_Instance{% if fault.type %}; Arg : in {{ fault.type }}{% endif %}) is abstract;
{% endfor %}

   -------------------------------------------
   -- Subprograms for fault handling
   -------------------------------------------
   -- Dispatch fault to correct handler:
   not overriding procedure Dispatch_Fault (Self : in out Base_Instance; Flt : in Fault.T);

   -- Private fault dispatching procedures:
{% for fault in faults %}
   not overriding procedure Dispatch_{{ fault.name }} (Self : in out Base_Instance; Flt : in Fault.T);
{% endfor %}

   -- Procedure lookup table for dispatching to correct connector handler:
   type Dispatch_Fault_Procedure is access procedure (Self : in out Base_Instance; Flt : in Fault.T);
   type Fault_Table_T is array ({{ faults.name }}.Local_Fault_Id_Type) of Dispatch_Fault_Procedure;
   Fault_Id_Table : Fault_Table_T := [
{% for fault in faults %}
      {{ faults.name }}.{{ fault.name }}_Id => Dispatch_{{ fault.name }}'Access{{ "," if not loop.last }}
{% endfor %}
   ];

{% endif %}
   ---------------------------------------
   -- Logging functions:
   ---------------------------------------
   procedure Set_Logger (Self : in out Base_Instance; Logger : in File_Logger.Instance_Access);
   -- procedure End_Logging (Self : in out Base_Instance; File_Name : in String);
   procedure Log (Self : in out Base_Instance; String_To_Log : in String);
{% if data_dependencies %}
   procedure Log_Data_Dependencies (Self : in out Base_Instance; Arg : in Data_Product_Fetch.T;   Dp : in Data_Product_Return.T);
{% endif %}
{% if parameters and parameters.parameters %}
   procedure Log_Incoming_Parameter (Self : in out Base_Instance; Param : in Parameter.T);
{% endif %}
{% if commands and commands.commands %}
   procedure Log_Incoming_Command (Self : in out Base_Instance; Cmd : in Command.T);
{% endif %}

end Component.{{ name }}_Reciprocal;
