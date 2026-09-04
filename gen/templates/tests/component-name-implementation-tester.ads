--------------------------------------------------------------------------------
-- {{ formatType(model_name) }} {{ formatType(model_type) }} Tester Spec
--------------------------------------------------------------------------------

{% if tester_template_ads_includes %}
-- Includes:
{% for include in tester_template_ads_includes %}
with {{ include }};
{% endfor %}

{% endif %}
{% if description %}
{{ printMultiLine(description, '-- ') }}
{% endif %}
{% if generic %}
generic
{% endif %}
package Component.{{ name }}.Implementation.Tester is

{% if generic %}
   package {{ name }}_Package is new Component.{{ name }}_Reciprocal ({% for name in generic.formal_parameter_names %}{{ name }}{{ ", " if not loop.last }}{% endfor %});
   use {{ name }}_Package;
{% else %}
   use Component.{{ name }}_Reciprocal;
{% endif %}
{% if connectors.invoker() %}
   -- Invoker connector history packages:
{% for connector in connectors.invoker() %}
{% if connector.generic %}
{% if connector.kind == "get" %}
   package {{ connector.tester_name }}_History_Package is new History ({{ connector.return_type }});
{% else %}
   package {{ connector.tester_name }}_History_Package is new History ({{ connector.type }});
{% endif %}
{% else %}
{% if connector.kind == "get" %}
   package {{ connector.tester_name }}_History_Package is new Printable_History ({{ connector.return_type }}, {% if connector.return_type_model %}{{ connector.return_type_package }}.Representation.Image{% else %}{{ connector.return_type }}'Image{% endif %});
{% else %}
   package {{ connector.tester_name }}_History_Package is new Printable_History ({{ connector.type }}, {% if connector.type_model %}{{ connector.type_package }}.Representation.Image{% else %}{{ connector.type }}'Image{% endif %});
{% if connector.kind == "request" %}
   package {{ connector.tester_name }}_Return_History_Package is new Printable_History ({{ connector.return_type }}, {% if connector.return_type_model %}{{ connector.return_type_package }}.Representation.Image{% else %}{{ connector.return_type }}'Image{% endif %});
{% endif %}
{% endif %}
{% endif %}
{% endfor %}

{% endif %}
{% if events %}
   -- Event history packages:
{% for event in events %}
   package {{ event.name }}_History_Package is new Printable_History ({% if event.type %}{{ event.type }}, {% if event.type_model %}{{ event.type_package }}.Representation.Image{% else %}{{ event.type }}'Image{% endif %}{% else %}Natural, Natural'Image{% endif %});
{% endfor %}

{% endif %}
{% if data_products %}
   -- Data product history packages:
{% for dp in data_products %}
   package {{ dp.name }}_History_Package is new Printable_History ({{ dp.type }}, {% if dp.type_model %}{{ dp.type_package }}.Representation.Image{% else %}{{ dp.type }}'Image{% endif %});
{% endfor %}

{% endif %}
{% if packets %}
   -- Packet history packages:
{% for p in packets %}
{% if p.type %}
   package {{ p.name }}_History_Package is new Printable_History ({{ p.type }}, {% if p.type_model %}{{ p.type_package }}.Representation.Image{% else %}{{ p.type }}'Image{% endif %});
{% else %}
   package {{ p.name }}_History_Package is new Printable_History (Packet.T, Packet.Representation.Image);
{% endif %}
{% endfor %}

{% endif %}
{% if faults %}
   -- Fault history packages:
{% for fault in faults %}
   package {{ fault.name }}_History_Package is new Printable_History ({% if fault.type %}{{ fault.type }}, {% if fault.type_model %}{{ fault.type_package }}.Representation.Image{% else %}{{ fault.type }}'Image{% endif %}{% else %}Natural, Natural'Image{% endif %});
{% endfor %}

{% endif %}
   -- Component class instance:
   type Instance is new {% if generic %}{{ name }}_Package{% else %}Component.{{ name }}_Reciprocal{% endif %}.Base_Instance with record
      -- The component instance under test:
      Component_Instance : aliased Component.{{ name }}.Implementation.Instance;
{% if connectors.invoker() or events or data_products or packets or faults %}
{% if connectors.invoker() %}
      -- Connector histories:
{% for connector in connectors.invoker() %}
      {{ connector.tester_name }}_History : {{ connector.tester_name }}_History_Package.Instance;
{% endfor %}
{% endif %}
{% if events %}
      -- Event histories:
{% for event in events %}
      {{ event.name }}_History : {{ event.name }}_History_Package.Instance;
{% endfor %}
{% endif %}
{% if data_products %}
      -- Data product histories:
{% for dp in data_products %}
      {{ dp.name }}_History : {{ dp.name }}_History_Package.Instance;
{% endfor %}
{% endif %}
{% if packets %}
      -- Packet histories:
{% for p in packets %}
      {{ p.name }}_History : {{ p.name }}_History_Package.Instance;
{% endfor %}
{% endif %}
{% if faults %}
      -- Fault histories:
{% for fault in faults %}
      {{ fault.name }}_History : {{ fault.name }}_History_Package.Instance;
{% endfor %}
{% endif %}
{% endif %}
{% if data_dependencies %}
      -- Data dependency return values. These can be set during unit test
      -- and will be returned to the component when a data dependency call
      -- is made.
{% for dd in data_dependencies %}
      {{ dd.name }} : {{ dd.type }};
{% endfor %}
      -- The return status for the data dependency fetch. This can be set
      -- during unit test to return something other than Success.
      Data_Dependency_Return_Status_Override : Data_Product_Enums.Fetch_Status.E := Data_Product_Enums.Fetch_Status.Success;
      -- The ID to return with the data dependency. If this is set to zero then
      -- the valid ID for the requested dependency is returned, otherwise, the
      -- value of this variable is returned.
      Data_Dependency_Return_Id_Override : Data_Product_Types.Data_Product_Id := 0;
      -- The length to return with the data dependency. If this is set to zero then
      -- the valid length for the requested dependency is returned, otherwise, the
      -- value of this variable is returned.
      Data_Dependency_Return_Length_Override : Data_Product_Types.Data_Product_Buffer_Length_Type := 0;
      -- The timestamp to return with the data dependency. If this is set to (0, 0) then
      -- the System_Time (above) is returned, otherwise, the value of this variable is returned.
      Data_Dependency_Timestamp_Override : Sys_Time.T := (0, 0);
{% endif %}
{% if connectors.of_kind('recv_async') %}
      -- Booleans to control assertion if message is dropped on async queue:
{% for connector in connectors.of_kind('recv_async') %}
      Expect_{{ connector.tester_name }}_Dropped : Boolean := False;
      {{ connector.tester_name }}_Dropped_Count : Natural := 0;
{% endfor %}
{% endif %}
   end record;
   type Instance_Access is access all Instance;

{% if init_base or connectors.invoker() or events or data_products or packets or faults %}
   ---------------------------------------
   -- Initialize component heap variables:
   ---------------------------------------
   procedure Init_Base (Self : in out Instance{% if init_base.parameters %}; {{ init_base.parameter_declaration_string() }}{% endif %});
   procedure Final_Base (Self : in out Instance);

{% endif %}
{% if connectors %}
   ---------------------------------------
   -- Test initialization functions:
   ---------------------------------------
   procedure Connect (Self : in out Instance);

{% endif %}
{% if connectors.invoker() %}
   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
{% for connector in connectors.invoker() %}
{% if connector.description %}
{{ printMultiLine(connector.description, '   -- ') }}
{% endif %}
{% if connector.kind == "get" %}
   overriding function {{ connector.tester_name }} (Self : in out Instance) return {{ connector.return_type }};
{% elif connector.kind == "request" %}
   overriding function {{ connector.tester_name }} (Self : in out Instance; Arg : {{ connector.mode }} {{ connector.type }}) return {{ connector.return_type }};
{% else %}
   overriding procedure {{ connector.tester_name }} (Self : in out Instance; Arg : {{ connector.mode }} {{ connector.type }});
{% endif %}
{% endfor %}

{% endif %}
{% if connectors.of_kind('recv_async') %}
   ---------------------------------------
   -- Invoker connector primitives:
   ---------------------------------------
{% for connector in connectors.of_kind('recv_async') %}
   -- This procedure is called when a {{ connector.tester_name }} message is dropped due to a full queue.
   overriding procedure {{ connector.tester_name }}_Dropped (Self : in out Instance{% if connector.count == 0 or connector.count > 1 %}; Index : in {{ connector.tester_name }}_Index{% endif %}; Arg : in {{ connector.type }});

{% endfor %}
{% endif %}
{% if events %}
   -----------------------------------------------
   -- Event handler primitive:
   -----------------------------------------------
{% if events.description %}
   -- Description:
{{ printMultiLine(events.description, '   --    ') }}
{% endif %}
{% for event in events %}
{% if event.description %}
{{ printMultiLine(event.description, '   -- ') }}
{% endif %}
   overriding procedure {{ event.name }} (Self : in out Instance{% if event.type %}; Arg : in {{ event.type }}{% endif %});
{% endfor %}

{% endif %}
{% if data_products %}
   -----------------------------------------------
   -- Data product handler primitives:
   -----------------------------------------------
{% if data_products.description %}
   -- Description:
{{ printMultiLine(data_products.description, '   --    ') }}
{% endif %}
{% for dp in data_products.data_products %}
{% if dp.description %}
{{ printMultiLine(dp.description, '   -- ') }}
{% endif %}
   overriding procedure {{ dp.name }} (Self : in out Instance; Arg : in {{ dp.type }});
{% endfor %}

{% endif %}
{% if packets %}
   -----------------------------------------------
   -- Packet handler primitives:
   -----------------------------------------------
{% if packets.description %}
   -- Description:
{{ printMultiLine(packets.description, '   --    ') }}
{% endif %}
{% for p in packets%}
{% if p.description %}
{{ printMultiLine(p.description, '   -- ') }}
{% endif %}
{% if p.type %}
   overriding procedure {{ p.name }} (Self : in out Instance; Arg : in {{ p.type }});
{% else %}
   overriding procedure {{ p.name }} (Self : in out Instance; Arg : in Packet.T);
{% endif %}
{% endfor %}

{% endif %}
{% if faults %}
   -----------------------------------------------
   -- Fault handler primitive:
   -----------------------------------------------
{% if faults.description %}
   -- Description:
{{ printMultiLine(faults.description, '   --    ') }}
{% endif %}
{% for fault in faults %}
{% if fault.description %}
{{ printMultiLine(fault.description, '   -- ') }}
{% endif %}
   overriding procedure {{ fault.name }} (Self : in out Instance{% if fault.type %}; Arg : in {{ fault.type }}{% endif %});
{% endfor %}

{% endif %}
{% if connectors.of_kind("recv_async") %}
   -----------------------------------------------
   -- Special primitives for activating component
   -- queue:
   -----------------------------------------------
   -- Tell the component to dispatch all items off of its queue:
   not overriding function Dispatch_All (Self : in out Instance) return Natural;
   -- Tell the component to dispatch n items off of its queue:
   not overriding function Dispatch_N (Self : in out Instance; N : in Positive := 1) return Natural;

{% endif %}
{% if execution in ["active", "either"] and not connectors.of_kind("recv_async") %}
   -----------------------------------------------
   -- Special primitives for running an active component
   -- with no queue:
   -----------------------------------------------
   -- Force the component to cycle from the tester
   not overriding procedure Cycle_Component (Self : in out Instance);

{% endif %}
{% if parameters %}
   -----------------------------------------------
   -- Special primitives for aiding in the staging,
   -- fetching, and updating of parameters
   -----------------------------------------------
   -- Stage a parameter value within the component
   not overriding function Stage_Parameter (Self : in out Instance; Par : in Parameter.T) return Parameter_Update_Status.E;
   -- Fetch the value of a parameter with the component
   not overriding function Fetch_Parameter (Self : in out Instance; Id : in Parameter_Types.Parameter_Id; Par : out Parameter.T) return Parameter_Update_Status.E;
   -- Ask the component to validate all parameters. This will call the
   -- Validate_Parameters subprogram within the component implementation,
   -- which allows custom checking of the parameter set prior to updating.
   not overriding function Validate_Parameters (Self : in out Instance) return Parameter_Update_Status.E;
   -- Tell the component it is OK to atomically update all of its
   -- working parameter values with the staged values.
   not overriding function Update_Parameters (Self : in out Instance) return Parameter_Update_Status.E;

{% endif %}
end Component.{{ name }}.Implementation.Tester;
