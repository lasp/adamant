--------------------------------------------------------------------------------
-- {{ formatType(model_name) }} {{ formatType(model_type) }} Implementation Spec
--------------------------------------------------------------------------------

{% if template_ads_includes %}
-- Includes:
{% for include in template_ads_includes %}
with {{ include }};
{% endfor %}

{% endif %}
{% if description %}
{{ printMultiLine(description, '-- ') }}
{% endif %}
{% if generic %}
generic
{% endif %}
package Component.{{ name }}.Implementation is

   -- The component class instance record:
{% if discriminant.description %}
{{ printMultiLine(discriminant.description, '   -- ') }}
{% endif %}
{% if discriminant.parameters %}
   --
{{ printMultiLine("Discriminant Parameters:", '   -- ') }}
{% for p in discriminant.parameters %}
{{ printMultiLine(p.name + " : " + p.type + ((" - " + p.description) if (p.description) else ""), '   -- ') }}
{% endfor %}
   --
{% endif %}
   type Instance{% if discriminant.parameters %} ({{ discriminant.parameter_declaration_string(include_mode=False) }}){% endif %} is new {{ name }}.Base_Instance with private;

{% if init %}
   --------------------------------------------------
   -- Subprogram for implementation init method:
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
   overriding procedure Init (Self : in out Instance{% if init.parameters %}; {{ init.parameter_declaration_string() }}{% endif %});

{% endif %}
private

   -- The component class instance record:
{% if discriminant.description %}
{{ printMultiLine(discriminant.description, '   -- ') }}
{% endif %}
{% if discriminant.parameters %}
   --
{{ printMultiLine("Discriminant Parameters:", '   -- ') }}
{% for p in discriminant.parameters %}
{{ printMultiLine(p.name + " : " + p.type + ((" - " + p.description) if (p.description) else ""), '   -- ') }}
{% endfor %}
   --
{% endif %}
   type Instance{% if discriminant.parameters %} ({{ discriminant.parameter_declaration_string(include_mode=False) }}){% endif %} is new {{ name }}.Base_Instance with record
      null; -- TODO
   end record;

   ---------------------------------------
   -- Set Up Procedure
   ---------------------------------------
   -- Null method which can be implemented to provide some component
   -- set up code. This method is generally called by the assembly
   -- main.adb after all component initialization and tasks have been started.
   -- Some activities need to only be run once at startup, but cannot be run
   -- safely until everything is up and running, i.e. command registration, initial
   -- data product updates. This procedure should be implemented to do these things
   -- if necessary.
   overriding procedure Set_Up (Self : in out Instance) is null;

{% if connectors.invokee() %}
   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
{% for connector in connectors.invokee() %}
{% if connector.description %}
{{ printMultiLine(connector.description, '   -- ') }}
{% endif %}
{% if connector.kind == "return" %}
   overriding function {{ connector.name }} (Self : in out Instance{% if connector.count == 0 or connector.count > 1 %}; Index : in {{ connector.name }}_Index{% endif %}) return {{ connector.return_type }};
{% elif connector.kind == "service" %}
   overriding function {{ connector.name }} (Self : in out Instance{% if connector.count == 0 or connector.count > 1 %}; Index : in {{ connector.name }}_Index{% endif %}; Arg : {{ connector.mode }} {{ connector.type }}) return {{ connector.return_type }};
{% else %}
   overriding procedure {{ connector.name }} (Self : in out Instance{% if connector.count == 0 or connector.count > 1 %}; Index : in {{ connector.name }}_Index{% endif %}; Arg : {{ connector.mode }} {{ connector.type }});
{% endif %}
{% if connector.kind == "recv_async" %}
   -- This procedure is called when a {{ connector.name }} message is dropped due to a full queue.
   overriding procedure {{ connector.name }}_Dropped (Self : in out Instance{% if connector.count == 0 or connector.count > 1 %}; Index : in {{ connector.name }}_Index{% endif %}; Arg : {{ connector.mode }} {{ connector.type }});
{% endif %}
{% endfor %}

{% endif %}
{% if connectors.of_kind("send") %}
   ---------------------------------------
   -- Invoker connector primitives:
   ---------------------------------------
{% for connector in connectors.of_kind("send") %}
   -- This procedure is called when a {{ connector.name }} message is dropped due to a full queue.
   overriding procedure {{ connector.name }}_Dropped (Self : in out Instance{% if connector.count == 0 or connector.count > 1 %}; Index : in {{ connector.name }}_Index{% endif %}; Arg : in {{ connector.type }}) is null;
{% endfor %}

{% endif %}
{% if execution in ["active", "either"] and not connectors.of_kind("recv_async") %}
   ---------------------------------------------------
   -- Definition of cycle function for task execution:
   ---------------------------------------------------
   -- This is an active component with no queue, so the
   -- cycle function for the component's task must be
   -- implemented here in the implementation class as
   -- a user defined custom function.
   overriding procedure Cycle (Self : in out Instance);

{% endif %}
{% if tasks.has_subtasks %}
   -------------------------------------------------------
   -- Definition of subtasks functions for task execution:
   -------------------------------------------------------
{% for subtask in tasks.subtask_list %}
{% if subtask.description %}
{{ printMultiLine(subtask.description, '   -- ') }}
{% endif %}
   overriding procedure {{ subtask.name }} (Self : in out Instance);
{% endfor %}

{% endif %}
{% if commands %}
   -----------------------------------------------
   -- Command handler primitives:
   -----------------------------------------------
{% if commands.description %}
   -- Description:
{{ printMultiLine(commands.description, '   --    ') }}
{% endif %}
{% for command in commands %}
{% if command.description %}
{{ printMultiLine(command.description, '   -- ') }}
{% endif %}
   overriding function {{ command.name }} (Self : in out Instance{% if command.type %}; Arg : in {{ command.type }}{% endif %}) return Command_Execution_Status.E;
{% endfor %}

   -- Invalid command handler. This procedure is called when a command's arguments are found to be invalid:
   overriding procedure Invalid_Command (Self : in out Instance; Cmd : in Command.T; Errant_Field_Number : in Unsigned_32; Errant_Field : in Basic_Types.Poly_Type);

{% endif %}
{% if parameters %}
   -----------------------------------------------
   -- Parameter primitives:
   -----------------------------------------------
{% if parameters.description %}
   -- Description:
{{ printMultiLine(parameters.description, '   --    ') }}
{% endif %}

   -- Invalid parameter handler. This procedure is called when a parameter's type is found to be invalid:
   overriding procedure Invalid_Parameter (Self : in out Instance; Par : in Parameter.T; Errant_Field_Number : in Unsigned_32; Errant_Field : in Basic_Types.Poly_Type);
   -- This procedure is called when the parameters of a component have been updated. The default implementation of this
   -- subprogram in the implementation package is a null procedure. However, this procedure can, and should be implemented if
   -- something special needs to happen after a parameter update. Examples of this might be copying certain parameters to
   -- hardware registers, or performing other special functionality that only needs to be performed after parameters have
   -- been updated.
   overriding procedure Update_Parameters_Action (Self : in out Instance) is null;
   -- This function is called when the parameter operation type is "Validate". The default implementation of this
   -- subprogram in the implementation package is a function that returns "Valid". However, this function can, and should be
   -- overridden if something special needs to happen to further validate a parameter. Examples of this might be validation of
   -- certain parameters beyond individual type ranges, or performing other special functionality that only needs to be
   -- performed after parameters have been validated. Note that range checking is performed during staging, and does not need
   -- to be implemented here.
   overriding function Validate_Parameters (
      Self : in out Instance;
{% for par in parameters %}
      {{ par.name }} : in {% if par.type_package %}{{ par.type_package }}.U{% else %}{{ par.type }}{% endif %}{{ ";" if not loop.last }}
{% endfor %}
   ) return Parameter_Validation_Status.E is (Parameter_Validation_Status.Valid);

{% endif %}
{% if data_dependencies %}
   -----------------------------------------------
   -- Data dependency primitives:
   -----------------------------------------------
{% if data_dependencies.description %}
   -- Description:
{{ printMultiLine(data_dependencies.description, '   --    ') }}
{% endif %}
   -- Function which retrieves a data dependency.
   -- The default implementation is to simply call the Data_Product_Fetch_T_Request connector. Change the implementation if this component
   -- needs to do something different.
   overriding function Get_Data_Dependency (Self : in out Instance; Id : in Data_Product_Types.Data_Product_Id) return Data_Product_Return.T is (Self.Data_Product_Fetch_T_Request ((Id => Id)));

   -- Invalid data dependency handler. This procedure is called when a data dependency's id or length are found to be invalid:
   overriding procedure Invalid_Data_Dependency (Self : in out Instance; Id : in Data_Product_Types.Data_Product_Id; Ret : in Data_Product_Return.T);

{% endif %}
end Component.{{ name }}.Implementation;
