--------------------------------------------------------------------------------
-- {{ formatType(model_name) }} {{ formatType(model_type) }} Implementation Body
--------------------------------------------------------------------------------

{% if template_adb_includes %}
-- Includes:
{% for include in template_adb_includes %}
with {{ include }};
{% endfor %}

{% endif %}
package body Component.{{ name }}.Implementation is

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
   overriding procedure Init (Self : in out Instance{% if init.parameters %}; {{ init.parameter_declaration_string() }}{% endif %}) is
      -- TODO declarations
   begin
      null; -- TODO statements
   end Init;

{% endif %}
{% if connectors.invokee() %}
   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
{% for connector in connectors.invokee() %}
{% if connector.description %}
{{ printMultiLine(connector.description, '   -- ') }}
{% endif %}
{% if connector.kind == "return" %}
   overriding function {{ connector.name }} (Self : in out Instance{% if connector.count == 0 or connector.count > 1 %}; Index : in {{ connector.name }}_Index{% endif %}) return {{ connector.return_type }} is
{% elif connector.kind == "service" %}
   overriding function {{ connector.name }} (Self : in out Instance{% if connector.count == 0 or connector.count > 1 %}; Index : in {{ connector.name }}_Index{% endif %}; Arg : {{ connector.mode }} {{ connector.type }}) return {{ connector.return_type }} is
{% else %}
   overriding procedure {{ connector.name }} (Self : in out Instance{% if connector.count == 0 or connector.count > 1 %}; Index : in {{ connector.name }}_Index{% endif %}; Arg : {{ connector.mode }} {{ connector.type }}) is
{% endif %}
{% if connector.return_type %}
      To_Return : {{ connector.return_type }};
{% elif connector.type == "Buffer.T" %}
      -- Safe, range-checked, data variable. Use this instead of directly accessing buf.Data_Pointer.
      use Basic_Types;
      subtype Safe_Buffer_Type is Byte_Array (Arg.Data_Pointer'First .. Arg.Data_Pointer'First + Arg.Length - 1);
      Safe_Data : Safe_Buffer_Type renames Arg.Data_Pointer (Safe_Buffer_Type'First .. Safe_Buffer_Type'First + Arg.Length - 1);
{% elif connector.type == "Command.T" and commands and connector.kind not in ["return"] %}
      -- Execute the command:
      Stat : constant Command_Response_Status.E := Self.Execute_Command (Arg);
{% elif connector.kind in ["modify"] and connector.type == "Parameter_Update.T" and parameters %}
{% else %}
      -- TODO declarations
{% endif %}
   begin
{% if connector.type == "Command.T" and commands and connector.kind not in ["return"] %}
      -- Send the return status:
      Self.Command_Response_T_Send_If_Connected ((Source_Id => Arg.Header.Source_Id, Registration_Id => Self.Command_Reg_Id, Command_Id => Arg.Header.Id, Status => Stat));
{% elif connector.type == "Command_Response.T" and connector.kind in ["recv_async", "recv_sync"] %}
      -- If the status of the command response is a Register_Source status, then we need to set our command
      -- source id. Otherwise we should perform the action associated with receiving a command response.
      -- if Arg.Status = Command_Response_Status.Register_Source then
      --    -- TODO set my local command source id:
      --    -- Self.Command_Source_Id := Arg.Source_Id;
      --    null;
      -- else
      --    -- TODO perform command response action.
      --    null;
      -- end if;
      null;
{% elif connector.kind in ["modify"] and connector.type == "Parameter_Update.T" and parameters %}
      -- Process the parameter update, staging or fetching parameters as requested.
      Self.Process_Parameter_Update (Arg);
{% else %}
{% if connector.return_type %}
      -- TODO statements
      return To_Return;
{% else %}
      null; -- TODO statements
{% endif %}
{% endif %}
   end {{ connector.name }};

{% endfor %}
{% for connector in connectors.invokee() %}
{% if connector.kind == "recv_async" %}
   -- This procedure is called when a {{ connector.name }} message is dropped due to a full queue.
   overriding procedure {{ connector.name }}_Dropped (Self : in out Instance{% if connector.count == 0 or connector.count > 1 %}; Index : in {{ connector.name }}_Index{% endif %}; Arg : {{ connector.mode }} {{ connector.type }}) is
   begin
      -- TODO: Usually in Adamant this procedure is implemented on the invokee side of a connector, so it is
      -- recommended that the appropriate queue overflow action be implemented below.
      -- Example:
      -- -- Throw event:
      -- Self.Event_T_Send_If_Connected (Self.Events.Dropped_Message (
      --    Self.Sys_Time_T_Get, Arg
      -- ));
      null;
   end {{ connector.name }}_Dropped;

{% endif %}
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
   overriding procedure Cycle (Self : in out Instance) is
      Ignore : Instance renames Self;
   begin
      -- TODO: implement custom task procedure. This function
      -- will be called indefinitely inside the task loop.
      pragma Assert (False);
   end Cycle;

{% endif %}
{% if tasks.has_subtasks %}
   -------------------------------------------------------
   -- Definition of subtasks functions for task execution:
   -------------------------------------------------------
{% for subtask in tasks.subtask_list %}
{% if subtask.description %}
{{ printMultiLine(subtask.description, '   -- ') }}
{% endif %}
   overriding procedure {{ subtask.name }} (Self : in out Instance) is
      Ignore : Instance renames Self;
   begin
      -- TODO: implement custom subtask procedure. This function
      -- will be called indefinitely inside the task loop.
      pragma Assert (False);
   end {{ subtask.name }};
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
   overriding function {{ command.name }} (Self : in out Instance{% if command.type %}; Arg : in {{ command.type }}{% endif %}) return Command_Execution_Status.E is
      use Command_Execution_Status;
   begin
      -- TODO statements
      return Success;
   end {{ command.name }};

{% endfor %}
   -- Invalid command handler. This procedure is called when a command's arguments are found to be invalid:
   overriding procedure Invalid_Command (Self : in out Instance; Cmd : in Command.T; Errant_Field_Number : in Unsigned_32; Errant_Field : in Basic_Types.Poly_Type) is
   begin
      -- TODO: Perform action to handle an invalid command.
      -- Example:
      -- -- Throw event:
      -- Self.Event_T_Send_If_Connected (Self.Events.Invalid_Command_Received (
      --    Self.Sys_Time_T_Get,
      --    (Id => Cmd.Header.Id, Errant_Field_Number => Errant_Field_Number, Errant_Field => Errant_Field)
      -- ));
      null;
   end Invalid_Command;

{% endif %}
{% if parameters %}
   -----------------------------------------------
   -- Parameter handlers:
   -----------------------------------------------
{% if parameters.description %}
   -- Description:
{{ printMultiLine(parameters.description, '   --    ') }}
{% endif %}
   -- Invalid Parameter handler. This procedure is called when a parameter's type is found to be invalid:
   overriding procedure Invalid_Parameter (Self : in out Instance; Par : in Parameter.T; Errant_Field_Number : in Unsigned_32; Errant_Field : in Basic_Types.Poly_Type) is
   begin
      -- TODO: Perform action to handle an invalid parameter.
      -- Example:
      -- -- Throw event:
      -- Self.Event_T_Send_If_Connected (Self.Events.Invalid_Parameter_Received (
      --    Self.Sys_Time_T_Get,
      --    (Id => Par.Header.Id, Errant_Field_Number => Errant_Field_Number, Errant_Field => Errant_Field)
      -- ));
      null;
   end Invalid_Parameter;

{% endif %}
{% if data_dependencies %}
   -----------------------------------------------
   -- Data dependency handlers:
   -----------------------------------------------
{% if data_dependencies.description %}
   -- Description:
{{ printMultiLine(data_dependencies.description, '   --    ') }}
{% endif %}
   -- Invalid data dependency handler. This procedure is called when a data dependency's id or length are found to be invalid:
   overriding procedure Invalid_Data_Dependency (Self : in out Instance; Id : in Data_Product_Types.Data_Product_Id; Ret : in Data_Product_Return.T) is
   begin
      -- TODO: Perform action to handle an invalid data dependency.
      -- Example:
      -- -- Throw event:
      -- Self.Event_T_Send_If_Connected (Self.Events.Invalid_Data_Dependency_Received (
      --    Self.Sys_Time_T_Get,
      --    (Id => Id, Request_Status => Ret.The_Status, Header => Ret.The_Data_Product.Header)
      -- ));
      null;
   end Invalid_Data_Dependency;

{% endif %}
end Component.{{ name }}.Implementation;
