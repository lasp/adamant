--------------------------------------------------------------------------------
-- {{ formatType(model_name) }} {{ formatType(model_type) }} Body
--
-- Generated from {{ filename }} on {{ time }}.
--------------------------------------------------------------------------------

{% if adb_includes %}
-- Includes:
{% for include in adb_includes %}
with {{ include }};
{% endfor %}
{% for include in includes %}
{% if include not in adb_includes %}
pragma Warnings (Off, "unit ""{{ include }}"" is not referenced");
with {{ include }};
pragma Warnings (On, "unit ""{{ include }}"" is not referenced");
{% endif %}
{% endfor %}

{% endif %}
package body {{ name }} is

   use {{ name }}_Components;

{% if component_kind_dict["init_base"] %}
   procedure Init_Base is
   begin
      -----------------------------------
      -- Heap Initialization:
      -----------------------------------
{% for component in component_kind_dict["init_base"] %}
      {{ component.instance_name }}.Init_Base{% if component.init_base.parameter_call_string() %} ({{ component.init_base.parameter_call_string() }}){% endif %};
{% endfor %}
   end Init_Base;

{% endif %}
{% if component_kind_dict["set_id_bases"] %}
   procedure Set_Id_Bases is
   begin
      -----------------------------------
      -- ID Base Initialization:
      -----------------------------------
{% for component in component_kind_dict["set_id_bases"] %}
      {{ component.instance_name }}.Set_Id_Bases{% if component.set_id_bases.parameter_call_string() %} ({{ component.set_id_bases.parameter_call_string() }}){% endif %};
{% endfor %}

      -----------------------------------
      -- Data dependency ID Mapping:
      -----------------------------------
{% for component in component_kind_dict["data_dependencies"] %}
      {{ component.instance_name }}.Map_Data_Dependencies{% if component.map_data_dependencies.parameter_call_string() %} ({{ component.map_data_dependencies.parameter_call_string() }}){% endif %};
{% endfor %}
   end Set_Id_Bases;

{% endif %}
{% if connections %}
   procedure Connect_Components is
   begin
      -----------------------------------
      -- Component Connections:
      -----------------------------------
{% for connection in connections %}
{% if connection.description %}
{{ printMultiLine(connection.description, '      -- ') }}
{% endif %}
      {{ connection.from_component.instance_name }}.Attach_{{ connection.from_connector.name }} ({% if connection.from_connector.count != 1 or connection.from_connector.unconstrained %}From_Index => {{ connection.from_index }}, {% endif %}To_Component => {{ connection.to_component.instance_name }}'Access, Hook => {{ connection.to_component.instance_name }}.{{ connection.to_connector.name }}_Access{% if connection.to_connector.count != 1 or connection.to_connector.unconstrained %} (Index => {{ connection.to_index }}), To_Index => {{ connection.to_index }}{% endif %});
{% endfor %}
   end Connect_Components;

{% endif %}
{% if component_kind_dict["init"] %}
   procedure Init_Components is
   begin
      -----------------------------------
      -- Component Initialization:
      -----------------------------------
{% for component in component_kind_dict["init"] %}
      {{ component.instance_name }}.Init{% if component.init.parameter_call_string() %} ({{ component.init.parameter_call_string() }}){% endif %};
{% endfor %}
   end Init_Components;

{% endif %}
{% if task_list %}
   procedure Start_Components is
   begin
      -----------------------------------
      -- Component Task Start:
      -----------------------------------
{% for task in task_list %}
      Ada.Synchronous_Task_Control.Set_True ({{ task.component_name }}_{{ task.name }}_Task_Signal);
{% endfor %}
   end Start_Components;

   procedure Stop_Components is
   begin
      -----------------------------------
      -- Component Task Stop:
      -----------------------------------
{% for task in task_list %}
      Ada.Synchronous_Task_Control.Set_True ({{ task.component_name }}_{{ task.name }}_Task_Signal);
{% if task.component.connectors.of_kind("recv_async") %}
      {{ task.component_name }}.Stop_Task;
{% endif %}
{% endfor %}
   end Stop_Components;

{% endif %}
   -- Call the component set up procedures. This is generally called after all
   -- component initialization has been completed and tasks have been started.
   procedure Set_Up_Components is
   begin
      -----------------------------------
      -- Component Set Up:
      -----------------------------------
{% for component in components.values() %}
      {{ component.instance_name }}.Set_Up;
{% endfor %}
   end Set_Up_Components;

end {{ name }};
