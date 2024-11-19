--------------------------------------------------------------------------------
-- {{ formatType(model_name) }} {{ formatType(model_type) }} Spec
--
-- Generated from {{ filename }} on {{ time }}.
--------------------------------------------------------------------------------

{% if description %}
{{ printMultiLine(description, '-- ') }}
{% endif %}
package {{ name }} is
{% if components %}

   -----------------------------------
   -- Public Subprograms:
   -----------------------------------
   -- The subprograms below are listed in the recommended order that they
   -- should be called at program start up.
   --
{% if component_kind_dict["init_base"] %}
   -- Initialize the base classes of each component. This procedure
   -- allocates necessary heap memory for things like queues and arrayed
   -- connectors.
   procedure Init_Base;
{% endif %}
{% if component_kind_dict["set_id_bases"] %}
   -- Set the appropriate command, event, parameter, packet and data product id bases in all the
   -- components. This also resolves the data dependency ids for components that have data
   -- dependencies.
   procedure Set_Id_Bases;
{% endif %}
{% if connections %}
   -- Attach the connectors for all the components in the assembly
   procedure Connect_Components;
{% endif %}
{% if component_kind_dict["init"] %}
   -- Call the implementation initialization procedure of each component
   procedure Init_Components;
{% endif %}
{% if task_list %}
   -- Procedure which releases all the active components in the system, allowing
   -- their tasks to execute:
   procedure Start_Components;
   -- Procedure which ends all of the active components in the system, exiting their
   -- task loops. This procedure should not be called on an embedded system.
   procedure Stop_Components;
{% endif %}
   -- Call the component set up procedures. This is generally called after all
   -- component initialization has been completed and tasks have been started.
   procedure Set_Up_Components;

{% endif %}
end {{ name }};
