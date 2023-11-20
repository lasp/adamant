--------------------------------------------------------------------------------
-- {{ formatType(model_name) }} {{ formatType(model_type) }} Spec
--------------------------------------------------------------------------------
{% if component and component.generic %}

-- Component Tester Include:
with Component.{{ component.name }}.Implementation.Tester;
{% endif %}
{% if description %}

{{ printMultiLine(description, '-- ') }}
{% endif %}
package {{ name }}.Implementation is

   -- Test data and state:
   type Instance is new {{ name }}.Base_Instance with private;
   type Class_Access is access all Instance'Class;

private
   -- Fixture procedures:
   overriding procedure Set_Up_Test (Self : in out Instance);
   overriding procedure Tear_Down_Test (Self : in out Instance);

{% for test in tests.values() %}
{% if test.description %}
{{ printMultiLine(test.description, '   -- ') }}
{% endif %}
   overriding procedure {{ test.name }} (Self : in out Instance);
{% endfor %}
{% if component and component.generic %}

   -- TODO: Instantiate generic component package:
   -- package Component_Package is new Component.{{ component.name }}({{ component.generic.formal_parameter_call_string() }});
   package Component_Implementation_Package is new Component_Package.Implementation;
   package Component_Tester_Package is new Component_Implementation_Package.Tester;
{% endif %}

   -- Test data and state:
   type Instance is new {{ name }}.Base_Instance with record
{% if component and component.generic %}
      -- The tester component:
      Tester : Component_Tester_Package.Instance_Access;
{% else %}
      null;
{% endif %}
   end record;
end {{ name }}.Implementation;
