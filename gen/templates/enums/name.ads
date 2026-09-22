--------------------------------------------------------------------------------
-- {{ formatType(model_name) }} {{ formatType(model_type) }} Spec
--
-- Generated from {{ filename }} on {{ time }}.
--------------------------------------------------------------------------------

{% if includes %}

-- Custom Includes:
{% for include in includes %}
with {{ include }};
{% endfor %}

{% endif %}
{% if description %}
{{ printMultiLine(description, '-- ') }}
{% endif %}
package {{ name }} is
{% if preamble %}

   -- Preamble code:
{{ printMultiLine(preamble, '   ', 10000) }}
{% endif %}

   --
   -- Enumeration types:
   --

{% for enum in enums.values() %}
{% set name_width = enum.literals|map(attribute='name')|map('length')|max %}
{% set value_width = enum.literals|map(attribute='value')|map('string')|map('length')|max %}
   -- {{ enum.name }} Definition:
{% if enum.description %}
{{ printMultiLine(enum.description, '   -- ') }}
{% endif %}
   package {{ enum.name }} is
      -- Enumeration type definition:
      type E is (
{% for literal in enum.literals %}
{% if literal.description %}
         {{ "%-*s"|format(name_width + 1, literal.name ~ ("," if not loop.last else "")) }} -- {{ literal.description }}
{% else %}
         {{ literal.name }}{{ "," if not loop.last }}
{% endif %}
{% endfor %}
      );
      -- Enumeration type values:
      for E use (
{% for literal in enum.literals %}
         {{ "%-*s => %*d"|format(name_width, literal.name, value_width, literal.value) }}{{ "," if not loop.last }}
{% endfor %}
      );
   end {{ enum.name }};

{% endfor %}
end {{ name }};
