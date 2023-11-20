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
   -- {{ enum.name }} Definition:
{% if enum.description %}
{{ printMultiLine(enum.description, '   -- ') }}
{% endif %}
   package {{ enum.name }} is
      -- Enumeration type definition:
      type E is (
{% for literal in enum.literals %}
         {{ "%10s"|format(literal.name) }}{{ ", " if not loop.last else "   " }}{% if literal.description %} -- {{ literal.description + "\n" }}{% else %} --{{ "\n" }}{% endif %}
{% endfor %}
      );
      -- Enumeration type values:
      for E use (
{% for literal in enum.literals %}
         {{ "%10s => %2d"|format(literal.name, literal.value) }}{{ "," if not loop.last }}
{% endfor %}
      );
   end {{ enum.name }};

{% endfor %}
end {{ name }};
