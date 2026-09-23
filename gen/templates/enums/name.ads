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

      -- C version of E for passing across a C/C++ binding. C and Ada use
      -- different sizes to hold an enumeration: C uses an int, Ada uses the
      -- smallest size that fits the literals. E_C has the size of a C int and
      -- the same literal names and values as E.
      package C is
         -- C enumeration type definition:
         type E_C is (
{% for literal in enum.literals %}
            {{ literal.name }}{{ "," if not loop.last }}
{% endfor %}
         ) with Convention => C;
         -- C enumeration type values:
         for E_C use (
{% for literal in enum.literals %}
            {{ "%-*s => %*d"|format(name_width, literal.name, value_width, literal.value) }}{{ "," if not loop.last }}
{% endfor %}
         );

         -- Conversions between E and E_C. Both map by literal value.
         function To_C (Src : in E) return E_C is (E_C'Enum_Val (E'Enum_Rep (Src)))
            with Inline => True;

         -- A value that arrives from C may hold any int. To_Ada raises
         -- Constraint_Error when Src is not a literal of E. Check Src'Valid
         -- first to handle that case without an exception.
         function To_Ada (Src : in E_C) return E is (E'Enum_Val (E_C'Enum_Rep (Src)))
            with Inline => True;
      end C;
   end {{ enum.name }};

{% endfor %}
end {{ name }};
