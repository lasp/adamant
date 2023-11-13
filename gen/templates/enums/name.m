% Below is a set of enumerations: {{ name }}.
% These will be split into their own .m files by the generator.
%
{% if description %}
{{ printMultiLine(description, '  % ') }}
{% endif %}
%
{% for enum in enums.values() %}
%!%!-split-here!%!%
{{ enum.name }}.m
%!%!-split-again-here!%!%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% {{ formatType(model_name) }} {{ formatType(model_type) }}
%%
%% Generated from {{ filename }} on {{ time }}.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
classdef {{ enum.name }} < uint64
  % {{ enum.name }} Definition:
{% if enum.description %}
{{ printMultiLine(enum.description, '  % ') }}
{% endif %}
  enumeration
{% for literal in enum.literals %}
    {{ literal.name }}{{ "_" if literal.name in ["None", "True", "False"] }} ({{ literal.value }}){% if literal.description %} % {{ literal.description + "\n" }}{% else %}{{ "\n" }}{% endif %}
{% endfor %}
  end
end
{% endfor %}
