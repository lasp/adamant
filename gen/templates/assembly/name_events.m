%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% {{ formatType(model_name) }} {{ formatType(model_type) }}
%%
%% Generated from {{ filename }} on {{ time }}.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function [component, event_name, param_type]={{ name|lower }}_events(id)
  switch id
{% for id, event in events.items() %}
    case {{ id }}
      component = "{{ event.suite.component.instance_name }}";
      event_name = "{{ event.name }}";
      param_type = "{% if event.type_model %}{{ event.type_package }}{% endif %}";
{% endfor %}
    otherwise
      error("Unrecognized event found of ID: " + id);
  end
end
