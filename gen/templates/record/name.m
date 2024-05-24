%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% {{ formatType(model_name) }} {{ formatType(model_type) }}
%%
%% Generated from {{ filename }} on {{ time }}.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

classdef {{ name }}
{% if description %}
{{ printMultiLine(description, '  % ') }}
{% endif %}

  properties
{% for field in fields.values() %}
    {{ field.name }}
{% endfor %}
    size
    size_in_bytes
    min_serialized_length_in_bits
    max_serialized_length_in_bits
    min_serialized_length
    max_serialized_length
    num_fields
  end

  methods
    function self = {{ name }}({% for field in fields.values() %}{{ field.name }}{{ ", " if not loop.last }}{% endfor %})
      % Constructor

      % If no arguments are provided, the return an empty (zero version) of the type:
      if nargin == 0
        self = {{ name }}.create_empty();
        return
      end

      % If more than zero arguments provided, make sure an argument was provided for each
      % field in the packed type, and no more.
      if nargin ~= {{ fields|length }}
        error("{{ name }} expects {{ fields|length }} arguments but only got " + nargin + ".");
      end

      %
      % Construct and instance of the class by filling in the object's properties.
      %

{% for field in fields.values() %}
      % Check and set {{ field.name }}:
{% if field.description %}
{{ printMultiLine(field.description, '      % ') }}
{% endif %}
{% if field.is_enum %}
{% set split_enum_type = field.type.split('.') %}
      if class({{ field.name }}) ~= "{{ split_enum_type[1] }}"
        error("Expected type for field '{{ field.name }}' to be '{{ split_enum_type[1] }}' and instead found '" + class({{ field.name }}) + "'.");
      end
      self.{{ field.name }} = {{ field.name }};
{% elif field.is_packed_type %}
      if class({{ field.name }}) ~= "{{ field.type_package }}"
        error("Expected type for field '{{ field.name }}' to be '{{ field.type_package }}' and instead found '" + class({{ field.name }}) + "'.");
      end
{% if field.variable_length %}
      if length({{ field.name }}) > {{ field.type_package }}().length()
        error("Length of '{{ field.name }}' is determined by '{{ field.variable_length }}' which is '" + self.{{ field.variable_length }} + "' which exceeds maximum allowed length: " + {{ field.type_package }}().length());
      end
{% endif %}
      self.{{ field.name }} = {{ field.name }};
{% elif field.format %}
{% if field.format.length %}
{% if field.variable_length %}
      if length({{ field.name }}) > {{ field.format.length }}
        error("Length of {{ field.name }} is '" + length({{ field.name }}) + "' which exceeds maximum allowed length: " + {{ field.format.length }});
      end
      if {{ field.variable_length_field.format.get_matlab_type_string() }}(self.{{ field.variable_length }}) + {{ field.variable_length_field.format.get_matlab_type_string() }}({{ field.variable_length_offset }}) ~= length({{ field.name }})
        error("Expected length of variable length field '{{ field.name }}' to match value of '{{ field.variable_length }} + {{ field.variable_length_offset }}' which is '" + self.{{ field.variable_length }} + {{ field.variable_length_offset }} + "', but instead found length of '" + length({{ field.name }}) + "'.");
      end
{% else %}
      if length({{ field.name }}) ~= {{ field.format.length }}
        error("Expected length of list for field '{{ field.name }}' to be '{{ field.format.length }}' but instead found a list of length '" + length({{ field.name }}) + "'.");
      end
{% endif %}
      self.{{ field.name }} = {{ field.format.get_matlab_type_string() }}({{ field.name }});
{% else %}
      if ~isnumeric({{ field.name }})
        error("Expected type for field '{{ field.name }}' to be numeric and instead found item of type '" + class({{ field.name }}) + "'.");
      end
{% if field.format.type[0] == "U" or field.format.type[0] == "I" %}
      if {{ field.name }} - round({{ field.name }}) ~= 0
        error("Expected type for field '{{ field.name }}' to be an integer type and instead found item of type '" + class({{ field.name }}) + "'.");
      end
      if {{ field.name }} > intmax('{{ field.format.get_matlab_type_string() }}')
        error("Expected type for field '{{ field.name }}' to be an integer type with a max value of " + intmax('{{ field.format.get_matlab_type_string() }}') + " but a value of '" + {{ field.name }} + "' was provided.");
      end
{% endif %}
{% if field.format.type[0] == "U" %}
      if {{ field.name }} < 0
        error("Field '{{ field.name }}' is unsigned and must be >= zero. Received value of '" + {{ field.name }} + "' is not allowed.");
      end
{% endif %}
      self.{{ field.name }} = {{ field.format.get_matlab_type_string() }}({{ field.name }});
{% endif %}
{% endif %}

{% endfor %}
      % Set the size properties
      self.size = {{ size }};
      self.size_in_bytes = {{ name }}.length();
{% if variable_length %}
      self.min_serialized_length_in_bits = {{ min_size }}; % in bits
      self.max_serialized_length_in_bits = self.size; % in bits
      self.min_serialized_length = {{ name }}.min_length(); % in bytes
      self.max_serialized_length = self.size_in_bytes; % in bytes
{% else %}
      self.min_serialized_length_in_bits = self.size; % in bits
      self.max_serialized_length_in_bits = self.size; % in bits
      self.min_serialized_length = self.size_in_bytes; % in bytes
      self.max_serialized_length = self.size_in_bytes; % in bytes
{% endif %}

      % Other attributes:
      self.num_fields = {{ num_fields }};
    end

    function pred = eq(self, other)
      % Equality operator, ie. "=="
      pred = length(class(self)) == length(class(other));
      if pred
        pred = all(class(self) == class(other)) && ...
{% for field in fields.values() %}
{% if field.is_packed_type and field.variable_length %}
               self.{{ field.name }}.is_equal(other.{{ field.name }}, {{ field.variable_length_field.format.get_matlab_type_string() }}(self.{{ field.variable_length }}) + {{ field.variable_length_field.format.get_matlab_type_string() }}({{ field.variable_length_offset }})){{ " && ..." if not loop.last else ";" }}
{% else %}
               all(self.{{ field.name }} == other.{{ field.name }}){{ " && ..." if not loop.last else ";" }}
{% endif %}
{% endfor %}
      end
    end

    function pred = ne(self, other)
      % Inequality operator, ie. "~="
      pred = ~eq(self, other);
    end

    function len_in_bytes = serialized_length(self) % in bytes
      % Return the length of this object when serialized into an array of bytes
{% if variable_length %}
      len_in_bytes = length(self.to_byte_array());
{% else %}
      len_in_bytes = self.size_in_bytes;
{% endif %}
    end

    function self = from_byte_array(~, bytes)
      % Deserialize the object from a flat array of bytes into the class properties. This
      % one operates on self.
      self = {{ name }}.create_from_byte_array(bytes);
    end

    function bytes = to_byte_array(self)
      % Serialize the object properties into a flat array of bytes

      % Create a Bit_Array class to serialize our data into a byte array:
      ba = Bit_Array();
      ba = ba.zeros(self.size_in_bytes);

{% for field in fields.values() %}
      % Serialize {{ field.name }}:
{% if field.is_packed_type %}
{% if field.variable_length %}
      element_length_in_bytes = idivide(uint64(self.{{ field.name }}.element_size), uint64(8));
      num_bytes_to_serialize = uint64({{ field.variable_length_field.format.get_matlab_type_string() }}(self.{{ field.variable_length }}) + {{ field.variable_length_field.format.get_matlab_type_string() }}({{ field.variable_length_offset }})) * element_length_in_bytes;
      temp = self.{{ field.name }}.to_byte_array();
      for jdx=1:1:num_bytes_to_serialize
        ba = ba.set({{ field.start_bit + 1 }} + (jdx-1)*8, {{ field.start_bit + 8 }} + (jdx-1)*8, temp(jdx));
      end
{% else %}
      temp = self.{{ field.name }}.to_byte_array();
      for jdx=1:1:length(temp)
        ba = ba.set({{ field.start_bit + 1 }} + (jdx-1)*8, {{ field.start_bit + 8 }} + (jdx-1)*8, temp(jdx));
      end
{% endif %}
{% else %}
{% if field.format and field.format.length %}
{% if field.variable_length %}
      for jdx=1:1:({{ field.variable_length_field.format.get_matlab_type_string() }}(self.{{ field.variable_length }}) + {{ field.variable_length_field.format.get_matlab_type_string() }}({{ field.variable_length_offset }}))
{% else %}
      for jdx=1:1:length(self.{{ field.name }})
{% endif %}
        jdx_m1 = uint64(jdx - 1);
        ba = ba.set({{ field.start_bit + 1 }} + jdx_m1*{{ field.format.unit_size }}, {{ field.start_bit }} + {{ field.format.unit_size }} + jdx_m1*{{ field.format.unit_size }}, self.{{ field.name }}(jdx));
      end
{% else %}
{% if field.is_enum %}
      ba = ba.set({{ field.start_bit + 1 }}, {{ field.end_bit + 1 }}, {{ field.format.get_matlab_type_string() }}(self.{{ field.name }}));
{% else %}
      ba = ba.set({{ field.start_bit + 1 }}, {{ field.end_bit + 1 }}, self.{{ field.name }});
{% endif %}
{% endif %}
{% endif %}

{% endfor %}
      % Return the byte array.
      bytes = ba.bytes();
{% if variable_length %}
      bytes = bytes(1:ba.last_byte_set);
{% endif %}
    end

    function strn = to_byte_string(self)
      % Return a print out of the bytes in the serialized class.
      bytes = self.to_byte_array();
      strn = "[";
      for idx=1:length(bytes)
        strn = strn + sprintf("%02x ", bytes(idx));
      end
      strn = strtrim(strn) + "]";
    end

    function strn = to_string(self, prefix)
      % Return a human readable string representation of the object.
      if nargin < 2
        prefix = "";
      end

      % Add the byte string to start:
      strn = self.to_byte_string();

      % Append human readable items:
      strn = prefix + strn + newline + ...
{% for field in fields.values() %}
             prefix + "{{ field.name }} : {{ field.type }} => " + ...
{% if field.is_enum %}
             sprintf("%s", self.{{ field.name }}) + ...
{% elif field.type_model %}
             newline + self.{{ field.name }}.to_string(prefix + "  ") + ...
{% elif field.format and field.format.length %}
             "[" + strtrim(sprintf("%{{ field.format.get_printf_string() }} ", self.{{ field.name }})) + "]" + ...
{% else %}
             self.{{ field.name }} + ...
{% endif %}
             {{ "newline + ..." if not loop.last else "'';"}}
{% endfor %}
    end

    function strn = to_tuple_string(self)
      % Return a comma separated representation of the object.
      strn = '(' + ...
{% for field in fields.values() %}
             "{{ field.name }} => " + ...
{% if field.is_enum %}
             sprintf("%s", self.{{ field.name }}) + ...
{% elif field.type_model %}
             self.{{ field.name }}.to_tuple_string() + ...
{% elif field.format and field.format.length %}
             "[" + strtrim(sprintf("%{{ field.format.get_printf_string() }} ", self.{{ field.name }})) + "]" + ...
{% else %}
             self.{{ field.name }} + ...
{% endif %}
             {{ "', ' + ..." if not loop.last else "')';"}}
{% endfor %}
    end
  end

  methods(Static)
    function self = create_from_byte_array(bytes)
      % Deserialize the object from a flat array of bytes into the class properties.

      % Check type and size of incoming byte array:
      if class(bytes) ~= "uint8"
        error("type of 'bytes' must be an array of 'uint8', instead bytes is of type '" + class(bytes) + "'");
      end
      if length(bytes) < {{ name }}.min_length()
        error("size of 'bytes' (" + length(bytes) + ") too small to extract {{ name }} (must be at least " + {{ name }}.min_length() + ")");
      end

      % Create a Bit_Array class to deserialize the bytes into the
      % class properties:
      ba = Bit_Array(bytes);

{% for field in fields.values() %}
      % Deserialize {{ field.name }}:
{% if field.is_packed_type %}
{% if field.variable_length %}
      % Pad end of stream with necessary bits to make sure packed array deserialization succeeds:
      field_length_in_bytes = idivide(uint64({{ field.size }}), uint64(8));
      field_bytes = zeros(1, field_length_in_bytes, 'uint8');
      for jdx=1:1:field_length_in_bytes
        if {{ field.start_bit + 8 }} + (jdx-1)*8 <= ba.size()
          field_bytes(jdx) = ba.extract({{ field.start_bit + 1 }} + (jdx-1)*8, {{ field.start_bit + 8 }} + (jdx-1)*8, 'uint8');
        else
          break;
        end
      end
      {{ field.name }}_temp = {{ field.type_package }}.create_from_byte_array(field_bytes);
{% else %}
      field_length_in_bytes = idivide(uint64({{ field.size }}), uint64(8));
      field_length_in_bytes = min(field_length_in_bytes, length(bytes) - idivide(uint64({{ field.start_bit }}), uint64(8)));
      field_bytes = zeros(1, field_length_in_bytes, 'uint8');
      for jdx=1:1:field_length_in_bytes
        field_bytes(jdx) = ba.extract({{ field.start_bit + 1 }} + (jdx-1)*8, {{ field.start_bit + 8 }} + (jdx-1)*8, 'uint8');
      end
      {{ field.name }}_temp = {{ field.type_package }}.create_from_byte_array(field_bytes);
{% endif %}
{% else %}
{% if field.format and field.format.length %}
{% if field.variable_length %}
{% set split_variable_length_field = field.variable_length.split('.') %}
      {{ field.name }}_temp = zeros(1, {{ field.variable_length_field.format.get_matlab_type_string() }}({{ split_variable_length_field[0] }}_temp{% if split_variable_length_field|length > 1 %}{{ "." + ".".join(split_variable_length_field[1:]) }}{%endif %}) + {{ field.variable_length_field.format.get_matlab_type_string() }}({{ field.variable_length_offset }}), '{{ field.format.get_matlab_type_string() }}');
{% else %}
      {{ field.name }}_temp = zeros(1, {{ field.format.length }}, '{{ field.format.get_matlab_type_string() }}');
{% endif %}
      for jdx=1:1:length({{ field.name }}_temp)
        {{ field.name }}_temp(jdx) = ba.extract({{ field.start_bit + 1 }} + (jdx-1)*{{ field.format.unit_size }}, {{ field.start_bit }} + {{ field.format.unit_size }} + (jdx-1)*{{ field.format.unit_size }}, '{{ field.format.get_matlab_type_string() }}');
      end
{% elif field.is_enum %}
{% set split_enum_type = field.type.split('.') %}
      {{ field.name }}_temp = {{ split_enum_type[1] }}(ba.extract({{ field.start_bit + 1 }}, {{ field.end_bit + 1 }}, 'uint64'));
{% else %}
      {{ field.name }}_temp = ba.extract({{ field.start_bit + 1 }}, {{ field.end_bit + 1 }}, '{{ field.format.get_matlab_type_string() }}');
{% endif %}
{% endif %}

{% endfor %}
      % Fill in self:
      self = {{ name }}( ...
{% for field in fields.values() %}
        {{ field.name }}_temp{{ ", ..." if not loop.last else " ..."}}
{% endfor %}
      );
    end

    function self = create_empty()
      % Create an empty version of the packed type.
      self = {{ name }}.create_from_byte_array(zeros(1, {{ name }}.min_length() + uint64({{ total_variable_length_offset_bytes }}), 'uint8'));
    end

    function size_in_bytes = length()
      % Get the size (maximum) of the packed type in bytes
      size_in_bytes = {{ name }}.max_length();
    end

    function size_in_bytes = max_length()
      % Get the size (maximum) of the packed type in bytes
      size_in_bytes = idivide(uint64({{ size }} - 1), uint64(8)) + uint64(1); % in bytes
    end

    function size_in_bytes = min_length()
      % Get the size (minimum) of the packed type in bytes
      size_in_bytes = idivide(uint64({{ min_size }} - 1), uint64(8)) + uint64(1); % in bytes
    end
  end
end
