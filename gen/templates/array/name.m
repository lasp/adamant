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
    elements
    array_length 
    element_size
    size
    size_in_bytes
    min_serialized_length_in_bits
    max_serialized_length_in_bits
    min_serialized_length
    max_serialized_length
  end

  methods
    function self = {{ name }}(elements)
      % Constructor

      % If no arguments are provided, the return an empty (zero version) of the type:
      if nargin == 0
        self = {{ name }}.create_empty();
        return
      end

      % If more than zero arguments provided, make sure only one argument was provided for
      % the element list
      if nargin ~= 1
        error("{{ name }} expects 1 argument (the list of elements) but got " + nargin + ".");
      end

      %
      % Construct and instance of the class by filling in the object's properties.
      %

      % Static length:
      self.array_length = {{ length }};

      % Check elements:
      if length(elements) ~= self.array_length
        error("Expected length of element array to be '" + self.array_length + "' but instead found length of: " + length(elements))
      end
{% if element.is_enum %}
{% set split_enum_type = element.type.split('.') %}
      if class(elements) ~= "{{ split_enum_type[1] }}"
        error("Expected type for elements to be '{{ split_enum_type[1] }}' and instead found '" + class(elements) + "'.");
      end
      self.elements = elements;
{% elif element.is_packed_type %}
      if class(elements) ~= "{{ element.type_package }}"
        error("Expected type for elements to be '{{ element.type_package }}' and instead found '" + class(elements) + "'.");
      end
      self.elements = elements;
{% elif element.format %}
{% if element.format.length %}
      if length(elements(:, 1)) ~= {{ element.format.length }}
        error("Expected length of list for elements to be '{{ element.format.length }}' but instead found a list of length '" + length(elements(:, 1)) + "'.");
      end
      self.elements = {{ element.format.get_matlab_type_string() }}(elements);
{% else %}
      if ~isnumeric(elements)
        error("Expected type for elements to be numeric and instead found item of type '" + class(elements) + "'.");
      end
      for idx=1:length(elements)
{% if element.format.type[0] == "U" or element.format.type[0] == "I" %}
        if elements(idx) - round(elements(idx)) ~= 0
          error("Expected type for elements to be an integer type and instead found item of type '" + class(elements) + "'.");
        end
        if elements(idx) > intmax('{{ element.format.get_matlab_type_string() }}')
          error("Expected type for elements to be an integer type with a max value of " + intmax('{{ element.format.get_matlab_type_string() }}') + " but a value of '" + elements(idx) + "' was provided.");
        end
{% endif %}
{% if element.format.type[0] == "U" %}
        if elements(idx) < 0
          error("Elements is unsigned and must be >= zero. Received value of '" + elements(idx) + "' is not allowed.");
        end
{% endif %}
      end
      self.elements = {{ element.format.get_matlab_type_string() }}(elements);
{% endif %}
{% endif %}

      % Set the size properties
{% if element.is_packed_type %}
      self.element_size = {{ element.type_package }}().size; % in bits
{% else %}
      self.element_size = {{ element.size }}; % in bits
{% endif %}
      self.size = self.element_size * self.array_length; % in bits
      self.size_in_bytes = {{ name }}.length();
      self.min_serialized_length_in_bits = self.size; % in bits
      self.max_serialized_length_in_bits = self.size; % in bits
      self.min_serialized_length = self.size_in_bytes; % in bytes
      self.max_serialized_length = self.size_in_bytes; % in bytes
    end

    function pred = eq(self, other)
      pred = self.is_equal(other, self.array_length)
    end

    function pred = is_equal(self, other, num_elements_to_compare)
      % Special __eq__ function when you only want to compare a certain number of elements in the array
      % not every element in the array.
      pred = length(class(self)) == length(class(other));
      if pred
{% if element.is_packed_type %}
        for idx=1:num_elements_to_compare
          pred = pred && (self.elements(idx) == other.elements(idx));
        end
{% elif element.format and element.format.length %}
        pred = all(class(self) == class(other)) && ...
               self.array_length == other.array_length;
        for idx=1:num_elements_to_compare
          pred = pred && all(self.elements(:, idx) == other.elements(:, idx));
        end
{% else %}
        pred = all(class(self) == class(other)) && ...
               self.array_length == other.array_length && ...
               all(self.elements(1:num_elements_to_compare) == other.elements(1:num_elements_to_compare));
{% endif %}
      end
    end

    function pred = ne(self, other)
      % Inequality operator, ie. "~="
      pred = ~eq(self, other);
    end

    function len_in_bytes = serialized_length(self) % in bytes
      % Return the length of this object when serialized into an array of bytes
      len_in_bytes = self.size_in_bytes;
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

      % Serialize each element:
      for idx=1:self.array_length
{% if element.is_packed_type %}
        temp = self.elements(idx).to_byte_array();
        for jdx=1:1:length(temp)
          ba = ba.set(1 + (idx-1)*self.element_size + (jdx-1)*8, 8 + (idx-1)*self.element_size + (jdx-1)*8, temp(jdx));
        end
{% elif element.is_enum %}
        ba = ba.set(1 + (idx-1)*self.element_size, self.element_size + (idx-1)*self.element_size, {{ element.format.get_matlab_type_string() }}(self.elements(idx)));
{% elif element.format and element.format.length %}
        for jdx=1:1:length(self.elements(:, idx))
          ba = ba.set(1 + (idx-1)*self.element_size + (jdx-1)*{{ element.format.unit_size }}, {{ element.format.unit_size }} + (idx-1)*self.element_size + (jdx-1)*{{ element.format.unit_size }}, self.elements(jdx, idx));
        end
{% else %}
        ba = ba.set(1 + (idx-1)*self.element_size, self.element_size + (idx-1)*self.element_size, self.elements(idx));
{% endif %}
      end

      % Return the byte array.
      bytes = ba.bytes();
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

      % Return a human readable representation of the object.
      strn = prefix + self.to_byte_string() + newline;
      strn = strn + prefix + "{{ name }} : array {{ element.type }} [0:{{ length - 1 }}] => [" + newline;
      for idx=1:self.array_length
        strn = strn + prefix + ...
{% if element.is_enum %}
               sprintf("%s", self.elements(idx)) + ...
{% elif element.type_model %}
               self.elements(idx).to_tuple_string() + ...
{% elif element.format and element.format.length %}
               "[" + strtrim(sprintf("%{{ element.format.get_printf_string() }} ", self.elements(idx))) + "]" + ...
{% else %}
               self.elements(idx) + ...
{% endif %}
               newline;
      end
      strn = strn + prefix + "]";
      strn = char(strn);
    end

    function strn = to_tuple_string(self)
      % Return a comma separated representation of the object.
      strn = "[";
      for idx=1:self.array_length
        strn = strn + ...
{% if element.is_enum %}
               sprintf("%s", self.elements(idx)) + ...
{% elif element.type_model %}
               self.elements(idx).to_tuple_string() + ...
{% elif element.format and element.format.length %}
               "[" + strtrim(sprintf("%{{ element.format.get_printf_string() }} ", self.elements(idx))) + "]" + ...
{% else %}
               self.elements(idx) + ...
{% endif %}
               ", ";
      end
      strn = char(strn);
      strn = [strn(1:length(strn)-2), ']'];
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
{% if element.is_packed_type %}
      elements = repmat({{ element.type_package }}(), 1, {{ length }}); 
{% else %}
      elements = zeros({% if element.format.length %}{{ element.format.length }}{% else %}1{% endif %}, {{ length }}, '{{ element.format.get_matlab_type_string() }}');
{% endif %}
{% if element.is_packed_type %}
      element_size = {{ element.type_package }}().size; % in bits
{% else %}
      element_size = {{ element.size }}; % in bits
{% endif %}

      for idx=1:{{ length }}
        % Deserialize element:
{% if element.is_packed_type %}
        element_length_in_bytes = idivide(uint64(element_size), uint64(8));
        element_bytes = zeros(1, element_length_in_bytes, 'uint8');
        for jdx=1:1:element_length_in_bytes
          element_bytes(jdx) = ba.extract(1 + (idx-1)*element_size + (jdx-1)*8, 8 + (idx-1)*element_size + (jdx-1)*8, 'uint8');
        end
        element_temp = {{ element.type_package }}.create_from_byte_array(element_bytes);
{% else %}
{% if element.format and element.format.length %}
        element_temp = zeros(1, {{ element.format.length }}, '{{ element.format.get_matlab_type_string() }}');
        for jdx=1:1:length(element_temp)
          element_temp(jdx) = ba.extract(1 + (idx-1)*element_size + (jdx-1)*{{ element.format.unit_size }}, (idx-1)*element_size + {{ element.format.unit_size }} + (jdx-1)*{{ element.format.unit_size }}, '{{ element.format.get_matlab_type_string() }}');
        end
{% elif element.is_enum %}
{% set split_enum_type = element.type.split('.') %}
        element_temp = ba.extract(1 + (idx-1)*element_size, (idx-1)*element_size + element_size, 'uint64');
{% else %}
        element_temp = ba.extract(1 + (idx-1)*element_size, (idx-1)*element_size + element_size, '{{ element.format.get_matlab_type_string() }}');
{% endif %}
{% endif %}
        % Append new element to list:
        elements(:, idx) = element_temp;
      end

      % Fill in self:
{% if element.is_enum %}
{% set split_enum_type = element.type.split('.') %}
      self = {{ name }}({{ split_enum_type[1] }}(elements));
{% else %}
      self = {{ name }}(elements);
{% endif %}
    end

    function self = create_empty()
      % Create an empty version of the packed type.
      self = {{ name }}.create_from_byte_array(zeros(1, {{ name }}.length(), 'uint8'));
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
      size_in_bytes = {{ name }}.max_length();
    end
  end
end
