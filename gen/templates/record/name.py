################################################################################
# {{ formatType(model_name) }} {{ formatType(model_type) }}
#
# Generated from {{ filename }} on {{ time }}.
################################################################################

from base_classes.packed_type_base import PackedTypeBase
from bitstring import BitArray

# Internal packed type imports:
{% if complex_type_models %}
{% for model in complex_type_models %}
from {{ model.name|lower }} import {{ model.name }}
{% endfor %}
{% endif %}
{% if enum_models %}
{% for model in enum_models %}
from {{ model.suite.name|lower }} import {{ model.name }}
{% endfor %}
{% endif %}


{% if description %}
{{ printMultiLine(description, '# ') }}
{% endif %}
class {{ name }}(PackedTypeBase):

    def __init__(
        self,
{% for field in fields.values() %}
        {{ field.name }}={% if field.default_value and field.default_value.lstrip('-').replace('.','',1).isdigit() %}{{ field.default_value }}{% else %}None{% endif %}{{ "," if not loop.last }}
{% endfor %}
    ):
        # Fields:
{% for field in fields.values() %}
        # Check and set {{ field.name }}:
{% if field.description %}
{{ printMultiLine(field.description, '        # ') }}
{% endif %}
        if {{ field.name }} is not None:
{% if field.is_enum %}
            assert isinstance({{ field.name }}, {{ field.type_model.name }}), \
                ("Expected type for field '{{ field.name }}' to be '{{ field.type_model.name }}'"
                 " and instead found '" + str(type({{ field.name }})))
{% elif field.is_packed_type %}
            assert isinstance({{ field.name }}, {{ field.type_package }}), \
                ("Expected type for field '{{ field.name }}' to be '{{ field.type_package }}'"
                 "and instead found '" + str(type({{ field.name }})))
{% if field.variable_length %}
            # Make sure variable length field value is not larger than the packed array length
            if self.{{ field.variable_length}} is not None:
                assert self.{{ field.variable_length }} <= {{ field.type_package }}().length, \
                    ("Length of '{{ field.name }}' is determined by '{{ field.variable_length }}' which is '"
                     "" + str(self.{{ field.variable_length }}) + "' which exceeds maximum allowed length: " + str({{ field.type_package }}().length))
{% endif %}
{% elif field.format %}
{% if field.format.length %}
            assert isinstance({{ field.name }}, list), \
                "Expected type for field '{{ field.name }}' to be 'list' and instead found '" + str(type({{ field.name }}))
{% if field.variable_length %}
            assert len({{ field.name }}) <= {{ field.format.length }}, \
                "Length of {{ field.name }} is '" + str(len({{ field.name }})) + "' which exceeds maximum allowed length: " + str({{ field.format.length }})
            # Make sure variable length field value and actual buffer length match.
            if self.{{ field.variable_length }} is not None:
                assert (self.{{ field.variable_length }} + int({{ field.variable_length_offset }})) == len({{ field.name }}), \
                    ("Expected length of variable length field '{{ field.name }}' to match value of "
                     "'{{ field.variable_length }} + int({{ field.variable_length_offset }})'"
                     "which is '" + str((self.{{ field.variable_length }} + int({{ field.variable_length_offset }}))) + ""
                     "', but instead found length of '" + str(len({{ field.name }})) + "'.")
{% else %}
            assert len({{ field.name }}) == {{ field.format.length }}, \
                ("Expected length of list for field '{{ field.name }}' to be '{{ field.format.length }}' "
                 "but instead found a list of length '" + str(len({{ field.name }})) + "'.")
{% endif %}
{% else %}
            assert isinstance({{ field.name }}, {{ field.format.get_python_type_string() }}), \
                ("Expected type for field '{{ field.name }}' to be "
                 "'{{ field.format.get_python_type_string() }}'"
                 "and instead found '" + str(type({{ field.name }})))
{% if field.format.type[0] == "U" %}
            assert {{ field.name }} >= 0, \
                ("{{ field.name }} is unsigned and must be positive. "
                 "Received value of '" + str({{ field.name }}) + "' "
                 " is not allowed.")
{% endif %}
{% endif %}
{% endif %}
        self.{{ field.name }} = \
            {{ field.name }}
{% endfor %}

        # Sizes:
        self._size = {{ size }}  # in bits
        self._size_in_bytes = int(int(self._size - 1)/8 + 1)  # in bytes
{% if variable_length %}
        self._min_serialized_length_in_bits = {{ min_size }}  # in bits
        self._max_serialized_length_in_bits = self._size  # in bits
        self._min_serialized_length = int(int({{ min_size }} - 1)/8 + 1)  # in bytes
        self._max_serialized_length = self._size_in_bytes  # in bytes
{% else %}
        self._min_serialized_length_in_bits = self._size  # in bits
        self._max_serialized_length_in_bits = self._size  # in bits
        self._min_serialized_length = self._size_in_bytes  # in bytes
        self._max_serialized_length = self._size_in_bytes  # in bytes
{% endif %}

        # Other attributes:
        self.num_fields = {{ num_fields }}

    def __eq__(self, other):
        return isinstance(other, self.__class__) and \
{% for field in fields.values() %}
{% if field.variable_length and field.is_packed_type %}
            (self.{{ field.name }} is None and other.{{ field.name }} is None) or \
            (
                (self.{{ field.name }} is not None and other.{{ field.name }} is not None) and
                (self.{{ field.name }}.is_equal(other.{{ field.name }}, num_elements_to_compare=(self.{{ field.variable_length }} + int({{ field.variable_length_offset }}))))
            ){{ " and \\" if not loop.last }}
{% else %}
            self.{{ field.name }} == \
            other.{{ field.name }}{{ " and \\" if not loop.last }}
{% endif %}
{% endfor %}

    def serialized_length(self):  # in bytes
{% if variable_length %}
        return len(self.to_byte_array())
{% else %}
        return self._size_in_bytes
{% endif %}

    def _from_byte_array(self, stream):
        # Extract each field from the bitstream:
{% for field in fields.values() %}
{% if field.is_packed_type %}
{% if field.variable_length %}
        # Pad end of stream with necessary bits to make sure packed array deserialization succeeds:
        bits = BitArray(stream[stream.pos:])
        num_bits_to_pad = {{ field.type_package }}()._max_serialized_length_in_bits - len(bits)
        if num_bits_to_pad > 0:
            bits += BitArray("uint:" + str(num_bits_to_pad) + "=0")
        from bitstring import BitStream
        _{{ field.name }} = {{ field.type_package }}._create_from_stream(BitStream(bits))
        # Update the stream position:
        stream.pos += (_{{ field.variable_length }} + int({{ field.variable_length_offset }})) * _{{ field.name }}.element_size
{% else %}
        _{{ field.name }} = {{ field.type_package }}._create_from_stream(stream)
{% endif %}
{% else %}
{% if field.variable_length %}
        _{{ field.name }} = stream.readlist(str(_{{ field.variable_length }} + int({{ field.variable_length_offset }})) + "*{{ field.format.get_element_bitarray_string() }}")
{% elif field.is_enum %}
        _{{ field.name }} = {{ field.type_model.name }}(stream.read("{{ field.format.get_bitarray_string() }}"))
{% else %}
        _{{ field.name }} = stream.read{% if field.format and field.format.length %}list{% endif %}("{{ field.format.get_bitarray_string() }}")
{% endif %}
{% endif %}
{% endfor %}
        # Fill in self:
        {{ name }}.__init__(
            self=self,
{% for field in fields.values() %}
            {{ field.name }}=_{{ field.name }}{{ "," if not loop.last else ""}}
{% endfor %}
        )

    def _to_byte_array(self):
        bits = BitArray()
{% for field in fields.values() %}
{% if field.is_packed_type %}
{% if field.variable_length %}
        the_bits = None
        if self.{{ field.name }} is not None:
            the_bits = self.{{ field.name }}._to_byte_array()
        else:
            the_bits = BitArray("uint:{{ field.size }}=0")
        if self.{{ field.variable_length }} is not None:
            bits += the_bits[:((self.{{ field.variable_length }} + int({{ field.variable_length_offset }})) * self.{{ field.name }}.element_size)]  # Only serialize valid data.
        else:
            bits += the_bits
{% else %}
        bits += self.{{ field.name }}._to_byte_array() \
            if self.{{ field.name }} is not None else BitArray("uint:{{ field.size }}=0")
{% endif %}
{% else %}
{% if field.format and field.format.length %}
        if self.{{ field.name }}:
            for item in self.{{ field.name }}:
                bits += BitArray("{{ field.format.get_element_bitarray_string() }}=" + str(item))
{% if field.variable_length %}
        else:
            pass
{% else %}
        else:
            bits += BitArray("uint:{{ field.size }}=0")
{% endif %}
{% else %}
        bits += BitArray(
            "{{ field.format.get_bitarray_string() }}=" +
            str(self.{{ field.name }}{% if field.is_enum %}.value{% endif %}{{ "\n" }}
                if self.{{ field.name }} is not None else "0")
        )
{% endif %}
{% endif %}
{% endfor %}
        # Make sure the bit array is the correct size:
        assert len(bits) <= self._max_serialized_length_in_bits, \
            "Expected: " + str(len(bits)) + " <= " + str(self._max_serialized_length_in_bits)
        assert len(bits) >= self._min_serialized_length_in_bits, \
            "Expected: " + str(len(bits)) + " >= " + str(self._min_serialized_length_in_bits)
        return bits

    def to_string(self, prefix=""):
        strn = prefix + self.to_byte_string() + "\n"
{% for field in fields.values() %}
        strn += prefix + "{{ field.name }} : {{ field.type }} => " + \
{% if field.type_model %}
            (("\n" + self.{{ field.name }}.to_string(prefix + "    "))
                if self.{{ field.name }} is not None else str(None))
{% else %}
            str(self.{{ field.name }})
{% endif %}
        strn += "\n"
{% endfor %}
        return strn[:-1]

    def to_tuple_string(self):
        strn = "("
{% for field in fields.values() %}
        strn += "{{ field.name }} => " + \
{% if field.type_model %}
            (self.{{ field.name }}.to_tuple_string()
                if self.{{ field.name }} is not None else str(None))
{% else %}
            str(self.{{ field.name }})
{% endif %}
        strn += ", "
{% endfor %}
        return strn[:-2] + ")"
