################################################################################
# {{ formatType(model_name) }} {{ formatType(model_type) }}
#
# Generated from {{ filename }} on {{ time }}.
################################################################################
{% if length %}

from base_classes.packed_type_base import PackedTypeBase
from bitstring import BitArray
{% if element.is_packed_type %}

# Internal packed type imports:
from {{ element.type_package|lower }} import {{ element.type_package }}
{% elif element.is_enum %}

# Internal packed type imports:
from {{ element.type_package|lower }} import {{ element.type_model.name }}
{% endif %}
{% endif %}

{% if length %}
{% if description %}

{{ printMultiLine(description, '# ') }}
{% endif %}


class {{ name }}(PackedTypeBase):

    def __init__(self, elements=[]):

        # Static length:
        self.length = {{ length }}

        # Check elements:
        if elements:
            assert isinstance(elements, list), \
                "Expected type for elements to be 'list' and instead found '" + str(type(elements))
            assert len(elements) == self.length, \
                ("Expected length of element array to be '" + str(self.length) + "' "
                 "but instead found length of: " + str(len(elements)))
            for e in elements:
                if e is not None:
{% if element.is_packed_type %}
                    assert isinstance(e, {{ element.type_package }}), \
                        ("Expected type for elements to be '{{ element.type_package }}' "
                         "and instead found '" + str(type(e)))
{% elif element.is_enum %}
                    assert isinstance(e, {{ element.type_model.name }}), \
                        ("Expected type for elements to be '{{ element.type_model.name }}' "
                         "and instead found '" + str(type(e)))
{% else %}
{% if element.format %}
{% if element.format.length %}
                    assert isinstance(e, list), \
                        "Expected type for elements to be 'list' and instead found '" + str(type(e))
                    assert len(e) == {{ element.format.length }}, \
                        ("Expected length of list for elements to be '{{ element.format.length }}' "
                         "but instead found a list of length '" + str(len(e)) + "'.")
{% else %}
                    assert isinstance(e, {{ element.format.get_python_type_string() }}), \
                        ("Expected type for elements to be '{{ element.format.get_python_type_string() }}' "
                         "and instead found '" + str(type(e)))
{% if element.format.type[0] == "U" %}
                    assert e >= 0, \
                        ("elements are of unsigned type and must be positive. Received value of "
                         "'" + str(e) + "' is not allowed.")
{% endif %}
{% endif %}
{% endif %}
{% endif %}
        else:
            elements = [None for idx in range(self.length)]
        self.elements = elements

        # Sizes:
{% if element.is_packed_type %}
        self.element_size = {{ element.type_package }}()._size  # in bits
{% else %}
        self.element_size = {{ element.size }}  # in bits
{% endif %}
        self._size = self.element_size * self.length  # in bits
        self._size_in_bytes = int(int(self._size - 1)/8 + 1)  # in bytes
        self._min_serialized_length_in_bits = self._size  # in bits
        self._max_serialized_length_in_bits = self._size  # in bits
        self._min_serialized_length = self._size_in_bytes  # in bytes
        self._max_serialized_length = self._size_in_bytes  # in bytes

    def __eq__(self, other):
        return isinstance(other, self.__class__) and other.length == self.length and other.elements == self.elements

    def serialized_length(self):  # in bytes
        return self._size_in_bytes

    def is_equal(self, other, num_elements_to_compare=None):
        """
        Special __eq__ function when you only want to compare a certain number of elements in the array
        not every element in the array.
        """
        if num_elements_to_compare is None:
            num_elements_to_compare = self.length
        return isinstance(other, self.__class__) and \
            other.length == self.length and \
            other.elements[:num_elements_to_compare] == self.elements[:num_elements_to_compare]

    def _from_byte_array(self, stream):
        # Extract each element from the bitstream:
        elements = [None for idx in range(self.length)]
        for idx in range(len(elements)):
{% if element.is_packed_type %}
            elements[idx] = {{ element.type_package }}._create_from_stream(stream)
{% elif element.is_enum %}
            elements[idx] = {{ element.type_model.name }}(stream.read("{{ element.format.get_bitarray_string() }}"))
{% else %}
            elements[idx] = stream.read{% if element.format and element.format.length %}list{% endif %}("{{ element.format.get_bitarray_string() }}")
{% endif %}
        # Fill in self:
        {{ name }}.__init__(self=self, elements=elements)

    def _to_byte_array(self):
        bits = BitArray()
        for e in self.elements:
{% if element.is_packed_type %}
            bits += e._to_byte_array() if e is not None else BitArray("uint:" + str(self.element_size) + "=0")
{% elif element.format and element.format.length %}
            if e:
                for item in e:
                    bits += BitArray("{{ element.format.get_element_bitarray_string() }}=" + str(item))
            else:
                bits += BitArray("uint:" + str(self.element_size) + "=0")
{% else %}
            bits += BitArray("{{ element.format.get_bitarray_string() }}=" + str(e{% if element.is_enum %}.value{% endif %} if e is not None else "0"))
{% endif %}
        # Make sure the bit array is the correct size:
        assert len(bits) <= self._max_serialized_length_in_bits, \
            "Expected: " + str(len(bits)) + " <= " + str(self._max_serialized_length_in_bits)
        assert len(bits) >= self._min_serialized_length_in_bits, \
            "Expected: " + str(len(bits)) + " >= " + str(self._min_serialized_length_in_bits)
        return bits

    def to_string(self, prefix=""):
        strn = prefix + self.to_byte_string() + "\n"
        strn += prefix + "{{ name }} : array {{ element.type }} [0:{{ length - 1 }}] => [\n"
        for e in self.elements:
{% if element.type_model %}
            strn += prefix + "    " + ((e.to_tuple_string()) if e is not None else str(None)) + "\n"
{% else %}
            strn += prefix + "    " + str(e) + "\n"
{% endif %}
        return strn + prefix + "]"

    def to_tuple_string(self):
        strn = "["
        for e in self.elements:
{% if element.type_model %}
            strn += (e.to_tuple_string() if e is not None else str(None)) + ", "
{% else %}
            strn += str(e) + ", "
{% endif %}
        return strn[:-2] + "]"
{%- else %}
# Python class not generated for unconstrained arrays (length not specified).
# Unconstrained arrays have dynamic length and cannot be represented by this fixed-length class.
{%- endif -%}
