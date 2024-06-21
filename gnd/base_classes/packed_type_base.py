import abc
from bitstring import BitStream


class PackedTypeBase(metaclass=abc.ABCMeta):
    """This is the base object for adamant based packed types."""
    @abc.abstractmethod
    def _from_byte_array(self, stream):
        pass

    def from_byte_array(self, byte_array):
        return self._from_byte_array(BitStream(byte_array))

    @classmethod
    def create_from_byte_array(cls, byte_array):
        obj = cls()
        obj.from_byte_array(byte_array)
        return obj

    @classmethod
    def _create_from_stream(cls, stream):
        obj = cls()
        obj._from_byte_array(stream)
        return obj

    @abc.abstractmethod
    def _to_byte_array(self):
        pass

    def to_byte_array(self):
        return self._to_byte_array().tobytes()

    @abc.abstractmethod
    def serialized_length(self):
        pass  # in bytes

    # @abc.abstractmethod
    # def get_field(self, field_number): pass

    @abc.abstractmethod
    def to_string(self, prefix=""):
        pass

    @abc.abstractmethod
    def to_tuple_string(self):
        pass

    def to_byte_string(self):
        return "[" + " ".join(["%02X" % a for a in list(self.to_byte_array())]) + "]"

    def __repr__(self):
        return self.to_string()

    def __str__(self):
        return self.__repr__()
