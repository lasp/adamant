import abc
import contextvars
from bitstring import BitStream

# Context variable for epsilon - default 0.0 matches Ada exact comparison convention
_epsilon_var = contextvars.ContextVar('epsilon', default=0.0)


class epsilon:
    """Context manager for temporarily changing the epsilon used in float comparisons.

    Usage:
        with epsilon(0.0001):
            a == b  # Uses epsilon=0.0001 (tolerant comparison)
        a == b  # Uses default epsilon=0.0 (exact comparison)
    """
    def __init__(self, value):
        self.value = value
        self.token = None

    def __enter__(self):
        self.token = _epsilon_var.set(self.value)
        return self

    def __exit__(self, *args):
        _epsilon_var.reset(self.token)


def get_epsilon():
    """Get the current epsilon value from context."""
    return _epsilon_var.get()


def set_default_epsilon(value):
    """Set the default epsilon value (affects current context)."""
    _epsilon_var.set(value)


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

    @staticmethod
    def _float_equals(a, b, epsilon=None):
        """Helper function for epsilon-based float comparison.

        Args:
            a: First float value
            b: Second float value
            epsilon: Comparison tolerance. If None, uses value from context (default 0.0).
                     Use epsilon(0.0001) context manager for tolerant comparison.
        """
        if a is None or b is None:
            return a == b
        if epsilon is None:
            epsilon = _epsilon_var.get()
        return abs(a - b) <= epsilon

    def __repr__(self):
        return self.to_string()

    def __str__(self):
        return self.__repr__()
