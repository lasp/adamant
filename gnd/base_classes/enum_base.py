from enum import Enum, unique


@unique
class EnumBase(Enum):
    """
    Base class that all Adamant python enums should inherit from.
    It provides methods common to all enumerations.
    """
    def to_tuple_string(self):
        return self.name

    def to_string(self, prefix=""):
        return prefix + ("%s (%d)" % (self.name, self.value))
