from enum import Enum, unique


# Base class that all Adamant python enums should enherit from
# It provides methods common to all enumerations.
@unique
class EnumBase(Enum):
    def to_tuple_string(self):
        return self.name

    def to_string(self, prefix=""):
        return prefix + ("%s (%d)" % (self.name, self.value))
