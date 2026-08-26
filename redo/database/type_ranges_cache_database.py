from database.database import database
from database.database import DATABASE_MODE
from database.util import get_database_file

# The purpose of the type ranges cache is to save off the computed type
# range (min/max bounds or enumeration literals) of each underlying Ada
# type encountered while producing *.type_ranges.yaml files. Computing
# these ranges requires generating, compiling, linking, and running an
# Ada program, which is expensive. Many packed types share the same
# underlying Ada types (ie. Natural or Interfaces.Unsigned_16), so
# caching computed ranges by type name allows most of these programs to
# never be built at all.
#
# The cache database lives in the session temporary directory, so
# entries only live for a single redo session. This is what makes the
# cache safe to key by type name alone: redo builds each target at most
# once per session, so the Ada source that defines a type cannot change
# underneath the cache while it is alive.

# Ada predefined scalar types (declared in package Standard) that are
# directly visible to generated code without any "with" clause. These
# are the only undotted type names that can be safely cached, since any
# other undotted name must come from a model's preamble, whose contents
# are local to that model. This list intentionally errs on the side of
# omission. A name missing from this list simply results in a cache
# miss, which is always safe.
ADA_PREDEFINED_SCALAR_TYPES = frozenset(
    [
        "Boolean",
        "Character",
        "Duration",
        "Float",
        "Integer",
        "Long_Float",
        "Long_Integer",
        "Long_Long_Float",
        "Long_Long_Integer",
        "Natural",
        "Positive",
        "Short_Float",
        "Short_Integer",
        "Short_Short_Integer",
        "Wide_Character",
        "Wide_Wide_Character",
    ]
)


def is_cacheable_type_name(type_name, model_has_preamble):
    """
    Return True if a type name may be shared through the cache across
    different packed type models.

    Dotted names (ie. Interfaces.Unsigned_16) are package-qualified and
    resolve to the same declaration everywhere in the build, so they are
    always cacheable. Undotted names are only cacheable when they are
    Ada predefined scalars AND the model has no preamble, since a
    preamble may declare a local type that shadows a predefined name.
    """
    if "." in type_name:
        return True
    return not model_has_preamble and type_name in ADA_PREDEFINED_SCALAR_TYPES


def get_type_key(type_name):
    """
    Form the cache key for a type name. The key includes the build
    directory the type ranges programs are compiled for, so that ranges
    computed under different native targets never mix. This is the same
    helper the *.type_ranges.yaml build rule uses to locate those
    programs.
    """
    from util import target

    return target.get_native_build_for() + "//" + type_name


class type_ranges_cache_database(database):
    def __init__(self, mode=DATABASE_MODE.READ_ONLY):
        """Initialize the database."""
        super(type_ranges_cache_database, self).__init__(
            get_database_file("type_ranges_cache"), mode
        )

    def store_type_range(self, type_name, type_range, source_yaml):
        """
        Store the computed range object (a type_number or type_enum from
        models.type_ranges) for a type name, along with the path of the
        *.type_ranges.yaml file whose program computed it. The source
        yaml is kept so that cache readers can depend on it via redo,
        preserving the dependency chain from any target consuming the
        cached range, through the program that computed it, to the Ada
        source files that define the type.
        """
        self.store(get_type_key(type_name), (type_range, source_yaml))

    def try_get_type_range(self, type_name):
        """
        Fetch the cache entry for a type name. Returns a tuple of
        (type_range, source_yaml) or None if no entry exists.
        """
        return self.try_fetch(get_type_key(type_name))


def touch_type_ranges_cache_database():
    """
    Create an empty type ranges cache database file, if one does
    not already exist:
    """
    import os.path

    filename = get_database_file("type_ranges_cache")
    if not os.path.isfile(filename):
        with type_ranges_cache_database(mode=DATABASE_MODE.CREATE) as db:
            # Get a dummy element to make sure database is created
            db.try_get_type_range("dummy")
