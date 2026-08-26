from models.base import base
from util import ada
from util import redo_arg
from util import model_loader
from util import redo
from collections import OrderedDict
import os
import abc


class _type_ranges_cache_miss(Exception):
    """
    Raised internally when a type range cannot be served from the
    session cache. It tells load_type_ranges() to fall back to building
    and running this model's own type ranges program.
    """
    pass


class _cached_type_ranges(object):
    """
    A stand-in for a type_ranges model that resolves lookups from the
    session type ranges cache instead of from a *.type_ranges.yaml file.

    This is what lets the cache work without any packed type subclass
    knowing it exists. Every set_type_ranges() implementation already
    asks a type ranges model for exactly the types it needs, one name at
    a time, so serving those same calls from the cache requires no
    duplicate knowledge of which fields need ranges. A name that cannot
    be served raises _type_ranges_cache_miss.
    """
    def __init__(self, db, model_has_preamble):
        self.db = db
        self.model_has_preamble = model_has_preamble
        # The *.type_ranges.yaml files whose programs computed the
        # entries served here, in first seen order:
        self.source_yamls = OrderedDict()

    def get_type_by_name(self, name):
        from database.type_ranges_cache_database import is_cacheable_type_name

        # An undotted name that is not an Ada predefined scalar may be
        # declared in this model's preamble, which makes it local to
        # this model and unsafe to share by name. Note that a single
        # such type puts the whole model on the slow path: its range can
        # only come from this model's own type ranges program, and once
        # that program has to be built there is nothing left to save.
        if not is_cacheable_type_name(name, self.model_has_preamble):
            raise _type_ranges_cache_miss(name)

        try:
            entry = self.db.try_get_type_range(name)
        except Exception:
            # Treat an unreadable cache as a miss. A cache failure must
            # never fail a build, it just costs us the slow path.
            raise _type_ranges_cache_miss(name)

        if entry is None:
            raise _type_ranges_cache_miss(name)

        type_range, source_yaml = entry
        self.source_yamls[source_yaml] = None
        return type_range


class packed_type(base):
    """
    This is the object model for a packed type. It is meant to be
    the base class for more specific packed types like: record, array
    and simple packed types (ie. type).
    """
    def __init__(self, filename, template):
        """
        Initialize the packed array object, ingest data, and check it by
        calling the base class init function.
        """
        # Load the object from the file:
        super(packed_type, self).__init__(filename, template)

    @abc.abstractmethod
    def _load(self):
        """
        Abstract method which does specific loading of the packed type.
        The load() method below does the common loading for all packed types
        and then calls this abstract method.
        """
        pass

    @abc.abstractmethod
    def get_all_types_recursive(self):
        """Get the model types, recursively."""
        pass

    @abc.abstractmethod
    def get_all_type_models_recursive(self):
        """Get all type models, recursively."""
        pass

    @abc.abstractmethod
    def set_type_ranges(self, type_ranges_model):
        """Set all the type ranges inside given a type ranges model."""
        pass

    def load(self):
        """Load record specific data structures with information from YAML file."""
        # Initialize object members:
        self.includes = []
        self.description = None
        self.preamble = None
        self.name = None
        self.size = (
            0  # The maximum size of the type in the case of a variable sized type
        )
        self.num_fields = 0
        self.complex_types = None
        self.simple_types = None
        self.deps_list = []
        self.type_includes = None
        self.complex_type_includes = None
        self.variable_length = False
        self.type_ranges_loaded = False

        # Populate the object with the contents of the
        # file data:
        if "with" in self.data and self.data["with"]:
            self.includes = self.data["with"]
        for include in self.includes:
            include = ada.formatType(include)
        self.name = ada.formatType(self.model_name)
        if "description" in self.data:
            self.description = self.data["description"]
        if "preamble" in self.data:
            self.preamble = self.data["preamble"]
        self._load()

    def get_dependencies(self):
        """Get model dependencies."""
        return super().get_dependencies() + self.deps_list

    def _load_type_ranges_from_cache(self):
        """
        Try to satisfy set_type_ranges() from the session type ranges
        cache instead of building and running this model's type ranges
        program. Returns True if every type the model asked for was
        served from the cache, False otherwise.
        """
        from database.type_ranges_cache_database import type_ranges_cache_database

        try:
            db = type_ranges_cache_database()
        except Exception:
            # No readable cache database for this session. Fall back to
            # the slow path, which needs no cache at all.
            return False

        with db:
            resolver = _cached_type_ranges(db, bool(self.preamble))
            try:
                self.set_type_ranges(resolver)
            except _type_ranges_cache_miss:
                # This model needs at least one type the cache cannot
                # serve, so its type ranges program has to be built
                # anyway. Any field already set from the cache holds the
                # value that program reports for it, since a type name
                # resolves to one declaration for the whole session, and
                # set_type_ranges() on the slow path leaves fields that
                # are already loaded alone.
                return False

        # Depend on the type ranges yaml files whose programs computed
        # the entries used here. This preserves the redo dependency chain
        # from whatever target is being built, through those programs,
        # down to the Ada source files that define these types, so that a
        # later change to those sources still rebuilds the consumer.
        if resolver.source_yamls:
            redo.redo_ifchange(list(resolver.source_yamls.keys()))
        return True

    def _save_type_ranges_to_cache(self, type_ranges_model, type_ranges_yaml):
        """
        Store every cacheable type from a freshly built type ranges model
        into the session cache, recording this model's type ranges yaml
        as the source that computed them.
        """
        from database.type_ranges_cache_database import (
            type_ranges_cache_database,
            is_cacheable_type_name,
        )
        from database.database import DATABASE_MODE

        try:
            entries = [
                (type_name, type_range)
                for type_name, type_range in type_ranges_model.types.items()
                if is_cacheable_type_name(type_name, bool(self.preamble))
            ]
            if entries:
                with type_ranges_cache_database(
                    mode=DATABASE_MODE.READ_WRITE
                ) as db:
                    for type_name, type_range in entries:
                        db.store_type_range(type_name, type_range, type_ranges_yaml)
        except Exception:
            # Failing to populate the cache must never break the build.
            # The next model needing these types just recomputes them.
            pass

    def load_type_ranges(self):
        if not self.type_ranges_loaded:
            cache_enabled = "DISABLE_TYPE_RANGES_CACHE" not in os.environ
            if not (cache_enabled and self._load_type_ranges_from_cache()):
                # Build and load the type ranges model for this packed record.
                type_ranges_yaml = (
                    redo_arg.get_src_dir(self.full_filename)
                    + os.sep
                    + "build"
                    + os.sep
                    + "yaml"
                    + os.sep
                    + self.model_name
                    + ".type_ranges.yaml"
                )
                redo.redo_ifchange(type_ranges_yaml)
                type_ranges_model = model_loader.load_model(type_ranges_yaml)
                if cache_enabled:
                    self._save_type_ranges_to_cache(
                        type_ranges_model, type_ranges_yaml
                    )

                # Call the inherited abstract method.
                self.set_type_ranges(type_ranges_model)
        self.type_ranges_loaded = True
