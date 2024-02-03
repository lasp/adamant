from models.base import base
from models.exceptions import (
    ModelException,
    throw_exception_with_lineno,
    throw_exception_with_filename,
)
from util import ada
from util import model_loader
from util import redo_arg
import os
from collections import OrderedDict


# This is the submodel class for an assembly . All assembly submodels
# should inherit from this model.
class assembly_submodel(base):
    # Initialize some members. This base method should be called by inheriting classes
    # load() methods.
    def load(self):
        self.assembly = None

    # Load an assembly model from disk. This is usually called by generators that need some assembly
    # information inside the submodel prior to autocoding. Calling this function loads the assembly,
    # which in turn is going to cause the set_assembly() function below to be called. By default, this
    # function will update "self" with the new version of this object found in the loaded assembly.
    # Because of this, if you need something special done to this object when loading the assembly, it
    # is wise to override the set_assembly() method to achieve that.
    #
    # The shallow_load flag should almost always be set to false. Sometimes it is useful
    # to set it to false to resolve a circular redo dependency that hangs the build system. You will
    # know when that happens, and this flag can help.
    def load_assembly(self, shallow_load=False, shallow_load_component_list=[]):
        if not self.assembly:
            # If no assembly was provided, the load it.
            dirname, view_name, assembly_name, *ignore = redo_arg.split_model_filename(
                self.full_filename
            )
            self.assembly = model_loader.try_load_model_by_name(
                assembly_name,
                model_types="assembly",
                shallow_load=shallow_load,
                shallow_load_component_list=shallow_load_component_list,
            )
            if not self.assembly:
                raise ModelException(
                    "Assembly submodel '"
                    + self.model_name
                    + "' could not find a model file for assembly '"
                    + assembly_name
                    + "'."
                )

            if shallow_load:
                # If shallow load is true, then we need to call set_assembly()
                # manually since it won't be called by the assembly model we loaded.
                self.set_assembly(self.assembly)
            else:
                # Now that the assembly is loaded, a "fresher" version of this model should be saved in the
                # assembly we just loaded, because the assembly would have called the "set_assembly()" method
                # below. So update the object with the newest version in the assembly.
                if self.full_filename in self.assembly.submodels:
                    self.__dict__.update(
                        self.assembly.submodels[self.full_filename].__dict__
                    )

                # Update cache for speed next time this is loaded:
                self.save_to_cache()

    # Override this method to do something more specific. By default we set the assembly object
    # and add ourselves to the assembly by class name.
    @throw_exception_with_filename
    def set_assembly(self, assembly):
        # Set the assembly:
        self.assembly = assembly

        # Now modify the assembly, so it knows about this
        # submodel.
        # Set the assembly.submodel_name = submodel_model
        # ie. assembly.super_packets = <super_packets_model_obj>
        setattr(self.assembly, self.__class__.__name__, self)

        # Also store the submodels in a dictionary. We will load ourselves from here
        # in the load_assembly() method, should it be called.
        self.assembly.submodels[self.full_filename] = self

    # Override this method to do something more specific at the end of
    # the load process. final() is called after set_assembly
    # once the entire load process is finished. Some things like IDed entity ID
    # assignment does not occur until the very end of the assembly load. final()
    # will get called after this, so may be useful if the component submodel
    # needs access to the IDs. You should prefer using set_assembly
    # where possible.
    def final(self):
        pass


# This class holds a connection. When instantiated the two connectors are
# connected.
class connection(object):
    def __init__(self, filename, data):
        self.connected = False
        self.ignored = False
        self.data = data
        self.lineno = data.lc.line

    # Make the actual connection, connecting one connector to another.
    def connect(self, components):
        # Collect from-side info:
        from_component_name = ada.formatVariable(self.data["from_component"])
        from_connector_name = ada.formatType(self.data["from_connector"])
        from_component = None
        from_connector = None
        if (
            from_component_name.lower() == "ignore"
            or from_connector_name.lower() == "ignore"
        ):
            from_component_name = "ignore"
            from_connector_name = "ignore"
        else:
            try:
                from_component = components[from_component_name]
            except KeyError:
                raise ModelException(
                    'Connection uses from_component "'
                    + from_component_name
                    + '" which does not exist in the assembly.',
                    lineno=self.lineno,
                )

            try:
                from_connector = from_component.connectors.of_name(from_connector_name)
            except ModelException as e:
                raise ModelException(
                    "Error encountered when looking for 'from' connector '"
                    + from_connector_name
                    + "' in component '"
                    + from_component_name
                    + "': "
                    + str(e),
                    lineno=self.lineno,
                )

        # Collect to-side info:
        to_component_name = ada.formatVariable(self.data["to_component"])
        to_connector_name = ada.formatType(self.data["to_connector"])
        to_component = None
        to_connector = None
        if (
            to_component_name.lower() == "ignore"
            or to_connector_name.lower() == "ignore"
        ):
            to_component_name = "ignore"
            to_connector_name = "ignore"
        else:
            try:
                to_component = components[to_component_name]
            except KeyError:
                raise ModelException(
                    'Connection uses to_component "'
                    + to_component_name
                    + '" which does not exist in the assembly.',
                    lineno=self.lineno,
                )

            try:
                to_connector = to_component.connectors.of_name(to_connector_name)
            except ModelException as e:
                raise ModelException(
                    "Error encountered when looking for 'to' connector '"
                    + to_connector_name
                    + "' in component '"
                    + to_component_name
                    + "': "
                    + str(e),
                    lineno=self.lineno,
                )

        # Get connector from index:
        from_index_specified = False
        from_index = 1
        if "from_index" in self.data:
            from_index = self.data["from_index"]
            from_index_specified = True

        # Get connector from index:
        to_index_specified = False
        to_index = 1
        if "to_index" in self.data:
            to_index = self.data["to_index"]
            to_index_specified = True

        # Get description:
        description = None
        if "description" in self.data:
            description = self.data["description"]

        # Set class variables:
        self.from_component = from_component
        self.from_connector = from_connector
        self.from_index = from_index
        self.to_component = to_component
        self.to_connector = to_connector
        self.to_index = to_index
        self.description = description

        if from_connector:
            self.from_name = (
                from_component.instance_name
                + "."
                + from_connector.name
                + (
                    "[" + str(self.from_index) + "]"
                    if self.from_connector.count != 1
                    else ""
                )
            )

            if from_connector.count != 1 and not from_index_specified:
                raise ModelException(
                    "Error encountered when connecting 'from' connector '"
                    + from_connector_name
                    + "' in component '"
                    + from_component_name
                    + "': From connector is arrayed with count of "
                    + str(from_connector.count)
                    + " but no 'from_index' is specified in connection.",
                    lineno=self.lineno,
                )
        else:
            self.from_name = "ignore"

        if to_connector:
            self.to_name = (
                to_component.instance_name
                + "."
                + to_connector.name
                + (
                    "[" + str(self.to_index) + "]"
                    if self.to_connector.count != 1
                    else ""
                )
            )

            # If either side of the connector is arrayed, then the connected index
            # MUST be specified. We don't default to 1 in this case.
            if to_connector.count != 1 and not to_index_specified:
                raise ModelException(
                    "Error encountered when connecting 'to' connector '"
                    + to_connector_name
                    + "' in component '"
                    + to_component_name
                    + "': To connector is arrayed with count of "
                    + str(to_connector.count)
                    + " but no 'to_index' is specified in connection.",
                    lineno=self.lineno,
                )
        else:
            self.to_name = "ignore"

        self.name = self.from_name + "-" + self.to_name

        # Connect the connector objects:
        if from_connector is not None and to_connector is not None:
            try:
                self.from_connector.connect_to(
                    connector=to_connector,
                    connection=self,
                    from_index=self.from_index,
                    to_index=self.to_index,
                )
            except ModelException as e:
                raise ModelException(
                    "Failed to connect '"
                    + self.from_name
                    + "' to '"
                    + self.to_name
                    + "': "
                    + str(e),
                    lineno=self.lineno,
                )
            self.connected = True
        else:
            if from_connector is not None:
                try:
                    self.from_connector.ignore(index=self.from_index)
                except ModelException as e:
                    raise ModelException(
                        "Failed to ignore connection '"
                        + self.from_name
                        + "' to '"
                        + self.to_name
                        + "': "
                        + str(e),
                        lineno=self.lineno,
                    )
            elif to_connector is not None:
                try:
                    self.to_connector.ignore(index=self.to_index)
                except ModelException as e:
                    raise ModelException(
                        "Failed to ignore connection '"
                        + self.from_name
                        + "' to '"
                        + self.to_name
                        + "': "
                        + str(e),
                        lineno=self.lineno,
                    )
            else:
                raise ModelException(
                    "Error, cannot make a connection from ignore to ignore.",
                    lineno=self.lineno,
                )
            self.ignored = True

    # Create a connection object from connection data found within an assembly model.
    @classmethod
    @throw_exception_with_lineno
    def from_connection_data(cls, filename, connection_data):
        return cls(filename=filename, data=connection_data)


# This is the object model for a assembly. It extracts data from a
# .assembly.yaml input file and stores the data as object member
# variables for use in file generation using templates.
class subassembly(base):
    # Initialize the assembly object, ingest data, and check it by
    # calling the base class init function.
    def __init__(self, filename=None):
        # Load the object from the file:
        if filename:
            super(subassembly, self).__init__(
                filename, os.environ["SCHEMAPATH"] + "/assembly.yaml"
            )

    #############################################################
    # Standard public functions:
    #############################################################

    def get_component_with_name(self, instance_name):
        try:
            return self.components[ada.formatVariable(instance_name)]
        except KeyError:
            return None

    def get_component_dependencies(self):
        return list(
            OrderedDict.fromkeys(c.full_filename for c in self.components.values())
        )

    # Load assembly specific data structures with information from YAML file.
    def load(self):
        # Initialize internal objects:
        self.components = (
            OrderedDict()
        )  # Map of component instance name to component model
        self.connections = []  # List of connection models
        self.submodel_files = []
        self.id_bases = OrderedDict()  # map of suite type to assembly-wide id base

        # Initialize other member variables:
        self.name = None
        self.lowercasename = None
        self.prettyname = None
        self.preamble = ""
        self.prepreamble = ""
        self.includes = []
        self.description = None

        # Populate the object with the contents of the
        # file:
        if "with" in self.data and self.data["with"]:
            self.includes = list(self.data["with"])
        for include in self.includes:
            include = ada.formatType(include)
        if "preamble" in self.data:
            self.preamble = self.data["preamble"]
        if "prepreamble" in self.data:
            self.prepreamble = self.data["prepreamble"]
        self.name = ada.formatType(self.model_name)
        self.lowercasename = self.name.lower()
        self.prettyname = self.name.replace("_", " ")
        if "description" in self.data:
            self.description = self.data["description"]

        if "id_bases" in self.data:
            id_bases = self.data["id_bases"]
            if id_bases:
                for name_value in id_bases:
                    nv = name_value.split("=>")
                    if len(nv) != 2:
                        raise ModelException(
                            "id_bases parameter '"
                            + str(name_value)
                            + "' is not of format 'name => value'."
                        )
                    name = ada.formatVariable(nv[0].strip())
                    if not name.endswith("_Id_Base"):
                        raise ModelException(
                            "id_bases parameter '"
                            + str(name)
                            + "' is not of format <type>_Id_Base. ie. event_Id_Base"
                        )
                    try:
                        value = int(nv[1].strip())
                    except ValueError:
                        raise ModelException(
                            "id_bases parameter '"
                            + str(name)
                            + "' value '"
                            + str(nv[1].strip())
                            + "' must be a positive number."
                        )
                    if value <= 0:
                        raise ModelException(
                            "id_bases parameter '"
                            + str(name)
                            + "' value '"
                            + str(value)
                            + "' must be a positive number."
                        )
                    if name in self.id_bases:
                        raise ModelException(
                            "duplicate id_bases parameter '"
                            + str(name)
                            + "' not allowed."
                        )
                    self.id_bases[name] = value

        @throw_exception_with_lineno
        def load_component(component):
            # Get component name:
            if "name" in component:
                component_name = ada.formatVariable(component["name"])
            else:
                component_name = ada.formatVariable(
                    component["type"].split(".")[0] + "_Instance"
                )

            # Make sure this is not a duplicate name:
            if component_name in self.components:
                raise ModelException(
                    "Duplicate component name '"
                    + component_name
                    + "' found in assembly. Component names must be unique."
                )

            # Load component model itself.
            component_model = model_loader.try_load_model_by_name(
                component["type"], model_types="component"
            )
            if not component_model:
                raise ModelException(
                    "Could not load model for component '"
                    + component_name
                    + "' of type '"
                    + component["type"]
                    + "'. Make sure the model exists in the path."
                )

            # Load data into the component model from the assembly:
            try:
                component_model.set_component_instance_data(component_name, component)
            except ModelException as e:
                raise ModelException(
                    "Failed to load instance data into component '"
                    + component_name
                    + "': "
                    + str(e)
                )

            # Add the component model to the list:
            self.components[component_name] = component_model

        # Load component models based on types given in assembly model:
        for component in self.data["components"]:
            load_component(component)

        # Load all connections:
        if "connections" in self.data:
            for connection_data in self.data["connections"]:
                self.connections.append(
                    connection.from_connection_data(self.full_filename, connection_data)
                )

        # Load assembly submodels:
        self.submodel_files = [
            f
            for f in model_loader.get_model_file_paths(self.name)
            if f != self.full_filename
        ]


# Helper function for error printing:
def _component_file_lineno(component_model):
    return (
        component_model.instance_assembly_model.full_filename
        + ":"
        + str(component_model.lineno)
    )


# This is the object model for a assembly. It extracts data from a
# .assembly.yaml input file and stores the data as object member
# variables for use in file generation using templates.
class assembly(subassembly):
    def __init__(
        self,
        filename=None,
        is_subassembly=False,
        shallow_load=False,
        shallow_load_component_list=[],
    ):
        # Save off flags:
        self.is_subassembly = is_subassembly
        # Shallow load set to true is quicker, and does not fully load all submodels
        # and component models. This helps resolve circular dependency problems that may arise
        # with the build system. This should almost always be set to False, unless you
        # need to resolve a circular dependency issue.
        self.shallow_load = shallow_load

        # List used to shallow load only selected components for more finely controlling
        # circular dependency issues.
        self.shallow_load_component_list = shallow_load_component_list

        # Don't save to cache if this is only a shallow load, since not all the data is
        # available.
        if self.shallow_load or self.shallow_load_component_list:
            self.do_save_to_cache = False

        # Call the base class initialization:
        super(assembly, self).__init__(filename)

    def load(self):
        # Initialize the subassemblies dictionary:
        self.subassemblies = OrderedDict()  # Map of subassembly name to assembly object

        # Initialize other member:
        self.ided_suites = OrderedDict()  # map of suite type to objects of that type
        self.data_dependency_suites = (
            []
        )  # the data dependency suites, handled differently than other ided_suites
        self.events = {}  # map of event id to event model
        self.parameters = {}  # map of parameter id to parameter model
        self.data_products = {}  # map of data_product id to data product model
        self.data_products_by_name = {}  # map of data_product id to data product model
        self.data_dependencies = (
            {}
        )  # map of data_dependency id to data dependency model
        self.commands = {}  # map of command id to command model
        self.packets = {}  # map of packet id to packet model
        self.faults = {}  # map of fault id to fault model
        self.views = []
        self.submodels = OrderedDict()
        self.task_total_stack_size = (
            0  # sum of all stack sizes of all tasks in assembly
        )
        self.task_total_secondary_stack_size = (
            0  # sum of all secondary stack sizes of all tasks in assembly
        )

        # Lists:
        self.component_kind_dict = {
            "active": [],
            "passive": [],
            "simple": [],
            "queued": [],
            "init": [],
            "init_base": [],
            "set_id_bases": [],
            "commands": [],
            "events": [],
            "data_products": [],
            "data_dependencies": [],
            "packets": [],
            "parameters": [],
            "faults": [],
            "interrupts": [],
        }
        self.component_types_dict = {}  # map of component type to component
        self.generic_type_includes = []  # List of generic types to include in "with"
        self.additional_types = []  # To be added by generator if needed
        self.complex_types = OrderedDict()  # mapping of type name to type model
        self.complex_types_alpha = (
            OrderedDict()
        )  # Same as above but in alphabetical order
        self.enums = OrderedDict()  # mapping of enum name to enum model
        self.interrupt_list = None  # list of interrupts in assembly
        self.ads_includes = []
        self.adb_includes = []
        self.task_dict = {}  # Tasks indexed by priority
        self.task_list = []  # Tasks in order of task number (and priority rank)
        self.entity_dict = (
            {}
        )  # Dictionary containing things like events, commands, etc.
        self.models_dependent_on = []
        self.arrayed_connections = []

        # Loading booleans for speed optimization:
        self.view_models_loaded = False

        # These are default items that need to be filled in for creating views of the assembly. We only
        # want to initialize these if this is an assembly model, not an assembly model that has been filtered
        # for creating a specific view.
        if not hasattr(self, "is_view"):
            self.is_view = False
            self.show_switches = {}
            self.layout = "TB"  # Default for graphvis representation

            def default_show_switches(keys):
                for k in keys:
                    self.show_switches[k] = True

            default_show_switches(
                [
                    "show_component_type",
                    "show_component_execution",
                    "show_component_priority",
                    "show_component_name",
                    "show_connector_type",
                    "hide_group_outline",
                ]
            )

        #
        # Subassembly load:
        #

        # The load for an assembly first loads all listed subassemblies, if any, and then
        # loads information from this file into the subassembly list. Lastly, it merges
        # the information from all the subassemblies into a master assembly and runs
        # load.

        # First run the base class to collect basic information like the model name, and
        # the components and connections of this assembly model (not including subassemblies).
        super(assembly, self).load()

        # Helper function for loading a subassembly into the subassembly dictionary:
        def load_subassembly(name):
            subassembly = name.lower()
            subassembly_model = model_loader.try_load_model_by_name(
                subassembly, "assembly", is_subassembly=True
            )

            if not subassembly_model:
                raise ModelException(
                    "Could not load model for subassembly '"
                    + subassembly
                    + "'. Make sure the model exists in the path."
                )
            if subassembly in self.subassemblies:
                raise ModelException(
                    "Duplicate subassembly '" + subassembly + "' not allowed."
                )
            else:
                self.subassemblies[subassembly] = subassembly_model

        # Load any named subassemblies into the subassembly dictionary:
        if "subassemblies" in self.data:
            for subassembly in self.data["subassemblies"]:
                load_subassembly(subassembly)

        # OK, now we need to overwrite the components and connections dictionaries
        # with all the loaded components and connections from all our subassemblies:

        # Load component dictionary:
        for subassembly in self.subassemblies.values():
            for component_name, component_model in subassembly.components.items():
                if component_name in self.components:
                    raise ModelException(
                        "Duplicate component '"
                        + component_name
                        + "' not allowed. Found in files: "
                        + str(
                            [
                                _component_file_lineno(component_model),
                                _component_file_lineno(self.components[component_name]),
                            ]
                        )
                    )
                else:
                    self.components[component_name] = component_model

        # Load connection list:
        for subassembly in self.subassemblies.values():
            self.connections.extend(subassembly.connections)

        # Make all connections that have not yet been made in
        # our subassemblies:
        for connection in self.connections:
            if not connection.connected and not connection.ignored:
                connection.connect(self.components)

        # Remove ignored connections from connection list, we don't
        # want to see them anymore.
        self.connections = [c for c in self.connections if not c.ignored]

        # Append includes and preambles:
        preamble = ""
        prepreamble = ""
        for subassembly in self.subassemblies.values():
            self.includes.extend(subassembly.includes)
            if subassembly.preamble:
                preamble += subassembly.preamble + "\n"
            if subassembly.prepreamble:
                prepreamble += subassembly.prepreamble + "\n"
        self.preamble = preamble + self.preamble
        self.prepreamble = prepreamble + self.prepreamble

        # Append id_bases dictionary:
        for subassembly in self.subassemblies.values():
            if subassembly.id_bases:
                for name, value in subassembly.id_bases.items():
                    if name in self.id_bases:
                        raise ModelException(
                            "Duplicate id_base '"
                            + name
                            + "' found in "
                            + subassembly.full_filename
                            + "."
                        )
                    self.id_bases[name] = value

        # Grab all submodel files from subassemblies:
        for subassembly in self.subassemblies.values():
            self.submodel_files.extend(subassembly.submodel_files)

        # Load all submodels dynamically:
        submodels = list(
            filter(
                None,
                [
                    model_loader.try_load_model_of_subclass(
                        f, parent_class=assembly_submodel
                    )
                    for f in list(set(self.submodel_files))
                ],
            )
        )

        # Only run the following code if this is an actual assembly and NOT a subassembly load:
        if not self.is_subassembly:
            # Make sure all connectors on all associated components are connected. If they are not, then warn:
            for component in self.components.values():
                if component.connectors:
                    for connector in component.connectors:
                        if connector.count > 1:
                            for index0 in range(connector.count):
                                index = index0 + 1
                                if (
                                    not connector.connected(index)
                                    and not connector.ignored(index)
                                    and not self.shallow_load
                                ):
                                    self.warn(
                                        "component '"
                                        + component.instance_name
                                        + "' has unattached connector '"
                                        + connector.name
                                        + "["
                                        + str(index)
                                        + "]'."
                                    )
                        else:
                            if (
                                not connector.connected()
                                and not connector.ignored()
                                and not self.shallow_load
                            ):
                                self.warn(
                                    "component '"
                                    + component.instance_name
                                    + "' has unattached connector '"
                                    + connector.name
                                    + "'."
                                )

            # We almost always want to run the following code, however there are very special times, to avoid
            # circular dependencies, that a generator might disable the running of this code. That is why
            # this flag exists.
            if not self.shallow_load:
                # Load the assembly into the component models:
                for component in self.components.values():
                    try:
                        if component.name not in self.shallow_load_component_list:
                            component.set_assembly(self)
                    except ModelException as e:
                        raise ModelException(
                            "Failed to load assembly into component instance '"
                            + component.instance_name
                            + "': "
                            + str(e)
                        )

                # Load the assembly into each submodel. Each submodel will store itself by filename in the
                # self.submodels OrderedDict.
                for submodel in submodels:
                    submodel.set_assembly(self)

            # Add components to useful lists:
            for component in self.components.values():
                if component.instance_execution == "active":
                    self.component_kind_dict["active"].append(component)
                if component.instance_execution == "passive":
                    self.component_kind_dict["passive"].append(component)
                if (
                    component.connectors.of_kind("recv_async")
                    and component.instance_queue_size != "0"
                ):
                    self.component_kind_dict["queued"].append(component)
                else:
                    self.component_kind_dict["simple"].append(component)
                if component.init:
                    self.component_kind_dict["init"].append(component)
                if component.init_base:
                    self.component_kind_dict["init_base"].append(component)
                if component.set_id_bases:
                    self.component_kind_dict["set_id_bases"].append(component)
                if component.commands:
                    self.component_kind_dict["commands"].append(component)
                if component.parameters:
                    self.component_kind_dict["parameters"].append(component)
                if component.events:
                    self.component_kind_dict["events"].append(component)
                if component.data_products:
                    self.component_kind_dict["data_products"].append(component)
                if component.data_dependencies:
                    self.component_kind_dict["data_dependencies"].append(component)
                if component.packets:
                    self.component_kind_dict["packets"].append(component)
                if component.faults:
                    self.component_kind_dict["faults"].append(component)
                if component.interrupts:
                    self.component_kind_dict["interrupts"].append(component)

                if component.tasks:
                    for task in component.tasks.instantiated:
                        try:
                            self.task_dict[task.priority].append(task)
                        except KeyError:
                            self.task_dict[task.priority] = [task]

                if component.name in self.component_types_dict:
                    self.component_types_dict[component.name].append(component)
                else:
                    self.component_types_dict[component.name] = [component]

            # Generate priority ranks:
            priorities = reversed(sorted(self.task_dict.keys()))
            rank = 1
            task_number = 0
            for p in priorities:
                for task in self.task_dict[p]:
                    task.set_num_and_rank(task_number, rank)
                    self.task_list.append(task)
                    task_number += 1
                rank += 1

            # Calculate total stack size:
            self.task_total_stack_size = 0
            self.task_total_secondary_stack_size = 0
            for task in self.task_list:
                if task.instantiated:
                    self.task_total_stack_size += task.stack_size
                    self.task_total_secondary_stack_size += task.secondary_stack_size

            # Gather any generic types that need to be included:
            g_types = []
            for component in self.components.values():
                if component.generic:
                    g_types.extend(component.generic.includes)
            self.generic_type_includes = list(OrderedDict.fromkeys(g_types))

            # Create a master sorted priority list.
            priority_dict = {}
            for component in self.component_kind_dict["interrupts"]:
                for interrupt in component.interrupts:
                    try:
                        priority_dict[interrupt.priority].append(interrupt)
                    except KeyError:
                        priority_dict[interrupt.priority] = [interrupt]
            sorted_priorities = sorted(priority_dict.keys())
            sorted_interrupts = []
            rank = 1
            for pri in reversed(sorted_priorities):
                for interrupt in priority_dict[pri]:
                    interrupt.priority_rank = rank
                    sorted_interrupts.append(interrupt)
                rank += 1
            self.interrupt_list = sorted_interrupts

            # Store all includes for the assembly:
            self.ads_includes.extend(
                list(
                    OrderedDict.fromkeys(
                        ["Task_Types", "Interrupt_Types"]
                        + (
                            [
                                "Ada.Synchronous_Task_Control",
                                "Ada.Task_Identification",
                                "System",
                            ]
                            if self.task_list
                            else []
                        )
                        + (["Ada.Interrupts.Names"] if self.interrupt_list else [])
                        + (
                            [
                                "Component." + c[0].name + ".Implementation"
                                for c in self.component_types_dict.values()
                            ]
                        )
                        + (self.generic_type_includes)
                        + (self.includes)
                    )
                )
            )
            self.ads_includes = list(OrderedDict.fromkeys(self.ads_includes))

            # Store all includes for the assembly:
            self.adb_includes.extend(
                [inc for inc in self.adb_includes if inc not in self.ads_includes]
            )
            self.adb_includes = list(OrderedDict.fromkeys(self.adb_includes))

            # Now generate all entity ids each component:
            if not self.shallow_load:
                self._generate_component_ids()

            # Load the complex types:
            self._load_complex_types()

            # Create list of arrayed connections:
            self.arrayed_connections = list(
                filter(lambda x: x.from_connector.count > 1, self.connections)
            )
            self.arrayed_connections.sort(
                key=lambda x: (
                    x.from_component.instance_name + x.from_connector.name,
                    x.from_index,
                )
            )  # sort name then by index
            to_connections = list(
                filter(lambda x: x.to_connector.count > 1, self.connections)
            )
            to_connections.sort(
                key=lambda x: (
                    x.to_component.instance_name + x.to_connector.name,
                    x.to_index,
                )
            )  # sort name then by index
            self.arrayed_connections += to_connections

            # Set all assembly dependencies:
            def subassembly_files(assem):
                files = []
                for a in assem.subassemblies.values():
                    files.extend(subassembly_files(a))
                    files.append(a.full_filename)
                return files

            self.models_dependent_on += subassembly_files(self)
            for c in self.components.values():
                self.models_dependent_on += [c.full_filename] + c.get_dependencies()
            for m in submodels:
                self.models_dependent_on.extend([m.full_filename] + m.get_dependencies())
            self.models_dependent_on = list(set(self.models_dependent_on))

            # FOR DEBUG ONLY
            # Print a histogram of connection types in the assembly:
            # This can be useful for determining which connectors Adamant should include
            # in common_connectors.ads
            # connection_type_map = {}
            # for connection in self.connections:
            #   try:
            #     connection_type_map[connection.from_connector.common_connector_package] += 1
            #   except KeyError:
            #     connection_type_map[connection.from_connector.common_connector_package] = 1
            #   try:
            #     connection_type_map[connection.to_connector.common_connector_package] += 1
            #   except KeyError:
            #     connection_type_map[connection.to_connector.common_connector_package] = 1
            # connection_type_map = dict(sorted(connection_type_map.items(), key=lambda item: item[1], reverse=True))
            # import sys
            # for key, value in connection_type_map.items():
            #   sys.stderr.write(str(key) + ": " + str(value) + "\n")

    def _generate_component_ids(self):
        # Find an open swath of IDs in the id list at the lowest
        # position possible.
        def find_Open_Id_Base(id_List, num_Ids_To_Reserve, min_Id=1):
            count = 0
            curr_Id = min_Id
            while count < num_Ids_To_Reserve:
                if curr_Id in id_List:
                    count = 0
                else:
                    count += 1
                curr_Id += 1
            return curr_Id - num_Ids_To_Reserve

        # Grab all ided entities from the components:
        for component in self.components.values():
            for suite_type, suite in component.ided_suites.items():
                if suite_type == "data_dependencies":
                    self.data_dependency_suites.append(suite)
                else:
                    try:
                        self.ided_suites[suite_type].append(suite)
                    except KeyError:
                        self.ided_suites[suite_type] = [suite]

        # Check assembly-wide id bases and make sure there is suites
        # of that type, otherwise issue a warning.
        possible_id_base_names = [
            ada.formatVariable(k[:-1] + "_Id_Base") for k in self.ided_suites.keys()
        ]
        for id_base_name in self.id_bases.keys():
            if id_base_name not in possible_id_base_names:
                self.warn(
                    "id_bases parameter '"
                    + id_base_name
                    + "' not used! Valid id_bases are "
                    + str(possible_id_base_names)
                )

        # Reserve ids:
        for suite_type, suite_list in self.ided_suites.items():
            # Initialize the entity dictionary with this entity type:
            if suite_type not in self.entity_dict:
                self.entity_dict[suite_type] = {}

            # Reserve any entities that already have static ids set:
            for suite in suite_list:
                for entity in suite.entities.values():
                    if entity.id:
                        if entity.id in self.entity_dict[suite_type]:
                            that = self.entity_dict[suite_type][entity.id]
                            if hasattr(suite, "component"):
                                this_str = (
                                    suite_type[:-1].capitalize()
                                    + " '"
                                    + entity.name
                                    + "' in component '"
                                    + suite.component.instance_name
                                    + "' has a conflicting id '"
                                    + str(entity.id)
                                    + "' with "
                                )
                            else:
                                this_str = (
                                    suite_type[:-1].capitalize()
                                    + " '"
                                    + entity.name
                                    + "' has a conflicting id '"
                                    + str(entity.id)
                                    + "' with "
                                )
                            if hasattr(that.suite, "component"):
                                that_str = (
                                    suite_type[:-1]
                                    + " '"
                                    + that.name
                                    + "' in component '"
                                    + that.suite.component.instance_name
                                    + "'. Alter "
                                    + suite_type[:-1]
                                    + " ids or component "
                                    + suite_type[:-1]
                                    + "_Id_Bases to deconflict."
                                )
                            else:
                                that_str = (
                                    suite_type[:-1]
                                    + " '"
                                    + that.name
                                    + "'. Alter "
                                    + suite_type[:-1]
                                    + " ids or component "
                                    + suite_type[:-1]
                                    + "_Id_Bases to deconflict."
                                )
                            raise ModelException(this_str + that_str)
                        else:
                            self.entity_dict[suite_type][entity.id] = entity

            # Compute the base id for any suites that do not yet have suite ids set:
            for suite in suite_list:
                if suite.id_base is None:
                    num_ids = len(suite.entities)
                    min_id = 1
                    param_Name = suite_type[:-1].capitalize() + "_Id_Base"
                    if param_Name in self.id_bases:
                        min_id = self.id_bases[param_Name]
                    start_id = find_Open_Id_Base(
                        self.entity_dict[suite_type], num_ids, min_id
                    )
                    suite.set_id_base(start_id)

                    # If the suite id base got set by the operation above,
                    # make sure the ids don't conflict with others in the assembly:
                    if suite.id_base is not None:
                        for entity in suite.entities.values():
                            # Warn if this entities ID is below the base for this ID
                            # type. This is most likely a use error:
                            if entity.id < suite.id_base:
                                self.warn(
                                    suite_type[:-1].capitalize()
                                    + ": (Id: "
                                    + str(entity.id)
                                    + ", Name: '"
                                    + entity.name
                                    + "', Component: '"
                                    + entity.component.instance_name
                                    + "') id is below the id base for this entity type: "
                                    + str(suite.id_base)
                                    + ". This is OK, but this warning exists because this is a common error."
                                )

                            # Add entity to the assembly entity dictionaray:
                            if entity.id not in self.entity_dict[suite_type]:
                                self.entity_dict[suite_type][entity.id] = entity
                            else:
                                raise ModelException(
                                    suite_type[:-1].capitalize()
                                    + ": (Id: "
                                    + str(entity.id)
                                    + ", Name: '"
                                    + entity.name
                                    + "', Component: '"
                                    + entity.suite.component.instance_name
                                    + "') conflicts with "
                                    + entity.name
                                    + ": (Id: "
                                    + str(entity.id)
                                    + ", Name: '"
                                    + self.entity_dict[suite_type][entity.id].name
                                    + "', Component: '"
                                    + self.entity_dict[suite_type][
                                        entity.id
                                    ].suite.component.instance_name
                                    + "'). Adjust component "
                                    + entity.name
                                    + "_Id_Base to deconflict the Ids."
                                )

        # Make sure all component set id bases are set. This should never error or there is a software bug, since all
        # values should have been filled in above.
        for component in self.components.values():
            if component.set_id_bases:
                component.set_id_bases.check_for_missing_parameter_values()

        # Sort the dictionaries:
        for suite_type, entities in self.entity_dict.items():
            self.entity_dict[suite_type] = OrderedDict(sorted(entities.items()))
            # Set local attribute in model for convenience, ie. self.packets or self.events
            setattr(self, suite_type, self.entity_dict[suite_type])
            largest_id = list(self.entity_dict[suite_type].keys())[-1]
            if largest_id > (2**16 - 1):
                raise ModelException(
                    str(suite_type)
                    + " includes ID "
                    + str(largest_id)
                    + " that is too large to fit into a 16-bit integer type. Please adjust the ID bases."
                )

        # Special handling for data dependencies:
        if self.data_dependency_suites:
            # Create a dictionary that maps data product names to data products:
            for dp in self.data_products.values():
                self.data_products_by_name[
                    dp.suite.component.instance_name + "." + dp.name
                ] = dp

        # For each data dependency suite, resolve the ids.
        for suite in self.data_dependency_suites:
            suite.resolve_data_dependency_ids(self.data_products_by_name)
            # Add to self.data_dependencies dictionary for convenience:
            for dd in suite:
                self.data_dependencies[dd.id] = dd

        # Call the final function:
        self.final()

    # The final function for the assembly will be called as the last part of the load, giving submodels
    # and components the ability to calculate any last data using the final state of the assembly before
    # autocoding begins. This function will call final on all components in the assembly and all submodels.
    def final(self):
        # We almost always want to run the following code, however there are very special times, to avoid
        # circular dependencies, that a generator might disable the running of this code. That is why
        # this flag exists.
        if not self.shallow_load:
            # Load the assembly into the component models:
            for component in self.components.values():
                try:
                    if component.name not in self.shallow_load_component_list:
                        component.final()
                except ModelException as e:
                    raise ModelException(
                        "Failed to run 'final' on component instance '"
                        + component.instance_name
                        + "': "
                        + str(e)
                    )

        for submodel in self.submodels.values():
            submodel.final()

    # Special function to load all the complex types in the assembly. Most
    # generators will not need this:
    def _load_complex_types(self):
        # Load all additional types:
        complex_types = dict()
        for t in self.additional_types:
            t = ada.formatType(t)
            type_name = t.split(".")[-1]
            if type_name in ["T", "Packed"]:
                model = model_loader.try_load_model_by_name(ada.getPackage(t))
            if model:
                complex_types[model.name] = model

        # Load all the component complex types:
        for component in self.components.values():
            # Collect dictionary of types:
            types = component.complex_types
            for t, v in types.items():
                complex_types[t] = v

        # Ok now we need to order the complex types in order of dependency. This is useful for Hydra
        # which must not ever load a type that includes a type that hasn't yet been loaded.
        def _search_type(a_type_model):
            for sub_type_model in a_type_model.complex_type_models:
                _search_type(sub_type_model)
                self.complex_types[sub_type_model.name] = sub_type_model

        for t, model in complex_types.items():
            _search_type(model)
            self.complex_types[t] = model

        # Grab any enumerations found in the complex types:
        enum_models = []
        for model in self.complex_types.values():
            # Grab enumerations:
            # TODO: This assumes all enumerations for a component are found
            # within the complex types for that component. This is not strictly true,
            # however, because no component yet uses a naked enumeration within a connector
            # type or ided_suite, this feature is not added yet. When a component uses an
            # enumeration like this, then we can extend the that feature.
            enum_models.extend(model.get_all_enum_models_recursive())

        # Sort enums by alphabetical order of suite name:
        for enum_model in enum_models:
            self.enums[enum_model.suite.name + "." + enum_model.name] = enum_model
        self.enums = OrderedDict(
            sorted(self.enums.items(), key=lambda item: item[1].suite.name)
        )

        # Create complex types model in alphabetical order:
        self.complex_types_alpha = OrderedDict(sorted(self.complex_types.items()))

        # DEBUG ONLY:
        # Print out if each packed record is always valid or not:
        # import sys
        # sys.stderr.write("Are these types always valid?:\n")
        # for t in self.complex_types_alpha.values():
        #   sys.stderr.write(t.name + ": " + str(t.is_always_valid()) + "\n")

    #############################################################
    # Special public functions:
    #############################################################

    def get_dependencies(self):
        return super().get_dependencies() + self.models_dependent_on

    # Note: all of these functions are optional, and only need be called if
    # a generator needs the data that they produce. These functions generate
    # additional data and can be expensive to run, so they each re-cache the
    # model after being run, so that they are not run more than once per build.

    # Special function to load all the view model files in
    # the assembly. This is only needed by a few templates, and is a costly
    # operation, so it has been broken out into its own function. Generators
    # that need this capability can call this function manually after init.
    @throw_exception_with_filename
    def load_view_models(self):
        # Only load models if they haven't been loaded already:
        if not self.view_models_loaded:
            self._load_view_models()
            self.view_models_loaded = True
            # Store updated results in cache:
            self.save_to_cache()

    def _load_view_models(self):
        # Make sure this function is only called when it needs to be:
        assert not self.view_models_loaded

        # Load view models:
        self.views = model_loader.try_load_models_by_name(self.name, model_types="view")

    # This takes a long time, and most generators don't need it. It loads the type ranges
    # for all the types used in the assembly model commands.
    def load_command_type_ranges(self):
        for command in self.commands.values():
            command.load_type_ranges()

    # This takes a long time, and most generators don't need it. It loads the type ranges
    # for all the types used in the assembly model packets.
    def load_packet_type_ranges(self):
        for packet in self.packets.values():
            packet.load_type_ranges()

    # This takes a long time, and most generators don't need it. It loads the type ranges
    # for all the types used in the assembly model data products.
    def load_data_product_type_ranges(self):
        for dp in self.data_products.values():
            dp.load_type_ranges()
