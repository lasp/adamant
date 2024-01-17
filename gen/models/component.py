from models.base import base
from models.exceptions import ModelException
from models.submodels.discriminant import discriminant
from models.submodels.init import init
from models.submodels.interrupts import interrupts
from models.submodels.subprogram import subprogram
from models.submodels.parameter import parameter
from models.submodels.generic_unit import generic_unit
from models.submodels.connector import connectors
from models.submodels.tasks import tasks
import models.submodels.ided_suite as ided_suite_module
from util import ada
from util import model_loader
from util import redo_arg
import os
from collections import OrderedDict


# This is the submodel class for a component. All component submodels
# should inherit from this model.
class component_submodel(base):
    # Initialize some members. This base method should be called by inheriting classes
    # load() methods.
    def load(self):
        self.component = None
        self.assembly = None

    # The default name for the submodel. By default the submodel gets attached
    # to the component with this name.
    def submodel_name(self):
        return self.__class__.__name__

    # Load a component model from disk. This is usually called by generators that need some component
    # information inside the submodel prior to autocoding. Calling this function loads the component,
    # which in turn is going to cause the set_component() function below to be called. By default, this
    # function will update "self" with the new version of this object found in the loaded assembly.
    # Because of this, if you need something special done to this object when loading the assembly, it
    # is wise to override the set_assembly() method to achieve that.
    def load_component(self):
        if not self.component:
            # If no component was provided, the load it.
            dirname, view_name, component_name, *ignore = redo_arg.split_model_filename(
                self.full_filename
            )
            self.component = model_loader.try_load_model_by_name(
                component_name, model_types="component"
            )
            if not self.component:
                raise ModelException(
                    "Component submodel '"
                    + self.model_name
                    + "' could not find a model file for component '"
                    + component_name
                    + "'."
                )

            # Now that the component is loaded, a "fresher" version of this model should be saved in the
            # component we just loaded, because the component would have called the "set_component()" method
            # below. So update the object with the newest version in the component.
            if self.full_filename in self.component.submodels:
                self.__dict__.update(
                    self.component.submodels[self.full_filename].__dict__
                )

            # Update cache for speed next time this is loaded:
            self.save_to_cache()

    # Override this method to do something more specific, if the
    # submodel needs to alter the component that it is attached to
    # or needs alter itself based on the component it is attached to.
    def set_component(self, component):
        # Set the component:
        self.component = component

        # Now modify the component, so it knows about this
        # submodel.
        # Set the component.submodel_name = submodel_model
        # ie. component.commands = <commands_model_obj>
        setattr(self.component, self.submodel_name(), self)

        # Also store the submodels in a dictionary. We will load ourselves from here
        # in the load_component() method, should it be called.
        self.component.submodels[self.full_filename] = self

    # Override this method to do something more specific, if the
    # submodel needs to be altered based on the assembly that it
    # is part of.
    def set_assembly(self, assembly):
        self.assembly = assembly

    # Override this method to do something more specific at the end of
    # the load process. final() is called after set_component and set_assembly
    # once the entire load process is finished. Some things like IDed entity ID
    # assignment does not occur until the very end of the assembly load. final()
    # will get called after this, so may be useful if the component submodel
    # needs access to the IDs. You should prefer using set_assembly and set_component
    # where possible.
    def final(self):
        pass


# This is the object model for a component. It extracts data from a
# input file and stores the data as object member variables.
class component(base):
    # Call the base class with the appropriate schema file.
    def __init__(self, filename):
        super(component, self).__init__(
            filename, os.environ["SCHEMAPATH"] + "/component.yaml"
        )

    # Load component specific data structures with information from YAML file.
    def load(self):
        # Initialize internal objects:
        self.init_base = None
        self.init = None
        self.set_id_bases = None
        self.set_id_bases_parameters = []
        self.map_data_dependencies = None
        self.ided_suites = []
        self.generic = None
        self.discriminant = None
        self.interrupts = None
        self.tasks = None
        self.commands = None
        self.events = None
        self.faults = None
        self.data_products = None
        self.data_dependencies = None
        self.parameters = None
        self.packets = None
        self.connectors = None
        self.requirements = None
        self.unit_tests = []
        self.submodels = OrderedDict()

        # Initialize other things:
        self.execution = None
        self.models_dependent_on = []
        self.base_ads_includes = []
        self.base_adb_includes = []
        self.template_ads_includes = []
        self.template_adb_includes = []
        self.tester_base_ads_includes = []
        self.tester_base_adb_includes = []
        self.tester_template_ads_includes = []
        self.tester_template_adb_includes = []
        self.includes = []
        self.do_not_includes = []
        self.preamble = None
        self.description = None
        self.name = None
        self.prettyname = None
        self.lowercasename = None
        self.additional_types = []
        self.complex_types = OrderedDict()  # mapping of type name to model
        self.complex_types_alpha = (
            OrderedDict()
        )  # Same as above but in alphabetical order
        self.enums = OrderedDict()  # mapping of enum name to model

        # Instance variables, resolved by the load_assembly_component_instance()
        # method below:
        self.instance_name = None
        self.instance_execution = None
        self.instance_queue_size = None
        self.instance_description = None

        # Populate the object with the contents of the file:
        if "with" in self.data and self.data["with"]:
            self.includes = list(self.data["with"])
        if "without" in self.data and self.data["without"]:
            self.do_not_includes = self.data["without"]
        for include in self.includes:
            include = ada.formatType(include)
        self.name = ada.formatType(os.path.basename(self.model_name))
        self.prettyname = self.name.replace("_", " ")
        self.lowercasename = self.name.lower()
        if "description" in self.data:
            self.description = self.data["description"]
        if "preamble" in self.data:
            self.preamble = self.data["preamble"]
        self.execution = self.data["execution"]

        # Handle connectors:
        self.connectors = connectors.from_component_data(self.data)

        # Handle component initialization:
        self.discriminant = discriminant.from_component_data(self.data)
        self.init = init.from_component_data(self.data)
        self.interrupts = interrupts.from_component_data(self.data)
        self.tasks = tasks.from_component_data(self.data)

        # Handle generic initialization:
        self.generic = generic_unit.from_component_data(self.data)

        # Find any connectors of generic type:
        if self.generic:
            formal_parameters = self.generic.formal_parameter_names
            for connector in self.connectors:
                if connector.type in formal_parameters:
                    # Make the connector type generic, and variable length func if it exists.
                    serialized_length_func = self.generic.get_formal_parameter(
                        connector.type
                    ).serialized_length_func
                    connector.set_type_generic(
                        serialized_length_func=serialized_length_func
                    )
                if connector.return_type in formal_parameters:
                    # Make the connector return type generic, and variable length func if it exists.
                    serialized_length_func = self.generic.get_formal_parameter(
                        connector.return_type
                    ).serialized_length_func
                    connector.set_return_type_generic(
                        serialized_length_func=serialized_length_func
                    )

            # Throw a special warning if the following is true:
            #   1) The component has a generic recv async connector
            #   2) The component does NOT have a serialized_length function of the same type.
            # In this case, we throw a warning telling the user that they likely want to have a serialized length
            # function, which can be used to properly enqueue a variable length packed record. Without such a function
            # the maximum size of the variable length type will always be used for enqueueing, which is not ideal.
            # This warning strongly encourages the user to add a serialized_length function. Currently, there is
            # not a known use case for not having a serialized length function, so this warning is instead a modeling
            # exception.
            generic_recv_async_conn = self.connectors.of_kind("recv_async").generic()
            if generic_recv_async_conn:
                for connector in generic_recv_async_conn.list:
                    if not connector.has_generic_serialized_length_func():
                        raise ModelException(
                            'Component has generic recv_async connector "'
                            + connector.name
                            + '". For generic recv_async connectors you MUST also provide a generic formal parameter '
                            + 'that is a function Serialized_Length which returns the length of the generic type. '
                            + 'This requirement must be followed to allow Adamant to serialize a variable length '
                            + 'generic type appropriately onto the internal queue. Without the provided '
                            + 'Serialized_Length function the autocoder will not know how to accomplish the '
                            + 'serialization of a variable length type. Please see the Limiter component located in '
                            + 'src/components/generics/limiter for an example of this pattern."'
                        )

        # Handle base class initialization:
        # Create a subprogram from the component properties:
        init_base_parameters = []
        if self.connectors.requires_priority_queue():
            init_base_parameters.append(
                parameter(
                    name="priority_Queue_Depth",
                    type="Positive",
                    description=("The maximum number of entries that can be stored in the component's internal "
                                 "priority queue. Each entry will be sized to hold the largest recv_async type "
                                 "for the component."),
                )
            )
        elif self.connectors.requires_queue():
            init_base_parameters.append(
                parameter(
                    name="queue_Size",
                    type="Natural",
                    description="The number of bytes that can be stored in the component's internal queue.",
                )
            )
        # Add parameter for each invoker arrayed connector with undefined length:
        for connector in self.connectors.n_arrayed().invoker():
            init_base_parameters.append(
                parameter(
                    name=connector.name + "_Count",
                    type="Connector_Count_Type",
                    description="The size of the "
                    + connector.name
                    + " invoker connector array.",
                )
            )
        # Add parameter for each invokee arrayed connector with undefined length:
        for connector in self.connectors.n_arrayed().invokee():
            init_base_parameters.append(
                parameter(
                    name=connector.name + "_Count",
                    type="Connector_Count_Type",
                    description="The size of the "
                    + connector.name
                    + " invokee connector array.",
                )
            )
        if init_base_parameters:
            self.init_base = subprogram(
                name="init_Base",
                description=("This procedure allocates memory for the component base class including the "
                             "internal queue and/or arrayed connectors."),
                parameters=init_base_parameters,
            )
        else:
            self.init_base = None

        # Load component submodels dynamically, we ignore this model and any test models during this load:
        submodel_files = [
            f
            for f in model_loader._get_model_file_paths(self.name)
            if f != self.full_filename and not f.endswith(".tests.yaml")
        ]
        submodels = list(
            filter(
                None,
                [
                    model_loader.try_load_model_of_subclass(
                        f, parent_class=component_submodel
                    )
                    for f in list(set(submodel_files))
                ],
            )
        )
        # Load this component into all of the submodels:
        for submodel in submodels:
            submodel.set_component(self)
        # Store the submodels that are ided suites in a dictionary as well:
        self.ided_suites = OrderedDict(
            [
                (m.submodel_name(), m)
                for m in submodels
                if issubclass(m.__class__, ided_suite_module.ided_suite)
            ]
        )

        # We have a naming requirement that any entity in an ided entity submodel must have a unique name. Ie. A command
        # for a component cannot have the same name as an event for that component. This creates problems with Hydra and
        # sometimes with Ada compilation. To keep things simple, we do not allow this to happen.
        entity_names = []
        suite_map = []
        for suite in self.ided_suites.values():
            suite_entity_names = suite.names()
            entity_names += suite_entity_names
            suite_map += [suite.name] * len(suite_entity_names)
        for name in entity_names:
            if entity_names.count(name) > 1:
                indexes = [i for i, e in enumerate(entity_names) if e == name]
                error_str = ""
                for index in indexes:
                    error_str += " and " + suite_map[index] + "." + entity_names[index]
                raise ModelException(
                    "Duplicated IDed entity name found: "
                    + error_str[5:]
                    + ". All component entities must have unique names."
                )

        # Handle ID base initialization:
        # Set the component's set_id_bases parameters. This list is filled in by
        # submodels that need an id base.
        if self.set_id_bases_parameters:
            self.set_id_bases = subprogram(
                name="set_Id_Bases",
                description=("This procedure sets the component's internal identifier base numbers for "
                             "various entities like commands, events, data products, parameters, "
                             "faults, or packets."),
                parameters=self.set_id_bases_parameters,
            )
        else:
            self.set_id_bases = None

        # Load all complex types used in the component. This includes any records or
        # arrays used in the components connectors, events, data products
        # commands, etc.
        additional_type_models = []
        for t in self.additional_types:
            type_name = t.split(".")[-1]
            if type_name in ["T", "Packed"]:
                model = model_loader.try_load_model_by_name(ada.getPackage(t))
            if model:
                additional_type_models.append(model)
                self.models_dependent_on.extend([model.full_filename] + model.get_dependencies())

        # Gather all type models for component.
        # Grab all type models from our ided_suites, ie. commands, events, etc.
        complex_type_models = []
        for ided_suite in self.ided_suites.values():
            complex_type_models.extend(ided_suite.type_models)
        # Add connector type models:
        complex_type_models += list(
            set(self.connectors.type_models if self.connectors else [])
            | set(additional_type_models)
        )
        # Make sure we have a unique list of type models, no dupes:
        complex_type_models = list(set(complex_type_models))

        # Some models depend on other models, make sure those are loaded into
        # the complex_types dict as well.
        enum_models = []
        for model in complex_type_models:
            # Grab complex types:
            self.complex_types[model.name] = model
            embedded_models = model.get_all_type_models_recursive()
            for embedded_model in embedded_models:
                self.complex_types[embedded_model.name] = embedded_model

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

        # Store all includes for the component:
        # Many of the includes are filled in by loading the submodels, ie. commands, events, etc. We add
        # the remaining packages here, and then make sure we have a unique set for each list.
        self.base_ads_includes = list(
            OrderedDict.fromkeys(
                (["Connector_Types"] if self.connectors else [])
                + (
                    ["Connector_Types"]
                    if (
                        self.connectors.of_kind("send")
                        or self.connectors.of_kind("recv_async")
                        or self.connectors.of_kind("recv_sync")
                        or self.connectors.n_arrayed()
                    )
                    else []
                )
                + (
                    ["Protected_Priority_Queue"]
                    if self.connectors.requires_priority_queue()
                    else []
                )
                + (
                    ["Labeled_Queue"]
                    if not self.connectors.requires_priority_queue()
                    and self.connectors.requires_queue()
                    else []
                )
                + (["Basic_Types"] if self.connectors.of_kind("recv_async") else [])
                + (
                    self.connectors.includes
                    if self.connectors and not self.generic
                    else []
                )
                + (
                    self.connectors.includes_for_generic
                    if self.connectors and self.generic
                    else []
                )
                + (
                    ["Command_Registration_Request"]
                    if "Command_Registration" in self.connectors.invoker().includes
                    else []
                )
                + (self.init.includes if self.init else [])
                + self.base_ads_includes
                + (self.includes)
            )
        )
        self.base_ads_includes = [
            inc
            for inc in self.base_ads_includes
            if inc not in self.do_not_includes and inc != "Component"
        ]

        self.base_adb_includes = list(
            OrderedDict.fromkeys(
                (
                    ["Serializer"]
                    if self.connectors.of_kind("recv_async").basic_types
                    else []
                )
                + (
                    ["Serializer_Types"]
                    if self.connectors.of_kind("recv_async").variable_length_types
                    else []
                )
                + (
                    ["Sleep"]
                    if self.connectors.of_kind("recv_async") or self.commands
                    else []
                )
                + (["Configuration"] if self.commands else [])
                + (["Task_Util"] if self.tasks and self.tasks.has_subtasks else [])
                + self.base_adb_includes
            )
        )
        self.base_adb_includes = [
            inc for inc in self.base_adb_includes if inc not in self.base_ads_includes
        ]

        self.template_ads_includes = list(
            OrderedDict.fromkeys(
                (self.discriminant.includes if self.discriminant else [])
                + (
                    self.connectors.invokee().type_includes
                    if self.connectors.invokee().includes
                    else []
                )
                + self.template_ads_includes
            )
        )

        self.template_adb_includes = list(
            OrderedDict.fromkeys(
                (
                    ["Basic_Types"]
                    if ("Buffer.T" in [c.type for c in self.connectors.invokee()])
                    else []
                )
                + self.template_adb_includes
            )
        )
        self.template_adb_includes = [
            inc
            for inc in self.template_adb_includes
            if inc not in self.template_ads_includes
        ]

        self.tester_base_ads_includes = list(
            OrderedDict.fromkeys(
                (["Sys_Time"])
                + (["Connector_Types"] if self.connectors else [])
                + (
                    self.connectors.includes
                    if self.connectors and not self.generic
                    else []
                )
                + (
                    self.connectors.includes_for_generic
                    if self.connectors and self.generic
                    else []
                )
                + self.tester_base_ads_includes
                + (self.includes)
            )
        )
        self.tester_base_ads_includes = [
            inc
            for inc in self.tester_base_ads_includes
            if inc not in self.do_not_includes
        ]

        ided_entity_includes = []

        for itemType in self.connectors.type_includes:
            if itemType in ["Command", "Data_Product", "Event", "Fault", "Packet"]:
                ided_entity_includes.extend(
                    [itemType + "_Header", itemType + "_Header.Representation"]
                )
            if itemType in ["Command_Response", "Parameter_Update"]:
                ided_entity_includes.extend([itemType + ".Representation"])
            if itemType in ["Ccsds_Space_Packet"]:
                ided_entity_includes.extend(
                    [
                        itemType + ".Representation",
                        "Ccsds_Primary_Header",
                        "Ccsds_Primary_Header.Representation",
                    ]
                )

        self.tester_base_adb_includes = list(
            OrderedDict.fromkeys(
                ided_entity_includes
                + self.tester_base_adb_includes
                + (
                    self.connectors.type_representation_includes
                    if self.connectors.type_representation_includes
                    else []
                )
            )
        )
        self.tester_base_adb_includes = [
            inc
            for inc in self.tester_base_adb_includes
            if inc not in self.tester_base_ads_includes
        ]

        self.tester_template_ads_includes = list(
            OrderedDict.fromkeys(
                ["Component." + self.name + "_Reciprocal"]
                + (["Printable_History"] if self.connectors.invoker() else [])
                + (["History"] if self.connectors.invoker().generic() else [])
                + (
                    self.connectors.invoker().type_representation_includes
                    if self.connectors.invoker().type_representation_includes
                    else []
                )
                + self.tester_template_ads_includes
            )
        )

        self.tester_template_adb_includes = list(
            OrderedDict.fromkeys(
                (["String_Util"] if self.connectors.of_kind("recv_async") else [])
                + self.tester_template_adb_includes
            )
        )
        self.tester_template_adb_includes = [
            inc
            for inc in self.tester_template_adb_includes
            if inc not in self.tester_template_ads_includes
        ]

        # Finally, let's load any unit tests:
        ut_model_files = self._load_unit_tests()

        # Gather dependencies of our dependencies:
        self.models_dependent_on.extend(ut_model_files)
        for m in submodels:
            self.models_dependent_on.extend([m.full_filename] + m.get_dependencies())
        self.models_dependent_on = list(set(self.models_dependent_on))

    def get_dependencies(self):
        return self.models_dependent_on

    # Look for and load all unit tests associated with this component.
    # Note: We cannot to this the standard way, ie. using the model loader, because unit tests
    # by default are not expected to be in the global path, so they are undiscoverable via the
    # model loader. Instead we search directories below this directory that include unit tests
    # models of the correct name.
    def _load_unit_tests(self):
        from models.tests import tests
        import glob

        ut_model_files = []
        self.full_file_dir = os.path.dirname(self.full_filename)
        for root, dirs, files in os.walk(self.full_file_dir):
            for dirname in dirs:
                if dirname not in ["build", "doc"]:
                    for unit_test_model in glob.iglob(
                        os.path.join(
                            os.path.join(root, dirname),
                            "*" + self.name.lower() + ".tests.yaml",
                        )
                    ):
                        # Unit test found, load it:
                        # sys.stderr.write(unit_test_model + "\n")
                        t = tests(unit_test_model)
                        t.set_component(self)
                        self.unit_tests.append(t)
                        ut_model_files.append(t.full_filename)
        return ut_model_files

    # This method should be called by the assembly to load assembly derived data about
    # the component into the component model itself. This includes resolved identifiers,
    # initialization parameters, resolved generic connectors, etc.
    def set_component_instance_data(
        self, component_instance_name, component_instance_data
    ):
        self.instance_data = component_instance_data
        self.lineno = (
            component_instance_data.lc.line + 1
        )  # Add line number for future use in error messages

        def load_generic_unit_values(generic_unit, generic_unit_name):
            def err(msg):
                raise ModelException(
                    'Error encountered while processing "'
                    + generic_unit_name
                    + '" for component "'
                    + component_instance_name
                    + '": '
                    + msg
                )

            # Basic error checking:
            if generic_unit and generic_unit_name not in self.instance_data:
                err(
                    '"'
                    + generic_unit_name
                    + '" is required with the following parameters: '
                    + str(list(generic_unit.formal_parameter_names))
                )
            if not generic_unit and generic_unit_name in self.instance_data:
                err('"' + generic_unit_name + '" is not needed by component.')

            # Set and check individual generic_unit parameter values:
            vals = None
            if generic_unit_name in self.instance_data:
                vals = self.instance_data[generic_unit_name]
            if generic_unit:
                try:
                    generic_unit.instantiate_types(
                        assembly_component_parameters_data=vals
                    )
                    generic_unit.check_for_missing_instantiations()
                except ModelException as e:
                    err(str(e))

        def load_subprogram_values(subprogram, subprogram_name, check_missing=True):
            def err(msg):
                raise ModelException(
                    'Error encountered while processing "'
                    + subprogram_name
                    + '" for component "'
                    + self.instance_name
                    + '": '
                    + msg
                )

            # Basic error checking:
            if subprogram:
                assert subprogram.name.lower() == subprogram_name.lower(), (
                    "This should always be true: "
                    + subprogram.name.lower()
                    + " == "
                    + subprogram_name.lower()
                )
            if check_missing:
                if subprogram and subprogram_name not in self.instance_data:
                    err(
                        '"'
                        + subprogram_name
                        + '" is expected and requires the following parameters: '
                        + str(list(subprogram.required_parameter_names))
                        + " and these additional parameters are optional: "
                        + str(list(subprogram.not_required_parameter_names))
                    )
            if not subprogram and subprogram_name in self.instance_data:
                err('"' + subprogram_name + '" does not exist.')

            # Set and check individual subprogram parameter values:
            vals = None
            if subprogram_name in self.instance_data:
                vals = self.instance_data[subprogram_name]
            if subprogram:
                try:
                    subprogram.set_values(assembly_component_parameters_data=vals)
                    if check_missing:
                        subprogram.check_for_missing_parameter_values()
                except ModelException as e:
                    err(str(e))

        # Make sure that the component instance type matches the name of this component
        # model. This is a programming error if ever false.
        assert self.instance_data["type"].lower() == self.name.lower()

        # Load basic values from component instance into the component model:
        self.instance_name = component_instance_name

        if "execution" in self.instance_data:
            self.instance_execution = self.instance_data["execution"]

        if "description" in self.instance_data:
            self.instance_description = self.instance_data["description"]

        # Check component execution details:
        if self.execution == "either":
            if not self.instance_execution:
                raise ModelException(
                    "component "
                    + self.instance_name
                    + ' has execution type of "either" its component model. An execution type must be '
                    + 'provided in the assembly model to resolve the component as either "active" or '
                    + '"passive".'
                )
        elif self.execution in ["passive", "active"]:
            if self.instance_execution:
                if self.execution != self.instance_execution:
                    raise ModelException(
                        "component "
                        + self.instance_name
                        + ' has an execution type of "'
                        + self.execution
                        + '" in its component model. The assembly cannot override the execution to "'
                        + self.instance_execution
                        + '".'
                    )
            else:
                self.instance_execution = self.execution
        else:
            assert False, "This should never happen."

        # Check component details:
        if self.instance_name.lower() == self.name.lower():
            raise ModelException(
                'component "'
                + self.instance_name
                + '" cannot have a name that matches its type.'
            )

        # Load subprogram values from assembly into component:
        load_generic_unit_values(self.generic, "generic_types")
        load_subprogram_values(self.discriminant, "discriminant")
        load_subprogram_values(self.init, "init")
        load_subprogram_values(self.init_base, "init_base")
        load_subprogram_values(
            self.set_id_bases, "set_id_bases", check_missing=False
        )  # Missing IDs are OK, since we will generate them if they are not set

        # Check data dependencies:
        if "map_data_dependencies" in self.instance_data and not self.data_dependencies:
            raise ModelException(
                'component "'
                + self.instance_name
                + '" has no data dependencies. You cannot specify a "map_data_dependencies" section.'
            )
        if self.data_dependencies:
            # Make sure map_data_dependencies section exists:
            if "map_data_dependencies" not in self.instance_data:
                raise ModelException(
                    'component "'
                    + self.instance_name
                    + '" has data dependencies that must be resolved at the assembly level. You must '
                    + 'include a "map_data_dependencies" section that maps data dependencies to data '
                    + 'products.'
                )
            # Ok resolve the data dependenc names:
            self.data_dependencies.set_data_dependencies_instance_data(
                self.instance_data["map_data_dependencies"]
            )

        # Check component task:
        if self.tasks:
            self.tasks.set_instance_data(
                component_instance_name,
                self.name,
                self,
                self.instance_data,
                self.execution == "active" or self.instance_execution == "active",
            )

        # Resolve interrupt data:
        if self.interrupts:
            self.interrupts.set_ids_and_priorities(
                self.discriminant, component_name=self.instance_name
            )

        # Fill in some connector data:
        for connector in self.connectors:
            # Resolve any unconstrained connector arrays:
            if connector.unconstrained:
                name = connector.name + "_Count"
                count = self.init_base.get_parameter_value(name)
                if count:
                    connector.set_count(int(count))
                else:
                    raise ModelException(
                        "Parameter '" + name + "' has not been set in 'Init_Base'."
                    )

            # Resolve any generic types:
            if connector.is_type_generic():
                type = self.generic.get_formal_parameter_type(connector.type)
                if type:
                    connector.set_generic_type(type)
                else:
                    raise ModelException(
                        "Generic type '"
                        + connector.type
                        + "' for connector '"
                        + connector.name
                        + "' has not been set in assembly."
                    )
            if connector.is_return_type_generic():
                type = self.generic.get_formal_parameter_type(connector.return_type)
                if type:
                    connector.set_generic_return_type(type)
                else:
                    raise ModelException(
                        "Generic return_type '"
                        + connector.return_type
                        + "' for connector '"
                        + connector.name
                        + "' has not been set in assembly."
                    )

        # Resolve queue data:
        if self.connectors.requires_priority_queue():
            # Calculate the queue size by multiplying the largest async connector type by the
            # configured queue depth.
            queue_depth = int(
                self.init_base.get_parameter_value("Priority_Queue_Depth")
            )
            async_connectors = self.connectors.of_kind("recv_async")
            max_size = max([conn.type_model.size / 8 for conn in async_connectors])
            self.instance_queue_size = int(queue_depth * max_size)
        elif self.connectors.requires_queue():
            self.instance_queue_size = self.init_base.get_parameter_value("Queue_Size")

        # Set the ids for the id bases, for any that were set in the assembly:
        if self.set_id_bases:
            for par in self.set_id_bases.parameters:
                if par.value is not None:
                    for suite in self.ided_suites.values():
                        if suite.id_base_parameter_name() == par.name:
                            suite.set_id_base(int(par.value))
                            # Conflicts will be checked at the assembly level.

    def set_assembly(self, assembly):
        self.instance_assembly_model = assembly

        # Load this assembly into all of the submodels:
        for m in self.submodels.values():
            # Load the assembly:
            m.set_assembly(assembly)

    # The final function for the component will be called as the last part of an assembly load, giving submodels
    # the ability to calculate any last data using the final state of the component and assembly before
    # autocoding begins. This function will call final on all submodels of this component.
    def final(self):
        # Load this assembly into all of the submodels:
        for m in self.submodels.values():
            # Load the assembly:
            m.final()
