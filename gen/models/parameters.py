from models.component import component_submodel
from util import ada
from models.submodels.ided_suite import ided_suite
from models.submodels.parameter import parameter
import os.path


# This is the object model for a parameters suite. It extracts data from a
# input file and stores the data as object member variables.
class parameters(component_submodel, ided_suite):
    # Initialize the parameters object, ingest data, and check it by
    # calling the base class init function.
    def __init__(self, filename):
        # Load the object from the file:
        ided_suite.__init__(self)
        component_submodel.__init__(
            self,
            filename=filename,
            schema=os.environ["SCHEMAPATH"] + "/parameters.yaml",
        )

    def load(self):
        # Call the component submodel load:
        component_submodel.load(self)

        # Load the base class:
        name = ada.adaCamelCase(self.model_name) + "_Parameters"
        self.load_suite_data(
            suite_name=name,
            suite_data=self.data,
            entities_name="parameters",
            type_name="type",
            packed_types_required=True,
        )

        # Rename entities to something more descriptive:
        self.parameters = list(self.entities.values())

    def set_component(self, component):
        # Set the id bases parameter:
        component.set_id_bases_parameters.append(
            parameter(
                name="parameter_Id_Base",
                type="Parameter_Types.Parameter_Id_Base",
                description="The value at which the component's parameter identifiers begin.",
            )
        )

        # Get any type packages for the defaults for parameters. Sometimes the defaults
        # reference external packages (especially when Adamant enums are involved. We need
        # to make sure these packages are included within the component for it to compile
        # without error. The following logic goes through the default parameter text and
        # finds any packages that need to be withed.
        defaults_includes = []
        for e in self.entities.values():
            # Get default value and split name and value.
            dv = e.default_value
            split_dv = dv.split("=>")
            tokens = []
            # Split on comma:
            for item in split_dv:
                split_item = item.split(",")
                tokens.extend(split_item)
            # For each string clean it from extraneous characters
            for item in tokens:
                cleaned_item = item.replace("(", "").replace(")", "").strip()
                # If there is a '.' then this might be a package
                split_item = cleaned_item.split(".")
                if len(split_item) > 1:
                    pkg = split_item[0].strip()
                    # If the first part of the string beore the '.' is
                    # a valid variable name then append it to our list
                    if ada.is_valid_variable_name(pkg):
                        pkg = ada.formatType(pkg)
                        if pkg not in defaults_includes:
                            defaults_includes.append(pkg)

        # Store all includes for the component:
        component.base_ads_includes.extend(
            [
                self.name,
                "Interfaces",
                "Parameter_Types",
                "Parameter",
                "Basic_Types",
                "Parameter_Enums",
            ]
            + self.includes
            + defaults_includes
        )
        if self.basic_types:
            component.base_adb_includes.append("Serializer")
        component.base_adb_includes.extend(
            ["Parameter_Buffer_Length", "Byte_Array_Util", "Parameter_Id"]
            + [t + ".Validation" for t in self.complex_type_includes]
        )
        component.tester_base_ads_includes.extend(["Parameter", self.name])
        component.tester_base_ads_includes.extend(["Parameter_Update"])
        component.tester_base_adb_includes.extend(
            [
                "Parameter_Types",
                "Parameter_Enums",
                "Parameter_Id.Representation",
                self.name,
            ]
            + self.representation_includes
        )
        component.tester_template_adb_includes.append("Parameter")

        # Do base class load:
        component_submodel.set_component(self, component)

    def get_dependencies(self):
        return component_submodel.get_dependencies(self) + \
               ided_suite.get_dependencies(self)
