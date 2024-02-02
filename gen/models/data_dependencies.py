from models.component import component_submodel
from util import ada
from models.submodels.ided_suite import ided_suite
from models.submodels.parameter import parameter
from models.submodels.subprogram import subprogram
import os.path
from models.exceptions import ModelException, throw_exception_with_lineno


# This is the object model for a data dependency suite. It extracts data from a
# input file and stores the data as object member variables.
class data_dependencies(component_submodel, ided_suite):
    # Initialize the data dependencies object, ingest data, and check it by
    # calling the base class init function.
    def __init__(self, filename):
        # Load the object from the file:
        ided_suite.__init__(self)
        component_submodel.__init__(
            self,
            filename=filename,
            schema=os.environ["SCHEMAPATH"] + "/data_dependencies.yaml",
        )

    def load(self):
        # Call the component submodel load:
        component_submodel.load(self)

        # Load the base class:
        name = ada.adaCamelCase(self.model_name) + "_Data_Dependencies"
        self.load_suite_data(
            suite_name=name,
            suite_data=self.data,
            entities_name="data_dependencies",
            type_name="type",
        )

        # Rename entities to something more descriptive:
        self.data_dependencies = list(self.entities.values())

    def set_component(self, component):
        #
        # We need a special initialization procedure in the component to set the data product
        # ids for these data dependencies.
        #

        # Create the parameters for the subprogram:
        data_dependency_id_parameters = []
        data_dependency_parameters = []
        for dd in self.data_dependencies:
            data_dependency_id_parameters.append(
                parameter(
                    name=dd.name + "_Id",
                    type="Data_Product_Types.Data_Product_Id",
                    description="The external data product ID used to fetch the value for the '"
                    + dd.name
                    + "' data dependency.",
                )
            )
            data_dependency_id_parameters.append(
                parameter(
                    name=dd.name + "_Stale_Limit",
                    type="Ada.Real_Time.Time_Span",
                    description="The stale limit used for the '"
                    + dd.name
                    + "' data dependency. If the fetched data product is older than the given "
                    + "Time_Span then it is deemed stale and unfit for use by the component.",
                )
            )
            data_dependency_parameters.append(
                parameter(
                    name=dd.name,
                    type="string",
                    description="This parameter does not exist, it is used for internal python handling only.",
                )
            )

        # Create the subprogram.
        component.map_data_dependencies = subprogram(
            name="Map_Data_Dependencies",
            description="This procedure maps the component's data dependencies to external data product IDs " +
                        "and initializes the stale limits for each.",
            parameters=data_dependency_id_parameters,
        )
        self.set_data_dependencies = subprogram(
            name="set_data_dependencies",
            description="This program does not exist, it is used for internal python handling only.",
            parameters=data_dependency_parameters,
        )

        # Store all includes for the component:
        component.base_ads_includes.extend(
            [
                self.name,
                "Data_Product_Types",
                "Data_Product_Return",
                "Ada.Real_Time",
                "Data_Product_Enums",
                "Sys_Time",
            ]
            + self.includes
        )
        component.base_adb_includes.extend([])
        component.tester_base_ads_includes.extend([self.name])
        if self.basic_types:
            component.tester_base_adb_includes.append("Serializer")
        component.tester_base_adb_includes.extend(
            ["Data_Product_Id.Representation"] + self.representation_includes
        )
        component.tester_template_ads_includes.extend(
            ["Printable_History"] + self.includes
        )

        # Do base class load:
        component_submodel.set_component(self, component)

    def get_dependencies(self):
        return component_submodel.get_dependencies(self) + \
               ided_suite.get_dependencies(self)

    # Called by component, which passes set_data_dependencies instance data.
    @throw_exception_with_lineno
    def set_data_dependencies_instance_data(self, set_data_dependencies_data):
        # Load data dependency data into our subpgrograms.
        if set_data_dependencies_data:
            for data in set_data_dependencies_data:
                data_dependency = ada.formatType(data["data_dependency"])
                data_product = ada.formatVariable(data["data_product"])
                stale_limit = data["stale_limit_us"]
                stale_limit_str = "Ada.Real_Time.Microseconds (%d)" % stale_limit

                if data_dependency not in self.entities:
                    raise ModelException(
                        "No data dependency of name '"
                        + str(data_dependency)
                        + "' exists in component '"
                        + str(self.component.instance_name)
                        + ". Data dependencies must be one of: "
                        + str(list(self.entities.keys()))
                    )

                # Set the data product:
                self.set_data_dependencies.set_parameter_value(
                    data_dependency, data_product
                )

                # Set the stale limit:
                self.component.map_data_dependencies.set_parameter_value(
                    data_dependency + "_Stale_Limit", stale_limit_str
                )
                self.entities[data_dependency].stale_limit = stale_limit

        # Check for missing:
        self.set_data_dependencies.check_for_missing_parameter_values()

    # Called by assembly, which passes dictionary mapping full, component-instance-qualified data product names to
    # the data product objects.
    @throw_exception_with_lineno
    def resolve_data_dependency_ids(self, dp_name_map):
        # OK we already saved off the names of the data products in our dummy subprogram
        # "set_data_dependencies". Now we can use these names to get the actual data
        # product ids.
        for param in self.set_data_dependencies.parameters:
            data_dependency_id_name = param.name + "_Id"
            data_product_name = param.value
            data_product = None

            try:
                # Fetch the data product
                data_product = dp_name_map[data_product_name]
            except KeyError:
                raise ModelException(
                    "Cannot find data product '"
                    + str(data_product_name)
                    + "' for data dependency '"
                    + str(self.component.instance_name)
                    + "."
                    + str(param.name)
                    + "' in assembly. Data product names should be in the form: "
                    + "component_Instance_Name.Data_Product_Name"
                )

            # Grab the data dependency:
            data_dependency = self.entities[ada.formatType(param.name)]

            # Make sure the data product and data dependency are of the exact same type:
            if (
                data_dependency.type != data_product.type
                or data_dependency.type_model.size != data_product.type_model.size
            ):
                raise ModelException(
                    "Data dependency '"
                    + self.component.instance_name
                    + "."
                    + data_dependency.name
                    + "' maps to data product '"
                    + data_product_name
                    + "' but their types do not match ("
                    + data_dependency.type
                    + " vs. "
                    + data_product.type
                    + "). The data dependency and data product types must exactly match."
                )

            # Set the id in the data dependency:
            data_dependency.id = data_product.id
            data_dependency.data_product = data_product

            # Set the id in the component map_data_dependencies subprogram:
            self.component.map_data_dependencies.set_parameter_value(
                data_dependency_id_name, data_product.id
            )

        # Error checking:
        self.component.map_data_dependencies.check_for_missing_parameter_values()

    # Overriding this so that we can make sure the assembly .adb includes Ada.Real_Time
    def set_assembly(self, assembly):
        # Call the base:
        super(data_dependencies, self).set_assembly(assembly)

        # Ok, add on to with list:
        if "Ada.Real_Time" not in self.assembly.adb_includes:
            self.assembly.adb_includes.append("Ada.Real_Time")
