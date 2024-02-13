from models.component import component_submodel
from util import ada
from models.submodels.ided_suite import ided_suite, ided_entity
from models.submodels.parameter import parameter
import os.path


class data_product(ided_entity):
    def __init__(self, name, type, description=None, id=None, suite=None):
        super(data_product, self).__init__(
            name,
            type,
            description,
            id,
            default_value=None,
            variable_types_allowed=False,
            suite=suite,
        )

    @classmethod
    def from_ided_entity(cls, entity):
        self = entity
        return self


# This is the object model for a data product suite. It extracts data from a
# input file and stores the data as object member variables.
class data_products(component_submodel, ided_suite):
    # Initialize the data product object, ingest data, and check it by
    # calling the base class init function.
    def __init__(self, filename):
        # Load the object from the file:
        ided_suite.__init__(self)
        component_submodel.__init__(
            self,
            filename=filename,
            schema=os.environ["SCHEMAPATH"] + "/data_products.yaml",
        )

    def load(self):
        # Call the component submodel load:
        component_submodel.load(self)

        # Load the base class:
        name = ada.adaCamelCase(self.model_name) + "_Data_Products"
        self.load_suite_data(
            suite_name=name,
            suite_data=self.data,
            entities_name="data_products",
            type_name="type",
        )

        # Rename entities to something more descriptive:
        self.data_products = list(self.entities.values())

    def set_component(self, component):
        # Set the id bases parameter:
        component.set_id_bases_parameters.append(
            parameter(
                name="data_Product_Id_Base",
                type="Data_Product_Types.Data_Product_Id_Base",
                description="The value at which the component's data product identifiers begin.",
            )
        )

        # Store all includes for the component:
        component.base_ads_includes.extend([self.name, "Data_Product_Types"])
        component.tester_base_ads_includes.extend(
            [self.name, "Data_Product"] + self.includes
        )
        if self.basic_types:
            component.tester_base_adb_includes.append("Serializer")
        component.tester_base_adb_includes.extend(self.representation_includes)
        component.tester_template_ads_includes.extend(
            ["Printable_History", "Data_Product"] + self.representation_includes
        )

        # Do base class load:
        component_submodel.set_component(self, component)

    def get_dependencies(self):
        return component_submodel.get_dependencies(self) + \
               ided_suite.get_dependencies(self)
