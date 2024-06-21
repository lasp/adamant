from models.faults import faults, fault
from models.exceptions import ModelException
from models.submodels.ided_suite import ided_suite
from util import model_loader


class task_watchdog_faults(faults):
    """
    This model is a specialized data product suite for the product extractor data products. This
    class inherits from the normal data products model but adds extra data products based on the user's model input.
    """
    def submodel_name(self):
        """This is just a "decorated" fault object, so make sure everyone treats it like that."""
        return "faults"

    def set_component(self, component):
        """
        We override this because we DO NOT want to add a Fault_Id_Base argument to the component Set_Id_Base initialization
        procedure. Watchdog faults are always defined in the model and never derived from a base.
        """
        self._set_component_no_id_bases(component)

    def set_assembly(self, assembly):
        # Make sure an assembly is set by the base class implementation.
        super(task_watchdog_faults, self).set_assembly(assembly)

        # Get the model for the watchdog to create data products for each item in the list
        fault_list = self.component.init.get_parameter_value(
            "Task_Watchdog_Entry_Init_List"
        )
        fault_list_package = fault_list.split(".")[0].replace("_Task_Watchdog_List", "")
        fault_list_model = model_loader.try_load_model_by_name(
            fault_list_package, model_types="task_watchdog_list"
        )
        if not fault_list_model:
            raise ModelException(
                "Could not find watchdog list model for the task watchdog component faults: Model is "
                + str(fault_list_model)
                + " Package: "
                + str(fault_list_package)
            )

        # Remove the dummy variable
        self.entities.pop("Dummy_Fault")

        # loop through the model to get all the data products we need to add
        entities = []
        for fault_connection in fault_list_model.watchdog_list.values():
            # Add a fault if there is a fault to add to the list
            if fault_connection.has_fault:
                entities.append(
                    fault(
                        name=fault_connection.product_name + "_Fault",
                        type="Packed_Connector_Index.T",
                        description=fault_connection.description,
                        id=fault_connection.fault_id,
                        suite=self,
                    )
                )

        # Reinitialize this class with the newly generated faults.
        ided_suite.__init__(
            self,
            name=self.name,
            entity_name=self.entity_name,
            entities=entities,
            description=self.description,
            component=self.component,
        )
