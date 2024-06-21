from models.exceptions import ModelException, throw_exception_with_lineno
from util import ada
from collections import OrderedDict


class task(object):
    """This class holds data concerning a parameter."""
    def __init__(self, name, description=None):
        self.name = ada.formatType(name)
        self.description = description

        # To be set by the assembly:
        self.priority = None
        self.priority_rank = None
        self.component_name = None
        self.component_type = None
        self.component = None
        self.stack_size = None
        self.secondary_stack_size = None
        self.number = None
        self.instantiated = False

    def set_num_and_rank(self, task_num, rank):
        self.priority_rank = rank
        self.number = task_num


class tasks(object):
    @throw_exception_with_lineno
    def __init__(self, subtask_data=None, has_primary_task=False):
        """
        To initialize the tasks object, component data must be
        passed in.
        """
        # Load task list into internal dictionary
        self._tasks = OrderedDict()
        self._subtasks = OrderedDict()

        if has_primary_task:
            self._tasks["Active"] = task("Active", "Primary component task.")

        if subtask_data:
            for subtask in subtask_data:
                task_obj = task(
                    name=subtask["name"],
                    description=(
                        subtask["description"] if "description" in subtask else None
                    ),
                )
                if task_obj.name not in self._tasks:
                    self._tasks[task_obj.name] = task_obj
                else:
                    raise ModelException(
                        "Duplicate task '" + task_obj.name + "' not allowed."
                    )
                if task_obj.name not in self._subtasks:
                    self._subtasks[task_obj.name] = task_obj
                else:
                    raise ModelException(
                        "Duplicate subtask '" + task_obj.name + "' not allowed."
                    )

    @classmethod
    def from_component_data(cls, component_data):
        has_primary_task = False
        if "execution" in component_data and (
            component_data["execution"] == "active"
            or component_data["execution"] == "either"
        ):
            has_primary_task = True

        subtasks = None
        if "subtasks" in component_data:
            subtasks = component_data["subtasks"]

        if has_primary_task or subtasks:
            return cls(subtask_data=subtasks, has_primary_task=has_primary_task)
        else:
            return None

    def __nonzero__(self):
        return bool(self._tasks)

    def __iter__(self):
        return iter(self._tasks.values())

    def __len__(self):
        return len(self._tasks)

    ################################
    # Properties:
    ################################
    @property
    def list(self):
        return self._tasks.values()

    @property
    def names(self):
        return self._tasks.keys()

    @property
    def has_active_task(self):
        return bool("Active" in self._tasks)

    @property
    def instantiated(self):
        return [t for t in self._tasks.values() if t.instantiated]

    @property
    def active_task(self):
        return self._tasks["Active"]

    @property
    def has_subtasks(self):
        return bool(self._subtasks)

    @property
    def subtasks(self):
        return self._subtasks

    @property
    def subtask_list(self):
        return self._subtasks.values()

    @property
    def subtask_names(self):
        return self._subtasks.keys()

    ################################
    # Methods:
    ################################
    def set_instance_data(
        self, component_name, component_type, component_obj, instance_data, is_active
    ):
        """Resolve data members from instance data found in assembly model."""
        # Extract primary task info:
        priority, stack_size, secondary_stack_size = (None, None, None)
        if "priority" in instance_data:
            priority = instance_data["priority"]
        if "stack_size" in instance_data:
            stack_size = instance_data["stack_size"]
        if "secondary_stack_size" in instance_data:
            secondary_stack_size = instance_data["secondary_stack_size"]

        # Active/passive task checking:
        if not is_active and (
            priority is not None or stack_size is not None or secondary_stack_size is not None
        ):
            raise ModelException(
                '"passive" component '
                + component_name
                + ' should not specify "priority" or "stack_size" or "secondary_stack_size" '
                + 'attributes. These are reserved for "active" components.'
            )
        if is_active and (
            priority is None or stack_size is None or secondary_stack_size is None
        ):
            raise ModelException(
                '"active" component '
                + component_name
                + ' must specify "priority", "stack_size", and "secondary_stack_size" attributes.'
            )
        if stack_size is not None and stack_size != 0 and stack_size < 2000:
            raise ModelException(
                '"active" component '
                + component_name
                + ' specifies stack size of "'
                + str(stack_size)
                + '" bytes. Adamant requires stack sizes to be greater than or equal to 2000 bytes in size.'
            )

        active_task = self._tasks["Active"]
        active_task.priority = priority
        active_task.component_name = component_name
        active_task.component_type = component_type
        active_task.component = component_obj
        active_task.stack_size = stack_size
        active_task.secondary_stack_size = secondary_stack_size
        if priority is not None:
            active_task.instantiated = True

        # Handle subtasks:
        if self._subtasks:
            # Checks:
            if "subtasks" not in instance_data:
                raise ModelException(
                    "component "
                    + component_name
                    + ' requires a subtask. You must specify a "subtask" initialization.'
                )
            subtask_names = [
                ada.formatType(s["name"]) for s in instance_data["subtasks"]
            ]
            if sorted(subtask_names) != sorted(self.subtask_names):
                raise ModelException(
                    "component "
                    + component_name
                    + " requires the following subtask initializations: "
                    + str(list(self.subtask_names))
                    + " but these initializations were provided: "
                    + str(subtask_names)
                )

            # Store subtask data:
            for subtask_data in instance_data["subtasks"]:
                subtask = self._subtasks[subtask_data["name"]]
                subtask.priority = subtask_data["priority"]
                subtask.component_name = component_name
                subtask.component_type = component_type
                subtask.component = component_obj
                subtask.stack_size = subtask_data["stack_size"]
                subtask.secondary_stack_size = subtask_data["secondary_stack_size"]
                subtask.instantiated = True
                if "disabled" in subtask_data and subtask_data["disabled"]:
                    subtask.instantiated = False
