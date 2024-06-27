from models.submodels.variable import variable


class parameter(variable):
    """This class holds data concerning a parameter."""
    def __init__(
        self, name, type, description=None, mode="in", value=None, default_value=None
    ):
        self.mode = mode

        # Init the base class:
        super(parameter, self).__init__(
            name=name,
            type=type,
            description=description,
            value=value,
            default_value=default_value,
        )

    ################################
    # String representations:
    ################################
    def declaration_string(self, include_mode=True):
        return (
            self.name
            + " : "
            + (self.mode + " " if include_mode else "")
            + self.type
            + (" := " + self.default_value if self.default_value else "")
        )
