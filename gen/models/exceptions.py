# Exception class for handling an error within a model. If a filename
# and linenumber are provided then those are printed as well.
class ModelException(Exception):
    def __init__(self, message, filename=None, lineno=None):
        self.message = message
        self.filename = filename
        self.lineno = lineno

    def __str__(self):
        return (
            ((str(self.filename) + ":") if self.filename else "")
            + ((str(self.lineno + 1) + ":") if self.lineno else "")
            + (" " if self.lineno or self.filename else "")
            + str(self.message)
        )


# Decorator which catches a model exception with a called
# function and adds "full_filename" to the error message
# if "full_filename" exists within the calling object
def throw_exception_with_filename(func):
    def inner(*args, **kwargs):
        try:
            return func(*args, **kwargs)
        except ModelException as e:
            if e.filename is None:
                try:
                    e.filename = args[0].full_filename
                except Exception:
                    pass
            raise e

    return inner


# Decorator which catches a model exception and addes
# a line number if the argument of the function contains
# an attribue "lc.line" which is the line number attribute
# of a round trip loaded yaml dictionary from ruamel.yaml
def throw_exception_with_lineno(func):
    def inner(*args, **kwargs):
        try:
            return func(*args, **kwargs)
        except ModelException as e:
            if e.lineno is None:
                for arg in args:
                    try:
                        e.lineno = arg.lc.line
                        break
                    except Exception:
                        pass
            if e.lineno is None:
                for arg in kwargs.values():
                    try:
                        e.lineno = arg.lc.line
                        break
                    except Exception:
                        pass
            raise e

    return inner
