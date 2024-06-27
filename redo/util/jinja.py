import os


def render(dictionary, template_file, template_path=None, extensions=[]):
    """Public function for redering using Jinja templates."""
    def _render(env):
        return env.get_template(template_file).render(dictionary)

    if template_path is None:
        template_path = os.environ["TEMPLATEPATH"]
    from jinja2 import FileSystemLoader

    loader = FileSystemLoader(template_path)

    # Special handling for latex files, since the syntax conflicts:
    if template_file.endswith(".tex"):
        from latex.jinja2 import make_env

        return _render(make_env(loader=loader, trim_blocks=True, extensions=extensions))
    else:
        from jinja2 import Environment, select_autoescape

        return _render(
            Environment(
                loader=loader,
                trim_blocks=True,
                extensions=extensions,
                autoescape=select_autoescape(["html", "htm", "xml"]),
            )
        )
