#!/usr/bin/env python3

try:
    from util import performance
except ModuleNotFoundError:
    import sys
    sys.stderr.write("Adamant environment not set up! Run:\n    source ~/adamant/env/activate\n")
    sys.exit(1)
# Optimize python path:
performance.optimize_path()

# Imports
import sys
from util import redo_arg

# This is the catch all .do file. If none of the other
# do files in this directory match a redo target, then
# this .do file is executed. It handles special redo
# commands like "redo all" or "redo what" as well as
# building files that could have any extension, such
# as those built by a generator.

if __name__ == "__main__":
    assert len(sys.argv) == 4
    directory, base = redo_arg.split_redo_arg(sys.argv[2])
    rule_cls = None
    # Is the file a product of the metric generator:
    if redo_arg.in_build_metric_dir(sys.argv[2]):
        from rules.build_metric import build_metric as rule_cls
    # Is the file a type range yaml file?
    elif base.endswith(".type_ranges.yaml"):
        from rules.build_type_ranges_yaml import build_type_ranges_yaml as rule_cls
    elif base.endswith("_h.ads") or base.endswith("_hpp.ads"):
        from rules.build_bindings import build_bindings as rule_cls
    # Is the file a product of a generator?
    elif redo_arg.in_build_dir(sys.argv[2]):
        from rules.build_via_generator import build_via_generator as rule_cls
    # Special redo directives:
    elif base == "clean":
        from rules.build_clean import build_clean as rule_cls
    elif base == "clean_all":
        from rules.build_clean_all import build_clean_all as rule_cls
    elif base == "clear_cache":
        from rules.build_clear_cache import build_clear_cache as rule_cls
    elif base == "what":
        from rules.build_what import build_what as rule_cls
    elif base == "prove":
        from rules.build_prove import build_prove as rule_cls
    elif base == "analyze":
        from rules.build_analyze import build_analyze as rule_cls
    elif base == "style":
        from rules.build_style import build_style as rule_cls
    elif base == "style_all":
        from rules.build_style_all import build_style_all as rule_cls
    elif base == "pretty":
        from rules.build_pretty import build_pretty as rule_cls
    elif base == "targets":
        from rules.build_targets import build_targets as rule_cls
    elif base == "templates":
        from rules.build_templates import build_templates as rule_cls
    elif base == "all":
        from rules.build_all import build_all as rule_cls
    elif base == "recursive":
        from rules.build_recursive import build_recursive as rule_cls
    elif base == "path":
        from rules.build_path import build_path as rule_cls
    elif base == "print_path":
        from rules.build_print_path import build_print_path as rule_cls
    elif base == "test":
        from rules.build_test import build_test as rule_cls
    elif base == "coverage":
        from rules.build_coverage import build_coverage as rule_cls
        from util import target as tgt
        tgt.set_default_coverage_target()
    elif base == "run":
        from rules.build_run import build_run as rule_cls
    elif base == "test_all":
        from rules.build_test_all import build_test_all as rule_cls
    elif base == "coverage_all":
        from rules.build_coverage_all import build_coverage_all as rule_cls
    elif base == "publish":
        from rules.build_publish import build_publish as rule_cls
    elif base == "codepeer_server":
        from rules.build_codepeer_server import build_codepeer_server as rule_cls
    elif base == "yaml_sloc":
        from rules.build_yaml_sloc import build_yaml_sloc as rule_cls

    # Run the rule
    if rule_cls:
        rule = rule_cls()
        rule.build(*sys.argv[1:])
    else:
        from util import error
        error.error_abort("default.do: No rule to build '" + sys.argv[1] + "'.")

# Exit fast:
performance.exit(sys.argv[2])
