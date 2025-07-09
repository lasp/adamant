from util import target

# Set the target to the test target be default in this directory,
# unless the target is already set to a coverage target.
build_target = target.try_get_target()
if build_target:
    if not build_target.endswith("Coverage") and not build_target.endswith("Analyze"):
        target.set_test_target(build_target)
else:
    target.set_default_test_target()
