# First run all the tests programs. We need to do this here because
# we cannot add all the tests programs to the path because of
# conflicting names.
find ../example_architecture | grep "output.*\.txt\.do$" | sed 's/\.txt\.do/\.txt/g' | xargs redo-ifchange

# A few more things to build manually:
redo-ifchange ../example_architecture/simple_package/test/build/gpr/test.gpr
redo-ifchange ../example_architecture/simple_package/test_better3/coverage
redo-ifchange ../example_architecture/simple_package/test_better3/build/metric/Linux_Test/simple_package_tests-implementation.adb.txt
redo-ifchange ../example_architecture/simple_package/test_better3/build/metric/Linux_Test/test.elf.txt
redo-ifchange ../example_architecture/simple_package/test_better3/build/tex/simple_package_tests.tex
redo ../example_architecture/simple_package/test_better/what 2>&1 | grep template | awk '{ print "../example_architecture/simple_package/test_better/"$2 }' | xargs redo-ifchange
redo-ifchange ../example_architecture/example_component/test3/coverage
redo-ifchange ../example_architecture/example_component/test3/build/tex/example_component_tests.tex
redo ../example_architecture/example_component/test/what 2>&1 | grep template | awk '{ print "../example_architecture/example_component/test/"$2 }' | xargs redo-ifchange
redo-ifchange ../example_architecture/example_component/all
redo-ifchange ../example_architecture/example_component/tester/build/eps/example_to_tester_simple_view.eps
redo-ifchange ../example_architecture/queued_component/all
redo-ifchange ../example_architecture/priority_queued_component/build/eps/priority_queued_component.eps
redo-ifchange ../example_architecture/active_component/all
redo-ifchange ../example_architecture/background_component/all
redo-ifchange ../example_architecture/discriminated_component/all
redo-ifchange ../example_architecture/init_base_component/all
redo-ifchange ../example_architecture/initialized_component/all
redo-ifchange ../example_architecture/event_component/all
redo-ifchange ../example_architecture/data_product_component/all
redo-ifchange ../example_architecture/data_dependency_component/all
redo-ifchange ../example_architecture/packet_component/all
redo-ifchange ../example_architecture/command_component/all
redo-ifchange ../example_architecture/parameter_component/all
redo-ifchange ../example_architecture/fault_component/all
redo-ifchange ../example_architecture/memory_map/all
redo-ifchange ../example_architecture/register_map/all
redo-ifchange ../example_architecture/yaml_sloc/all
redo-ifchange ../example_architecture/c_lib/templates
cp -f ../example_architecture/c_lib/build/template/Linux/c_lib_h.ads ../example_architecture/c_lib/c_lib_h.ads
redo-ifchange ../example_architecture/c_lib/all
$ADAMANT_DIR/gnd/matlab/build_all_matlab_autocode.sh ../example_architecture

# Now build the user guide.
redo build/pdf/user_guide.pdf
