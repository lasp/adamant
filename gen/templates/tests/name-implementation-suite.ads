--------------------------------------------------------------------------------
-- {{ formatType(model_name) }} {{ formatType(model_type) }} Implementation Suite Spec
--
-- Generated from {{ filename }} on {{ time }}.
--------------------------------------------------------------------------------

with AUnit.Test_Suites;

package {{ name }}.Implementation.Suite is
   -- Return the test suite:
   function Get return AUnit.Test_Suites.Access_Test_Suite;
end {{ name }}.Implementation.Suite;
