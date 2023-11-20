--------------------------------------------------------------------------------
-- {{ formatType(model_name) }} {{ formatType(model_type) }} Implementation Suite Body
--
-- Generated from {{ filename }} on {{ time }}.
--------------------------------------------------------------------------------

with AUnit.Test_Caller;

package body {{ name }}.Implementation.Suite is

   ------------------------------------------------------------
   -- Get test suite:
   ------------------------------------------------------------

   package Caller is new AUnit.Test_Caller (Instance);
   function Get return AUnit.Test_Suites.Access_Test_Suite is
      Ret : constant AUnit.Test_Suites.Access_Test_Suite := new AUnit.Test_Suites.Test_Suite;
   begin
      -- Dynamically add tests to the suite and return it:
{% for test in tests.values() %}
      Ret.Add_Test (Caller.Create ("{{ test.name }}", {{ test.name }}'Access));
{% endfor %}
      return Ret;
   end Get;

end {{ name }}.Implementation.Suite;
