--------------------------------------------------------------------------------
-- {{ formatType(model_name) }} {{ formatType(model_type) }} Base Spec
--
-- Generated from {{ filename }} on {{ time }}.
--------------------------------------------------------------------------------

-- Standard includes:
with AUnit;
with AUnit.Test_Fixtures;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Calendar; use Ada.Calendar;
with File_Logger;
{% if component and not component.generic %}
-- Component Tester Include:
with Component.{{ component.name }}.Implementation.Tester;
{% endif %}
{% if includes %}
-- Custom Includes:
{% for include in includes %}
with {{ include }};
{% endfor %}
{% endif %}

{% if description %}
{{ printMultiLine(description, '-- ') }}
{% endif %}
package {{ name }} is
   -- Test data and state:
   type Base_Instance is abstract new AUnit.Test_Fixtures.Test_Fixture with private;

   -- Abstract unit tests:
{% for test in tests.values() %}
{% if test.description %}
{{ printMultiLine(test.description, '   -- ') }}
{% endif %}
   procedure {{ test.name }} (Self : in out Base_Instance) is abstract;
{% endfor %}

   -- Fixture functions
   procedure Set_Up_Test (Self : in out Base_Instance) is abstract;
   procedure Tear_Down_Test (Self : in out Base_Instance) is abstract;

   -- Number of tests variable
   Num_Tests : constant Positive := {{ tests|length }};

   -- Test name list type for the names
   type Test_Name_List_Type is array (Natural range 0 .. Num_Tests - 1) of Unbounded_String;

   Test_Name_List : constant Test_Name_List_Type :=
   [
{% for test in tests.values() %}
      {{ loop.index0 }} => To_Unbounded_String ("{{ test.name }}"){{ "," if not loop.last }}
{% endfor %}
   ];

private
   -- Logging procedures:
   procedure Init_Logging (Self : in out Base_Instance; File_Name : in String);
   procedure End_Logging (Self : in out Base_Instance; File_Name : in String);
   procedure Log (Self : in out Base_Instance; String_To_Log : in String);

   -- Fixture procedures:
   overriding procedure Set_Up (Self : in out Base_Instance);
   overriding procedure Tear_Down (Self : in out Base_Instance);

   -- Test data and state:
   type Base_Instance is abstract new AUnit.Test_Fixtures.Test_Fixture with record
      -- Counter to track the test we are currently running so that we can log correctly.
      Test_Name_Index : Natural := 0;
      -- File for logging
      Logger : aliased File_Logger.Instance;
      -- Time for when the test starts to track the duration of the test
      Start_Test_Time : Time;
{% if component and not component.generic %}
      -- The tester component:
      Tester : Component.{{ component.name }}.Implementation.Tester.Instance_Access;
{% endif %}
   end record;
end {{ name }};
