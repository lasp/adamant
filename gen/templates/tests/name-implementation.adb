--------------------------------------------------------------------------------
-- {{ formatType(model_name) }} {{ formatType(model_type) }} Body
--------------------------------------------------------------------------------

with AUnit.Assertions; use AUnit.Assertions;
{% if component and component.generic %}
with Tester_Allocator;
{% endif %}

package body {{ name }}.Implementation is
{% if component and component.generic %}

   -- Target-aware tester allocator: heap on Linux, a static instance on
   -- bareboard, where Free also restores the Tester's fresh state.
   -- Generic components must instantiate this themselves; the generated
   -- unit test base cannot (it does not know the concrete types).
   package Tester_Alloc is new Tester_Allocator
     (Tester_Inst   => Component_Tester_Package.Instance,
      Tester_Access => Component_Tester_Package.Instance_Access);
{% endif %}

   -------------------------------------------------------------------------
   -- Fixtures:
   -------------------------------------------------------------------------

   overriding procedure Set_Up_Test (Self : in out Instance) is
   begin
{% if component %}
{% if component.generic %}
      -- Acquire a Tester from the target-aware allocator:
      Self.Tester := Tester_Alloc.Allocate;

      -- Set the logger in the component
      Self.Tester.Set_Logger (Self.Logger'Unchecked_Access);

{% endif %}
{% if component.connectors.invoker() or component.connectors.of_kind("recv_async") or component.events %}
      -- Allocate heap memory to component:
      Self.Tester.Init_Base{% if component.init_base.parameters %} ({% for n in component.init_base.parameter_names %}{% if n == "Queue_Size" %}{{ n }} => Self.Tester.Component_Instance.Get_Max_Queue_Element_Size * 10{% else %}{{ n }} => 3{% endif %}{{ ", " if not loop.last }}{% endfor %}){% endif %};

{% endif %}
{% if component.connectors %}
      -- Make necessary connections between tester and component:
      Self.Tester.Connect;

{% endif %}
{% if component.init %}
      -- TODO Call component init here.
      -- Self.Tester.Component_Instance.Init{% if component.init.parameters %} ({% for n in component.init.parameter_names %}{{ n }} => TBD{{ ", " if not loop.last }}{% endfor %}){% endif %};

{% endif %}
      -- Call the component set up method that the assembly would normally call.
      Self.Tester.Component_Instance.Set_Up;

{% endif %}
      -- TODO Insert custom set up code here.
      null;
   end Set_Up_Test;

   overriding procedure Tear_Down_Test (Self : in out Instance) is
   begin
      -- TODO Insert custom cleanup code here.
      null;
{% if component %}
{% if component.connectors.invoker() or component.connectors.of_kind("recv_async") or component.events %}
      -- Free component heap:
      Self.Tester.Final_Base;
{% endif %}
{% if component.generic %}

      -- Release the tester (restores its fresh state on bareboard):
      Tester_Alloc.Free (Self.Tester);
{% endif %}
{% endif %}
   end Tear_Down_Test;

   -------------------------------------------------------------------------
   -- Tests:
   -------------------------------------------------------------------------

{% for test in tests.values() %}
{% if test.description %}
{{ printMultiLine(test.description, '   -- ') }}
{% endif %}
   overriding procedure {{ test.name }} (Self : in out Instance) is
      -- TODO declarations
   begin
      -- TODO replace the following with actual test code.
      Assert (False, "Test '{{ test.name }}' is unimplemented.");
   end {{ test.name }};

{% endfor %}
end {{ name }}.Implementation;
