-- Generic provider of test-fixture Tester instances.
--
-- Each component test scaffold (gen/templates/tests/name.{ads,adb})
-- instantiates this generic for its own Tester type and calls Allocate
-- in Set_Up / Free in Tear_Down. The body that gets compiled depends
-- on the active build target's path filter:
--
-- Linux/ body: `new Tester_Inst` per call -- each scenario gets a
-- fresh Tester, exactly matching the historical heap-allocation
-- behavior. State cannot leak across scenarios.
--
-- bb/ body: returns 'Access of a generic-instantiation-local
-- aliased static instance. AUnit serializes test execution so a
-- single shared instance per generic instantiation is correct.
-- Required because Jorvik forbids both `new` of types containing
-- protected components (No_Protected_Type_Allocators) and
-- function-local protected objects (No_Local_Protected_Objects).
--
-- Generic instantiation must occur at library level in the generated
-- test scaffold so the bb body's static storage lives at library level
-- too -- declaring it inside a subprogram would still be a "local
-- protected object".
generic
   type Tester_Inst is limited private;
   type Tester_Access is access all Tester_Inst;
package Tester_Allocator is
   function Allocate return Tester_Access;
   procedure Free (T : in out Tester_Access);
end Tester_Allocator;
