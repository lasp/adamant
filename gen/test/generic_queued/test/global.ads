with Component.Generic_Queued.Implementation.Tester;
with Aa;
with Bb;

-- Library-level component instantiations for the test. The tester and
-- component contain protected queues, which the Jorvik profile forbids
-- as locals inside the test procedure.
package Global is

   package Generic_Component_Base is new Component.Generic_Queued (Aa.T, Bb.T, Bb.Serialized_Length);
   package Component_Package is new Generic_Component_Base.Implementation;
   package Tester_Package is new Component_Package.Tester;
   Tester : aliased Tester_Package.Instance;
   Comp : aliased Component_Package.Instance;

end Global;
