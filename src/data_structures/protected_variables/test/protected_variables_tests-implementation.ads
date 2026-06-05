--------------------------------------------------------------------------------
-- Protected_Variables Tests Spec
--------------------------------------------------------------------------------

-- Unit tests for the generic protected variable patterns in Protected_Variables.
package Protected_Variables_Tests.Implementation is

   -- Test data and state:
   type Instance is new Protected_Variables_Tests.Base_Instance with private;
   type Class_Access is access all Instance'Class;

private
   -- Fixture procedures:
   overriding procedure Set_Up_Test (Self : in out Instance);
   overriding procedure Tear_Down_Test (Self : in out Instance);

   -- Generic_Variable Set/Get round trip and overwrite semantics.
   overriding procedure Test_Variable (Self : in out Instance);
   -- Generic_Protected_Counter set, increment (default and by N), reset, return-
   -- previous, and modular wrap.
   overriding procedure Test_Protected_Counter (Self : in out Instance);
   -- Generic_Protected_Counter_Decrement set, decrement (default and by N), reset,
   -- and return-previous behavior over a signed range.
   overriding procedure Test_Protected_Counter_Decrement (Self : in out Instance);
   -- Generic_Protected_Periodic_Counter period set/get, count increment with modular
   -- wrap by period, Is_Count_At_Period boundary cases including period of zero.
   overriding procedure Test_Protected_Periodic_Counter (Self : in out Instance);
   -- Generic_Staged_Variable default-not-staged, Stage then Is_Staged,
   -- Copy_From_Staged returns value and clears, restage after copy, multiple Stage
   -- keeps only latest.
   overriding procedure Test_Staged_Variable (Self : in out Instance);

   -- Test data and state:
   type Instance is new Protected_Variables_Tests.Base_Instance with record
      null;
   end record;
end Protected_Variables_Tests.Implementation;
