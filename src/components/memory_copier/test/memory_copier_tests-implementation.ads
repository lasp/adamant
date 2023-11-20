--------------------------------------------------------------------------------
-- Memory_Copier Tests Spec
--------------------------------------------------------------------------------

-- This is a unit test suite for the Memory Copier component.
package Memory_Copier_Tests.Implementation is
   -- Test data and state:
   type Instance is new Memory_Copier_Tests.Base_Instance with private;
   type Class_Access is access all Instance'Class;
private
   -- Fixture procedures:
   overriding procedure Set_Up_Test (Self : in out Instance);
   overriding procedure Tear_Down_Test (Self : in out Instance);

   -- This unit test tests the nominal copy command from the source to the destination.
   overriding procedure Test_Nominal_Copy (Self : in out Instance);
   -- This unit test tests the component's response to a failed copy.
   overriding procedure Test_Copy_Failure (Self : in out Instance);
   -- This unit test tests the component's response when the destination component does not respond to a copy command before a timeout occurs.
   overriding procedure Test_Copy_Timeout (Self : in out Instance);
   -- This unit test tests the component's response when the source component is unavailable for copy.
   overriding procedure Test_Memory_Unavailable (Self : in out Instance);
   -- This unit test tests the component's response when source component returns a region that is too small.
   overriding procedure Test_Length_Mismatch (Self : in out Instance);
   -- This unit test tests a command being dropped due to a full queue.
   overriding procedure Test_Full_Queue (Self : in out Instance);
   -- This unit test exercises that an invalid command throws the appropriate event.
   overriding procedure Test_Invalid_Command (Self : in out Instance);

   -- Test data and state:
   type Instance is new Memory_Copier_Tests.Base_Instance with record
      null;
   end record;
end Memory_Copier_Tests.Implementation;
