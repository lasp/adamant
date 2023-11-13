--------------------------------------------------------------------------------
-- Event_Text_Logger Tests Spec
--------------------------------------------------------------------------------

-- This is a unit test suite for the Event Text Logger component
package Tests.Implementation is
   -- Test data and state:
   type Instance is new Tests.Base_Instance with private;
private
   -- Fixture procedures:
   overriding procedure Set_Up_Test (Self : in out Instance);
   overriding procedure Tear_Down_Test (Self : in out Instance);

   -- This unit test sends events to the event text logger and expects them to be printed to the screen.
   overriding procedure Test_Event_Printing (Self : in out Instance);

   -- Test data and state:
   type Instance is new Tests.Base_Instance with record
      null;
   end record;
end Tests.Implementation;
