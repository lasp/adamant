--------------------------------------------------------------------------------
-- Command_Router Tests Spec
--------------------------------------------------------------------------------

-- This is a unit test suite for the Command Router component
package Command_Router_Tests.Implementation is
   -- Test data and state:
   type Instance is new Command_Router_Tests.Base_Instance with private;
private
   -- Fixture procedures:
   overriding procedure Set_Up_Test (Self : in out Instance);
   overriding procedure Tear_Down_Test (Self : in out Instance);

   -- This unit test exercises command routing via the Command Router internal commands.
   overriding procedure Test_Nominal_Routing (Self : in out Instance);
   -- This unit test exercises command registration from the external testing component, and then makes sure the command routing works.
   overriding procedure Test_Nominal_Registration (Self : in out Instance);
   -- This unit test makes sure errors are thrown when unknown commands are sent.
   overriding procedure Test_Routing_Errors (Self : in out Instance);
   -- This unit test makes sure errors are thrown when command registration goes awry.
   overriding procedure Test_Registration_Errors (Self : in out Instance);
   -- This unit test makes sure errors are thrown when the command router queue gets full.
   overriding procedure Test_Full_Queue_Errors (Self : in out Instance);
   -- This unit test makes sure errors are thrown when a command is received with an invalid argument length.
   overriding procedure Test_Invalid_Argument_Length (Self : in out Instance);
   -- This unit test makes sure errors are thrown when a command is received with an invalid value.
   overriding procedure Test_Invalid_Argument_Value (Self : in out Instance);
   -- This unit test makes sure that a failed command reports the correct data products and events.
   overriding procedure Test_Failed_Command (Self : in out Instance);
   -- This unit test makes sure that the synchronous command connector works as expected, bypassing queue.
   overriding procedure Test_Synchronous_Command (Self : in out Instance);
   -- This unit test makes sure that the command response forwarding system and registration works as expected.
   overriding procedure Test_Command_Response_Forwarding (Self : in out Instance);
   -- This unit test makes sure that the component reports an event if a command response forward is dropped by a downstream component.
   overriding procedure Test_Command_Response_Forwarding_Dropped (Self : in out Instance);
   -- This unit test makes sure that the component reports an event if a command is dropped by a downstream component.
   overriding procedure Test_Outgoing_Command_Dropped (Self : in out Instance);

   -- Test data and state:
   type Instance is new Command_Router_Tests.Base_Instance with record
      null;
   end record;
end Command_Router_Tests.Implementation;
