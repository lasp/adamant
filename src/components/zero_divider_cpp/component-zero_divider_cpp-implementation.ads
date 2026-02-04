--------------------------------------------------------------------------------
-- Zero_Divider_Cpp Component Implementation Spec
--------------------------------------------------------------------------------

-- Includes:
with Command;

-- The purpose of this component is to provide a safe, commandable way to cause
-- the Ada Last Chance Handler to be called. To accomplish this, this component
-- provides a Divide_By_Zero_In_Cpp command which divides an integer by zero in
-- c++, which causes a c++ exception to be thrown, which is purposely not handled.
-- The Divide_By_Zero_In_Cpp command must be passed a magic number as an argument.
-- If the magic number does not match the number that this component is
-- instantiated with at initialization, then the Divide_By_Zero_In_Cpp command is
-- not executed. This feature prevents inadvertent execution of this command. The
-- usage of this component is dependent on the implementation of a Last Chance
-- Handler (LCH) in Ada in addition to a c++ termination hanlder, such that
-- exceptions thrown in c++ code cause the Ada LCH to be invoked. This component
-- is specifically intended for use in testing the Ada LCH implementation. This
-- component also supplies the packet definition for the assembly for a LCH packet
-- that is created by the LCH itself (which is not usually implemented as an
-- Adamant component). This provides the ground system the LCH packet definition
-- so it can be parsed and stored. The component does not contain a Packet.T send
-- connector, so will not send out this packet itself. You Last Chance Handler
-- should produce a packet with this packet definition.
package Component.Zero_Divider_Cpp.Implementation is

   -- The component class instance record:
   type Instance is new Zero_Divider_Cpp.Base_Instance with private;

   --------------------------------------------------
   -- Subprogram for implementation init method:
   --------------------------------------------------
   -- The magic number is provided at instantiation.
   --
   -- Init Parameters:
   -- Magic_Number : Magic_Number_Type - Pick a number that must be provided with the
   -- Divide_By_Zero_In_Cpp command for it to be executed. If any other number is
   -- provided, the command is failed and no divide by zero instruction is executed.
   -- Note - The values of 0 and 1 are not accepted as magic numbers.
   -- Sleep_Before_Divide_Ms : Natural - The number of milliseconds to sleep after
   -- receiving the command but before performing the divide by zero. This allows
   -- time for any events to be written by the component, if desired.
   --
   overriding procedure Init (Self : in out Instance; Magic_Number : in Magic_Number_Type; Sleep_Before_Divide_Ms : in Natural := 1000);

private

   -- The component class instance record:
   type Instance is new Zero_Divider_Cpp.Base_Instance with record
      null; -- TODO
   end record;

   ---------------------------------------
   -- Set Up Procedure
   ---------------------------------------
   -- Null method which can be implemented to provide some component
   -- set up code. This method is generally called by the assembly
   -- main.adb after all component initialization and tasks have been started.
   -- Some activities need to only be run once at startup, but cannot be run
   -- safely until everything is up and running, ie. command registration, initial
   -- data product updates. This procedure should be implemented to do these things
   -- if necessary.
   overriding procedure Set_Up (Self : in out Instance) is null;

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- The command receive connector
   overriding procedure Command_T_Recv_Sync (Self : in out Instance; Arg : in Command.T);

   ---------------------------------------
   -- Invoker connector primitives:
   ---------------------------------------
   -- This procedure is called when a Command_Response_T_Send message is dropped due to a full queue.
   overriding procedure Command_Response_T_Send_Dropped (Self : in out Instance; Arg : in Command_Response.T) is null;
   -- This procedure is called when a Event_T_Send message is dropped due to a full queue.
   overriding procedure Event_T_Send_Dropped (Self : in out Instance; Arg : in Event.T) is null;

end Component.Zero_Divider_Cpp.Implementation;
