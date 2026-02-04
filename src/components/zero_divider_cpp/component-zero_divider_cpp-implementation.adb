--------------------------------------------------------------------------------
-- Zero_Divider_Cpp Component Implementation Body
--------------------------------------------------------------------------------

package body Component.Zero_Divider_Cpp.Implementation is

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
   overriding procedure Init (Self : in out Instance; Magic_Number : in Magic_Number_Type; Sleep_Before_Divide_Ms : in Natural := 1000) is
      -- TODO declarations
   begin
      null; -- TODO statements
   end Init;

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- The command receive connector
   overriding procedure Command_T_Recv_Sync (Self : in out Instance; Arg : in Command.T) is
      -- TODO declarations
   begin
      null; -- TODO statements
   end Command_T_Recv_Sync;

end Component.Zero_Divider_Cpp.Implementation;
