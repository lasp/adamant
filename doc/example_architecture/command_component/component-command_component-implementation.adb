--------------------------------------------------------------------------------
-- Command_Component Component Implementation Body
--------------------------------------------------------------------------------

with Ada.Text_IO; use Ada.Text_IO;

package body Component.Command_Component.Implementation is

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- This is the command recieve connector.
   overriding procedure Command_T_Recv_Sync (Self : in out Instance; Arg : in Command.T) is
      -- Execute the command:
      Stat : constant Command_Response_Status.E := Self.Execute_Command (Arg);
   begin
      -- Send the return status:
      Self.Command_Response_T_Send_If_Connected ((Source_Id => Arg.Header.Source_Id, Registration_Id => Self.Command_Reg_Id, Command_Id => Arg.Header.Id, Status => Stat));
   end Command_T_Recv_Sync;

   -----------------------------------------------
   -- Command handler primitives:
   -----------------------------------------------
   -- Description:
   --    A set of commands for the Command Component.
   -- Prints Hello World when received.
   overriding function Hello_World (Self : in out Instance) return Command_Execution_Status.E is
      use Command_Execution_Status;
   begin
      Put_Line ("Hello, World!");
      return Success;
   end Hello_World;

   -- A command that when received checks to see if the commanded argument is equal to 5. If so the command succeeds, otherwise it fails.
   overriding function Was_Five_Sent (Self : in out Instance; Arg : in Packed_U16.T) return Command_Execution_Status.E is
      use Command_Execution_Status;
   begin
      Put_Line (Unsigned_16'Image (Arg.Value) & " was sent.");
      if Arg.Value = 5 then
         return Success;
      else
         return Failure;
      end if;
   end Was_Five_Sent;

   -- Invalid command handler. This procedure is called when a command's arguments are found to be invalid:
   overriding procedure Invalid_Command (Self : in out Instance; Cmd : in Command.T; Errant_Field_Number : in Unsigned_32; Errant_Field : in Basic_Types.Poly_Type) is
   begin
      Put_Line ("Oh, no! A command was sent that was malformed.");
   end Invalid_Command;

end Component.Command_Component.Implementation;
