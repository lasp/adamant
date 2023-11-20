--------------------------------------------------------------------------------
-- Connector_Counter_16 Component Implementation Body
--------------------------------------------------------------------------------

package body Component.Connector_Counter_16.Implementation is

   -- Initialize the counter:
   overriding procedure Set_Up (Self : in out Instance) is
   begin
      Self.Count.Set_Count (0);
      Self.Data_Product_T_Send_If_Connected (Self.Data_Products.Count (Self.Sys_Time_T_Get, (Value => Self.Count.Get_Count)));
   end Set_Up;

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- The generic invokee connector.
   overriding procedure T_Recv_Sync (Self : in out Instance; Arg : in T) is
   begin
      -- Send on the connector data if we are connected on the
      -- other side:
      Self.T_Send_If_Connected (Arg);

      -- Increment the count:
      Self.Count.Increment_Count;

      -- Send the data product:
      Self.Data_Product_T_Send_If_Connected (Self.Data_Products.Count (Self.Sys_Time_T_Get, (Value => Self.Count.Get_Count)));
   end T_Recv_Sync;

   -- The command receive connector.
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
   --    These are the commands for the component.
   -- This command resets the internal count.
   overriding function Reset_Count (Self : in out Instance) return Command_Execution_Status.E is
      use Command_Execution_Status;
      The_Time : constant Sys_Time.T := Self.Sys_Time_T_Get;
   begin
      -- Reset the counter and send data product:
      Self.Count.Set_Count (0);
      Self.Data_Product_T_Send_If_Connected (Self.Data_Products.Count (The_Time, (Value => Self.Count.Get_Count)));

      -- Send info event:
      Self.Event_T_Send_If_Connected (Self.Events.Count_Reset (The_Time));

      return Success;
   end Reset_Count;

   -- Invalid command handler. This procedure is called when a command's arguments are found to be invalid:
   overriding procedure Invalid_Command (Self : in out Instance; Cmd : in Command.T; Errant_Field_Number : in Unsigned_32; Errant_Field : in Basic_Types.Poly_Type) is
   begin
      -- Throw event:
      Self.Event_T_Send_If_Connected (Self.Events.Invalid_Command_Received (
         Self.Sys_Time_T_Get,
         (Id => Cmd.Header.Id, Errant_Field_Number => Errant_Field_Number, Errant_Field => Errant_Field)
      ));
   end Invalid_Command;

end Component.Connector_Counter_16.Implementation;
