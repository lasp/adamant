--------------------------------------------------------------------------------
-- Forwarder Component Implementation Body
--------------------------------------------------------------------------------

package body Component.Forwarder.Implementation is

   --------------------------------------------------
   -- Subprogram for implementation init method:
   --------------------------------------------------
   --
   -- Init Parameters:
   -- Startup_Forwarding_State : Basic_Enums.Enable_Disable_Type.E - Is the data stream enabled or disabled on startup. Disable means that the component does not forward any data it receives at startup.
   --
   overriding procedure Init (Self : in out Instance; Startup_Forwarding_State : in Basic_Enums.Enable_Disable_Type.E) is
   begin
      -- Set the startup state:
      Self.State.Set_Var (Startup_Forwarding_State);
   end Init;

   overriding procedure Set_Up (Self : in out Instance) is
   begin
      -- Send out data product:
      Self.Data_Product_T_Send_If_Connected (Self.Data_Products.Forwarding_State (Self.Sys_Time_T_Get, (State => Self.State.Get_Var)));
   end Set_Up;

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- This connector is the input connector for the data that is coming in.
   overriding procedure T_Recv_Sync (Self : in out Instance; Arg : in T) is
      use Basic_Enums.Enable_Disable_Type;
   begin
      case Self.State.Get_Var is
         when Enabled =>
            -- Forward data:
            Self.T_Send_If_Connected (Arg);
         when Disabled =>
            -- Nothing to do.
            null;
      end case;
   end T_Recv_Sync;

   -- This is the command receive connector.
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
   --    These are the commands for the Forwarder component.
   -- Enable the flow of data.
   overriding function Enable_Forwarding (Self : in out Instance) return Command_Execution_Status.E is
      use Command_Execution_Status;
      use Basic_Enums.Enable_Disable_Type;
      The_Time : constant Sys_Time.T := Self.Sys_Time_T_Get;
   begin
      -- Set state:
      Self.State.Set_Var (Enabled);

      -- Update data product:
      Self.Data_Product_T_Send_If_Connected (Self.Data_Products.Forwarding_State (The_Time, (State => Self.State.Get_Var)));

      -- Send event:
      Self.Event_T_Send_If_Connected (Self.Events.Forwarding_Enabled (The_Time));
      return Success;
   end Enable_Forwarding;

   -- Disable the flow of data.
   overriding function Disable_Forwarding (Self : in out Instance) return Command_Execution_Status.E is
      use Command_Execution_Status;
      use Basic_Enums.Enable_Disable_Type;
      The_Time : constant Sys_Time.T := Self.Sys_Time_T_Get;
   begin
      -- Set state:
      Self.State.Set_Var (Disabled);

      -- Update data product:
      Self.Data_Product_T_Send_If_Connected (Self.Data_Products.Forwarding_State (The_Time, (State => Self.State.Get_Var)));

      -- Send event:
      Self.Event_T_Send_If_Connected (Self.Events.Forwarding_Disabled (The_Time));
      return Success;
   end Disable_Forwarding;

   -- Invalid command handler. This procedure is called when a command's arguments are found to be invalid:
   overriding procedure Invalid_Command (Self : in out Instance; Cmd : in Command.T; Errant_Field_Number : in Unsigned_32; Errant_Field : in Basic_Types.Poly_Type) is
   begin
      -- Throw event:
      Self.Event_T_Send_If_Connected (Self.Events.Invalid_Command_Received (
         Self.Sys_Time_T_Get,
         (Id => Cmd.Header.Id, Errant_Field_Number => Errant_Field_Number, Errant_Field => Errant_Field)
      ));
   end Invalid_Command;

end Component.Forwarder.Implementation;
