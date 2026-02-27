--------------------------------------------------------------------------------
-- Limiter Component Implementation Body
--------------------------------------------------------------------------------

package body Component.Limiter.Implementation is

   --------------------------------------------------
   -- Subprogram for implementation init method:
   --------------------------------------------------
   -- This initialization function is used to set a threshold for the maximum number of data sends that the component will produce when a tick is received.
   --
   -- Init Parameters:
   -- Max_Sends_Per_Tick : Interfaces.Unsigned_16 - The maximum number of sends that this component will produce when a tick is received. The component will stop producing packets if the threshold is met or when the queue is empty, whichever happens first.
   --
   overriding procedure Init (Self : in out Instance; Max_Sends_Per_Tick : in Interfaces.Unsigned_16) is
   begin
      -- Initialize parameter values with init values. These will get overwritten
      -- by any incoming parameters, if parameters are being used with the instantiation
      -- of this component.
      Self.P_Max_Sends_Per_Tick.Set_Var (Max_Sends_Per_Tick);
   end Init;

   -- Send out max sends per tick data product:
   overriding procedure Set_Up (Self : in out Instance) is
   begin
      -- Update data product:
      Self.Data_Product_T_Send_If_Connected (Self.Data_Products.Max_Packet_Sends_Per_Tick (Self.Sys_Time_T_Get, (Value => Self.P_Max_Sends_Per_Tick.Get_Var)));
   end Set_Up;

   ---------------------------------------
   -- Parameter update action:
   ---------------------------------------
   -- This procedure is called when the parameters of a component have been updated. The default implementation of this
   -- subprogram in the implementation package is a null procedure. However, this procedure can, and should be implemented if
   -- something special needs to happen after a parameter update. Examples of this might be copying certain parameters to
   -- hardware registers, or performing other special functionality that only needs to be performed after parameters have
   -- been updated.
   overriding procedure Update_Parameters_Action (Self : in out Instance) is
   begin
      -- In this case, we need to update the values in the protected object with the parameter value, since we are not
      -- using the parameter value directly.
      Self.P_Max_Sends_Per_Tick.Set_Var (Self.Max_Sends_Per_Tick.Value);
      -- Also update the data product:
      Self.Data_Product_T_Send_If_Connected (Self.Data_Products.Max_Packet_Sends_Per_Tick (Self.Sys_Time_T_Get, (Value => Self.P_Max_Sends_Per_Tick.Get_Var)));
   end Update_Parameters_Action;

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- This is the base tick for the component.
   overriding procedure Tick_T_Recv_Sync (Self : in out Instance; Arg : in Tick.T) is
      Ignore : Tick.T renames Arg;
   begin
      -- Update our parameters:
      Self.Update_Parameters;

      -- Pull some items off the queue and send them based on the updated value for
      -- the number of sends per tick.
      declare
         Max_Items_To_Dispatch : constant Natural := Natural (Self.P_Max_Sends_Per_Tick.Get_Var);
         Items_Dispatched : Natural := 0;
      begin
         -- Dispatch up to our maximum items per tick off the queue:
         if Max_Items_To_Dispatch > 0 then
            Items_Dispatched := Self.Dispatch_N (Max_Items_To_Dispatch);
         end if;

         -- We should never dispatch more items off the queue than was
         -- requested in the call to Dispatch_N:
         pragma Assert (Items_Dispatched <= Max_Items_To_Dispatch);
      end;
   end Tick_T_Recv_Sync;

   -- The generic asynchronous invokee connector.
   overriding procedure T_Recv_Async (Self : in out Instance; Arg : in T) is
   begin
      -- Send data that just came off the queue:
      Self.T_Send_If_Connected (Arg);
   end T_Recv_Async;

   -- This is the command receive connector.
   overriding procedure Command_T_Recv_Sync (Self : in out Instance; Arg : in Command.T) is
      -- Execute the command:
      Stat : constant Command_Response_Status.E := Self.Execute_Command (Arg);
   begin
      -- Send the return status:
      Self.Command_Response_T_Send_If_Connected ((Source_Id => Arg.Header.Source_Id, Registration_Id => Self.Command_Reg_Id, Command_Id => Arg.Header.Id, Status => Stat));
   end Command_T_Recv_Sync;

   -- The parameter update connector. This does not need to be connected if the parameter for this component will not be used.
   overriding procedure Parameter_Update_T_Modify (Self : in out Instance; Arg : in out Parameter_Update.T) is
   begin
      -- Process the parameter update, staging or fetching parameters as requested.
      Self.Process_Parameter_Update (Arg);
   end Parameter_Update_T_Modify;

   -- This procedure is called when a T_Recv_Async message is dropped due to a full queue.
   overriding procedure T_Recv_Async_Dropped (Self : in out Instance; Arg : in T) is
      Ignore : T renames Arg;
   begin
      -- Send out info event.
      Self.Event_T_Send_If_Connected (Self.Events.Data_Dropped (Self.Sys_Time_T_Get));
   end T_Recv_Async_Dropped;

   -----------------------------------------------
   -- Command handler primitives:
   -----------------------------------------------
   -- Description:
   --    These are the commands for the limiter component.
   -- Set a new value for the maximum number of sends this component will produce per tick.
   overriding function Sends_Per_Tick (Self : in out Instance; Arg : in Packed_U16.T) return Command_Execution_Status.E is
      use Command_Execution_Status;
      The_Time : constant Sys_Time.T := Self.Sys_Time_T_Get;
   begin
      -- Set the rate:
      Self.P_Max_Sends_Per_Tick.Set_Var (Arg.Value);
      -- Send data product:
      Self.Data_Product_T_Send_If_Connected (Self.Data_Products.Max_Packet_Sends_Per_Tick (The_Time, (Value => Self.P_Max_Sends_Per_Tick.Get_Var)));
      -- Send event:
      Self.Event_T_Send_If_Connected (Self.Events.Max_Send_Per_Tick_Set (The_Time, Arg));
      return Success;
   end Sends_Per_Tick;

   -- Invalid command handler. This procedure is called when a command's arguments are found to be invalid:
   overriding procedure Invalid_Command (Self : in out Instance; Cmd : in Command.T; Errant_Field_Number : in Unsigned_32; Errant_Field : in Basic_Types.Poly_Type) is
   begin
      -- Perform action to handle an invalid command.
      Self.Event_T_Send_If_Connected (Self.Events.Invalid_Command_Received (Self.Sys_Time_T_Get, (Id => Cmd.Header.Id, Errant_Field_Number => Errant_Field_Number, Errant_Field => Errant_Field)));
   end Invalid_Command;

   -----------------------------------------------
   -- Parameter handlers:
   -----------------------------------------------
   -- Description:
   --    Parameters for the Limiter component.
   -- Invalid Parameter handler. This procedure is called when a parameter's type is found to be invalid:
   overriding procedure Invalid_Parameter (Self : in out Instance; Par : in Parameter.T; Errant_Field_Number : in Unsigned_32; Errant_Field : in Basic_Types.Poly_Type) is
   begin
      -- Perform action to handle an invalid parameter.
      Self.Event_T_Send_If_Connected (Self.Events.Invalid_Parameter_Received (Self.Sys_Time_T_Get, (
         Id => Par.Header.Id,
         Errant_Field_Number => Errant_Field_Number,
         Errant_Field => Errant_Field)
      ));
   end Invalid_Parameter;

end Component.Limiter.Implementation;
