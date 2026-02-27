--------------------------------------------------------------------------------
-- Command_Router Component Implementation Body
--------------------------------------------------------------------------------

with Registration_Commands;
with Command_Registration;

package body Component.Command_Router.Implementation is

   --------------------------------------------------
   -- Subprogram for implementation init method:
   --------------------------------------------------
   -- This component requires the maximum number of unique commands that it expects to need to route. This number will be used to size the internal router table on the heap.
   overriding procedure Init (Self : in out Instance; Max_Number_Of_Commands : in Natural) is
   begin
      -- Initialize the router table:
      Self.Table.Init (Max_Number_Of_Commands);

      -- Initialize counters:
      Self.Command_Receive_Count.Set_Count (0);
      Self.Command_Failure_Count.Set_Count (0);
   end Init;

   ---------------------------------------
   -- Custom public functions:
   ---------------------------------------
   not overriding procedure Final (Self : in out Instance) is
   begin
      -- Free the router table:
      Self.Table.Destroy;
   end Final;

   overriding procedure Set_Up (Self : in out Instance) is
      use Command_Types;
      use Command_Enums.Command_Response_Status;
      -- Special register command with ID and Base of 0:
      Reg_Cmd_Instance : Registration_Commands.Instance;
      Reg_Cmd : Command.T;
   begin
      -- Initialize the ID Base of our command package with the base package ID to be consistent
      Self.Commands.Set_Id_Base (Self.Command_Id_Base);

      -- Manually set the ID to zero, of the register command. ID 0 is reserved
      -- especially for this internal command:
      Reg_Cmd_Instance.Set_Id_Base (0);

      -- Initiate command registration within all connected components:
      for Index in Self.Connector_Command_T_Send'Range loop
         if Self.Is_Command_T_Send_Connected (Index) then
            -- Create the registration command with the correct registration Id:
            Reg_Cmd := Reg_Cmd_Instance.Register_Commands ((Registration_Id => Command_Types.Command_Registration_Id (Index)));
            -- Send the registration command to the component on the same connector
            -- index as the registration Id:
            Self.Command_T_Send_If_Connected (Command_T_Send_Index (Index), Reg_Cmd);
         end if;
      end loop;

      -- Now provide the source IDs to all connected command source components:
      for Index in Self.Connector_Command_Response_T_To_Forward_Send'Range loop
         -- Send special command response that has the Register status set. This should indicate to the
         -- component that it should assign the source id provided as its source id when sending commands.
         Self.Command_Response_T_To_Forward_Send (Command_Response_T_To_Forward_Send_Index (Index), (
            Source_Id => Command_Source_Id (Index),
            Registration_Id => 0,
            Command_Id => 0,
            Status => Register_Source
         ));
      end loop;
   end Set_Up;

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- The command receive connector
   overriding procedure Command_T_To_Route_Recv_Async (Self : in out Instance; Arg : in Command.T) is
   begin
      -- Directly call the synchronous implementation:
      Self.Command_T_To_Route_Recv_Sync (Arg);
   end Command_T_To_Route_Recv_Async;

   -- On this connector the Command Router receives incoming commands that need to be routed to the correct destination component. This connector is synchronous, and thus bypasses the internal queue. It should be used by components that need high priority command execution. It should only be called after command registration has occurred, or a race condition is present.
   overriding procedure Command_T_To_Route_Recv_Sync (Self : in out Instance; Arg : in Command.T) is
      -- Local variables:
      Send_Index : Command_Types.Command_Registration_Id;
      Status : Router_Table.Lookup_Status;
      The_Time : constant Sys_Time.T := Self.Sys_Time_T_Get;
   begin
      -- Send event out that command was received:
      Self.Event_T_Send_If_Connected (Self.Events.Command_Received (The_Time, Arg.Header));

      -- Send out data products:
      if Self.Is_Data_Product_T_Send_Connected then
         Self.Command_Receive_Count.Increment_Count;
         Self.Data_Product_T_Send (Self.Data_Products.Command_Receive_Count (The_Time, (Value => Self.Command_Receive_Count.Get_Count)));
         Self.Data_Product_T_Send (Self.Data_Products.Last_Received_Command (The_Time, (Id => Arg.Header.Id)));
      end if;

      -- Look up the command in the router table:
      Status := Self.Table.Lookup_Registration_Id (Arg.Header.Id, Send_Index);
      case Status is
         -- Send command to destination:
         when Router_Table.Success =>
            Self.Command_T_Send (Command_T_Send_Index (Send_Index), Arg);
            -- Command not registered, throw an event:
         when Router_Table.Id_Not_Found =>
            -- Send event out saying the id is invalid:
            Self.Event_T_Send_If_Connected (Self.Events.Command_Id_Not_Registered (The_Time, Arg.Header));

            -- Send out data products:
            if Self.Is_Data_Product_T_Send_Connected then
               Self.Command_Failure_Count.Increment_Count;
               Self.Data_Product_T_Send (Self.Data_Products.Command_Failure_Count (The_Time, (Value => Self.Command_Failure_Count.Get_Count)));
               Self.Data_Product_T_Send (Self.Data_Products.Last_Failed_Command (The_Time, (Id => Arg.Header.Id, Status => Command_Response_Status.Id_Error)));
            end if;
      end case;
   end Command_T_To_Route_Recv_Sync;

   -- This is the command receive connector for the Command Router. The NOOP commands sent on this connector will be executed by the command router. This connector will usually be connected in loopback from the Command_T_Send connector in order to provide aliveness test capabilities, or disconnected completely.
   overriding procedure Command_T_Recv_Async (Self : in out Instance; Arg : in Command.T) is
      -- Execute the command:
      Stat : constant Command_Response_Status.E := Self.Execute_Command (Arg);
   begin
      -- Send the return status:
      Self.Command_Response_T_Send_If_Connected ((Source_Id => Arg.Header.Source_Id, Registration_Id => Self.Command_Reg_Id, Command_Id => Arg.Header.Id, Status => Stat));
   end Command_T_Recv_Async;

   -- Private subprogram which registers a command in the router's table:
   procedure Register_Command (Self : in out Instance; Arg : in Command_Registration.U) is
      use Command_Types;
      Status : Router_Table.Add_Status;
   begin
      -- Make sure that the command registration is in a valid range:
      -- If not, this is definitely a software bug or bit flip.
      pragma Assert (Command_T_Send_Index (Arg.Registration_Id) >= Self.Connector_Command_T_Send'First and then
                            Command_T_Send_Index (Arg.Registration_Id) <= Self.Connector_Command_T_Send'Last);

      -- We only want to register commands that have ID > 0 since an ID of zero is the special
      -- internal register command.
      if Arg.Command_Id > 0 then
         -- Register the command:
         Status := Self.Table.Add (Arg);
         case Status is
            -- Everything is good, do nothing.
            when Router_Table.Success =>
               null;
               -- Table is full, throw an event.
            when Router_Table.Table_Full =>
               Self.Event_T_Send_If_Connected (Self.Events.Router_Table_Full (Self.Sys_Time_T_Get, (Id => Arg.Command_Id)));
               -- Id is already in table, throw an event.
            when Router_Table.Id_Conflict =>
               Self.Event_T_Send_If_Connected (Self.Events.Registration_Id_Conflict (Self.Sys_Time_T_Get, (Id => Arg.Command_Id)));
         end case;
      end if;
   end Register_Command;

   -- Command registrations are received on this connector during initialization. Command responses from connected components are received on this connector during execution.
   overriding procedure Command_Response_T_Recv_Async (Self : in out Instance; Arg : in Command_Response.T) is
      use Command_Response_Status;
      use Command_Types;
      The_Time : constant Sys_Time.T := Self.Sys_Time_T_Get;
   begin
      -- If the command response source id is not equal to zero, then we need to forward this command
      -- response to the source component that sent the command. We want to do this first, since this is
      -- this is the highest priority item in this function call.
      -- Note, we only want to do this if this is not a registration related command response.
      if Arg.Source_Id > 0 and then
          Arg.Status /= Register_Source and then
          Arg.Status /= Register
      then
         declare
            Idx : constant Command_Response_T_To_Forward_Send_Index := Command_Response_T_To_Forward_Send_Index (Arg.Source_Id);
         begin
            -- The source id must always be in the range of the command response forwarding output connector,
            -- otherwise throw an event.
            if Idx < Self.Connector_Command_Response_T_To_Forward_Send'First or else Idx > Self.Connector_Command_Response_T_To_Forward_Send'Last then
               Self.Event_T_Send_If_Connected (Self.Events.Invalid_Command_Source_Id (The_Time, Arg));
            else
               -- If the source id, is the command router's source id, then we are doing a self test, so just send an
               -- info event. Otherwise forward to the proper command source component.
               if Arg.Source_Id = Self.Commands.Get_Source_Id then
                  Self.Event_T_Send_If_Connected (Self.Events.Noop_Response_Forwarding_Success (The_Time, Arg));
               else
                  -- Forward the command response.
                  Self.Command_Response_T_To_Forward_Send_If_Connected (Idx, Arg);
               end if;
            end if;
         end;
      end if;

      -- If this is a command registration then handle that, otherwise handle the standard command response actions.
      case Arg.Status is
         -- Successful command action.
         when Success =>
            Self.Event_T_Send_If_Connected (Self.Events.Command_Execution_Successful (The_Time, Arg));
            -- Update data products if the data product connector is connected and if the command response
            -- is anything but this component's special reset data products command.
            if Self.Is_Data_Product_T_Send_Connected and then
                Arg.Command_Id /= Self.Commands.Get_Reset_Data_Products_Id
            then
               Self.Command_Success_Count := @ + 1;
               Self.Data_Product_T_Send (Self.Data_Products.Command_Success_Count (The_Time, (Value => Self.Command_Success_Count)));
               Self.Data_Product_T_Send (Self.Data_Products.Last_Successful_Command (The_Time, (Id => Arg.Command_Id)));
            end if;
         -- Failed command action.
         when Failure .. Dropped =>
            Self.Event_T_Send_If_Connected (Self.Events.Command_Execution_Failure (The_Time, Arg));
            -- Update data products if the data product connector is connected and if the command response
            -- is anything but this component's special reset data products command.
            if Self.Is_Data_Product_T_Send_Connected and then
                Arg.Command_Id /= Self.Commands.Get_Reset_Data_Products_Id
            then
               Self.Command_Failure_Count.Increment_Count;
               Self.Data_Product_T_Send (Self.Data_Products.Command_Failure_Count (The_Time, (Value => Self.Command_Failure_Count.Get_Count)));
               Self.Data_Product_T_Send (Self.Data_Products.Last_Failed_Command (The_Time, (Id => Arg.Command_Id, Status => Arg.Status)));
            end if;
         -- Perform the registration.
         when Register =>
            Register_Command (Self, (Registration_Id => Arg.Registration_Id, Command_Id => Arg.Command_Id));
            -- If the status of the command response is a Register_Source status, then we need to set our command
            -- source id. This is a special case for the command router, used to test command response forwarding.
         when Register_Source =>
            Self.Commands.Set_Source_Id (Arg.Source_Id);
      end case;
   end Command_Response_T_Recv_Async;

   -----------------------------------------------
   -- Command handler primitive:
   -----------------------------------------------
   -- Description:
   --    Commands for the command router component

   -- Simple Noop command which returns an event saying that it was triggered.
   overriding function Noop (Self : in out Instance) return Command_Execution_Status.E is
      use Command_Execution_Status;
   begin
      Self.Event_T_Send_If_Connected (Self.Events.Noop_Received (Self.Sys_Time_T_Get));
      return Success;
   end Noop;

   -- Simple Noop command which returns an event saying that it was triggered with a certain Arg.
   overriding function Noop_Arg (Self : in out Instance; Arg : in Command_Router_Arg.T) return Command_Execution_Status.E is
      use Command_Execution_Status;
      The_Time : constant Sys_Time.T := Self.Sys_Time_T_Get;
   begin
      -- Send event:
      Self.Event_T_Send_If_Connected (Self.Events.Noop_Arg_Received (The_Time, Arg));

      -- Update data product:
      Self.Data_Product_T_Send_If_Connected (Self.Data_Products.Noop_Arg_Last_Value (The_Time, Arg));

      -- Magic value which will return a failure status. Useful for testing.
      if Arg.Value = 868 then
         return Failure;
      end if;
      return Success;
   end Noop_Arg;

   -- A Noop command which self tests the command response forwarding mechanism. The command handler itself acts as a command sender component, and sends out a Noop command with a registered Source Id. The Command Router should then send out an event saying that the command response was forwarded and received.
   overriding function Noop_Response (Self : in out Instance) return Command_Execution_Status.E is
      use Command_Execution_Status;
   begin
      -- Send informational event saying that we got the command.
      Self.Event_T_Send_If_Connected (Self.Events.Noop_Response_Received (Self.Sys_Time_T_Get));
      -- OK now start self test by sending a noop command.
      Self.Command_T_To_Route_Recv_Sync (Self.Commands.Noop);
      return Success;
   end Noop_Response;

   -- This command resets the values of all the component's data product to the values at initialization.
   overriding function Reset_Data_Products (Self : in out Instance) return Command_Execution_Status.E is
      use Command_Execution_Status;
      The_Time : constant Sys_Time.T := Self.Sys_Time_T_Get;
   begin
      -- Reset the values that need to be reset:
      Self.Command_Receive_Count.Set_Count (0);
      Self.Command_Failure_Count.Set_Count (0);
      Self.Command_Success_Count := 0;

      -- Update data products:
      if Self.Is_Data_Product_T_Send_Connected then
         Self.Data_Product_T_Send (Self.Data_Products.Command_Receive_Count (The_Time, (Value => Self.Command_Receive_Count.Get_Count)));
         Self.Data_Product_T_Send (Self.Data_Products.Command_Failure_Count (The_Time, (Value => Self.Command_Failure_Count.Get_Count)));
         Self.Data_Product_T_Send (Self.Data_Products.Last_Received_Command (The_Time, (Id => Command_Types.Command_Id'First)));
         Self.Data_Product_T_Send (Self.Data_Products.Last_Failed_Command (The_Time, (Id => Command_Types.Command_Id'First, Status => Command_Response_Status.Success)));
         Self.Data_Product_T_Send (Self.Data_Products.Last_Successful_Command (The_Time, (Id => Command_Types.Command_Id'First)));
         Self.Data_Product_T_Send (Self.Data_Products.Command_Success_Count (The_Time, (Value => Self.Command_Success_Count)));
         Self.Data_Product_T_Send (Self.Data_Products.Noop_Arg_Last_Value (The_Time, (Value => 0)));
      end if;

      -- Send informational event saying that we got the command.
      Self.Event_T_Send_If_Connected (Self.Events.Data_Products_Reset (The_Time));

      return Success;
   end Reset_Data_Products;

   -- Invalid command handler. This procedure is called when a command's arguments are found to be invalid:
   overriding procedure Invalid_Command (Self : in out Instance; Cmd : in Command.T; Errant_Field_Number : in Unsigned_32; Errant_Field : in Basic_Types.Poly_Type) is
   begin
      -- Throw event:
      Self.Event_T_Send_If_Connected (Self.Events.Invalid_Command_Received (Self.Sys_Time_T_Get, (Id => Cmd.Header.Id, Errant_Field_Number => Errant_Field_Number, Errant_Field => Errant_Field)));
   end Invalid_Command;

   -----------------------------------------------
   -- Dropped message handlers:
   -----------------------------------------------

   -- Outgoing command dropped due to a component's full queue, send event:
   overriding procedure Command_T_Send_Dropped (Self : in out Instance; Index : in Command_T_Send_Index; Arg : in Command.T) is
      use Command_Response_Status;
   begin
      -- If an outgoing command was dropped then we do not expect the component to return a command response. In this
      -- case, we do this for the component, by directly sending the command response to ourself right now.
      Self.Command_Response_T_Recv_Async ((
         Source_Id => Arg.Header.Source_Id,
         Registration_Id => Command_Types.Command_Registration_Id (Index),
         Command_Id => Arg.Header.Id,
         Status => Dropped
      ));

      -- Send info event. The queue needs to be sized larger if this event ever gets thrown.
      Self.Event_T_Send_If_Connected (Self.Events.Outgoing_Command_Dropped (Self.Sys_Time_T_Get, Arg.Header));
   end Command_T_Send_Dropped;

   -- We tried to forward a command response but it got dropped due to a component's full queue, send event:
   overriding procedure Command_Response_T_To_Forward_Send_Dropped (Self : in out Instance; Index : in Command_Response_T_To_Forward_Send_Index; Arg : in Command_Response.T) is
      -- Ignore index, since it will always be the same as the command response source id:
      Ignore : Command_Response_T_To_Forward_Send_Index renames Index;
   begin
      -- Not much we can do here except throw an event. The receiving component can implement the dropped handler
      -- to appropriately respond to this case.
      Self.Event_T_Send_If_Connected (Self.Events.Forwarded_Command_Response_Dropped (Self.Sys_Time_T_Get, Arg));
   end Command_Response_T_To_Forward_Send_Dropped;

   overriding procedure Command_T_To_Route_Recv_Async_Dropped (Self : in out Instance; Arg : in Command.T) is
      use Command_Response_Status;
   begin
      -- If an incoming command was dropped then the command router itself needs to send a command response. In this
      -- case, we do this by directly sending the command response to ourself right now.
      Self.Command_Response_T_Recv_Async ((
         Source_Id => Arg.Header.Source_Id,
         Registration_Id => 0,
         Command_Id => Arg.Header.Id,
         Status => Dropped
      ));

      -- Send info event. The queue needs to be sized larger if this event ever gets thrown.
      Self.Event_T_Send_If_Connected (Self.Events.Incoming_Command_Dropped (Self.Sys_Time_T_Get, Arg.Header));
   end Command_T_To_Route_Recv_Async_Dropped;

   overriding procedure Command_T_Recv_Async_Dropped (Self : in out Instance; Arg : in Command.T) is
   begin
      -- Our queue overflowed while queueing a NOOP command. This is not a critical failure, so just
      -- throw an informational event.
      Self.Event_T_Send_If_Connected (Self.Events.Noop_Command_Dropped (Self.Sys_Time_T_Get, Arg.Header));
   end Command_T_Recv_Async_Dropped;

   overriding procedure Command_Response_T_Recv_Async_Dropped (Self : in out Instance; Arg : in Command_Response.T) is
      use Command_Response_Status;
   begin
      -- If an incoming command response was dropped then we synchronously forward this command response on by
      -- directly sending the command response to ourself right now. However, because the queue is full, something
      -- is not going right, and we want to alert the rest of the system so we modify the return status of the command
      -- response to Dropped.
      --
      -- Note: We only do this action the command response is NOT a registration. If we get a queue drop during
      -- registration, then there is nothing we can do. Trying to put this in the router table synchronously would
      -- be a race condition. So in that case just throw the event below. If the status is anything but registration
      -- then we know this was a normal command, so the forward will be OK.
      if Arg.Status /= Register and then Arg.Status /= Register_Source then
         declare
            Arg_Copy : Command_Response.T := Arg;
         begin
            Arg_Copy.Status := Dropped;
            Self.Command_Response_T_Recv_Async (Arg_Copy);
         end;
      end if;

      -- We report the drop in an event here with the original, unmodified command response. The queue needs
      -- to be sized larger if this event ever gets thrown.
      Self.Event_T_Send_If_Connected (Self.Events.Command_Response_Dropped (Self.Sys_Time_T_Get, Arg));
   end Command_Response_T_Recv_Async_Dropped;

end Component.Command_Router.Implementation;
