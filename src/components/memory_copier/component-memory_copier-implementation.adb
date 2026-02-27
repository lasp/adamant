--------------------------------------------------------------------------------
-- Memory_Copier Component Implementation Body
--------------------------------------------------------------------------------

with Memory_Manager_Enums;
with Byte_Array_Pointer.Packed;
with Virtual_Memory_Region;
with Memory_Enums;
with System;

package body Component.Memory_Copier.Implementation is

   --------------------------------------------------
   -- Subprogram for implementation init method:
   --------------------------------------------------
   -- Initialization parameters for the Memory Copier.
   --
   -- Init Parameters:
   -- Ticks_Until_Timeout : Natural - The component will wait until it has received at least this many ticks before reporting a timeout error while waiting for a memory copy to complete. For example, if the component is attached to a 10Hz rate group and this value is set to 7, then the component will wait between 700 and 800 ms before declaring a timeout error from an unresponsive downstream component.
   --
   overriding procedure Init (Self : in out Instance; Ticks_Until_Timeout : in Natural) is
   begin
      -- Save the ticks until timeout.
      Self.Sync_Object.Set_Timeout_Limit (Ticks_Until_Timeout);
   end Init;

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- The component should be attached to a periodic tick that is used to timeout waiting for a memory region copy response. See the Ticks_Until_Timeout initialization parameter.
   overriding procedure Timeout_Tick_Recv_Sync (Self : in out Instance; Arg : in Tick.T) is
      Ignore : Tick.T renames Arg;
   begin
      -- If the component is currently waiting on a response from a downstream component
      -- we need to implement the timeout logic. Otherwise reset our timeout counter.
      -- Increment the timeout counter on the sync object. This will only cause a timeout
      -- if the sync object is waiting and the timeout counter exceeds the timeout limit.
      Self.Sync_Object.Increment_Timeout_If_Waiting;
   end Timeout_Tick_Recv_Sync;

   -- The command receive connector.
   overriding procedure Command_T_Recv_Async (Self : in out Instance; Arg : in Command.T) is
      -- Execute the command:
      Stat : constant Command_Response_Status.E := Self.Execute_Command (Arg);
   begin
      -- Send the return status:
      Self.Command_Response_T_Send_If_Connected ((Source_Id => Arg.Header.Source_Id, Registration_Id => Self.Command_Reg_Id, Command_Id => Arg.Header.Id, Status => Stat));
   end Command_T_Recv_Async;

   -- The memory region is returned synchronously on this connector. The component waits internally for this response, or times out if the response is not received in time.
   overriding procedure Memory_Region_Release_T_Recv_Sync (Self : in out Instance; Arg : in Memory_Region_Release.T) is
   begin
      -- First set the protected response with the response from the component.
      -- In this function we simply store whatever we get. The error handling based on the
      -- contents of this response are done by this component's task (executing the command).
      Self.Response.Set_Var (Arg);

      -- Ok we have stored the response for the component to look at later. Now we signal
      -- to the component that a response has been received and it can read it.
      Self.Sync_Object.Release;

      -- Note, there is a possible race condition here. Think, we could set the response
      -- within the component, and then release it to allow reading of this data. Before
      -- the component reads the data, however, we may receive another response, overwriting
      -- the data the component receives before it can read the old data. This sounds serious,
      -- but this behavior should never occur, since the downstream components should not ever
      -- return a response to this component unprovoked.
      --
      -- Note, the protected buffer and the sync object are both protected objects, so there
      -- is no risk of data corruption (which would be a serious problem), there is just risk of
      -- out of order synchronization, which should not occur if the assembly is designed
      -- correctly, as described above.
   end Memory_Region_Release_T_Recv_Sync;

   -- This procedure is called when a Command_T_Recv_Async message is dropped due to a full queue.
   overriding procedure Command_T_Recv_Async_Dropped (Self : in out Instance; Arg : in Command.T) is
   begin
      -- Throw event:
      Self.Event_T_Send_If_Connected (Self.Events.Command_Dropped (Self.Sys_Time_T_Get, Arg.Header));
   end Command_T_Recv_Async_Dropped;

   -- Helper which requests source memory region and does some basic error handling on it. The returned
   -- region from this function will have the address and length specified by the Virtual_Region parameter.
   function Request_Memory_Region (Self : in out Instance; Virtual_Region : in Virtual_Memory_Region.T; Returned_Physical_Region : out Ided_Memory_Region.T) return Boolean is
      use Memory_Manager_Enums.Memory_Request_Status;
      -- Request the source memory region:
      Request : constant Memory_Region_Request.T := Self.Memory_Region_Request_T_Get;
      -- Calculate the minimum length that the requested region must have in order for
      -- the virtual memory region to be valid.
      Min_Length : constant Natural := Virtual_Region.Address + Virtual_Region.Length;
   begin
      -- Initialize out parameter to null in case we fail to find memory region
      Returned_Physical_Region := (Id => 0, Region => (Address => System.Null_Address, Length => 0));

      -- Check the request memory status:
      case Request.Status is
         when Failure =>
            -- Throw error event:
            Self.Event_T_Send_If_Connected (Self.Events.Memory_Region_Unavailable (Self.Sys_Time_T_Get));
            return False;
         when Success =>
            null; -- Everything is good.
      end case;

      -- Make sure the memory region returned is large enough to hold our virtual memory region request:
      if Request.Ided_Region.Region.Length < Min_Length then
         -- Throw info event:
         Self.Event_T_Send_If_Connected (Self.Events.Memory_Region_Length_Mismatch (Self.Sys_Time_T_Get, (Region => Request.Ided_Region.Region, Expected_Length => Min_Length)));
         -- Release the memory region immediately.
         Self.Ided_Memory_Region_Release (Request.Ided_Region);
         return False;
      end if;

      -- Form a memory region based on the virtual memory region:
      declare
         use Byte_Array_Pointer;
         Ptr : constant Byte_Array_Pointer.Instance := Byte_Array_Pointer.Packed.Unpack (Request.Ided_Region.Region);
         Ptr_Slice : constant Byte_Array_Pointer.Instance := Slice (
            Ptr,
            Start_Index => Virtual_Region.Address,
            End_Index => Virtual_Region.Address + Virtual_Region.Length - 1
         );
      begin
         -- Set returned physical region to the slice:
         Returned_Physical_Region := (Id => Request.Ided_Region.Id, Region => Byte_Array_Pointer.Packed.Pack (Ptr_Slice));
      end;

      -- No issues:
      return True;
   end Request_Memory_Region;

   -- Helper which waits for a response from a downstream component after a copy request. This
   -- function waits for the response and then does the boiler plate error checking and event throwing
   -- that all copy commands must perform.
   function Wait_For_Response (Self : in out Instance) return Boolean is
      Wait_Timed_Out : Boolean;
   begin
      -- OK wait for the response.
      Self.Sync_Object.Wait (Wait_Timed_Out);

      -- Check the wait return value:
      if Wait_Timed_Out then
         -- Send info event:
         Self.Event_T_Send_If_Connected (Self.Events.Copy_Timeout (Self.Sys_Time_T_Get));
         return False;
      end if;

      -- Take a look at the response:
      declare
         use Memory_Enums.Memory_Copy_Status;
         -- Read the response from the protected variable:
         Release : constant Memory_Region_Release.T := Self.Response.Get_Var;
      begin
         -- Check the status:
         if Release.Status /= Success then
            -- Send info event:
            Self.Event_T_Send_If_Connected (Self.Events.Copy_Failure (Self.Sys_Time_T_Get, Release));
            return False;
         end if;
      end;

      return True;
   end Wait_For_Response;

   -- Helper which sends a memory region to destination:
   function Copy_To_Destination (Self : in out Instance; Copy : in Memory_Region_Copy.T) return Boolean is
   begin
      -- First, clear the state of the synchronization
      -- object. This prevents us from just "falling through" the
      -- wait call below if some errant response was sent through
      -- to us while we were not listening.
      -- This also resets the timeout counter, so we start
      -- fresh.
      Self.Sync_Object.Reset;

      -- Send the region out the connector.
      Self.Memory_Region_Copy_T_Send (Copy);

      -- OK now we wait for and check the response.
      if not Self.Wait_For_Response then
         return False;
      end if;

      return True;
   end Copy_To_Destination;

   -----------------------------------------------
   -- Command handler primitives:
   -----------------------------------------------
   -- Description:
   --    These are the commands for the Memory Copier component.
   -- Copy Length bytes of memory from the source to the destination Address.
   overriding function Copy_Memory_Region (Self : in out Instance; Arg : in Virtual_Memory_Region_Copy.T) return Command_Execution_Status.E is
      use Command_Execution_Status;
      Ided_Region : Ided_Memory_Region.T;
   begin
      -- Send informational event:
      Self.Event_T_Send_If_Connected (Self.Events.Starting_Copy (Self.Sys_Time_T_Get, Arg));

      -- First request the scratch memory region. If this fails, return a failed command status,
      -- the appropriate event will have already been thrown.
      if not Self.Request_Memory_Region (Virtual_Region => (Address => Arg.Source_Address, Length => Arg.Source_Length), Returned_Physical_Region => Ided_Region) then
         return Failure;
      end if;
      pragma Assert (Ided_Region.Region.Length = Arg.Source_Length, "We assume it is this length after the function call.");

      -- Send Region to the destination
      if not Self.Copy_To_Destination ((Source_Region => Ided_Region.Region, Destination_Address => Arg.Destination_Address)) then
         -- Release the memory region.
         Self.Ided_Memory_Region_Release (Ided_Region);
         return Failure;
      end if;

      -- Release the memory region.
      Self.Ided_Memory_Region_Release (Ided_Region);

      -- Send informational event:
      Self.Event_T_Send_If_Connected (Self.Events.Finished_Copy (Self.Sys_Time_T_Get, Arg));

      return Success;
   end Copy_Memory_Region;

   -- Invalid command handler. This procedure is called when a command's arguments are found to be invalid:
   overriding procedure Invalid_Command (Self : in out Instance; Cmd : in Command.T; Errant_Field_Number : in Unsigned_32; Errant_Field : in Basic_Types.Poly_Type) is
   begin
      -- Throw event:
      Self.Event_T_Send_If_Connected (Self.Events.Invalid_Command_Received (Self.Sys_Time_T_Get, (
         Id => Cmd.Header.Id,
         Errant_Field_Number => Errant_Field_Number,
         Errant_Field => Errant_Field)
      ));
   end Invalid_Command;

end Component.Memory_Copier.Implementation;
