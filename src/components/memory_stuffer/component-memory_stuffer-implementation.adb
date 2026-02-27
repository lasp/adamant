--------------------------------------------------------------------------------
-- Memory_Stuffer Component Implementation Body
--------------------------------------------------------------------------------

with Byte_Array_Pointer;
with Memory_Region;
with Byte_Array_Pointer.Packed;
with Memory_Enums;
with Command_Protector_Enums;

package body Component.Memory_Stuffer.Implementation is

   --------------------------------------------------
   -- Subprogram for implementation init method:
   --------------------------------------------------
   -- This component requires a list of memory regions which it can write to. These regions can either be protected (requiring an arm command prior to execution) or unprotected, as specified by the second parameter.
   --
   -- Init Parameters:
   -- Memory_Regions : Memory_Manager_Types.Memory_Region_Array_Access - An access to a list of memory regions.
   -- Memory_Region_Protection_List : Memory_Manager_Types.Memory_Protection_Array_Access - An access to a list of the protected/unprotected state of each memory region. The index in this array corresponds to the index of the memory region affected in the previous parameter. If the array is null, then it is assumed that all memory regions are unprotected.
   --
   overriding procedure Init (Self : in out Instance; Memory_Regions : in not null Memory_Manager_Types.Memory_Region_Array_Access; Memory_Region_Protection_List : in Memory_Manager_Types.Memory_Protection_Array_Access := null) is
      use Memory_Manager_Types;
   begin
      -- Set the regions:
      Self.Regions := Memory_Regions;
      -- Set the memory region protection list:
      if Memory_Region_Protection_List /= null then
         pragma Assert (Memory_Region_Protection_List.all'Length = Self.Regions.all'Length, "Memory regions protection list length must match the memory regions length.");
         pragma Assert (Memory_Region_Protection_List.all'First = Self.Regions.all'First, "Memory regions protection list range must match the memory regions range.");
         pragma Assert (Memory_Region_Protection_List.all'Last = Self.Regions.all'Last, "Memory regions protection list range must match the memory regions range.");
         Self.Region_Protection_List := Memory_Region_Protection_List;
      end if;
   end Init;

   overriding procedure Set_Up (Self : in out Instance) is
      The_Time : constant Sys_Time.T := Self.Sys_Time_T_Get;
      Start_Timeout : Packed_Arm_Timeout.Arm_Timeout_Type;
      Start_State : constant Command_Protector_Enums.Armed_State.E := Self.Command_Arm_State.Get_State (Start_Timeout);
   begin
      Self.Data_Product_T_Send_If_Connected (Self.Data_Products.Armed_State (The_Time, (State => Start_State)));
      Self.Data_Product_T_Send_If_Connected (Self.Data_Products.Armed_State_Timeout (The_Time, (Timeout => Start_Timeout)));
   end Set_Up;

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- This tick is used to keep track of the armed state timeout and send the data product relating the current timeout value.
   overriding procedure Tick_T_Recv_Async (Self : in out Instance; Arg : in Tick.T) is
      Ignore : Tick.T renames Arg;
      Timed_Out : Boolean;
      Timeout_Val : Packed_Arm_Timeout.Arm_Timeout_Type;
      State : Command_Protector_Enums.Armed_State.E;
      The_Time : constant Sys_Time.T := Self.Sys_Time_T_Get;
   begin
      -- Decrement the timeout:
      Self.Command_Arm_State.Decrement_Timeout (Timeout_Val, State, Timed_Out);

      -- Publish the timeout data product:
      Self.Data_Product_T_Send_If_Connected (Self.Data_Products.Armed_State_Timeout (The_Time, (Timeout => Timeout_Val)));

      -- Throw an event and update armed data product if we timed out, otherwise do nothing else.
      case Timed_Out is
         when True =>
            Self.Data_Product_T_Send_If_Connected (Self.Data_Products.Armed_State (The_Time, (State => State)));
            Self.Event_T_Send_If_Connected (Self.Events.Protected_Write_Disabled_Timeout (The_Time));
         when False =>
            null; -- Nothing to do.
      end case;
   end Tick_T_Recv_Async;

   -- This is the command receive connector.
   overriding procedure Command_T_Recv_Async (Self : in out Instance; Arg : in Command.T) is
      -- Execute the command:
      Stat : constant Command_Response_Status.E := Self.Execute_Command (Arg);
   begin
      -- Send the return status:
      Self.Command_Response_T_Send_If_Connected ((Source_Id => Arg.Header.Source_Id, Registration_Id => Self.Command_Reg_Id, Command_Id => Arg.Header.Id, Status => Stat));
   end Command_T_Recv_Async;

   -- A memory region is received on this connector and stuffed to a different memory region, a memory copy.
   overriding procedure Memory_Region_Copy_T_Recv_Async (Self : in out Instance; Arg : in Memory_Region_Copy.T) is
      use Byte_Array_Pointer;
      use Byte_Array_Pointer.Packed;
      use Memory_Enums.Memory_Copy_Status;
      Ptr : Byte_Array_Pointer.Instance;
      Ignore : Natural;
      Destination_Region : constant Memory_Region.T := (Arg.Destination_Address, Arg.Source_Region.Length);
      Status : Memory_Enums.Memory_Copy_Status.E := Success;
   begin
      -- There is no protected region checking here, since this is a backdoor copy that
      -- bypasses a direct stuff.

      -- Do copy memory if the destination region is valid. We do not necessarily manage the source
      -- region, so we don't check it.
      if Memory_Manager_Types.Is_Region_Valid (Destination_Region, Self.Regions, Ptr, Ignore) then
         -- OK the memory region is valid. Perform actual memory copy:
         Self.Event_T_Send_If_Connected (Self.Events.Copying_Memory (Self.Sys_Time_T_Get, Arg));
         Copy (Ptr, Unpack (Arg.Source_Region));
         Self.Event_T_Send_If_Connected (Self.Events.Memory_Copied (Self.Sys_Time_T_Get, Arg));
      else
         -- Invalid, return failure status:
         Self.Event_T_Send_If_Connected (Self.Events.Invalid_Copy_Destination (Self.Sys_Time_T_Get, Destination_Region));
         Status := Failure;
      end if;

      -- We are done using source memory, so release it.
      Self.Memory_Region_Release_T_Send_If_Connected ((Region => Arg.Source_Region, Status => Status));
   end Memory_Region_Copy_T_Recv_Async;

   -----------------------------------------------
   -- Helper subprograms:
   -----------------------------------------------

   -- Helper subprogram which unarms the component for register write and sends out the appropriate events and data products.
   procedure Do_Unarm (Self : in out Instance) is
      The_Time : constant Sys_Time.T := Self.Sys_Time_T_Get;
   begin
      -- We now transition to the unarmed state since we received a command.
      Self.Command_Arm_State.Unarm;

      declare
         -- Get the new state:
         New_Timeout : Packed_Arm_Timeout.Arm_Timeout_Type;
         New_State : constant Command_Protector_Enums.Armed_State.E := Self.Command_Arm_State.Get_State (New_Timeout);
      begin
         -- Send new armed data product:
         Self.Data_Product_T_Send_If_Connected (Self.Data_Products.Armed_State (The_Time, (State => New_State)));
         Self.Data_Product_T_Send_If_Connected (Self.Data_Products.Armed_State_Timeout (The_Time, (Timeout => New_Timeout)));

         -- Send info event:
         Self.Event_T_Send_If_Connected (Self.Events.Protected_Write_Disabled (The_Time));
      end;
   end Do_Unarm;

   -----------------------------------------------
   -- Command handler primitive:
   -----------------------------------------------
   -- Description:
   --    These are the commands for the Memory Stuffer component.
   -- An "arm" command which enables the next write command to a protected memory to be accepted. The "armed" state of the component will expire on the next command to this component no matter what it is or after the configurable timeout.
   overriding function Arm_Protected_Write (Self : in out Instance; Arg : in Packed_Arm_Timeout.T) return Command_Execution_Status.E is
      use Command_Execution_Status;
   begin
      -- Transition to the armed state with the timeout:
      Self.Command_Arm_State.Arm (New_Timeout => Arg.Timeout);

      -- Send out data products and events:
      declare
         The_Time : constant Sys_Time.T := Self.Sys_Time_T_Get;
         New_Timeout : Packed_Arm_Timeout.Arm_Timeout_Type;
         New_State : constant Command_Protector_Enums.Armed_State.E := Self.Command_Arm_State.Get_State (New_Timeout);
      begin
         Self.Data_Product_T_Send_If_Connected (Self.Data_Products.Armed_State (The_Time, (State => New_State)));
         Self.Data_Product_T_Send_If_Connected (Self.Data_Products.Armed_State_Timeout (The_Time, (Timeout => New_Timeout)));

         -- Send info event:
         Self.Event_T_Send_If_Connected (Self.Events.Protected_Write_Enabled (The_Time, Arg));
      end;

      return Success;
   end Arm_Protected_Write;

   -- Write bytes to a region in memory.
   overriding function Write_Memory (Self : in out Instance; Arg : in Memory_Region_Write.T) return Command_Execution_Status.E is
      use Command_Execution_Status;
      Ptr : Byte_Array_Pointer.Instance;
      Region_Index : Natural;
      Region : constant Memory_Region.T := (Arg.Address, Arg.Length);
      To_Return : Command_Execution_Status.E := Success;
      Was_Armed : Boolean := True;
   begin
      -- Unarm the system if we are armed.
      declare
         use Command_Protector_Enums.Armed_State;
         -- Get the armed state:
         Ignore_Timeout : Packed_Arm_Timeout.Arm_Timeout_Type;
         State : constant Command_Protector_Enums.Armed_State.E := Self.Command_Arm_State.Get_State (Ignore_Timeout);
      begin
         -- Based on the arm/unarmed state we do things differently.
         case State is
            -- We are in the armed state:
            when Armed =>
               Do_Unarm (Self);

            when Unarmed =>
               Was_Armed := False;
         end case;
      end;

      -- Do write memory:
      if Memory_Manager_Types.Is_Region_Valid (Region, Self.Regions, Ptr, Region_Index) then
         declare
            use Byte_Array_Pointer;
            use Memory_Manager_Types;
            Do_Write : Boolean := True;
         begin
            -- OK the memory region is valid. Let's see if the region is protected:
            if Self.Region_Protection_List /= null then
               case Self.Region_Protection_List.all (Region_Index) is
                  when Protected_Region =>
                     case Was_Armed is
                        when True =>
                           null;
                        when False =>
                           Do_Write := False;
                     end case;
                  when Unprotected_Region =>
                     null;
               end case;
            end if;

            -- If we are allowed to write the memory region then do so, otherwise throw an event:
            if Do_Write then
               -- Perform actual memory stuff:
               Self.Event_T_Send_If_Connected (Self.Events.Writing_Memory (Self.Sys_Time_T_Get, Region));
               Copy_To (Ptr, Arg.Data (Arg.Data'First .. Arg.Data'First + Arg.Length - 1));
               Self.Event_T_Send_If_Connected (Self.Events.Memory_Written (Self.Sys_Time_T_Get, Region));
            else
               Self.Event_T_Send_If_Connected (Self.Events.Protected_Write_Denied (Self.Sys_Time_T_Get, Region));
               To_Return := Failure;
            end if;
         end;
      else
         Self.Event_T_Send_If_Connected (Self.Events.Invalid_Memory_Region (Self.Sys_Time_T_Get, Region));
         To_Return := Failure;
      end if;

      return To_Return;
   end Write_Memory;

   -- Invalid command handler. This procedure is called when a command's arguments are found to be invalid:
   overriding procedure Invalid_Command (Self : in out Instance; Cmd : in Command.T; Errant_Field_Number : in Unsigned_32; Errant_Field : in Basic_Types.Poly_Type) is
   begin
      -- Perform action to handle an invalid command.
      Self.Event_T_Send_If_Connected (Self.Events.Invalid_Command_Received (Self.Sys_Time_T_Get, (Id => Cmd.Header.Id, Errant_Field_Number => Errant_Field_Number, Errant_Field => Errant_Field)));
      -- Disable protected write:
      Do_Unarm (Self);
   end Invalid_Command;

end Component.Memory_Stuffer.Implementation;
