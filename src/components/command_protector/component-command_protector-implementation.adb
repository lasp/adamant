--------------------------------------------------------------------------------
-- Command_Protector Component Implementation Body
--------------------------------------------------------------------------------

with Packet_Types;
with Serializer_Types;
with Command_Protector_Enums;

package body Component.Command_Protector.Implementation is

   --------------------------------------------------
   -- Subprogram for implementation init method:
   --------------------------------------------------
   -- This component requires a list of protected command IDs at initialization.
   --
   -- Init Parameters:
   -- Protected_Command_Id_List : Command_Id_List - The list of command IDs to consider as protected commands.
   --
   overriding procedure Init (Self : in out Instance; Protected_Command_Id_List : in Command_Id_List) is
   begin
      -- The binary tree package needs to be allocated with a positive. This component is completely
      -- useless without a list of at least one protected command.
      pragma Assert (Protected_Command_Id_List'Length > 0, "Empty protected command list is not allowed.");

      -- Allocate space for protected command list:
      Self.Protected_Command_List.Init (Protected_Command_Id_List'Length);

      -- Store elements in the list:
      for Command_Identifier of Protected_Command_Id_List loop
         declare
            Ignore_1 : Command_Id;
            Ignore_2 : Natural;
            Ret : Boolean;
         begin
            -- Make sure the Command ID is not already stored in our list.
            Ret := Self.Protected_Command_List.Search (Command_Identifier, Ignore_1, Ignore_2);
            pragma Assert (not Ret, "Duplicate command ID '" & Command_Id'Image (Command_Identifier) & "' not allowed protected command ID list!");
            -- Add ID to the list:
            Ret := Self.Protected_Command_List.Add (Command_Identifier);
            pragma Assert (Ret, "Binary tree too small to hold ID. This should never happen unless there is a bug.");
         end;
      end loop;
   end Init;

   not overriding procedure Final (Self : in out Instance) is
   begin
      Self.Protected_Command_List.Destroy;
   end Final;

   -- Send out data products:
   overriding procedure Set_Up (Self : in out Instance) is
      The_Time : constant Sys_Time.T := Self.Sys_Time_T_Get;
      Start_Timeout : Packed_Arm_Timeout.Arm_Timeout_Type;
      Start_State : constant Command_Protector_Enums.Armed_State.E := Self.Command_Arm_State.Get_State (Start_Timeout);
   begin
      Self.Data_Product_T_Send_If_Connected (Self.Data_Products.Armed_State (The_Time, (State => Start_State)));
      Self.Data_Product_T_Send_If_Connected (Self.Data_Products.Armed_State_Timeout (The_Time, (Timeout => Start_Timeout)));
      Self.Data_Product_T_Send_If_Connected (Self.Data_Products.Protected_Command_Forward_Count (The_Time, (Value => Self.Protected_Command_Forward_Count)));
      Self.Data_Product_T_Send_If_Connected (Self.Data_Products.Protected_Command_Reject_Count (The_Time, (Value => Self.Protected_Command_Reject_Count)));

      -- Make sure that if we are configured to send packets, that a command fits within a packet, since we are creating
      -- an error packet that should be able to contain a maximum sized command. We usually do these kind of checks in
      -- the Init, but we only want to do this if the packet connector is connected, which does not occur until after
      -- Init is called.
      if Self.Is_Packet_T_Send_Connected then
         -- We assume that a command fits within a packet in order for us to produce an error packet:
         pragma Assert (Command.Size_In_Bytes <= Packet_Types.Packet_Buffer_Type'Length, "Command does not fit within an error packet!");
      end if;
   end Set_Up;

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- This tick is used to keep track of the armed state timeout and send the data product relating the current timeout value.
   overriding procedure Tick_T_Recv_Sync (Self : in out Instance; Arg : in Tick.T) is
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
            Self.Event_T_Send_If_Connected (Self.Events.Unarmed_Timeout (The_Time));
         when False =>
            null; -- Nothing to do.
      end case;
   end Tick_T_Recv_Sync;

   -- Commands received on this connector will be checked against the protected command list and rejected if the system is 'unarmed'. Commands not found in the protected command list they will be forwarded.
   overriding procedure Command_T_To_Forward_Recv_Sync (Self : in out Instance; Arg : in Command.T) is
      use Command_Protector_Enums.Armed_State;
      -- Get the armed state:
      Ignore_Timeout : Packed_Arm_Timeout.Arm_Timeout_Type;
      State : constant Command_Protector_Enums.Armed_State.E := Self.Command_Arm_State.Get_State (Ignore_Timeout);
      -- Look up to see if this command is in the protected list:
      Id_To_Find : Command_Id renames Arg.Header.Id;
      Ignore_Found_Id : Command_Id;
      Ignore_Found_Index : Natural;
      Is_Protected_Command : constant Boolean := Self.Protected_Command_List.Search (Id_To_Find, Ignore_Found_Id, Ignore_Found_Index);
   begin
      -- Based on the arm/unarmed state we do things differently.
      case State is
         -- In an armed state, we forward on the command no matter what, and transition to the armed state.
         -- We send some extra data products and events if this command was found in the protected command list.
         when Armed =>
            -- We are armed, so regardless if this is a protected command or not, we send it along.
            Self.Command_T_Send_If_Connected (Arg);

            -- We now transition to the unarmed state since we received a command.
            Self.Command_Arm_State.Unarm;

            declare
               -- Get the new state:
               New_Timeout : Packed_Arm_Timeout.Arm_Timeout_Type;
               New_State : constant Command_Protector_Enums.Armed_State.E := Self.Command_Arm_State.Get_State (New_Timeout);
               -- Timestamp:
               The_Time : constant Sys_Time.T := Self.Sys_Time_T_Get;
            begin
               -- Send new armed data product:
               Self.Data_Product_T_Send_If_Connected (Self.Data_Products.Armed_State (The_Time, (State => New_State)));
               Self.Data_Product_T_Send_If_Connected (Self.Data_Products.Armed_State_Timeout (The_Time, (Timeout => New_Timeout)));

               -- If this is a protected command, we need to increment a counter for telemetry.
               if Is_Protected_Command then
                  Self.Protected_Command_Forward_Count := @ + 1;
                  Self.Data_Product_T_Send_If_Connected (Self.Data_Products.Protected_Command_Forward_Count (The_Time, (Value => Self.Protected_Command_Forward_Count)));

                  -- Send info event:
                  Self.Event_T_Send_If_Connected (Self.Events.Accepted_Protected_Command (The_Time, Arg.Header));
               end if;

               -- Send info event:
               Self.Event_T_Send_If_Connected (Self.Events.Unarmed (The_Time));
            end;

            -- In the unarmed state, we only pass on the command if it is NOT found in the protected list,
            -- otherwise, we reject the command and send some data products/events.
         when Unarmed =>
            -- If it is a protected command, we need to reject it, otherwise forward it on.
            if Is_Protected_Command then
               declare
                  -- Timestamp:
                  The_Time : constant Sys_Time.T := Self.Sys_Time_T_Get;
               begin
                  -- Increment the reject counter:
                  Self.Protected_Command_Reject_Count := @ + 1;
                  Self.Data_Product_T_Send_If_Connected (Self.Data_Products.Protected_Command_Reject_Count (The_Time, (Value => Self.Protected_Command_Reject_Count)));

                  -- Send info event:
                  Self.Event_T_Send_If_Connected (Self.Events.Rejected_Protected_Command (The_Time, Arg.Header));

                  -- Send the error packet:
                  if Self.Is_Packet_T_Send_Connected then
                     declare
                        use Serializer_Types;
                        -- Create the error packet:
                        Pkt : Packet.T;
                        Stat : constant Serialization_Status := Self.Packets.Error_Packet (The_Time, Arg, Pkt);
                     begin
                        pragma Assert (Stat = Success, "This should never fail if Init was called first.");
                        Self.Packet_T_Send (Pkt);
                     end;
                  end if;
               end;
            else
               -- This is not a protected command, forward it on:
               Self.Command_T_Send_If_Connected (Arg);
            end if;
      end case;
   end Command_T_To_Forward_Recv_Sync;

   -- The command receive connector for this component's specific commands.
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
   --    These are the commands for the Command Protector component.
   -- Transition the component to the 'arm' state. A timeout is provided, which when expires, will transition the component back to the 'unarmed' state, unless a command is received first.
   overriding function Arm (Self : in out Instance; Arg : in Packed_Arm_Timeout.T) return Command_Execution_Status.E is
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
         Self.Event_T_Send_If_Connected (Self.Events.Armed (The_Time, Arg));
      end;

      return Success;
   end Arm;

   -- Invalid command handler. This procedure is called when a command's arguments are found to be invalid:
   overriding procedure Invalid_Command (Self : in out Instance; Cmd : in Command.T; Errant_Field_Number : in Unsigned_32; Errant_Field : in Basic_Types.Poly_Type) is
   begin
      -- Throw event:
      Self.Event_T_Send_If_Connected (Self.Events.Invalid_Command_Received (Self.Sys_Time_T_Get, (
         Id => Cmd.Header.Id,
         Errant_Field_Number => Errant_Field_Number,
         Errant_Field => Errant_Field
      )));
   end Invalid_Command;

end Component.Command_Protector.Implementation;
