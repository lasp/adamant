--------------------------------------------------------------------------------
-- Register_Stuffer Component Implementation Body
--------------------------------------------------------------------------------

with Packed_U32;
with System;
with Sys_Time;
with Command_Protector_Enums;
with System.Storage_Elements; use System.Storage_Elements;
with Register_Dump_Packet_Array;
with Register_Value; use Register_Value;
with Serializer_Types; use Serializer_Types;
with Ada.Unchecked_Conversion;
package body Component.Register_Stuffer.Implementation is

   --------------------------------------------------
   -- Subprogram for implementation init method:
   --------------------------------------------------
   -- Configuration for the register stuffer component.
   --
   -- Init Parameters:
   -- Protect_Registers : Boolean - If set to True, the arm command will be required before each register write command. This does not affect register reads. If set to False, an arm command is not required before each register write command.
   --
   overriding procedure Init (Self : in out Instance; Protect_Registers : in Boolean) is
   begin
      -- Store protection state:
      Self.Protect_Registers := Protect_Registers;
   end Init;

   overriding procedure Set_Up (Self : in out Instance) is
      The_Time : constant Sys_Time.T := Self.Sys_Time_T_Get;
      Start_Timeout : Packed_Arm_Timeout.Arm_Timeout_Type;
      Start_State : constant Command_Protector_Enums.Armed_State.E := Self.Command_Arm_State.Get_State (Start_Timeout);
   begin
      if Self.Protect_Registers then
         Self.Data_Product_T_Send_If_Connected (Self.Data_Products.Armed_State (The_Time, (State => Start_State)));
         Self.Data_Product_T_Send_If_Connected (Self.Data_Products.Armed_State_Timeout (The_Time, (Timeout => Start_Timeout)));
      end if;
   end Set_Up;

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- This tick is used to keep track of the armed state timeout and send the data product relating the current timeout value.
   overriding procedure Tick_T_Recv_Sync (Self : in out Instance; Arg : in Tick.T) is
      Ignore : Tick.T renames Arg;
   begin
      if Self.Protect_Registers then
         declare
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
         end;
      end if;
   end Tick_T_Recv_Sync;

   -- The command receive connector.
   overriding procedure Command_T_Recv_Sync (Self : in out Instance; Arg : in Command.T) is
      -- Execute the command:
      Stat : constant Command_Response_Status.E := Self.Execute_Command (Arg);
   begin
      -- Send the return status:
      Self.Command_Response_T_Send_If_Connected ((Source_Id => Arg.Header.Source_Id, Registration_Id => Self.Command_Reg_Id, Command_Id => Arg.Header.Id, Status => Stat));
   end Command_T_Recv_Sync;

   -- Helper function to check the validity of an address:
   function Is_Address_Valid (Self : in out Instance; Address : in System.Address) return Boolean is
      -- This conversion is okay as Address_Mod_Type will always be the correct size and alignment for System.Address
      function Address_To_Mod_Type is new Ada.Unchecked_Conversion (System.Address, Address_Mod_Type);
      -- Make sure the register address is on a 32-bit boundary:
      Is_Valid : constant Boolean := (Address_To_Mod_Type (Address) mod Address_Mod_Type (Packed_U32.Size_In_Bytes)) = 0;
   begin
      -- Throw event if necessary:
      if not Is_Valid then
         Self.Event_T_Send_If_Connected (Self.Events.Invalid_Register_Address (Self.Sys_Time_T_Get, (Address => Address)));
      end if;
      return Is_Valid;
   end Is_Address_Valid;

   function Is_End_Address_Valid (Self : in out Instance; Arg : in Register_Dump_Packet_Header.T) return Boolean is
      -- This conversion is okay as Address_Mod_Type will always be the correct size and alignment for System.Address
      function Address_To_Mod_Type is new Ada.Unchecked_Conversion (System.Address, Address_Mod_Type);
      -- Convert the address to an unsigned 64-bit integer for arithmetic operations
      Start_Address : constant Address_Mod_Type := Address_To_Mod_Type (Arg.Start_Address);
      -- Calculate the bytes to add (4 bytes per element)
      Bytes_To_Add : constant Address_Mod_Type := Address_Mod_Type (Arg.Num_Registers * Packed_U32.Size_In_Bytes);

      Max_Address : constant Address_Mod_Type := Address_Mod_Type'Last;

      -- Check if the end address would overflow
      Overflow : constant Boolean := Start_Address > (Max_Address - Bytes_To_Add);
   begin
      if Overflow then
         Self.Event_T_Send_If_Connected (Self.Events.Address_Range_Overflow (Self.Sys_Time_T_Get, Arg));
      end if;
      return not Overflow;
   end Is_End_Address_Valid;

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
         Self.Event_T_Send_If_Connected (Self.Events.Unarmed (The_Time));
      end;
   end Do_Unarm;

   -----------------------------------------------
   -- Command handler primitives:
   -----------------------------------------------
   -- Description:
   --    These are the commands for the Register Stuffer component.
   -- Write the value of a register.
   overriding function Write_Register (Self : in out Instance; Arg : in Register_Value.T) return Command_Execution_Status.E is
      use Command_Execution_Status;
      Do_Execute : Boolean := True;
      -- Timestamp:
      The_Time : constant Sys_Time.T := Self.Sys_Time_T_Get;
   begin
      if Self.Protect_Registers then
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
                  Do_Execute := False;

                  -- Send info event:
                  Self.Event_T_Send_If_Connected (Self.Events.Rejected_Protected_Register_Write (The_Time, Arg));
            end case;
         end;
      end if;

      -- Make sure the register address is on a 32-bit boundary:
      if Do_Execute and then
          Self.Is_Address_Valid (Arg.Address)
      then
         declare
            -- Define the register at the appropriate address:
            Reg : Packed_U32.Register_T_Le with Import, Convention => Ada, Address => Arg.Address;
         begin
            -- Write the register:
            Reg := (Value => Arg.Value);

            -- Update data product:
            Self.Data_Product_T_Send_If_Connected (Self.Data_Products.Last_Register_Written (The_Time, Arg));

            -- Throw info event:
            Self.Event_T_Send_If_Connected (Self.Events.Register_Written (The_Time, Arg));
            return Success;
         end;
      end if;
      return Failure;
   end Write_Register;

   -- Read the value of a register and reflect it in a data product.
   overriding function Read_Register (Self : in out Instance; Arg : in Packed_Address.T) return Command_Execution_Status.E is
      use Command_Execution_Status;
      -- Get the time:
      The_Time : constant Sys_Time.T := Self.Sys_Time_T_Get;
   begin
      -- Unarm if armed:
      if Self.Protect_Registers then
         Do_Unarm (Self);
      end if;

      -- Make sure the register address is on a 32-bit boundary:
      if Self.Is_Address_Valid (Arg.Address) then
         declare
            -- Define the register at the appropriate address:
            Reg : Packed_U32.Register_T_Le with Import, Convention => Ada, Address => Arg.Address;
            -- Read the register value:
            Reg_Copy : constant Packed_U32.T_Le := Packed_U32.T_Le (Reg);
         begin
            -- Update data product:
            Self.Data_Product_T_Send_If_Connected (Self.Data_Products.Last_Register_Read (The_Time, (Address => Arg.Address, Value => Reg_Copy.Value)));

            -- Throw info event:
            Self.Event_T_Send_If_Connected (Self.Events.Register_Read (The_Time, (Address => Arg.Address, Value => Reg_Copy.Value)));
            return Success;
         end;
      end if;
      return Failure;
   end Read_Register;

   -- An arm command which enables the next write command to a register to be accepted. The armed state of the component will expire on the next command to this component no matter what it is or after the configurable timeout.
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
         Self.Event_T_Send_If_Connected (Self.Events.Armed (The_Time, Arg));
      end;

      return Success;
   end Arm_Protected_Write;

   -- Read the value of multiple registers and dump them into a packet.
   overriding function Dump_Registers (Self : in out Instance; Arg : in Register_Dump_Packet_Header.T) return Command_Execution_Status.E is
      use Command_Execution_Status;
      -- Get the time:
      The_Time : constant Sys_Time.T := Self.Sys_Time_T_Get;
   begin
      -- Unarm if armed:
      if Self.Protect_Registers then
         Do_Unarm (Self);
      end if;

      declare
         Arr : Register_Dump_Packet_Array.T := [others => (Value => 0)];
         Last_Addr : constant System.Address := Arg.Start_Address + Storage_Offset ((Arg.Num_Registers - 1) * Packed_U32.Size_In_Bytes);
      begin
         if not Self.Is_Address_Valid (Arg.Start_Address) or else
            not Self.Is_End_Address_Valid (Arg)
         then
            return Failure;
         end if;

         for Register in 0 .. Arg.Num_Registers - 1 loop
            declare
               -- Define the register at the appropriate address:
               Reg : Packed_U32.Register_T_Le with Import, Convention => Ada, Address => Arg.Start_Address + Storage_Offset (Register * Packed_U32.Size_In_Bytes);
            begin
               -- Load the register value, in Reg_Copy to the Packet type.
               Arr (Register) := Packed_U32.Swap_Endianness (Packed_U32.T_Le (Reg));
            end;
         end loop;
         -- Construct and Send the packet based upon the returned array of register values
         declare
            Packet_To_Send : Packet.T;
            Serial_Status : constant Serialization_Status := Self.Packets.Register_Packet (The_Time,
               (
                  Header => Arg,
                  Buffer => Arr
               ),
               Packet_To_Send
            );
         begin
            -- This assertion should never fail, since the type is restricted to never being larger than a packet
            pragma Assert (Serial_Status = Success);
            Self.Packet_T_Send_If_Connected (Packet_To_Send);
         end;
         -- Update data product: (produce Last_Register_Read on last value)
         Self.Data_Product_T_Send_If_Connected (Self.Data_Products.Last_Register_Read (The_Time,
            (
               Address => Last_Addr,
               -- Arg.Num_Registers must be strictly >= 1, so we will not have constraint error with this array access
               Value => Arr (Arr'First + Arg.Num_Registers - 1).Value
            )
         ));
      end;

      -- Throw info event:
      Self.Event_T_Send_If_Connected (Self.Events.Registers_Dumped (The_Time, Arg));
      return Success;
   end Dump_Registers;

   -- Invalid command handler. This procedure is called when a command's arguments are found to be invalid:
   overriding procedure Invalid_Command (Self : in out Instance; Cmd : in Command.T; Errant_Field_Number : in Unsigned_32; Errant_Field : in Basic_Types.Poly_Type) is
   begin
      -- Throw event:
      Self.Event_T_Send_If_Connected (Self.Events.Invalid_Command_Received (Self.Sys_Time_T_Get, (
         Id => Cmd.Header.Id,
         Errant_Field_Number => Errant_Field_Number,
         Errant_Field => Errant_Field)
      ));

      -- Unarm if armed:
      if Self.Protect_Registers then
         Do_Unarm (Self);
      end if;
   end Invalid_Command;

end Component.Register_Stuffer.Implementation;
