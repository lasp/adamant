--------------------------------------------------------------------------------
-- Queue_Monitor Component Implementation Body
--------------------------------------------------------------------------------

with Packet_Types;

package body Component.Queue_Monitor.Implementation is

   --------------------------------------------------
   -- Subprogram for implementation init method:
   --------------------------------------------------
   -- This component requires a list of components whose queues it needs to monitor.
   --
   -- Init Parameters:
   -- Queued_Component_List : Component.Component_List_Access - A list of components to monitor.
   -- Packet_Period : Interfaces.Unsigned_16 - The period (in ticks) of how often to send out the queue usage packet. A value of zero disable sending of the packet.
   --
   overriding procedure Init (Self : in out Instance; Queued_Component_List : in not null Component.Component_List_Access; Packet_Period : in Interfaces.Unsigned_16 := 1) is
   begin
      Self.Queued_Component_List := Queued_Component_List;

      -- Set the packet length. Each data point for
      -- queue usage is a percentage that is 1 byte large. We store 2 of these per
      -- queued component.
      pragma Assert (Queued_Component_List'Length * 2 <= Self.Packet_To_Send.Buffer'Length, "Queue data cannot fit in single packet. Pass a queued component list with less components.");
      Self.Packet_To_Send := (
         Header => (
            Time => (0, 0),
            Id => Self.Packets.Get_Queue_Usage_Packet_Id,
            Sequence_Count => 0,
            Buffer_Length => Self.Queued_Component_List'Length * 2
         ),
         Buffer => [others => 0]
      );

      -- Initialize the packet period:
      Self.Packet_Counter.Set_Period_And_Reset_Count (Packet_Period);
   end Init;

   -- Send out packet period data product:
   overriding procedure Set_Up (Self : in out Instance) is
   begin
      -- Update period data product:
      Self.Data_Product_T_Send_If_Connected (Self.Data_Products.Packet_Period (Self.Sys_Time_T_Get, (Value => Self.Packet_Counter.Get_Period)));
   end Set_Up;

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- This is the base tick for the component.
   overriding procedure Tick_T_Recv_Sync (Self : in out Instance; Arg : in Tick.T) is
      use Packet_Types;
      Ignore : Tick.T renames Arg;
      Idx : Natural := 0;
   begin
      if Self.Packet_Counter.Is_Count_At_Period and then Self.Is_Packet_T_Send_Connected then
         -- Grab queue data for each component:
         for Comp of Self.Queued_Component_List.all loop
            -- Dispatching call to component's overridden method, because the type of
            -- comp is Component.Instance'Class:
            Self.Packet_To_Send.Buffer (Idx) := Comp.all.Get_Queue_Current_Percent_Used;
            Idx := @ + 1;
            Self.Packet_To_Send.Buffer (Idx) := Comp.all.Get_Queue_Maximum_Percent_Used;
            Idx := @ + 1;
         end loop;
         pragma Assert (Idx = Self.Packet_To_Send.Header.Buffer_Length, "Length calculation is wrong.");

         -- Send packet;
         Self.Packet_To_Send.Header.Time := Self.Sys_Time_T_Get;
         Self.Packet_T_Send (Self.Packet_To_Send);
         Self.Packet_To_Send.Header.Sequence_Count := @ + 1;
      end if;

      -- Increment the packet count:
      Self.Packet_Counter.Increment_Count;
   end Tick_T_Recv_Sync;

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
   --    These are the commands for the Queue Monitor component.
   -- Set the period of the packet. A period of zero disables the sending of the packet.
   overriding function Set_Packet_Period (Self : in out Instance; Arg : in Packed_U16.T) return Command_Execution_Status.E is
      use Command_Execution_Status;
      The_Time : constant Sys_Time.T := Self.Sys_Time_T_Get;
   begin
      -- Set new period:
      Self.Packet_Counter.Set_Period_And_Reset_Count (Arg.Value);
      -- Update period data product:
      Self.Data_Product_T_Send_If_Connected (Self.Data_Products.Packet_Period (The_Time, (Value => Self.Packet_Counter.Get_Period)));
      -- Send out event:
      Self.Event_T_Send_If_Connected (Self.Events.Packet_Period_Set (The_Time, Arg));
      return Success;
   end Set_Packet_Period;

   -- Invalid command handler. This procedure is called when a command's arguments are found to be invalid:
   overriding procedure Invalid_Command (Self : in out Instance; Cmd : in Command.T; Errant_Field_Number : in Unsigned_32; Errant_Field : in Basic_Types.Poly_Type) is
   begin
      Self.Event_T_Send_If_Connected (Self.Events.Invalid_Command_Received (Self.Sys_Time_T_Get, (
         Id => Cmd.Header.Id,
         Errant_Field_Number => Errant_Field_Number,
         Errant_Field => Errant_Field)
      ));
   end Invalid_Command;

end Component.Queue_Monitor.Implementation;
