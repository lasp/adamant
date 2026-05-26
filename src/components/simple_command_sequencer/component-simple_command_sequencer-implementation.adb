--------------------------------------------------------------------------------
-- Simple_Command_Sequencer Component Implementation Body
--------------------------------------------------------------------------------
with Sequence_Enums; use Sequence_Enums.Sequence_State;
with Packed_U32;
with Ada.Real_Time;
with Sys_Time.Arithmetic;
with Command_Types; use Command_Types;

package body Component.Simple_Command_Sequencer.Implementation is

   --------------------------------------------------
   -- Subprogram for implementation init method:
   --------------------------------------------------
   --
   -- Init Parameters:
   -- Num_Concurrent_Sequences : Interfaces.Unsigned_32 - Denotes the Number of
   -- Sequences that can be running at the same time. Any Run_Sequence commands that
   -- are sent that would increase the number of concurrent sequences beyond this
   -- number will be rejected.
   -- Sequences : Simple_Sequencer_Types.Sequences_Access - Access to statically
   -- defined sequence list.
   --
   overriding procedure Init (Self : in out Instance; Num_Concurrent_Sequences : in Interfaces.Unsigned_32; Sequences : in Simple_Sequencer_Types.Sequences_Access) is
      Time : constant Sys_Time.T := Self.Sys_Time_T_Get;
   begin
      Self.Sequence_Frames := new Sequence_Frame_Array (0 .. Num_Concurrent_Sequences - 1);
      Self.Sequence_Frames.all := [for Id in Self.Sequence_Frames.all'Range =>
         (Sequence_Id => 0,
          Frame_Id => Id,
          Step => 0,
          Wait_Until => Time,
          Status => Not_Running,
          Source_Id => 0,
          Flags => (
             Has_Source_Id => False,
             Last_Command_Success => False,
             Wait_For_Cmd_Resp => True,
             Abort_On_Failed_Cmd => True
          ),
          Response_Behavior => Sequence_Enums.Sequence_Response_Behavior.Send_After_Sequence_Start,
          Last_Command_Send => Time,
          Arg_Length => 0,
          Dynamic_Arg => [others => 0])];
      Self.Sequences := Sequences;
   end Init;

   function Find_Available_Sequence_Frame (Self : in Instance; Frame_Id : out Interfaces.Unsigned_32) return Boolean is
   begin
      Frame_Id := 0;

      for Id in Self.Sequence_Frames.all'Range loop
         declare
            Frame : Sequence_Frame.T renames Self.Sequence_Frames.all (Id);
         begin
            if Frame.Status = Not_Running and then Frame.Flags.Has_Source_Id then
               Frame_Id := Id;
               return True;
            end if;
         end;
      end loop;
      return False;
   end Find_Available_Sequence_Frame;

   function Find_Sequence_Frame_Id_From_Source_Id (Self : in Instance; Source_Id : in Command_Source_Id; Frame_Id : out Interfaces.Unsigned_32) return Boolean is
   begin
      Frame_Id := 0;

      for Id in Self.Sequence_Frames.all'Range loop
         declare
            Frame : Sequence_Frame.T renames Self.Sequence_Frames.all (Id);
         begin
            if Frame.Source_Id = Source_Id then
               Frame_Id := Id;
               return True;
            end if;
         end;
      end loop;
      return False;
   end Find_Sequence_Frame_Id_From_Source_Id;

   -- Attempts to put `Frame` into the Waiting_For_Time state with a wake time
   -- equal to now + `Arg` milliseconds. Returns False (and leaves Frame.Wait_Until
   -- unchanged) if the Sys_Time arithmetic overflows; the caller should treat that
   -- as an out-of-range sleep duration and emit the appropriate event.
   function Try_Schedule_Sleep (Self : in out Instance; Frame : in out Sequence_Frame.T; Arg : in Packed_U32.T) return Boolean is
      use Ada.Real_Time;
      use Sys_Time.Arithmetic;

      Time : constant Sys_Time.T := Self.Sys_Time_T_Get;
      Status : Sys_Time_Status;
   begin
      Frame.Status := Waiting_For_Time;
      -- Arg.Value is an unsigned 32-bit millisecond count (up to ~50 days). Ada.Real_Time.Milliseconds
      -- takes an Integer, so a value above Integer'Last cannot be expressed as a Time_Span on this
      -- platform - treat it as an out-of-range sleep.
      if Arg.Value > Interfaces.Unsigned_32 (Integer'Last) then
         return False;
      end if;
      Status := Add (Time, Milliseconds (Integer (Arg.Value)), Frame.Wait_Until);
      return Status = Success;
   end Try_Schedule_Sleep;

   procedure Execute_Sequence (Self : in out Instance; Frame : in out Sequence_Frame.T) is
      use Simple_Sequencer_Types;
   begin
      while Frame.Status = Running loop
         if Frame.Step > Self.Sequences.all (Frame.Sequence_Id).Steps.all'Last then
            -- Sequence end event
            Self.Event_T_Send_If_Connected (Self.Events.Sequence_Completed (Self.Sys_Time_T_Get, (Sequence_Id => Frame.Sequence_Id, Frame_Id => Frame.Frame_Id)));
            Frame.Status := Not_Running;
         else
            declare
               Step_Obj : Step renames Self.Sequences.all (Frame.Sequence_Id).Steps.all (Frame.Step);
            begin
               case Step_Obj.Kind is
                  when Command_Step =>
                     declare
                        Cmd : constant Command.T :=  (Header => (Source_Id => Frame.Source_Id, Id => Step_Obj.Id, Arg_Buffer_Length => Step_Obj.Arg_Length), Arg_Buffer => Step_Obj.Arg);
                     begin
                        Self.Command_T_Send (Cmd);
                        if Frame.Flags.Wait_For_Cmd_Resp then
                           Frame.Status := Waiting_For_Cmd_Resp;
                        end if;
                        Frame.Last_Command_Send := Self.Sys_Time_T_Get;
                     end;
                  when Runtime_Argument_Command_Step =>
                     declare
                        Resolved : constant Command_Types.Command_Arg_Buffer_Type :=
                           Step_Obj.Resolver.Resolve (Frame.Dynamic_Arg'Address);
                        Cmd : constant Command.T := (
                           Header     => (
                              Source_Id         => Frame.Source_Id,
                              Id                => Step_Obj.Id,
                              Arg_Buffer_Length => Step_Obj.Arg_Length),
                           Arg_Buffer => Resolved);
                     begin
                        Self.Command_T_Send (Cmd);
                        if Frame.Flags.Wait_For_Cmd_Resp then
                           Frame.Status := Waiting_For_Cmd_Resp;
                        end if;
                        Frame.Last_Command_Send := Self.Sys_Time_T_Get;
                     end;
                  when Sleep =>
                     if not Self.Try_Schedule_Sleep (Frame, Step_Obj.Sleep_Arg) then
                        Self.Event_T_Send_If_Connected (Self.Events.Sequence_Out_Of_Range_Sleep (Self.Sys_Time_T_Get, (Sequence_Id => Frame.Sequence_Id, Frame_Id => Frame.Frame_Id, Milliseconds => Step_Obj.Sleep_Arg.Value)));
                     end if;
               end case;
               if Frame.Step <= Self.Sequences.all (Frame.Sequence_Id).Steps.all'Last then
                  Frame.Step := Frame.Step + 1;
               end if;
            end;
         end if;
      end loop;
   end Execute_Sequence;

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- Sequence commands are received on this connector
   overriding procedure Command_T_Recv_Async (Self : in out Instance; Arg : in Command.T) is
      -- Execute the command:
      Stat : constant Command_Response_Status.E := Self.Execute_Command (Arg);
   begin
      -- Send the return status:
      Self.Command_Response_T_Send_If_Connected ((Source_Id => Arg.Header.Source_Id, Registration_Id => Self.Command_Reg_Id, Command_Id => Arg.Header.Id, Status => Stat));
   end Command_T_Recv_Async;

   -- Responses to sub-commands are received here. Two cases:
   --   1) Register_Source: the command router is allocating us a source ID for
   --      one of our frames. We claim the first frame that doesn't yet have one.
   --   2) Anything else: a downstream command we issued has returned a result.
   --      Look up the owning frame by source ID, advance or abort it.
   overriding procedure Command_Response_T_Recv_Async (Self : in out Instance; Arg : in Command_Response.T) is
      use Command_Response_Status;
   begin
      if Arg.Status = Command_Response_Status.Register_Source then
         declare
            Source_Id_Set : Boolean := False;
         begin
            for Frame of Self.Sequence_Frames.all loop
               if Frame.Flags.Has_Source_Id = False then
                  Frame.Source_Id := Arg.Source_Id;
                  Frame.Flags.Has_Source_Id := True;
                  Source_Id_Set := True;
                  exit;
               end if;
            end loop;

            if not Source_Id_Set then
               Self.Event_T_Send_If_Connected (Self.Events.Extra_Sequence_Id (Self.Sys_Time_T_Get));
            end if;
         end;
      else
         declare
            Frame_To_Wake_Id : Interfaces.Unsigned_32;
         begin
            if Self.Find_Sequence_Frame_Id_From_Source_Id (Arg.Source_Id, Frame_To_Wake_Id) then
               declare
                  Frame : Sequence_Frame.T renames Self.Sequence_Frames.all (Frame_To_Wake_Id);
               begin
                  if Frame.Status = Waiting_For_Cmd_Resp then
                     Frame.Status := Running;
                     Frame.Flags.Last_Command_Success := Arg.Status = Command_Response_Status.Success;

                     if Arg.Status = Command_Response_Status.Failure then
                        Self.Event_T_Send_If_Connected (Self.Events.Command_Failure (Self.Sys_Time_T_Get,
                           (Sequence_Id => Frame.Sequence_Id, Frame_Id => Frame_To_Wake_Id,
                            Step => Frame.Step, Command_Id => Arg.Command_Id)));

                        if Frame.Flags.Abort_On_Failed_Cmd then
                           Self.Event_T_Send_If_Connected (Self.Events.Sequence_Aborted (Self.Sys_Time_T_Get,
                              (Sequence_Id => Frame.Sequence_Id, Frame_Id => Frame_To_Wake_Id,
                               Step => Frame.Step)));
                           Frame.Status := Not_Running;
                        end if;
                     end if;
                  end if;
               end;
            else
               -- A command response came back tagged with a source ID we don't recognise.
               -- This is unexpected (usually a routing or registration bug); surface it
               -- so it isn't silently dropped.
               Self.Event_T_Send_If_Connected (Self.Events.Unexpected_Command_Response (Self.Sys_Time_T_Get, Arg));
            end if;
         end;
      end if;
   end Command_Response_T_Recv_Async;

   -- Tick for managing timeouts and delays
   overriding procedure Tick_T_Recv_Async (Self : in out Instance; Arg : in Tick.T) is
      use Sys_Time.Arithmetic;
      Time : constant Sys_Time.T := Self.Sys_Time_T_Get;
   begin
      -- Check if we can re-begin executing our Sequences and execute them until they wait again or finish.
      for Id in Self.Sequence_Frames.all'Range loop
         declare
            Frame : Sequence_Frame.T renames Self.Sequence_Frames.all (Id);
         begin
            case Frame.Status is
               when Running =>
                  Self.Execute_Sequence (Frame);
               when Waiting_For_Time =>
                  if Time >= Frame.Wait_Until then
                     Frame.Status := Running;
                     Self.Execute_Sequence (Frame);
                  end if;
               when Waiting_For_Cmd_Resp =>
                  -- Check timeout. The per-sequence timeout lives in the autocoded
                  -- sequence table (Command_Timeout_Millis), not in the frame.
                  declare
                     use Ada.Real_Time;
                     Timeout_Millis : constant Interfaces.Unsigned_32 :=
                        Self.Sequences.all (Frame.Sequence_Id).Command_Timeout_Millis;
                     Timeout : Sys_Time.T;
                     Status : Sys_Time_Status;
                     function To_Time_Span (Millis : Interfaces.Unsigned_32) return Time_Span is
                     begin
                        return Milliseconds (Integer (Millis));
                     end To_Time_Span;
                  begin
                     Status := Add (Frame.Last_Command_Send, To_Time_Span (Timeout_Millis), Timeout);
                     if Status /= Success then
                        Self.Event_T_Send_If_Connected (Self.Events.Sequence_Out_Of_Range_Timeout (Time, (Sequence_Id => Frame.Sequence_Id, Frame_Id => Id, Milliseconds => Timeout_Millis)));
                     elsif Time >= Timeout then
                        Self.Event_T_Send_If_Connected (Self.Events.Sequence_Timeout (Time, (Sequence_Id => Frame.Sequence_Id, Frame_Id => Id, Step => Frame.Step)));
                        Frame.Status := Not_Running;
                     end if;
                  end;
               when Not_Running =>
                  null;
            end case;
         end;
      end loop;
   end Tick_T_Recv_Async;

   -- This procedure is called when a Command_T_Recv_Async message is dropped due to a full queue.
   overriding procedure Command_T_Recv_Async_Dropped (Self : in out Instance; Arg : in Command.T) is
   begin
      Self.Event_T_Send_If_Connected (Self.Events.Dropped_Command (
        Self.Sys_Time_T_Get, Arg.Header
      ));
   end Command_T_Recv_Async_Dropped;

   -- This procedure is called when a Command_Response_T_Recv_Async message is dropped due to a full queue.
   overriding procedure Command_Response_T_Recv_Async_Dropped (Self : in out Instance; Arg : in Command_Response.T) is
   begin
      -- Should this abort the sequence? Likely because it will never be re-sent.
      -- So we just lose an executor
      Self.Event_T_Send_If_Connected (Self.Events.Dropped_Command_Response (
        Self.Sys_Time_T_Get, Arg
      ));
   end Command_Response_T_Recv_Async_Dropped;

   -- This procedure is called when a Tick_T_Recv_Async message is dropped due to a full queue.
   overriding procedure Tick_T_Recv_Async_Dropped (Self : in out Instance; Arg : in Tick.T) is
   begin
      Self.Event_T_Send_If_Connected (Self.Events.Dropped_Tick (
        Self.Sys_Time_T_Get, Arg
      ));
   end Tick_T_Recv_Async_Dropped;

   -----------------------------------------------
   -- Command handler primitives:
   -----------------------------------------------
   -- Run a Command Sequence. Allocates a free frame, copies the caller's
   -- buffer arg into the frame's Dynamic_Arg slot (for later Resolver
   -- traversal), and seeds frame state from the autocoded sequence table.
   overriding function Run_Sequence (Self : in out Instance; Arg : in Run_Sequence_Arg.T) return Command_Execution_Status.E is
      use Command_Execution_Status;
      Available_Id : Interfaces.Unsigned_32;
   begin
      if Interfaces.Unsigned_32 (Arg.Sequence_Id) not in Self.Sequences.all'Range then
         Self.Event_T_Send_If_Connected (Self.Events.Invalid_Sequence_Id (Self.Sys_Time_T_Get, (Value => Interfaces.Unsigned_32 (Arg.Sequence_Id))));
         return Failure;
      end if;
      if Self.Find_Available_Sequence_Frame (Available_Id) then
         declare
            Frame : Sequence_Frame.T renames Self.Sequence_Frames.all (Available_Id);
            Sequence : Simple_Sequencer_Types.Sequence_Type renames Self.Sequences.all (Interfaces.Unsigned_32 (Arg.Sequence_Id));
         begin
            Frame.Dynamic_Arg := Arg.Buffer_Arg;
            Frame.Arg_Length := Arg.Arg_Length;
            Frame.Sequence_Id := Interfaces.Unsigned_32 (Arg.Sequence_Id);
            Frame.Response_Behavior := Arg.Response_Behavior;
            Frame.Step := 0;
            Frame.Status := Running; -- Claim The Executor Frame
            Frame.Flags.Wait_For_Cmd_Resp := Sequence.Wait_For_Cmd_Resp;
            Frame.Flags.Abort_On_Failed_Cmd := Sequence.Abort_On_Failed_Cmd;
            Self.Event_T_Send_If_Connected (Self.Events.Sequence_Started (Self.Sys_Time_T_Get, (Sequence_Id => Interfaces.Unsigned_32 (Arg.Sequence_Id), Frame_Id => Available_Id)));
            return Success;
         end;
      end if;
      Self.Event_T_Send_If_Connected (Self.Events.No_Frame_Available (Self.Sys_Time_T_Get));
      return Failure;
   end Run_Sequence;

   -- Halt every running sequence and return each frame to a Not_Running idle state.
   -- The Source_Id assignment (made when the command router registers each frame at
   -- startup) is preserved so frames remain claimable by future Run_Sequence calls.
   overriding function Kill_All_Sequences (Self : in out Instance) return Command_Execution_Status.E is
      use Command_Execution_Status;
   begin
      for Frame of Self.Sequence_Frames.all loop
         if Frame.Status /= Not_Running then
            Frame.Status := Not_Running;
            Frame.Sequence_Id := 0;
            Frame.Step := 0;
            Frame.Arg_Length := 0;
         end if;
      end loop;
      Self.Event_T_Send_If_Connected (Self.Events.Killed_All_Sequences (Self.Sys_Time_T_Get));
      return Success;
   end Kill_All_Sequences;

   -- Set the summary packet period. The packet itself is wired in Phase 4; for
   -- now we just store the value so the command surface exists.
   overriding function Set_Summary_Packet_Period (Self : in out Instance; Arg : in Packed_U16.T) return Command_Execution_Status.E is
      use Command_Execution_Status;
   begin
      Self.Summary_Packet_Period := Arg.Value;
      return Success;
   end Set_Summary_Packet_Period;

   -- Invalid command handler. This procedure is called when a command's arguments are found to be invalid:
   overriding procedure Invalid_Command (Self : in out Instance; Cmd : in Command.T; Errant_Field_Number : in Unsigned_32; Errant_Field : in Basic_Types.Poly_Type) is
   begin
      Self.Event_T_Send_If_Connected (Self.Events.Invalid_Command_Received (
        Self.Sys_Time_T_Get,
        (Id => Cmd.Header.Id, Errant_Field_Number => Errant_Field_Number, Errant_Field => Errant_Field)
      ));
   end Invalid_Command;

end Component.Simple_Command_Sequencer.Implementation;
