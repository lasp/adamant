--------------------------------------------------------------------------------
-- Simple_Command_Sequencer Component Implementation Body
--------------------------------------------------------------------------------

with Sequence_Enums; use Sequence_Enums.Sequence_State; use Sequence_Enums.Sequence_Response_Behavior;
with Packed_U32;
with Ada.Real_Time;
with Sys_Time.Arithmetic;
with Command_Types; use Command_Types;
with Packet;
with Packet_Types;
with Sequence_Frame_Summary;
with Configuration;
with Sleep;

package body Component.Simple_Command_Sequencer.Implementation is

   -- The OS 'Sleep' package collides with the 'Sleep' Step_Kind enum literal.
   -- Alias it so we never reference the bare name as a package prefix.
   package Os_Sleep renames Sleep;

   -- Shorthand for the frame record; the full array types live in
   -- Simple_Sequencer_Types.
   subtype Sequence_Frame is Simple_Sequencer_Types.Sequence_Frame;

   overriding procedure Init (Self : in out Instance; Config : in Simple_Sequencer_Types.Sequencer_Config) is
   begin
      Self.Sequence_Frames := new Simple_Sequencer_Types.Sequence_Frame_Array (0 .. Config.Num_Concurrent_Sequences - 1);
      -- Frames start with their record defaults; only the identifying
      -- Frame_Id varies per element.
      Self.Sequence_Frames.all := [for Id in Self.Sequence_Frames.all'Range => (Frame_Id => Id, others => <>)];
      Self.Sequences := Config.Sequences;
   end Init;

   function Find_Available_Sequence_Frame (Self : in Instance; Frame_Id : out Interfaces.Unsigned_32) return Boolean is
   begin
      Frame_Id := 0;

      for Frame of Self.Sequence_Frames.all loop
         if Frame.Status = Not_Running and then Frame.Has_Source_Id then
            Frame_Id := Frame.Frame_Id;
            return True;
         end if;
      end loop;
      return False;
   end Find_Available_Sequence_Frame;

   function Find_Sequence_Frame_Id_From_Source_Id (Self : in Instance; Source_Id : in Command_Source_Id; Frame_Id : out Interfaces.Unsigned_32) return Boolean is
   begin
      Frame_Id := 0;

      for Frame of Self.Sequence_Frames.all loop
         if Frame.Has_Source_Id and then Frame.Source_Id = Source_Id then
            Frame_Id := Frame.Frame_Id;
            return True;
         end if;
      end loop;
      return False;
   end Find_Sequence_Frame_Id_From_Source_Id;

   -- Attempts to put `Frame` into the Waiting_For_Time state with a wake time
   -- of `Time` + `Millis` milliseconds.
   --
   -- Returns True if the sleep was scheduled successfully. The duration itself
   -- always fits a Time_Span by construction (it is a Natural), so the only
   -- failure left is Sys_Time arithmetic overflowing when adding it to the
   -- current time; on False the frame is left unchanged and the caller is
   -- expected to emit Sequence_Out_Of_Range_Sleep so the operator can see the
   -- step was skipped rather than silently lost.
   function Try_Schedule_Sleep (Frame : in out Sequence_Frame; Millis : in Natural; Time : in Sys_Time.T) return Boolean is
      use Ada.Real_Time;
      use Sys_Time.Arithmetic;
      Add_Status : Sys_Time_Status;
      Wake_Time : Sys_Time.T;
   begin
      Add_Status := Add (Time, Milliseconds (Millis), Wake_Time);
      if Add_Status /= Success then
         return False;
      end if;
      Frame.Wait_Until := Wake_Time;
      Frame.Status := Waiting_For_Time;
      return True;
   end Try_Schedule_Sleep;

   -- Emit a deferred Command_Response for `Frame`, but only if the frame was
   -- claimed with Send_After_Sequence_Completion (otherwise the immediate reply
   -- has already been sent from Command_T_Recv_Async and we do nothing). Called
   -- from every code path that ends a sequence: natural completion (Success),
   -- abort on sub-command failure / timeout / kill / out-of-range sleep or
   -- timeout (Failure). The reply uses the operator context captured on the
   -- frame at claim time and the sequencer's own registration id.
   procedure Send_Deferred_Response_If_Pending
     (Self  : in out Instance;
      Frame : in Sequence_Frame;
      Stat  : in Command_Response_Status.E) is
   begin
      if Frame.Response_Behavior = Send_After_Sequence_Completion then
         Self.Command_Response_T_Send_If_Connected
           ((Source_Id       => Frame.Operator_Source_Id,
             Registration_Id => Self.Command_Reg_Id,
             Command_Id      => Frame.Operator_Command_Id,
             Status          => Stat));
      end if;
   end Send_Deferred_Response_If_Pending;

   -- Recount the running frames and update the frame-count data products,
   -- raising the high water mark when exceeded. Called whenever a frame is
   -- claimed or returns to idle.
   procedure Send_Frame_Count_Data_Products (Self : in out Instance; Time : in Sys_Time.T) is
      Count : Interfaces.Unsigned_16 := 0;
   begin
      for Frame of Self.Sequence_Frames.all loop
         if Frame.Status /= Not_Running then
            Count := @ + 1;
         end if;
      end loop;
      if Count > Self.Frame_Running_Hwm then
         Self.Frame_Running_Hwm := Count;
      end if;
      Self.Data_Product_T_Send_If_Connected (Self.Data_Products.Frame_Running_Count (Time, (Value => Count)));
      Self.Data_Product_T_Send_If_Connected (Self.Data_Products.Frame_Running_High_Water_Mark (Time, (Value => Self.Frame_Running_Hwm)));
   end Send_Frame_Count_Data_Products;

   -- End the sequence running on `Frame`: return the frame to idle, update the
   -- finished/failed counters and last-sequence data products, refresh the
   -- frame-count products, and emit the deferred operator reply when the frame
   -- was claimed with Send_After_Sequence_Completion.
   --
   -- Only Status is reset here. All other per-run frame state is deliberately
   -- left in place -- it is fully re-seeded when Run_Sequence claims the frame
   -- again, and while idle it lets the summary packet report the frame's last
   -- run.
   procedure Finish_Sequence
     (Self  : in out Instance;
      Frame : in out Sequence_Frame;
      Stat  : in Command_Response_Status.E;
      Time  : in Sys_Time.T) is
      use Command_Response_Status;
   begin
      Frame.Status := Not_Running;
      if Stat = Success then
         Self.Sequences_Finished_Count := @ + 1;
         Self.Data_Product_T_Send_If_Connected (Self.Data_Products.Sequences_Finished_Count (Time, (Value => Self.Sequences_Finished_Count)));
         Self.Data_Product_T_Send_If_Connected (Self.Data_Products.Last_Sequence_Finished (Time, (Value => Frame.Sequence_Id)));
      else
         Self.Sequences_Failed_Count := @ + 1;
         Self.Data_Product_T_Send_If_Connected (Self.Data_Products.Sequences_Failed_Count (Time, (Value => Self.Sequences_Failed_Count)));
         Self.Data_Product_T_Send_If_Connected (Self.Data_Products.Last_Sequence_Failed (Time, (Value => Frame.Sequence_Id)));
      end if;
      Send_Frame_Count_Data_Products (Self, Time);
      Send_Deferred_Response_If_Pending (Self, Frame, Stat);
   end Finish_Sequence;

   -- Count a dispatched sub-command and update its data product.
   procedure Note_Command_Sent (Self : in out Instance; Time : in Sys_Time.T) is
   begin
      Self.Commands_Sent_Count := @ + 1;
      Self.Data_Product_T_Send_If_Connected (Self.Data_Products.Commands_Sent_Count (Time, (Value => Self.Commands_Sent_Count)));
   end Note_Command_Sent;

   -- Dispatch a step's sub-command: stamp the response deadline and park the
   -- frame on the response (when the sequence waits on command completion),
   -- then send and count the command. The deadline is computed once here --
   -- the tick handler only compares against it. If the deadline cannot be
   -- represented in system time, the sequence is ended instead of dispatching:
   -- the frame would otherwise wait forever on a deadline that never arrives.
   procedure Dispatch_Step_Command (Self : in out Instance; Frame : in out Sequence_Frame; Cmd : in Command.T; Time : in Sys_Time.T) is
      use Sys_Time.Arithmetic;
      Add_Status : Sys_Time_Status;
   begin
      if Frame.Sequence.Wait_For_Cmd_Resp then
         Add_Status := Add (Time, Frame.Sequence.Command_Timeout, Frame.Timeout_Deadline);
         if Add_Status /= Success then
            Self.Event_T_Send_If_Connected (Self.Events.Sequence_Out_Of_Range_Timeout (Time, (Sequence_Id => Frame.Sequence_Id, Frame_Id => Frame.Frame_Id, Step => Frame.Step)));
            Finish_Sequence (Self, Frame, Command_Response_Status.Failure, Time);
            return;
         end if;
         Frame.Status := Waiting_For_Cmd_Resp;
      end if;
      Self.Command_T_Send (Cmd);
      Note_Command_Sent (Self, Time);
   end Dispatch_Step_Command;

   procedure Execute_Sequence (Self : in out Instance; Frame : in out Sequence_Frame) is
      use Simple_Sequencer_Types;
   begin
      while Frame.Status = Running loop
         if Frame.Step > Frame.Sequence.Steps.all'Last then
            declare
               Time : constant Sys_Time.T := Self.Sys_Time_T_Get;
            begin
               -- Sequence end event
               Self.Event_T_Send_If_Connected (Self.Events.Sequence_Completed (Time, (Sequence_Id => Frame.Sequence_Id, Frame_Id => Frame.Frame_Id)));
               Finish_Sequence (Self, Frame, Command_Response_Status.Success, Time);
            end;
         else
            declare
               Step_Obj : Step renames Frame.Sequence.Steps.all (Frame.Step);
               Time : constant Sys_Time.T := Self.Sys_Time_T_Get;
            begin
               case Step_Obj.Kind is
                  when Command_Step =>
                     Dispatch_Step_Command (Self, Frame,
                        (Header => (Source_Id => Frame.Source_Id, Id => Step_Obj.Id, Arg_Buffer_Length => Step_Obj.Arg_Length), Arg_Buffer => Step_Obj.Arg), Time);
                  when Runtime_Argument_Command_Step =>
                     -- Dynamic-arg step: dispatch the per-step Resolver to
                     -- deserialize the sequence's per-call argument buffer and
                     -- extract this sub-command's typed argument. The resolver
                     -- (one type per dynamic step) encodes the traversal path
                     -- through the caller's arg record and returns the serialized
                     -- leaf, ready to use as the sub-command's Arg_Buffer.
                     declare
                        Resolved : Command_Types.Command_Arg_Buffer_Type;
                        Valid : constant Boolean := Step_Obj.Resolver (Frame.Dynamic_Arg, Resolved);
                     begin
                        if Valid then
                           Dispatch_Step_Command (Self, Frame,
                              (Header => (Source_Id => Frame.Source_Id, Id => Step_Obj.Id, Arg_Buffer_Length => Step_Obj.Arg_Length), Arg_Buffer => Resolved), Time);
                        else
                           Self.Event_T_Send_If_Connected (Self.Events.Invalid_Dynamic_Command_Argument (Time, (Sequence_Id => Frame.Sequence_Id, Frame_Id => Frame.Frame_Id, Step => Frame.Step, Command_Id => Step_Obj.Id)));
                           Finish_Sequence (Self, Frame, Command_Response_Status.Failure, Time);
                        end if;
                     end;
                  when Simple_Sequencer_Types.Sleep =>
                     -- Static sleeps are bounded to Natural by the model, so
                     -- the only failure left is Sys_Time overflow computing
                     -- the wake time. End the sequence cleanly in that case --
                     -- letting it continue would leave the frame stuck in
                     -- Waiting_For_Time with a stale Wait_Until -- and emit
                     -- Failure to any pending deferred reply.
                     if not Try_Schedule_Sleep (Frame, Step_Obj.Sleep_Arg, Time) then
                        Self.Event_T_Send_If_Connected (Self.Events.Sequence_Out_Of_Range_Sleep (Time, (Sequence_Id => Frame.Sequence_Id, Frame_Id => Frame.Frame_Id, Milliseconds => Interfaces.Unsigned_32 (Step_Obj.Sleep_Arg))));
                        Finish_Sequence (Self, Frame, Command_Response_Status.Failure, Time);
                     end if;
                  when Runtime_Sleep =>
                     -- Dynamic sleep: resolve the duration from the sequence's
                     -- per-call argument as a Packed_U32 millisecond count.
                     -- Unlike static sleeps the value is not bounded by the
                     -- model, so range check it here before scheduling.
                     declare
                        Resolved : Command_Types.Command_Arg_Buffer_Type;
                        Valid : constant Boolean := Step_Obj.Sleep_Resolver (Frame.Dynamic_Arg, Resolved);
                     begin
                        if Valid then
                           declare
                              Millis : constant Interfaces.Unsigned_32 :=
                                 Packed_U32.Serialization.From_Byte_Array (Resolved (Resolved'First .. Resolved'First + Packed_U32.Serialization.Serialized_Length - 1)).Value;
                           begin
                              if Millis > Interfaces.Unsigned_32 (Natural'Last)
                                 or else not Try_Schedule_Sleep (Frame, Natural (Millis), Time)
                              then
                                 Self.Event_T_Send_If_Connected (Self.Events.Sequence_Out_Of_Range_Sleep (Time, (Sequence_Id => Frame.Sequence_Id, Frame_Id => Frame.Frame_Id, Milliseconds => Millis)));
                                 Finish_Sequence (Self, Frame, Command_Response_Status.Failure, Time);
                              end if;
                           end;
                        else
                           Self.Event_T_Send_If_Connected (Self.Events.Invalid_Dynamic_Sleep_Argument (Time, (Sequence_Id => Frame.Sequence_Id, Frame_Id => Frame.Frame_Id, Step => Frame.Step)));
                           Finish_Sequence (Self, Frame, Command_Response_Status.Failure, Time);
                        end if;
                     end;
               end case;
               if Frame.Step <= Frame.Sequence.Steps.all'Last then
                  Frame.Step := Frame.Step + 1;
               end if;
            end;
         end if;
      end loop;
   end Execute_Sequence;

   -- Execute a modeled (non-ghost) command and emit its response. This mirrors
   -- the autocoded Command_T_Recv_Async body from the component's base package
   -- -- execute the command, then reply with its status -- with one addition:
   -- when Run_Sequence claims a frame whose sequence is configured
   -- Send_After_Sequence_Completion, it sets Caller.Defer_Command_Response and
   -- the reply is emitted by the sequence-end paths instead of here.
   procedure Execute_Command_And_Respond (Self : in out Instance; Arg : in Command.T) is
      Stat : constant Command_Response_Status.E := Self.Execute_Command (Arg);
   begin
      if not Self.Caller.Defer_Command_Response then
         Self.Command_Response_T_Send_If_Connected ((Source_Id => Arg.Header.Source_Id, Registration_Id => Self.Command_Reg_Id, Command_Id => Arg.Header.Id, Status => Stat));
      end if;
   end Execute_Command_And_Respond;

   -- Sequence commands are received on this connector
   overriding procedure Command_T_Recv_Async (Self : in out Instance; Arg : in Command.T) is
      -- The per-sequence "ghost" commands occupy the command-ID block
      -- immediately after the modeled commands. They are first-class in
      -- the assembly/COSMOS dictionary but absent from this component's static
      -- command model, so Execute_Command's range check would reject them.
      -- Intercept that block here and translate it to a Run_Sequence call.
      First_Ghost_Id : constant Command_Types.Command_Id :=
         Self.Command_Id_Base + Command_Types.Command_Id (Simple_Command_Sequencer_Commands.Num_Commands);
      Num_Ghosts : constant Command_Types.Command_Id :=
         Command_Types.Command_Id (Self.Sequences.all'Length);
   begin
      -- Stash the caller's response context before dispatch so Run_Sequence
      -- can copy it into the frame it claims. The active component's serial
      -- queue guarantees one dispatch in flight at a time, so this scratch
      -- can't be clobbered mid-flight. Defer_Command_Response is reset here so
      -- a prior command's defer flag can't leak into this one.
      Self.Caller := (Source_Id => Arg.Header.Source_Id, Command_Id => Arg.Header.Id, Defer_Command_Response => False);

      if Arg.Header.Id >= First_Ghost_Id and then Arg.Header.Id < First_Ghost_Id + Num_Ghosts then
         -- Ghost (per-sequence) command: the argument buffer carries the
         -- sequence's native argument verbatim (empty for argless sequences).
         -- Translate to a Run_Sequence_Arg.T and dispatch through the
         -- Run_Sequence backbone.
         declare
            Seq_Index : constant Interfaces.Unsigned_16 :=
               Interfaces.Unsigned_16 (Arg.Header.Id - First_Ghost_Id); -- 0-based = Sequence_Id
            Native_Len : constant Natural := Natural (Arg.Header.Arg_Buffer_Length);
         begin
            if Native_Len > Natural (Simple_Sequencer_Types.Run_Sequence_Arg_Buffer_Length_Type'Last) then
               -- The argument cannot fit the Run_Sequence passthrough buffer.
               -- No modeled sequence argument type can exceed it, so this is a
               -- malformed command -- reject it as invalid rather than
               -- truncating the argument.
               Self.Event_T_Send_If_Connected (Self.Events.Invalid_Command_Received (
                  Self.Sys_Time_T_Get,
                  (Id => Arg.Header.Id, Errant_Field_Number => 0, Errant_Field => [others => 0])));
               Self.Command_Response_T_Send_If_Connected ((Source_Id => Arg.Header.Source_Id, Registration_Id => Self.Command_Reg_Id, Command_Id => Arg.Header.Id, Status => Command_Response_Status.Failure));
            else
               declare
                  Run_Arg : Run_Sequence_Arg.T :=
                     (Sequence_Id => Seq_Index,
                      Arg_Length => Simple_Sequencer_Types.Run_Sequence_Arg_Buffer_Length_Type (Native_Len),
                      Buffer_Arg => [others => 0]);
               begin
                  if Native_Len > 0 then
                     Run_Arg.Buffer_Arg (Run_Arg.Buffer_Arg'First .. Run_Arg.Buffer_Arg'First + Native_Len - 1) :=
                        Arg.Arg_Buffer (Arg.Arg_Buffer'First .. Arg.Arg_Buffer'First + Native_Len - 1);
                  end if;

                  declare
                     Exec_Stat : constant Command_Execution_Status.E := Self.Run_Sequence (Run_Arg);
                  begin
                     -- The Command_Execution_Status -> Command_Response_Status
                     -- mapping is only needed when replying immediately; a
                     -- deferred reply is emitted by the sequence-end paths
                     -- with its own final status.
                     if not Self.Caller.Defer_Command_Response then
                        Self.Command_Response_T_Send_If_Connected
                          ((Source_Id       => Arg.Header.Source_Id,
                            Registration_Id => Self.Command_Reg_Id,
                            Command_Id      => Arg.Header.Id,
                            Status          =>
                              (case Exec_Stat is
                                 when Command_Execution_Status.Success => Command_Response_Status.Success,
                                 when Command_Execution_Status.Failure => Command_Response_Status.Failure)));
                     end if;
                  end;
               end;
            end if;
         end;
      else
         -- Not a ghost command: follow the normal component command execution
         -- logic.
         Execute_Command_And_Respond (Self, Arg);
      end if;
   end Command_T_Recv_Async;

   -- Responses to sub-commands are received here. Two cases:
   --   1) Register_Source: the command router is allocating us a source ID for
   --      one of our frames. We claim the first frame that doesn't yet have one.
   --   2) Anything else: a downstream command we issued has returned a result.
   --      Look up the owning frame by source ID, advance or abort it.
   overriding procedure Command_Response_T_Recv_Async (Self : in out Instance; Arg : in Command_Response.T) is
      use Command_Response_Status;
      Time : constant Sys_Time.T := Self.Sys_Time_T_Get;
   begin
      if Arg.Status = Command_Response_Status.Register_Source then
         declare
            Source_Id_Set : Boolean := False;
         begin
            for Frame of Self.Sequence_Frames.all loop
               if Frame.Has_Source_Id = False then
                  Frame.Source_Id := Arg.Source_Id;
                  Frame.Has_Source_Id := True;
                  Source_Id_Set := True;
                  exit;
               end if;
            end loop;

            if not Source_Id_Set then
               Self.Event_T_Send_If_Connected (Self.Events.Unexpected_Register_Source (Time));
            end if;
         end;
      else
         declare
            Frame_To_Wake_Id : Interfaces.Unsigned_32;
         begin
            if Find_Sequence_Frame_Id_From_Source_Id (Self, Arg.Source_Id, Frame_To_Wake_Id) then
               declare
                  Frame : Sequence_Frame renames Self.Sequence_Frames.all (Frame_To_Wake_Id);
               begin
                  -- Only a frame parked in Waiting_For_Cmd_Resp is advanced by a
                  -- response. In any other state the response is late or stale --
                  -- e.g. the frame already timed out, was killed, or was even
                  -- reused for a new sequence that hasn't issued a command yet --
                  -- and is deliberately ignored: acting on it would advance the
                  -- wrong step.
                  if Frame.Status = Waiting_For_Cmd_Resp then
                     -- Wake the frame and resume it below -- the response is the
                     -- event the frame was parked on, so the next step dispatches
                     -- now instead of waiting for the next tick.
                     Frame.Status := Running;

                     if Arg.Status = Command_Response_Status.Failure then
                        Self.Event_T_Send_If_Connected (Self.Events.Command_Failure (Time,
                           (Sequence_Id => Frame.Sequence_Id, Frame_Id => Frame_To_Wake_Id,
                            Step => Frame.Step, Command_Id => Arg.Command_Id)));

                        if Frame.Sequence.Abort_On_Failed_Cmd then
                           Self.Event_T_Send_If_Connected (Self.Events.Sequence_Aborted (Time,
                              (Sequence_Id => Frame.Sequence_Id, Frame_Id => Frame_To_Wake_Id,
                               Step => Frame.Step)));
                           Finish_Sequence (Self, Frame, Command_Response_Status.Failure, Time);
                        end if;
                     end if;

                     -- Continue executing the sequence unless the failure path
                     -- above already ended it. Execute_Sequence runs until the
                     -- frame parks again (next command response or sleep) or the
                     -- sequence completes. Timeouts and sleep wake-ups remain on
                     -- the tick cadence.
                     if Frame.Status = Running then
                        Execute_Sequence (Self, Frame);
                     end if;
                  end if;
               end;
            else
               -- A command response came back tagged with a source ID we don't recognise.
               -- This is unexpected (usually a routing or registration bug); surface it
               -- so it isn't silently dropped.
               Self.Event_T_Send_If_Connected (Self.Events.Unexpected_Command_Response (Time, Arg));
            end if;
         end;
      end if;
   end Command_Response_T_Recv_Async;

   -- Emit the summary packet if a period is set and enough ticks have elapsed.
   -- Called once per tick, after the frames have been advanced, so the packet
   -- reflects this tick's end state.
   procedure Send_Summary_Packet_If_Due (Self : in out Instance; Time : in Sys_Time.T) is
   begin
      if Self.Summary_Packet_Period = 0 then
         return;
      end if;

      Self.Summary_Packet_Tick_Count := @ + 1;
      if Self.Summary_Packet_Tick_Count < Self.Summary_Packet_Period then
         return;
      end if;
      Self.Summary_Packet_Tick_Count := 0;

      -- Build the summary packet: one Sequence_Frame_Summary per frame, in
      -- frame order. The packet's type is autogenerated per command sequences
      -- suite from the suite's num_concurrent_sequences (see the
      -- simple_command_sequencer_packets model), which makes the per-frame
      -- fields visible field-by-field in the ground system. The FSW does not
      -- need that type; it fills the packet buffer one frame at a time using
      -- the Sequence_Frame_Summary serializer.
      declare
         Pkt : Packet.T := Self.Packets.Summary_Packet_Empty (Time);
         Idx : Packet_Types.Packet_Buffer_Length_Type := Pkt.Buffer'First;
      begin
         for Frame of Self.Sequence_Frames.all loop
            Pkt.Buffer (Idx .. Idx + Sequence_Frame_Summary.Size_In_Bytes - 1) :=
               Sequence_Frame_Summary.Serialization.To_Byte_Array ((
                  Sequence_Id => Frame.Sequence_Id,
                  Step => Frame.Step,
                  Status => Frame.Status,
                  Response_Behavior => Frame.Response_Behavior,
                  Operator_Source_Id => Frame.Operator_Source_Id));
            Idx := @ + Sequence_Frame_Summary.Size_In_Bytes;
         end loop;
         Pkt.Header.Buffer_Length := Self.Sequence_Frames.all'Length * Sequence_Frame_Summary.Size_In_Bytes;
         pragma Assert (Pkt.Header.Buffer_Length = Idx - Pkt.Buffer'First);
         Self.Packet_T_Send_If_Connected (Pkt);
      end;
   end Send_Summary_Packet_If_Due;

   -- Tick for managing timeouts and delays
   overriding procedure Tick_T_Recv_Async (Self : in out Instance; Arg : in Tick.T) is
      Ignore : Tick.T renames Arg;
      use Sys_Time.Arithmetic;
      Time : constant Sys_Time.T := Self.Sys_Time_T_Get;
   begin
      -- Resume any parked frames whose wake condition has been met.
      for Frame of Self.Sequence_Frames.all loop
         case Frame.Status is
            when Waiting_For_Time =>
               if Time >= Frame.Wait_Until then
                  Frame.Status := Running;
                  Execute_Sequence (Self, Frame);
               end if;
            when Waiting_For_Cmd_Resp =>
               -- The response deadline was stamped when the sub-command was
               -- dispatched; only the comparison happens per tick.
               if Time >= Frame.Timeout_Deadline then
                  Self.Event_T_Send_If_Connected (Self.Events.Sequence_Timeout (Time, (Sequence_Id => Frame.Sequence_Id, Frame_Id => Frame.Frame_Id, Step => Frame.Step)));
                  Finish_Sequence (Self, Frame, Command_Response_Status.Failure, Time);
               end if;
            when Not_Running | Running =>
               -- Nothing to do for an idle frame. A frame is also never seen
               -- Running here: every path that sets Running (claim, response
               -- wake, sleep wake above) executes the sequence to a parked or
               -- idle state before returning.
               null;
         end case;
      end loop;

      Send_Summary_Packet_If_Due (Self, Time);
   end Tick_T_Recv_Async;

   -- This procedure is called when a Command_T_Recv_Async message is dropped due to a full queue.
   overriding procedure Command_T_Recv_Async_Dropped (Self : in out Instance; Arg : in Command.T) is
   begin
      Self.Event_T_Send_If_Connected (Self.Events.Dropped_Command (Self.Sys_Time_T_Get, Arg.Header));
   end Command_T_Recv_Async_Dropped;

   -- This procedure is called when a Command_Response_T_Recv_Async message is dropped due to a full queue.
   overriding procedure Command_Response_T_Recv_Async_Dropped (Self : in out Instance; Arg : in Command_Response.T) is
   begin
      -- Should this abort the sequence? Likely because it will never be re-sent.
      -- So we just lose an executor
      Self.Event_T_Send_If_Connected (Self.Events.Dropped_Command_Response (Self.Sys_Time_T_Get, Arg));
   end Command_Response_T_Recv_Async_Dropped;

   -- This procedure is called when a Tick_T_Recv_Async message is dropped due to a full queue.
   overriding procedure Tick_T_Recv_Async_Dropped (Self : in out Instance; Arg : in Tick.T) is
   begin
      Self.Event_T_Send_If_Connected (Self.Events.Dropped_Tick (Self.Sys_Time_T_Get, Arg));
   end Tick_T_Recv_Async_Dropped;

   -- Run a Command Sequence. Allocates a free frame, copies the caller's
   -- buffer arg into the frame's Dynamic_Arg slot (for later Resolver
   -- traversal), seeds frame state from the autocoded sequence table, and
   -- starts executing the sequence.
   overriding function Run_Sequence (Self : in out Instance; Arg : in Run_Sequence_Arg.T) return Command_Execution_Status.E is
      use Command_Execution_Status;
      Time : constant Sys_Time.T := Self.Sys_Time_T_Get;
      Available_Id : Interfaces.Unsigned_32;
   begin
      -- Early exits: unknown sequence id, or no idle frame to claim.
      if Arg.Sequence_Id not in Self.Sequences.all'Range then
         Self.Event_T_Send_If_Connected (Self.Events.Invalid_Sequence_Id (Time, (Value => Arg.Sequence_Id)));
         return Failure;
      end if;
      if not Find_Available_Sequence_Frame (Self, Available_Id) then
         Self.Event_T_Send_If_Connected (Self.Events.No_Frame_Available (Time));
         return Failure;
      end if;

      declare
         Frame : Sequence_Frame renames Self.Sequence_Frames.all (Available_Id);
      begin
         -- Seed ALL per-run frame state. The sequence-end paths only ever
         -- reset Status, so nothing here may rely on leftover state from a
         -- previous run.
         --
         -- Only the used prefix of the caller's buffer is meaningful (and the
         -- no-argument case is common), so skip the copy when it is empty.
         if Arg.Arg_Length > 0 then
            Frame.Dynamic_Arg (Frame.Dynamic_Arg'First .. Frame.Dynamic_Arg'First + Arg.Arg_Length - 1) :=
               Arg.Buffer_Arg (Arg.Buffer_Arg'First .. Arg.Buffer_Arg'First + Arg.Arg_Length - 1);
         end if;
         Frame.Arg_Length := Arg.Arg_Length;
         Frame.Sequence_Id := Arg.Sequence_Id;
         Frame.Sequence := Self.Sequences.all (Arg.Sequence_Id)'Access;
         Frame.Step := 0;
         -- Response behavior is the sequence's static configuration from the
         -- autocoded sequence table.
         Frame.Response_Behavior := Frame.Sequence.Response_Behavior;
         -- Snapshot the caller's response context (captured by
         -- Command_T_Recv_Async into Self.Caller). Always stored, but only
         -- read on the Send_After_Sequence_Completion emission paths; for the
         -- default Send_After_Sequence_Start the immediate reply is built from
         -- the Command.T header directly.
         Frame.Operator_Source_Id := Self.Caller.Source_Id;
         Frame.Operator_Command_Id := Self.Caller.Command_Id;
         if Frame.Sequence.Response_Behavior = Send_After_Sequence_Completion then
            -- Signal Command_T_Recv_Async to suppress the immediate reply --
            -- this frame will emit it on completion (or abort / timeout / kill).
            Self.Caller.Defer_Command_Response := True;
         end if;
         Frame.Status := Running; -- Claim The Executor Frame
         Self.Event_T_Send_If_Connected (Self.Events.Sequence_Started (Time, (Sequence_Id => Arg.Sequence_Id, Frame_Id => Available_Id)));
         -- Update the started counters and frame-count data products:
         Self.Sequences_Started_Count := @ + 1;
         Self.Data_Product_T_Send_If_Connected (Self.Data_Products.Sequences_Started_Count (Time, (Value => Self.Sequences_Started_Count)));
         Self.Data_Product_T_Send_If_Connected (Self.Data_Products.Last_Sequence_Started (Time, (Value => Arg.Sequence_Id)));
         Send_Frame_Count_Data_Products (Self, Time);
         -- Start executing immediately -- the sequence's first steps dispatch
         -- from the Run_Sequence command itself, running until the frame parks
         -- (command response or sleep) or the sequence completes. Ticks only
         -- resume parked frames.
         Execute_Sequence (Self, Frame);
         return Success;
      end;
   end Run_Sequence;

   -- Halt every running sequence and return each frame to a Not_Running idle state.
   -- The Source_Id assignment (made when the command router registers each frame at
   -- startup) is preserved so frames remain claimable by future Run_Sequence calls.
   overriding function Kill_All_Sequences (Self : in out Instance) return Command_Execution_Status.E is
      use Command_Execution_Status;
      Time : constant Sys_Time.T := Self.Sys_Time_T_Get;
   begin
      for Frame of Self.Sequence_Frames.all loop
         if Frame.Status /= Not_Running then
            -- If the operator was waiting via Send_After_Sequence_Completion,
            -- Finish_Sequence emits Failure now -- the sequence is being killed
            -- before it could complete and the originating command would
            -- otherwise hang.
            Finish_Sequence (Self, Frame, Command_Response_Status.Failure, Time);
         end if;
      end loop;
      Self.Event_T_Send_If_Connected (Self.Events.Killed_All_Sequences (Time));
      return Success;
   end Kill_All_Sequences;

   -- Halt the sequence running on a single frame and return that frame to an
   -- idle state. Fails if the frame ID is out of range; killing a frame that
   -- is not running has no effect and succeeds. As with Kill_All_Sequences,
   -- the frame's Source_Id assignment is preserved so it remains claimable by
   -- future Run_Sequence calls.
   overriding function Kill_Frame (Self : in out Instance; Arg : in Packed_U16.T) return Command_Execution_Status.E is
      use Command_Execution_Status;
      Frame_Id : constant Interfaces.Unsigned_32 := Interfaces.Unsigned_32 (Arg.Value);
      Time : constant Sys_Time.T := Self.Sys_Time_T_Get;
   begin
      if Frame_Id not in Self.Sequence_Frames.all'Range then
         Self.Event_T_Send_If_Connected (Self.Events.Invalid_Frame_Id (Time, (Value => Frame_Id)));
         return Failure;
      end if;
      declare
         Frame : Sequence_Frame renames Self.Sequence_Frames.all (Frame_Id);
      begin
         if Frame.Status = Not_Running then
            -- Nothing to kill -- note it and succeed.
            Self.Event_T_Send_If_Connected (Self.Events.Frame_Not_Running (Time, (Value => Frame_Id)));
            return Success;
         end if;
         Self.Event_T_Send_If_Connected (Self.Events.Killed_Frame (Time, (Sequence_Id => Frame.Sequence_Id, Frame_Id => Frame_Id)));
         -- If the operator was waiting via Send_After_Sequence_Completion,
         -- Finish_Sequence emits Failure now -- the sequence is being killed
         -- before it could complete and the originating command would
         -- otherwise hang.
         Finish_Sequence (Self, Frame, Command_Response_Status.Failure, Time);
         return Success;
      end;
   end Kill_Frame;

   -- Set the summary packet period, in ticks. Zero disables emission. The
   -- tick counter is reset so the new period starts a fresh phase.
   overriding function Set_Summary_Packet_Period (Self : in out Instance; Arg : in Packed_U16.T) return Command_Execution_Status.E is
      use Command_Execution_Status;
   begin
      Self.Summary_Packet_Period := Arg.Value;
      Self.Summary_Packet_Tick_Count := 0;
      Self.Event_T_Send_If_Connected (Self.Events.Summary_Packet_Period_Set (Self.Sys_Time_T_Get, Arg));
      return Success;
   end Set_Summary_Packet_Period;

   -- Send out the initial values of all data products, seeded from the
   -- component state so the startup defaults are observable:
   overriding procedure Set_Up (Self : in out Instance) is
      Time : constant Sys_Time.T := Self.Sys_Time_T_Get;
   begin
      Send_Frame_Count_Data_Products (Self, Time);
      Self.Data_Product_T_Send_If_Connected (Self.Data_Products.Sequences_Started_Count (Time, (Value => Self.Sequences_Started_Count)));
      Self.Data_Product_T_Send_If_Connected (Self.Data_Products.Sequences_Finished_Count (Time, (Value => Self.Sequences_Finished_Count)));
      Self.Data_Product_T_Send_If_Connected (Self.Data_Products.Sequences_Failed_Count (Time, (Value => Self.Sequences_Failed_Count)));
      Self.Data_Product_T_Send_If_Connected (Self.Data_Products.Commands_Sent_Count (Time, (Value => Self.Commands_Sent_Count)));
      Self.Data_Product_T_Send_If_Connected (Self.Data_Products.Last_Sequence_Started (Time, (Value => 0)));
      Self.Data_Product_T_Send_If_Connected (Self.Data_Products.Last_Sequence_Finished (Time, (Value => 0)));
      Self.Data_Product_T_Send_If_Connected (Self.Data_Products.Last_Sequence_Failed (Time, (Value => 0)));
   end Set_Up;

   -- Invalid command handler. This procedure is called when a command's arguments are found to be invalid:
   overriding procedure Invalid_Command (Self : in out Instance; Cmd : in Command.T; Errant_Field_Number : in Interfaces.Unsigned_32; Errant_Field : in Basic_Types.Poly_Type) is
   begin
      Self.Event_T_Send_If_Connected (Self.Events.Invalid_Command_Received (
         Self.Sys_Time_T_Get,
         (Id => Cmd.Header.Id, Errant_Field_Number => Errant_Field_Number, Errant_Field => Errant_Field)));
   end Invalid_Command;

   overriding procedure Register_Commands (Self : in out Instance; Arg : in Command_Registration_Request.T) is
   begin
      -- Register the statically-modeled commands. The inherited version also
      -- stashes Self.Command_Reg_Id := Arg.Registration_Id for us.
      Component.Simple_Command_Sequencer.Base_Instance (Self).Register_Commands (Arg);

      -- Register one "ghost" command per defined sequence. These aren't in the
      -- model; their IDs continue right after the modeled block
      -- (Command_Id_Base + Num_Commands + I) and line up with the IDs the
      -- assembly reserved for the per-sequence commands (e.g. 22..26).
      for I in 0 .. Self.Sequences.all'Length - 1 loop
         Self.Command_Response_T_Send_If_Connected
         ((Source_Id       => 0,
            Registration_Id => Self.Command_Reg_Id,
            Command_Id      => Self.Command_Id_Base
                              + Command_Types.Command_Id (Simple_Command_Sequencer_Commands.Num_Commands)
                              + Command_Types.Command_Id (I),
            Status          => Command_Response_Status.Register),
            Full_Queue_Behavior => Connector_Types.Drop);
         Os_Sleep.Sleep_Us (Configuration.Command_Registration_Delay);
      end loop;
   end Register_Commands;

end Component.Simple_Command_Sequencer.Implementation;
