--------------------------------------------------------------------------------
-- Simple_Command_Sequencer Component Implementation Body
--------------------------------------------------------------------------------

with Sequence_Enums; use Sequence_Enums.Sequence_State; use Sequence_Enums.Sequence_Response_Behavior;
with Packed_Natural;
with Ada.Real_Time;
with Sys_Time.Arithmetic;
with Command_Types; use Command_Types;
with Packet;
with Packet_Types;
with Sequence_Frame_Summary;

package body Component.Simple_Command_Sequencer.Implementation is

   subtype Sequence_Frame is Simple_Sequencer_Types.Sequence_Frame;
   subtype Sequence_Type is Simple_Sequencer_Types.Sequence_Type;
   subtype Frame_Id_Type is Simple_Sequencer_Types.Frame_Id_Type;

   overriding procedure Init (Self : in out Instance; Config : in Simple_Sequencer_Types.Sequencer_Config) is
   begin
      Self.Sequence_Frames := new Simple_Sequencer_Types.Sequence_Frame_Array (0 .. Frame_Id_Type (Config.Num_Concurrent_Sequences - 1));
      Self.Sequence_Frames.all := [for Id in Self.Sequence_Frames.all'Range => (Frame_Id => Id, others => <>)];
      Self.Sequences := Config.Sequences;
   end Init;

   function Find_Available_Sequence_Frame (Self : in Instance; Frame_Id : out Frame_Id_Type) return Boolean is
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

   -- Frame source ids are unique by design: registration refuses a duplicate
   -- (see Duplicate_Register_Source), so the first match here is the only match.
   function Find_Sequence_Frame_Id_From_Source_Id (Self : in Instance; Source_Id : in Command_Source_Id; Frame_Id : out Frame_Id_Type) return Boolean is
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

   -- Park `Frame` in Waiting_For_Time until `Time` + `Millis`. Returns False,
   -- leaving the frame unchanged, if the wake time overflows Sys_Time; the
   -- duration itself always fits a Time_Span.
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

   -- Emit the deferred reply for a frame claimed with
   -- Send_After_Sequence_Completion. Called from every path that ends a sequence.
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

   -- Recount the running frames and update the frame-count data products.
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

   -- Return `Frame` to idle, update the counters and data products, and emit
   -- any deferred reply. Only Status is reset: the next claim re-seeds the
   -- rest, and while idle it lets the summary packet report the last run.
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

   -- Count a dispatched sub-command.
   procedure Note_Command_Sent (Self : in out Instance; Time : in Sys_Time.T) is
   begin
      Self.Commands_Sent_Count := @ + 1;
      Self.Data_Product_T_Send_If_Connected (Self.Data_Products.Commands_Sent_Count (Time, (Value => Self.Commands_Sent_Count)));
   end Note_Command_Sent;

   -- Send a step's sub-command. If the sequence waits on responses, stamp the
   -- deadline and pending command id and park the frame first. A deadline that
   -- overflows Sys_Time ends the sequence instead; the frame would otherwise
   -- wait forever.
   procedure Dispatch_Step_Command (Self : in out Instance; Frame : in out Sequence_Frame; Seq : in Sequence_Type; Cmd : in Command.T; Time : in Sys_Time.T) is
      use Sys_Time.Arithmetic;
      Add_Status : Sys_Time_Status;
   begin
      if Seq.Wait_For_Cmd_Resp then
         Add_Status := Add (Time, Seq.Command_Timeout, Frame.Timeout_Deadline);
         if Add_Status /= Success then
            Self.Event_T_Send_If_Connected (Self.Events.Sequence_Out_Of_Range_Timeout (Time, (Sequence_Id => Frame.Sequence_Id, Frame_Id => Frame.Frame_Id, Step => Frame.Step)));
            Finish_Sequence (Self, Frame, Command_Response_Status.Failure, Time);
            return;
         end if;
         Frame.Pending_Command_Id := Cmd.Header.Id;
         Frame.Status := Waiting_For_Cmd_Resp;
      end if;
      Self.Command_T_Send (Cmd);
      Note_Command_Sent (Self, Time);
   end Dispatch_Step_Command;

   -- Run `Frame` until it parks (command response or sleep) or its sequence ends.
   -- `Time` stamps every event, deadline, and wake time in the run; the
   -- staleness across a no-wait step chain is immaterial next to the
   -- second-scale timeouts and sleeps.
   procedure Execute_Sequence (Self : in out Instance; Frame : in out Sequence_Frame; Time : in Sys_Time.T) is
      use Simple_Sequencer_Types;
      Seq : Sequence_Type renames Self.Sequences.all (Frame.Sequence_Id);
   begin
      while Frame.Status = Running loop
         if Frame.Step > Seq.Steps.all'Last then
            Self.Event_T_Send_If_Connected (Self.Events.Sequence_Completed (Time, (Sequence_Id => Frame.Sequence_Id, Frame_Id => Frame.Frame_Id)));
            Finish_Sequence (Self, Frame, Command_Response_Status.Success, Time);
         else
            declare
               Step_Obj : Step renames Seq.Steps.all (Frame.Step);
            begin
               case Step_Obj.Kind is
                  when Command_Step =>
                     Dispatch_Step_Command (Self, Frame, Seq,
                        (Header => (Source_Id => Frame.Source_Id, Id => Step_Obj.Id, Arg_Buffer_Length => Step_Obj.Arg_Length), Arg_Buffer => Step_Obj.Arg), Time);
                  when Runtime_Argument_Command_Step =>
                     -- The step's Resolver validates the sequence argument and
                     -- extracts this sub-command's argument from it.
                     declare
                        Resolved : Command_Types.Command_Arg_Buffer_Type;
                        Valid : constant Boolean := Step_Obj.Resolver (Frame.Dynamic_Arg, Resolved);
                     begin
                        if Valid then
                           Dispatch_Step_Command (Self, Frame, Seq,
                              (Header => (Source_Id => Frame.Source_Id, Id => Step_Obj.Id, Arg_Buffer_Length => Step_Obj.Arg_Length), Arg_Buffer => Resolved), Time);
                        else
                           Self.Event_T_Send_If_Connected (Self.Events.Invalid_Dynamic_Command_Argument (Time, (Sequence_Id => Frame.Sequence_Id, Frame_Id => Frame.Frame_Id, Step => Frame.Step, Command_Id => Step_Obj.Id)));
                           Finish_Sequence (Self, Frame, Command_Response_Status.Failure, Time);
                        end if;
                     end;
                  when Simple_Sequencer_Types.Sleep =>
                     -- Static sleeps are model-bounded, so only Sys_Time overflow
                     -- of the wake time can fail. End the sequence rather than
                     -- leave the frame parked on a stale wake time.
                     if not Try_Schedule_Sleep (Frame, Step_Obj.Sleep_Arg, Time) then
                        Finish_Sequence (Self, Frame, Command_Response_Status.Failure, Time);
                        Self.Event_T_Send_If_Connected (Self.Events.Sequence_Out_Of_Range_Sleep (Time, (Sequence_Id => Frame.Sequence_Id, Frame_Id => Frame.Frame_Id, Milliseconds => Step_Obj.Sleep_Arg)));
                     end if;
                  when Runtime_Sleep =>
                     -- The step's Resolver validates the sequence argument and
                     -- extracts the duration as a Packed_Natural, so it always
                     -- fits a Time_Span. As above, only wake-time overflow remains.
                     declare
                        Resolved : Command_Types.Command_Arg_Buffer_Type;
                        Valid : constant Boolean := Step_Obj.Sleep_Resolver (Frame.Dynamic_Arg, Resolved);
                     begin
                        if Valid then
                           declare
                              Millis : constant Natural :=
                                 Packed_Natural.Serialization.From_Byte_Array (Resolved (Resolved'First .. Resolved'First + Packed_Natural.Serialization.Serialized_Length - 1)).Value;
                           begin
                              if not Try_Schedule_Sleep (Frame, Millis, Time) then
                                 Finish_Sequence (Self, Frame, Command_Response_Status.Failure, Time);
                                 Self.Event_T_Send_If_Connected (Self.Events.Sequence_Out_Of_Range_Sleep (Time, (Sequence_Id => Frame.Sequence_Id, Frame_Id => Frame.Frame_Id, Milliseconds => Millis)));
                              end if;
                           end;
                        else
                           Finish_Sequence (Self, Frame, Command_Response_Status.Failure, Time);
                           Self.Event_T_Send_If_Connected (Self.Events.Invalid_Dynamic_Sleep_Argument (Time, (Sequence_Id => Frame.Sequence_Id, Frame_Id => Frame.Frame_Id, Step => Frame.Step)));
                        end if;
                     end;
               end case;
               -- A parked frame keeps pointing at its step; the resume paths
               -- advance it. A frame that ended stays on the step it ended on.
               if Frame.Status = Running then
                  Frame.Step := Frame.Step + 1;
               end if;
            end;
         end if;
      end loop;
   end Execute_Sequence;

   -- Execute a modeled command and reply, as the autocoded Command_T_Recv_Async
   -- does, unless Run_Sequence deferred the reply to the sequence-end paths.
   procedure Execute_Command_And_Respond (Self : in out Instance; Arg : in Command.T) is
      Stat : constant Command_Response_Status.E := Self.Execute_Command (Arg);
   begin
      if not Self.Caller.Defer_Command_Response then
         Self.Command_Response_T_Send_If_Connected ((Source_Id => Arg.Header.Source_Id, Registration_Id => Self.Command_Reg_Id, Command_Id => Arg.Header.Id, Status => Stat));
      end if;
   end Execute_Command_And_Respond;

   -- Sequence commands are received on this connector
   overriding procedure Command_T_Recv_Async (Self : in out Instance; Arg : in Command.T) is
      -- The per-sequence "ghost" commands occupy the id block right after the
      -- modeled commands. They are absent from this component's command model,
      -- so intercept them here and translate to Run_Sequence rather than let
      -- Execute_Command reject them.
      First_Ghost_Id : constant Command_Types.Command_Id :=
         Self.Command_Id_Base + Command_Types.Command_Id (Simple_Command_Sequencer_Commands.Num_Commands);
      Num_Ghosts : constant Command_Types.Command_Id :=
         Command_Types.Command_Id (Self.Sequences.all'Length);
   begin
      -- Stash the caller's response context for Run_Sequence. The active
      -- component's serial queue dispatches one command at a time, so this
      -- cannot be clobbered. Defer is reset so it cannot leak between commands.
      Self.Caller := (Source_Id => Arg.Header.Source_Id, Command_Id => Arg.Header.Id, Defer_Command_Response => False);

      if Arg.Header.Id >= First_Ghost_Id and then Arg.Header.Id < First_Ghost_Id + Num_Ghosts then
         -- The argument buffer carries the sequence's native argument verbatim.
         declare
            Seq_Index : constant Interfaces.Unsigned_16 := Interfaces.Unsigned_16 (Arg.Header.Id - First_Ghost_Id);
            Native_Len : constant Natural := Natural (Arg.Header.Arg_Buffer_Length);
         begin
            if Native_Len > Natural (Simple_Sequencer_Types.Run_Sequence_Arg_Buffer_Length_Type'Last) then
               -- Too long for the passthrough buffer; no sequence argument type is.
               Self.Event_T_Send_If_Connected (Self.Events.Invalid_Sequence_Argument_Length (Self.Sys_Time_T_Get,
                  (Sequence_Id => Seq_Index, Received_Length => Native_Len, Expected_Length => Self.Sequences.all (Seq_Index).Arg_Length)));
               Self.Command_Response_T_Send_If_Connected ((Source_Id => Arg.Header.Source_Id, Registration_Id => Self.Command_Reg_Id, Command_Id => Arg.Header.Id, Status => Command_Response_Status.Failure));
            else
               declare
                  Exec_Stat : constant Command_Execution_Status.E := Self.Run_Sequence (
                     (Sequence_Id => Seq_Index,
                      Arg_Length => Simple_Sequencer_Types.Run_Sequence_Arg_Buffer_Length_Type (Native_Len),
                      Buffer_Arg => Arg.Arg_Buffer (Arg.Arg_Buffer'First .. Arg.Arg_Buffer'First + Simple_Sequencer_Types.Run_Sequence_Buffer_Type'Length - 1)));
               begin
                  -- A deferred reply is emitted by the sequence-end paths instead.
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
            end if;
         end;
      else
         Execute_Command_And_Respond (Self, Arg);
      end if;
   end Command_T_Recv_Async;

   -- A Register_Source response assigns a source id to the next unassigned
   -- frame. Any other response is a sub-command result: find the owning frame
   -- by source id and advance or abort it.
   overriding procedure Command_Response_T_Recv_Async (Self : in out Instance; Arg : in Command_Response.T) is
      use Command_Response_Status;
      Time : constant Sys_Time.T := Self.Sys_Time_T_Get;
   begin
      if Arg.Status = Command_Response_Status.Register_Source then
         declare
            Source_Id_Set : Boolean := False;
         begin
            -- Refuse a source id some frame already holds. Responses are routed
            -- to frames by source id alone, so a duplicate would leave one frame
            -- shadowed: its responses would resolve to the other frame and it
            -- would only ever time out. Refusing registration keeps every
            -- assigned id unique, making that routing unambiguous.
            for Frame of Self.Sequence_Frames.all loop
               if Frame.Has_Source_Id and then Frame.Source_Id = Arg.Source_Id then
                  Self.Event_T_Send_If_Connected (Self.Events.Duplicate_Register_Source (Time, Arg));
                  return;
               end if;
            end loop;

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
            Frame_To_Wake_Id : Frame_Id_Type;
         begin
            if Find_Sequence_Frame_Id_From_Source_Id (Self, Arg.Source_Id, Frame_To_Wake_Id) then
               declare
                  Frame : Sequence_Frame renames Self.Sequence_Frames.all (Frame_To_Wake_Id);
                  Seq : Sequence_Type renames Self.Sequences.all (Frame.Sequence_Id);
               begin
                  -- Only the response the frame is parked on advances it. Anything
                  -- else is late or stale (timed out, killed, or the frame was
                  -- reused) and is ignored: acting on it would advance the wrong step.
                  if Frame.Status = Waiting_For_Cmd_Resp and then Arg.Command_Id = Frame.Pending_Command_Id then
                     declare
                        Failed : constant Boolean := Arg.Status /= Command_Response_Status.Success;
                     begin
                        if Failed then
                           -- Any non-success status is a failed sub-command: Failure, Id_Error,
                           -- Validation_Error, Length_Error, or Dropped.
                           Self.Event_T_Send_If_Connected (Self.Events.Command_Failure (Time,
                              (Sequence_Id => Frame.Sequence_Id, Frame_Id => Frame_To_Wake_Id,
                               Step => Frame.Step, Command_Id => Arg.Command_Id)));
                        end if;

                        if Failed and then Seq.Abort_On_Failed_Cmd then
                           Finish_Sequence (Self, Frame, Command_Response_Status.Failure, Time);
                           Self.Event_T_Send_If_Connected (Self.Events.Sequence_Aborted (Time,
                              (Sequence_Id => Frame.Sequence_Id, Frame_Id => Frame_To_Wake_Id,
                               Step => Frame.Step)));
                        else
                           Frame.Step := @ + 1;
                           Frame.Status := Running;
                           Execute_Sequence (Self, Frame, Time);
                        end if;
                     end;
                  end if;
               end;
            else
               -- Unknown source id: usually a routing or registration bug.
               Self.Event_T_Send_If_Connected (Self.Events.Unexpected_Command_Response (Time, Arg));
            end if;
         end;
      end if;
   end Command_Response_T_Recv_Async;

   -- Emit the summary packet if a period is set and enough ticks have elapsed.
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

      -- One Sequence_Frame_Summary per frame, in frame order. The packet's
      -- ground type is generated per sequences suite; the FSW just fills the
      -- buffer with the Sequence_Frame_Summary serializer.
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
                  Frame.Step := @ + 1;
                  Frame.Status := Running;
                  Execute_Sequence (Self, Frame, Time);
               end if;
            when Waiting_For_Cmd_Resp =>
               -- The deadline was stamped at dispatch; only the comparison happens here.
               if Time >= Frame.Timeout_Deadline then
                  Finish_Sequence (Self, Frame, Command_Response_Status.Failure, Time);
                  Self.Event_T_Send_If_Connected (Self.Events.Sequence_Timeout (Time, (Sequence_Id => Frame.Sequence_Id, Frame_Id => Frame.Frame_Id, Step => Frame.Step)));
               end if;
            when Not_Running =>
               null;
            when Running =>
               -- Every path that sets Running executes the sequence to a parked
               -- or idle state before returning.
               pragma Assert (False, "Sequence frame found Running at tick, which should not be possible.");
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
      -- The waiting frame is not aborted here; its response timeout will end it.
      Self.Event_T_Send_If_Connected (Self.Events.Dropped_Command_Response (Self.Sys_Time_T_Get, Arg));
   end Command_Response_T_Recv_Async_Dropped;

   -- This procedure is called when a Tick_T_Recv_Async message is dropped due to a full queue.
   overriding procedure Tick_T_Recv_Async_Dropped (Self : in out Instance; Arg : in Tick.T) is
   begin
      Self.Event_T_Send_If_Connected (Self.Events.Dropped_Tick (Self.Sys_Time_T_Get, Arg));
   end Tick_T_Recv_Async_Dropped;

   -- Validate the request, claim a free frame, seed it, and start executing.
   overriding function Run_Sequence (Self : in out Instance; Arg : in Run_Sequence_Arg.T) return Command_Execution_Status.E is
      use Command_Execution_Status;
      Time : constant Sys_Time.T := Self.Sys_Time_T_Get;
      Available_Id : Frame_Id_Type;
   begin
      if Arg.Sequence_Id not in Self.Sequences.all'Range then
         Self.Event_T_Send_If_Connected (Self.Events.Invalid_Sequence_Id (Time, (Value => Arg.Sequence_Id)));
         return Failure;
      end if;

      declare
         Seq : Sequence_Type renames Self.Sequences.all (Arg.Sequence_Id);
      begin
         if Arg.Arg_Length /= Seq.Arg_Length then
            Self.Event_T_Send_If_Connected (Self.Events.Invalid_Sequence_Argument_Length (Time,
               (Sequence_Id => Arg.Sequence_Id, Received_Length => Arg.Arg_Length, Expected_Length => Seq.Arg_Length)));
            return Failure;
         end if;
         if not Find_Available_Sequence_Frame (Self, Available_Id) then
            Self.Event_T_Send_If_Connected (Self.Events.No_Frame_Available (Time));
            return Failure;
         end if;

         declare
            Frame : Sequence_Frame renames Self.Sequence_Frames.all (Available_Id);
         begin
            -- Seed every per-run field; nothing may rely on state left from a
            -- previous run. Frame_Id and the registered source id are kept.
            Frame := (Frame_Id            => Frame.Frame_Id,
                      Source_Id           => Frame.Source_Id,
                      Has_Source_Id       => Frame.Has_Source_Id,
                      Sequence_Id         => Arg.Sequence_Id,
                      Step                => 0,
                      Status              => Running,
                      Wait_Until          => (0, 0),
                      Timeout_Deadline    => (0, 0),
                      Pending_Command_Id  => 0,
                      Response_Behavior   => Seq.Response_Behavior,
                      Operator_Source_Id  => Self.Caller.Source_Id,
                      Operator_Command_Id => Self.Caller.Command_Id,
                      Dynamic_Arg         => Arg.Buffer_Arg);
            -- Command_T_Recv_Async then suppresses its immediate reply; the
            -- sequence-end paths send it instead.
            if Seq.Response_Behavior = Send_After_Sequence_Completion then
               Self.Caller.Defer_Command_Response := True;
            end if;
            Self.Event_T_Send_If_Connected (Self.Events.Sequence_Started (Time, (Sequence_Id => Arg.Sequence_Id, Frame_Id => Available_Id)));
            Self.Sequences_Started_Count := @ + 1;
            Self.Data_Product_T_Send_If_Connected (Self.Data_Products.Sequences_Started_Count (Time, (Value => Self.Sequences_Started_Count)));
            Self.Data_Product_T_Send_If_Connected (Self.Data_Products.Last_Sequence_Started (Time, (Value => Arg.Sequence_Id)));
            Send_Frame_Count_Data_Products (Self, Time);
            -- Run until the frame parks or the sequence completes. Ticks only
            -- resume parked frames.
            Execute_Sequence (Self, Frame, Time);
            return Success;
         end;
      end;
   end Run_Sequence;

   -- Halt every running sequence. Registered source ids are kept, so the frames
   -- remain claimable.
   overriding function Kill_All_Sequences (Self : in out Instance) return Command_Execution_Status.E is
      use Command_Execution_Status;
      Time : constant Sys_Time.T := Self.Sys_Time_T_Get;
   begin
      for Frame of Self.Sequence_Frames.all loop
         if Frame.Status /= Not_Running then
            -- Any deferred reply is sent now with Failure, so the operator's
            -- command does not hang.
            Finish_Sequence (Self, Frame, Command_Response_Status.Failure, Time);
         end if;
      end loop;
      Self.Event_T_Send_If_Connected (Self.Events.Killed_All_Sequences (Time));
      return Success;
   end Kill_All_Sequences;

   -- Halt the sequence on one frame. Fails for an out of range frame id;
   -- killing an idle frame is a no-op that succeeds.
   overriding function Kill_Frame (Self : in out Instance; Arg : in Packed_U16.T) return Command_Execution_Status.E is
      use Command_Execution_Status;
      Frame_Id : constant Interfaces.Unsigned_16 := Arg.Value;
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
            Self.Event_T_Send_If_Connected (Self.Events.Frame_Not_Running (Time, (Value => Frame_Id)));
            return Success;
         end if;
         -- Any deferred reply is sent now with Failure, so the operator's
         -- command does not hang.
         Finish_Sequence (Self, Frame, Command_Response_Status.Failure, Time);
         Self.Event_T_Send_If_Connected (Self.Events.Killed_Frame (Time, (Sequence_Id => Frame.Sequence_Id, Frame_Id => Frame_Id)));
         return Success;
      end;
   end Kill_Frame;

   -- Set the summary packet period, in ticks. Zero disables emission.
   overriding function Set_Summary_Packet_Period (Self : in out Instance; Arg : in Packed_U16.T) return Command_Execution_Status.E is
      use Command_Execution_Status;
   begin
      Self.Summary_Packet_Period := Arg.Value;
      Self.Summary_Packet_Tick_Count := 0;
      Self.Event_T_Send_If_Connected (Self.Events.Summary_Packet_Period_Set (Self.Sys_Time_T_Get, Arg));
      return Success;
   end Set_Summary_Packet_Period;

   -- Send out the initial values of all data products:
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
      -- The inherited version also stores Self.Command_Reg_Id.
      Component.Simple_Command_Sequencer.Base_Instance (Self).Register_Commands (Arg);

      -- Register one "ghost" command per sequence. Their ids continue right
      -- after the modeled block, matching the ids the assembly reserved.
      for I in 0 .. Self.Sequences.all'Length - 1 loop
         Self.Register_Command (Command_Types.Command_Id (Simple_Command_Sequencer_Commands.Num_Commands) + Command_Types.Command_Id (I));
      end loop;
   end Register_Commands;

end Component.Simple_Command_Sequencer.Implementation;
