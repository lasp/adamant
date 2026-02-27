--------------------------------------------------------------------------------
-- Command_Sequencer Component Implementation Body
--------------------------------------------------------------------------------

with Command_Enums; use Command_Enums;
with Interfaces; use Interfaces;
with Command_Types; use Command_Types;
with Safe_Deallocator;
with Command_Sequencer_Enums;
with Telemetry_Record;
with Engine_Summary_Type;
with Engine_Details_Type;
with Engine_Error_Type;
with Sequence_Details_Type;
with Packet_Types;
with Crc_16;
with Sequence_Header;
with Sequence_Util;
with Sequence_Types;
with Seq_Enums; use Seq_Enums;
with Data_Product_Types;
with Data_Product_Enums;
with Data_Product;
with Byte_Array_Util;
with Sys_Time.Arithmetic;
with Seq_Print;
with Seq_Print_Event_Record;
with Packed_U32;

package body Component.Command_Sequencer.Implementation is

   use Seq_Types;
   use Seq_Engine_State;
   use Seq_Execute_State;
   use Seq_Error;

   --------------------------------------------------
   -- Subprogram for implementation init method:
   --------------------------------------------------
   -- The initialization subprogram creates a sequencer with the desired number of engines and internal stack sizes.
   --
   -- Init Parameters:
   -- Num_Engines : Seq_Types.Num_Engines_Type - The number of engines allocated in the sequencer. This determines the number of sequences the component can run in parallel.
   -- Stack_Size : Seq_Types.Stack_Depth_Type - The size of the stack allocated for each engine in entries. Each stack entry contains a single running sequence, and additional stack entries can be used for subsequence calls. A value of 5 here would allow a sequence to call subsequences up to 5 levels deep.
   -- Create_Sequence_Load_Command_Function : Create_Sequence_Load_Command_Access - When a sequence loads or spans or calls another sequence, the command sequencer will call this function to formulate the correct sequence load command for the assembly. Since the specifics of sequence loading often varies on a mission by mission basis, this function allows the encoding of that mission specific behavior by the user.
   -- Packet_Period : Interfaces.Unsigned_16 - The initial packet rate for the sequencer summary packet in ticks. A value of 0 disabled the packet.
   -- Continue_On_Command_Failure : Boolean - If set to True, then the sequence engines will continue to execute even if a sent command fails. If set to False, then the engines will halt with an error status if a sent command fails.
   -- Timeout_Limit : Natural - The number of ticks to wait before timing out sequencer operations such as waiting on a command response or subsequence load. If a timeout of this type occurs the engine will transition to an error state. A value of zero disables these timeouts.
   -- Instruction_Limit : Positive - The maximum number of sequence instructions we allow the sequence to execute without hitting a pausing action such as sending a command, waiting on telemetry, or waiting for a relative or absolute time. The purpose of this parameter is to prevent a sequence from entering an infinite execution loop which would cause the entire component task to hang indefinitely. You should set the value to some maximum number of instructions that you never expect any of your compiled sequences to hit.
   --
   overriding procedure Init (Self : in out Instance; Num_Engines : in Seq_Types.Num_Engines_Type; Stack_Size : in Seq_Types.Stack_Depth_Type; Create_Sequence_Load_Command_Function : in not null Create_Sequence_Load_Command_Access; Packet_Period : in Interfaces.Unsigned_16; Continue_On_Command_Failure : in Boolean; Timeout_Limit : in Natural; Instruction_Limit : in Positive) is
   begin
      -- Allocate an array of engines and timeout counters:
      pragma Assert (Num_Engines < 255, "255 means 'any' engine and thus we cannot create a 255th engine.");
      Self.Seq_Engines := new Seq_Engine_Array (Seq_Types.Sequence_Engine_Id'First .. Seq_Types.Sequence_Engine_Id'First + Num_Engines - 1);
      Self.Engine_Aux_Data := new Engine_Aux_Data_Array (Seq_Types.Sequence_Engine_Id'First .. Seq_Types.Sequence_Engine_Id'First + Num_Engines - 1);

      -- Initialize each engine and counter:
      for Id in Self.Seq_Engines.all'Range loop
         -- Provide engine its stack size and engine identifier:
         Self.Seq_Engines.all (Id).Initialize (Stack_Depth => Stack_Size, Engine_Id => Id);
         -- Reset the engine aux data to startup values:
         Self.Engine_Aux_Data.all (Id) := (Timeout_Counter => 0, Command_Error_Counter => 0);
      end loop;

      -- Set the instruction limit and timeout limit and continue flag:
      Self.Instruction_Limit := Instruction_Limit;
      Self.Timeout_Limit := Timeout_Limit;
      Self.Continue_On_Command_Failure := Continue_On_Command_Failure;

      -- Set packet period:
      Self.Packet_Period := Packet_Period;

      -- Set the create sequence load function:
      Self.Create_Sequence_Load_Command_Function := Create_Sequence_Load_Command_Function;

      -- Packet size assertions - make sure that we can fit all the data for the engines/sequence stack slots into
      -- the packets.
      pragma Assert (
         Self.Seq_Engines.all'Length * Engine_Summary_Type.Size_In_Bytes <= Packet_Types.Packet_Buffer_Length_Type'Last,
         "Too many engines to report summary packet!"
      );
      pragma Assert (
         Engine_Details_Type.Size_In_Bytes + Natural (Stack_Size) * Sequence_Details_Type.Size_In_Bytes <= Packet_Types.Packet_Buffer_Length_Type'Last,
         "Too many engines to report details packet!"
      );
   end Init;

   not overriding procedure Final (Self : in out Instance) is
      procedure Free_Engines_If_Testing is new Safe_Deallocator.Deallocate_If_Testing (
         Object => Seq_Engine_Array,
         Name => Seq_Engine_Array_Access
      );
      procedure Free_Aux_Data_If_Testing is new Safe_Deallocator.Deallocate_If_Testing (
         Object => Engine_Aux_Data_Array,
         Name => Engine_Aux_Data_Array_Access
      );
   begin
      -- First destroy all the engines
      for Engine of Self.Seq_Engines.all loop
         Engine.Destroy;
      end loop;

      -- Now free the dynamically allocated arrays:
      Free_Engines_If_Testing (Self.Seq_Engines);
      Free_Aux_Data_If_Testing (Self.Engine_Aux_Data);

      -- Set to null to be sure:
      Self.Engine_Aux_Data := null;
      Self.Seq_Engines := null;
   end Final;

   -- Send out packet period data product:
   overriding procedure Set_Up (Self : in out Instance) is
   begin
      -- Update period data product:
      Self.Data_Product_T_Send_If_Connected (Self.Data_Products.Summary_Packet_Period (Self.Sys_Time_T_Get, (Value => Self.Packet_Period)));
   end Set_Up;

   ---------------------------------------
   -- Helper functions:
   ---------------------------------------

   -- Retrieve an engine id with the given command source identifier. The return value is True if
   -- an engine with the command source ID is found, otherwise it is False.
   function Get_Engine_With_Source_Id (Self : in Instance; Source_Id : in Command_Source_Id; Engine_Id : out Seq_Types.Sequence_Engine_Id) return Boolean is
      use Seq;
   begin
      -- Initialize the out parameter to something valid:
      Engine_Id := Self.Seq_Engines.all'First;

      -- Go through each engine looking for a matching source id:
      for Id in Self.Seq_Engines.all'Range loop
         -- If we found the source ID then return it:
         if Self.Seq_Engines.all (Id).Get_Engine_State /= Uninitialized and then
             Source_Id = Self.Seq_Engines.all (Id).Get_Source_Id
         then
            Engine_Id := Id;
            return True;
         end if;
      end loop;

      -- We did not find a matching source id, return false:
      return False;
   end Get_Engine_With_Source_Id;

   -- Returns true if the engine ID is in range of the allocated engines and False otherwise
   function Is_Engine_Id_Valid (Self : in Instance; Engine_Id : in Seq_Types.Sequence_Engine_Id) return Boolean
      is (Engine_Id in Self.Seq_Engines.all'Range) with Inline => True;

   -- Returns True if an engine is available for loading and running a new sequence, False otherwise.
   function Is_Engine_Available (Engine : in Seq.Engine) return Boolean is
      use Seq;
   begin
      case Engine.Get_Engine_State is
         when Inactive | Engine_Error =>
            return True;
         when Uninitialized | Reserved | Waiting | Active =>
            return False;
      end case;
   end Is_Engine_Available;

   -- Find an unused engine. False is returned if no engine is available.
   function Find_Available_Engine (Self : in Instance; Engine_Id : out Seq_Types.Sequence_Engine_Id) return Boolean is
      use Seq;
   begin
      -- Initialize the out parameter:
      Engine_Id := Seq_Types.Sequence_Engine_Id'First;

      -- Finds an inactive engine to run a new sequence on. Then tells that engine to execute, which should trigger a block or complete the sequence.
      for An_Engine of Self.Seq_Engines.all loop
         if Is_Engine_Available (An_Engine) then
            Engine_Id := An_Engine.Get_Engine_Id;
            return True;
         end if;
      end loop;

      return False;
   end Find_Available_Engine;

   -- Form an error record for a particular engine:
   function Get_Error_Report (Engine : in Seq.Engine) return Engine_Error_Type.T is
   begin
      return (
         Engine_Id => Engine.Get_Engine_Id,
         Sequence_Id => Engine.Get_Lowest_Child_Id,
         Engine_State => Engine.Get_Engine_State,
         Sequence_State => Engine.Get_Running_Sequence_State,
         Stack_Level => Engine.Get_Stack_Level,
         Program_Counter => Engine.Get_Lowest_Child_Position,
         Error_Type => Engine.Get_Seq_Error_Code,
         Errant_Field_Number => Engine.Get_Errant_Field_Number
      );
   end Get_Error_Report;

   -- Grab telemetry for an engine and set it within the engine. If this operation was successful then
   -- True is returned, else False is returned. False is only returned when the system is unable to fetch
   -- telemetry due to an invalid ID or unable to extract telemetry due to a length mismatch. In all other
   -- cases the fetch is considered nominal, and the engine can be safely executed after the call to this
   -- function.
   type Get_Telemetry_Status is (Success, Not_Available, Error);
   function Get_Telemetry_For_Engine (Self : in out Instance; Engine : in out Seq.Engine) return Get_Telemetry_Status is
      use Data_Product_Types;
      use Data_Product_Enums.Fetch_Status;
      use Sys_Time.Arithmetic;
      use Byte_Array_Util;

      function Get_And_Validate_Telemetry_Request (Telem_Record : out Telemetry_Record.T; Is_Signed : out Boolean)   return Boolean is
         use Seq_Enums.Seq_Data_Format;
         -- Get the telemetry record from the engine that tells us which data product
         -- we need to fetch.
         To_Return : constant Telemetry_Record.T := Engine.Get_Telemetry_Request;
      begin
         -- Set the out parameters:
         Telem_Record := To_Return;
         Is_Signed := False;

         -- Validate the telemetry record. We can only handle certain telemetry types:
         case To_Return.Tlm_Type is
            -- We do not support little endian telemetry values yet.
            -- NOTE: that this code is not reachable during unit test since the current version of the
            -- Adamant sequence compiler cannot produce instructions with little-endian telemetry types:
            when Unsigned_Word_Le | Unsigned_Long_Le | Signed_Word_Le | Signed_Long_Le | Float_Le =>
               return False;
            -- We need to know if the telemetry is signed, since bit representation must be
            -- handled differently (two's complement) than with other types.
            when Signed_Byte | Signed_Word_Be | Signed_Long_Be =>
               Is_Signed := True;
               return True;
            when Unsigned_Byte | Unsigned_Word_Be | Unsigned_Long_Be | Float_Be =>
               return True; -- Nothing to do
         end case;
      end Get_And_Validate_Telemetry_Request;

      -- Get the telemetry request from the sequence engine:
      Telem_Record : Telemetry_Record.T;
      Telem_Signed : Boolean := False;
      Telem_Valid : constant Boolean := Get_And_Validate_Telemetry_Request (Telem_Record, Telem_Signed);
   begin
      -- Check the telemetry request status:
      if not Telem_Valid then
         -- NOTE: that this code is not reachable during unit test since the current version of the
         -- Adamant sequence compiler cannot produce instructions with little-endian telemetry types:
         Engine.Set_Engine_Error (Telemetry_Fail);
         Self.Event_T_Send_If_Connected (Self.Events.Unhandled_Telemetry_Type (Self.Sys_Time_T_Get, Get_Error_Report (Engine)));
         return Error;
      end if;

      declare
         -- Fetch the data product:
         Dp_Return : constant Data_Product_Return.T := Self.Data_Product_Fetch_T_Request ((Id => Data_Product_Id (Telem_Record.Id)));
         Product : Data_Product.T renames Dp_Return.The_Data_Product;
         Telemetry_Value : Basic_Types.Poly_32_Type;
         Status : Byte_Array_Util.Extract_Poly_Type_Status;
         Do_Set_Telemetry : Boolean := True;
      begin
         -- Check the data product return status:
         case Dp_Return.The_Status is
            when Success =>
               -- If new value required, need to compare timestamp of data product against the time
               -- when we started requesting this data product.
               if Telem_Record.New_Value_Required then
                  -- If the data product is newer (larger) than the start time of the telemetry wait
                  -- then it is a new data product and we can start using it for comparison. Otherwise
                  -- we need to execute without parsing the data in the data product.
                  if Product.Header.Time < Engine.Get_Sequence_Telemetry_Wait_Start_Time (Engine.Get_Stack_Level) then
                     -- The data product is not new, so we are not going to set a new telemetry point in the
                     -- engine. If this condition persists, eventually the sequence will timeout on the wait
                     -- telemetry condition.
                     Do_Set_Telemetry := False;
                  end if;
               end if;

               if Do_Set_Telemetry then
                  -- Extract the data from the data product as a poly type in big endian.
                  Status := Byte_Array_Util.Extract_Poly_Type (
                     Src => Product.Buffer (
                        Product.Buffer'First ..
                        Product.Buffer'First + Product.Header.Buffer_Length - 1
                     ),
                     Offset => Natural (Telem_Record.Offset),
                     Size => Telem_Record.Size,
                     Is_Signed => Telem_Signed,
                     Value => Telemetry_Value
                  );

                  -- Check the status of the extraction:
                  case Status is
                     when Success =>
                        -- Set the telemetry in the sequence engine.
                        Engine.Set_Telemetry (Telemetry_Value);
                     when Error =>
                        -- Telemetry extraction failed. This is not good, go to error state.
                        Engine.Set_Engine_Error (Telemetry_Fail);
                        Self.Event_T_Send_If_Connected (Self.Events.Data_Product_Extraction_Error (Self.Sys_Time_T_Get, Get_Error_Report (Engine)));
                        return Error;
                  end case;
               end if;

               -- If we got here then all is good. We need to execute the engine.
               return Success;
            when Not_Available =>
               -- No telemetry available yet. Return True, since this is not an error, we will just execute the
               -- engine to progress the timeout:
               return Not_Available;
            when Id_Out_Of_Range =>
               -- This is not good. The sequence got compiled with a data product ID that does not
               -- exist.
               Engine.Set_Engine_Error (Telemetry_Fail);
               Self.Event_T_Send_If_Connected (Self.Events.Data_Product_Id_Out_Of_Range_Error (Self.Sys_Time_T_Get, Get_Error_Report (Engine)));
               return Error;
         end case;
      end;
   end Get_Telemetry_For_Engine;

   -- Increment the timeout for a particular engine.
   -- Returns false if the timeout has expired.
   function Increment_Timeout (Self : in out Instance; Engine : in Seq.Engine) return Boolean is
      Timeout_Counter : Natural renames Self.Engine_Aux_Data.all (Engine.Get_Engine_Id).Timeout_Counter;
   begin
      -- Only increment the timeout if we have a non-zero timeout limit to compare against.
      if Self.Timeout_Limit /= Natural'First then
         -- Increment up to maximum.
         if Timeout_Counter < Natural'Last then
            Timeout_Counter := @ + 1;
         end if;

         -- Check against limit:
         if Timeout_Counter >= Self.Timeout_Limit then
            return False;
         end if;
      end if;

      return True;
   end Increment_Timeout;

   -- Reset a timeout counter for a particular engine.
   procedure Reset_Timeout (Self : in out Instance; Engine : in Seq.Engine) is
      Timeout_Counter : Natural renames Self.Engine_Aux_Data.all (Engine.Get_Engine_Id).Timeout_Counter;
   begin
      Timeout_Counter := Natural'First;
   end Reset_Timeout;

   -- Takes one single engine and runs it until it is blocked or finished.
   procedure Execute_Engine (Self : in out Instance; Engine : in out Seq.Engine; Recursion_Depth : in Natural := 0) with
      -- Execute engine should never be called on a non-active engine.
      Pre => (Engine.Get_Engine_State /= Seq_Engine_State.Uninitialized)
   is
      procedure Send_Engine_Command (Cmd : in Command.T) is
         -- Create a copy of the command on the stack:
         To_Send : Command.T := Cmd;
      begin
         -- Modify the command header to have the correct source id for the engine
         -- that is sending it:
         To_Send.Header.Source_Id := Engine.Get_Source_Id;

         -- Send out the command
         Self.Command_T_Send_If_Connected (To_Send);
      end Send_Engine_Command;

      -- Helper function which creates a sequence load command for a subsequence load or spawn:
      procedure Send_Sequence_Load_Command (
         Sequence_Id : in Sequence_Types.Sequence_Id;
         Destination_Engine_Id : in Seq_Types.Sequence_Engine_Id
      ) is
         use Command_Sequencer_Enums;
         use Command_Sequencer_Enums.Sequence_Load_Engine_Request_Type;
      begin
         -- Build and send the command:
         Send_Engine_Command (Self.Create_Sequence_Load_Command_Function.all (
            Id => Sequence_Id,
            Engine_Number => Destination_Engine_Id,
            Engine_Request => Specific_Engine
         ));
      end Send_Sequence_Load_Command;

      -- This function gets telemetry for an engine that needs it. It does this synchronously as long as it can
      -- until it detects that the same instruction is requesting the same telemetry point for a second time or
      -- the engine goes into a state other than Wait_Telemetry.
      procedure Get_Telemetry_One_Time is
         Current_Sequence_State : Seq_Execute_State.E := Wait_Telemetry;
         -- Get the starting sequence position:
         Starting_Position : Seq_Position;
      begin
         -- Make sure we are not already within a recursion. This helps us unroll the recursion. Although it
         -- makes the code a bit dirtier, it prevents a sequence with 100 telemetry variable sets in a row from
         -- overflowing the sequencer component stack due to the 100x recursion. Instead we unroll that recursion
         -- and only allow a single recursive call to Execute_Engine.
         if Recursion_Depth = 0 then
            -- We grab telemetry and set it in the engine in a loop in order to grab all
            -- telemetry for all instructions that need it. The loop exits if we have already gotten telemetry
            -- for an instruction (meaning the comparison did not pass so we need to get the same telemetry ID
            -- again at a future time) or if there was some error grabbing telemetry. If there are a few instructions
            -- in a row that simply grab telemetry values and save them in local engine variables, we will call
            -- execute until all those variables have been set without waiting for the next tick.
            while Current_Sequence_State = Wait_Telemetry or else Current_Sequence_State = Set_Telemetry loop
               -- Get the telemetry for the engine. If this is successful, then we can execute the
               -- engine. Otherwise, an error event has already been sent and we stop executing the
               -- engine, since we are in an error state.
               exit when Self.Get_Telemetry_For_Engine (Engine) = Error;

               -- Grab a new starting position prior to execute:
               Starting_Position := Engine.Get_Lowest_Child_Position;

               -- Execute the engine, but prevent any extra recursion if the resulting final state is
               -- Wait_Telemetry or Wait_Telemetry_Relative again. We unroll the recursion here.
               Self.Execute_Engine (Engine, Recursion_Depth => Recursion_Depth + 1);

               -- NOTE: It is important that we don't endlessly call execute recursively. We only want
               -- to get a single telemetry point per instruction that requires it. To determine if we
               -- have already fetched telemetry to satisfy this instruction we check the sequence position
               -- when execute was called against the position now, right after execute was called. If these
               -- are the same then we know that the comparison must have failed because the sequence wants us
               -- to grab the same telemetry value again. In that case, we don't execute the engine, and we will
               -- wait for the next tick to grab a new telemetry point and start again.
               --
               -- If position equal, comparison failed, grab telemetry at the next tick instead.
               exit when Starting_Position = Engine.Get_Lowest_Child_Position;

               -- Get the new sequence state:
               Current_Sequence_State := Engine.Get_Last_Execute_State;
            end loop;
         end if;
      end Get_Telemetry_One_Time;

      -- Get the current time:
      Execute_Time : constant Sys_Time.T := Self.Sys_Time_T_Get;
      -- Executes the sequence until it finishes or encounters a blocked state.
      Sequence_State : constant Seq_Execute_State.E := Engine.Execute (
         -- Prevents a sequence from entering an infinite loop where we never return
         Instruction_Limit => Self.Instruction_Limit,
         -- Provide the engine the current time
         Timestamp => Execute_Time
      );
      Ignore_Engine_State : Seq_Engine_State.E;
      -- Grab the current time after execution.
      Current_Time : constant Sys_Time.T := Self.Sys_Time_T_Get;
      -- Define the recursion limit as related to the number of engines. This allows someone to write a sequence that
      -- kills each engine one after another using a single "kill" command. We set minimum to 10. This also allows at
      -- least 10 print statements in a row.
      Recursion_Limit : constant Natural := Natural'Max (Self.Seq_Engines.all'Length + 1, 10);
   begin
      -- Make sure we have not exceeded the recursion limit. The way a sequence is constructed can cause us to recurse on
      -- Execute_Engine in some rare circumstances. This happens when we need to continue to execute the engine without
      -- it entering a blocking state. This is OK up to a limit. We want to avoid an infinite recursion from overflowing
      -- our component stack. One could write a silly sequence the kills sequences over and over in a loop that would
      -- trigger this condition in the implementation below.
      if Recursion_Depth > Recursion_Limit then
         -- Put engine into error condition and stop executing it.
         Engine.Set_Engine_Error (Seq_Enums.Seq_Error.Recursion);

         -- Send error event.
         Self.Event_T_Send_If_Connected (Self.Events.Execute_Recursion_Limit_Exceeded (Current_Time, (Engine_Id => Engine.Get_Engine_Id)));
         return;
      end if;

      -- If we are executing the engine, we can reset the timeout for this engine, since a timeout only occurs when we have not executed
      -- in a certain amount of time.
      Self.Reset_Timeout (Engine);

      -- Check the new sequence state:
      case Sequence_State is
         -- The following two states may be combined.
         when Unloaded =>
            -- The engine is done running its parent sequence, send info event:
            Self.Event_T_Send_If_Connected (Self.Events.Finished_Sequence (Current_Time, (Engine_Id => Engine.Get_Engine_Id)));
         when Wait_Relative =>
            -- Changes the wait from relative to absolute, sets state to Wait_Absolute.
            Engine.Change_Relative_Wait_To_Absolute (Current_Time);
         when Wait_Absolute =>
            -- Engine is sleeping, nothing needs to be done here, will check wake status on tick.
            null;
         when Wait_Command =>
            -- Send out the command from the engine:
            Send_Engine_Command (Engine.Get_Command);
         when Wait_Telemetry | Set_Telemetry =>
            -- Get telemetry for the engine a single time. If we are requested to get telemetry for the
            -- same instruction a second time, we will stop executing and wait for the next tick.
            Get_Telemetry_One_Time;
         when Wait_Telemetry_Relative =>
            -- This is like Wait_Telemetry except we need to provide an absolute time so the
            -- engine can transform the relative wait into an absolute wait.
            Engine.Change_Relative_Timeout_To_Absolute (Current_Time);
            pragma Assert (Engine.Get_Last_Execute_State = Wait_Telemetry);

            -- Get telemetry for the engine a single time. If we are requested to get telemetry for the
            -- same instruction a second time, we will stop executing and wait for the next tick.
            Get_Telemetry_One_Time;
         when Wait_Load_Seq =>
            -- Get the destination engine Id
            declare
               Do_Load : Boolean := True;
               Destination_Engine_Id : Seq_Types.Sequence_Engine_Id := Engine.Get_Load_Destination;
            begin
               -- First check the destination engine. If it is the magic number 255, this means we load the
               -- sequence into any currently available engine. Otherwise, let's range check the engine.
               if Destination_Engine_Id /= Engine.Get_Engine_Id then
                  if Destination_Engine_Id = 255 then
                     -- Search for an available engine and use the first one we find.
                     declare
                        Available_Engine_Id : Seq_Types.Sequence_Engine_Id;
                        Engine_Available : constant Boolean := Self.Find_Available_Engine (Available_Engine_Id);
                     begin
                        -- If an engine was found, save the Id for later
                        if Engine_Available then
                           Destination_Engine_Id := Available_Engine_Id;
                        else
                           -- Send out info event if no engine is available for loading.
                           Self.Event_T_Send_If_Connected (Self.Events.No_Engine_Available_For_Load (Self.Sys_Time_T_Get, (Engine_Id => Engine.Get_Engine_Id)));
                           Do_Load := False;
                        end if;
                     end;

                  -- For loading into other engines, range check the destination Id, also check if available.
                  elsif Self.Is_Engine_Id_Valid (Destination_Engine_Id) then
                     -- Make sure the engine we are trying to load into is currently available for loading.
                     declare
                        -- Extract the destination engine using the ID that has been validated above:
                        Destination_Engine : Seq.Engine renames Self.Seq_Engines.all (Destination_Engine_Id);
                     begin
                        if Is_Engine_Available (Destination_Engine) = False then
                           -- Send out info event if no engine is available for loading.
                           Self.Event_T_Send_If_Connected (Self.Events.Engine_Unavailable_For_Load (
                              Self.Sys_Time_T_Get, (Engine_Id => Engine.Get_Engine_Id, Engine_Id_To_Load => Destination_Engine_Id)
                           ));
                           Do_Load := False;
                        end if;
                     end;
                  else
                     -- Throw event and set engine to error.
                     Self.Event_T_Send_If_Connected (Self.Events.Engine_Id_Out_Of_Range_Error (
                        Self.Sys_Time_T_Get, (Engine_Id => Engine.Get_Engine_Id, Engine_Id_To_Load => Destination_Engine_Id)
                     ));
                     Do_Load := False;
                  end if;
               end if;

               -- Load the sequence arguments into the destination engine.
               if Do_Load then
                  declare
                     -- Get the sequence ID to load:
                     Sequence_Id_To_Load : constant Sequence_Types.Sequence_Id := Engine.Get_Seq_To_Load;
                     -- Get the arguments from the source engine:
                     Args : constant Variable_Array := Engine.Get_Arguments;
                     -- Extract the destination engine using the ID that has been validated above:
                     Destination_Engine : Seq.Engine renames Self.Seq_Engines.all (Destination_Engine_Id);
                  begin
                     -- Load arguments into the destination engine:
                     Destination_Engine.Set_Arguments (Args);

                     -- Reserve the destination engine (sometimes this is the engine we just called)
                     Destination_Engine.Reserve_Engine (Sequence_Id_To_Load);

                     -- Send the command to load the sequence into this engine.
                     Send_Sequence_Load_Command (Sequence_Id => Sequence_Id_To_Load, Destination_Engine_Id => Destination_Engine_Id);
                  end;
               else
                  -- Set the engine to an error state:
                  Engine.Set_Engine_Error (Seq_Enums.Seq_Error.Load);
               end if;
            end;
         when Kill_Engines =>
            declare
               -- Get the engines to kill from the engine:
               First_Engine : constant Sequence_Engine_Id := Engine.Get_Kill_Eng_Start;
               Num_Engines : constant Sequence_Engine_Id := Engine.Get_Num_Eng_Kill;
            begin
               -- If Num_Engines is not positive, then there is nothing to kill. We treat this as
               -- a noop.
               if Num_Engines > 0 then
                  if First_Engine < Self.Seq_Engines.all'First or else
                      First_Engine > Self.Seq_Engines.all'Last or else
                      First_Engine + Num_Engines - 1 > Self.Seq_Engines.all'Last
                  then
                     -- Put sequence into error condition:
                     Engine.Set_Engine_Error (Seq_Enums.Seq_Error.Kill);

                     -- Send error event:
                     Self.Event_T_Send_If_Connected (Self.Events.Invalid_Engine_Kill_Range (Current_Time, (
                        Executing_Engine => Engine.Get_Engine_Id, First_Engine => First_Engine, Num_Engines => Num_Engines
                     )));
                  else
                     -- Reset all engines in range:
                     for Idx in First_Engine .. First_Engine + Num_Engines - 1 loop
                        Self.Seq_Engines.all (Idx).Reset;
                     end loop;

                     -- Send info event:
                     Self.Event_T_Send_If_Connected (Self.Events.Engines_Killed (Current_Time, (
                        Executing_Engine => Engine.Get_Engine_Id, First_Engine => First_Engine, Num_Engines => Num_Engines
                     )));
                  end if;
               end if;

               -- If we did not encounter an error and this engine is still active, then execute this engine again.
               case Engine.Get_Engine_State is
                  when Uninitialized | Inactive | Engine_Error =>
                     null; -- Do nothing
                  when Reserved | Active | Waiting =>
                     Self.Execute_Engine (Engine, Recursion_Depth => Recursion_Depth + 1);
               end case;
            end;
         when Print =>
            declare
               -- Grab the print string from the engine.
               Print_From_Engine : constant Seq_Print.T := Engine.Get_String_To_Print;
               -- Declare the print string for the event.
               Print_To_Send : Seq_Print_Event_Record.T := (
                  Header => (
                     Engine_Id => Engine.Get_Engine_Id,
                     Sequence_Id => Engine.Get_Lowest_Child_Id,
                     Print_Type => Print_From_Engine.Print_Type
                  ),
                  Print_String => [others => 0]
               );
            begin
               -- Safe copy print from engine to event. This truncates the string if necessary:
               Byte_Array_Util.Safe_Left_Copy (
                  Dest => Print_To_Send.Print_String,
                  Src => Print_From_Engine.Encoded_String
               );
               -- Send out the event.
               Self.Event_T_Send_If_Connected (Self.Events.Print (Current_Time, Print_To_Send));
               -- Continue to execute the engine.
               Self.Execute_Engine (Engine, Recursion_Depth => Recursion_Depth + 1);
            end;
         when Error =>
            -- Produce an error record and send it out as an event.
            Self.Event_T_Send_If_Connected (Self.Events.Sequence_Execution_Error (Current_Time, Get_Error_Report (Engine)));
      end case;
   end Execute_Engine;

   -- Procedure which build and sends the summary packet:
   procedure Send_Summary_Packet (Self : in out Instance) is
      -- Get an empty packet:
      Pkt : Packet.T := Self.Packets.Summary_Packet_Empty (Self.Sys_Time_T_Get);
      Idx : Packet_Types.Packet_Buffer_Length_Type := Pkt.Buffer'First;
   begin
      -- Fill in the data:
      for Engine of Self.Seq_Engines.all loop
         Pkt.Buffer (Idx .. Idx + Engine_Summary_Type.Size_In_Bytes - 1) :=
            Engine_Summary_Type.Serialization.To_Byte_Array ((
               Engine_State => Engine.Get_Engine_State,
               Sequence_State => Engine.Get_Running_Sequence_State,
               Sequence_Error_Code => Engine.Get_Seq_Error_Code,
               Stack_Level => Engine.Get_Stack_Level,
               Parent_Sequence_Id => Engine.Get_Parent_Id,
               Parent_Program_Counter => Engine.Get_Parent_Position,
               Lowest_Child_Id => Engine.Get_Lowest_Child_Id,
               Lowest_Child_Program_Counter => Engine.Get_Lowest_Child_Position,
               Wakeup_Time => Engine.Get_Wakeup_Time.Seconds,
               Command_Error_Counter => Self.Engine_Aux_Data.all (Engine.Get_Engine_Id).Command_Error_Counter
            ));

         -- Increment the index:
         Idx := @ + Engine_Summary_Type.Size_In_Bytes;
      end loop;

      -- Fill in packet length:
      Pkt.Header.Buffer_Length := Self.Seq_Engines.all'Length * Engine_Summary_Type.Size_In_Bytes;

      -- Send the packet:
      Self.Packet_T_Send (Pkt);
   end Send_Summary_Packet;

   -- Procedure which build and sends the summary packet:
   procedure Send_Details_Packet (Self : in out Instance; Engine_Id : in Seq_Types.Sequence_Engine_Id) is
      use Seq;
      -- Get reference to engine:
      The_Engine : Seq.Engine renames Self.Seq_Engines.all (Engine_Id);
      -- Get an empty packet:
      Pkt : Packet.T := Self.Packets.Details_Packet_Empty (Self.Sys_Time_T_Get);
      Idx : Packet_Types.Packet_Buffer_Length_Type := Pkt.Buffer'First;
      -- Get the stack depth of the engine:
      Stack_Depth : constant Seq_Types.Max_Seq_Num := The_Engine.Get_Stack_Depth;
   begin
      Pkt.Buffer (Idx .. Idx + Engine_Details_Type.Size_In_Bytes - 1) :=
         Engine_Details_Type.Serialization.To_Byte_Array ((
            Engine_Id => Engine_Id,
            Source_Id => The_Engine.Get_Source_Id,
            Engine_State => The_Engine.Get_Engine_State,
            Last_Command_Id_Sent => The_Engine.Get_Last_Command_Id_Sent,
            Engine_Command_Send_Counter => The_Engine.Get_Num_Commands_Sent,
            Engine_Command_Error_Counter => Self.Engine_Aux_Data.all (Engine_Id).Command_Error_Counter,
            Sequence_Error_Code => The_Engine.Get_Seq_Error_Code,
            Wakeup_Time => The_Engine.Get_Wakeup_Time.Seconds,
            Stack_Level => The_Engine.Get_Stack_Level
         ));

      -- Increment the index:
      Idx := @ + Engine_Details_Type.Size_In_Bytes;

      for Stack_Index in Seq_Types.Max_Seq_Num'First .. Seq_Types.Max_Seq_Num'First + Stack_Depth - 1 loop
         Pkt.Buffer (Idx .. Idx + Sequence_Details_Type.Size_In_Bytes - 1) :=
            Sequence_Details_Type.Serialization.To_Byte_Array ((
               Header => The_Engine.Get_Sequence_Header (Stack_Index),
               Sequence_State => The_Engine.Get_Sequence_State (Stack_Index),
               Sequence_Address => (Address => The_Engine.Get_Sequence_Region (Stack_Index).Address),
               Program_Counter => The_Engine.Get_Sequence_Position (Stack_Index),
               Start_Time => The_Engine.Get_Sequence_Start_Time (Stack_Index).Seconds,
               Last_Executed_Time => The_Engine.Get_Sequence_Last_Executed_Time (Stack_Index).Seconds
            ));

         -- Increment the index:
         Idx := @ + Sequence_Details_Type.Size_In_Bytes;
      end loop;

      -- Fill in packet length:
      Pkt.Header.Buffer_Length := Engine_Details_Type.Size_In_Bytes + Natural (Stack_Depth) * Natural (Sequence_Details_Type.Size_In_Bytes);

      -- Send the packet:
      Self.Packet_T_Send (Pkt);
   end Send_Details_Packet;

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- The schedule invokee connector. This is used to detect sequence timeout errors, meter out the checking of telemetry for sequence conditionals, and determine when to resume a sequence after a relative or absolute wait.
   overriding procedure Tick_T_Recv_Async (Self : in out Instance; Arg : in Tick.T) is
      -- Get the current time:
      Current_Time : constant Sys_Time.T := Self.Sys_Time_T_Get;
   begin
      -- If an engine is sleeping, check to see if it's time to wake up
      for Engine of Self.Seq_Engines.all loop
         -- Check the engine state first:
         case Engine.Get_Engine_State is

            -- For some states, we don't need to do anything...
            when Uninitialized | Engine_Error | Inactive =>
               null; -- Nothing to do.

            -- If we are in the reserved state, we need to execute some timeout logic. We don't expect to be in this state forever.
            when Reserved =>
               if not Self.Increment_Timeout (Engine) then
                  -- We just failed to receive an expected sequence load in an appropriate amount of time.
                  -- Put the engine into the error state, because we don't want to just keep waiting forever. We need
                  -- to let the ground know something unrecoverable happened.
                  Engine.Set_Engine_Error (Load_Timeout);
                  -- Now send out info event, since the state of the engine has been updated.
                  Self.Event_T_Send_If_Connected (Self.Events.Sequence_Timeout_Error (Current_Time, Get_Error_Report (Engine)));
               end if;

            -- If the engine is waiting, check to see if it is done. If so, execute the engine.
            when Waiting =>
               case Engine.Is_Done_Waiting (Current_Time) is
                  when Seq.Done =>
                     Self.Execute_Engine (Engine);
                  when Seq.Still_Waiting =>
                     null; -- Nothing to do...
               end case;

            when Active =>
               -- Engine is active, check the running sequence state.
               case Engine.Get_Last_Execute_State is
                  -- Nothing to do in these cases. NOTE: that the following is unreachable because if the engine is active then the last executed
                  -- sequence will never have a state of Unloaded or Error. This is not a strict coding contract that must be adhered to, so an
                  -- assertion here is not advisable. The null statement just may not be coverable in unit testing.
                  when Unloaded | Error =>
                     null;

                  -- This should never happen since we either transform the timeout, or handle waits when the engine is in the "waiting" state
                  when Wait_Relative | Wait_Telemetry_Relative | Wait_Absolute | Kill_Engines | Print =>
                     pragma Assert (False);

                  -- These waits all require timeout logic to be exercised.
                  when Wait_Command | Wait_Load_Seq =>
                     -- Check for a timeout:
                     if not Self.Increment_Timeout (Engine) then
                        -- If we are set to continue on command failure, then fail, otherwise, give up on waiting for the
                        -- last command.
                        if Self.Continue_On_Command_Failure then
                           -- Send out info event:
                           Self.Event_T_Send_If_Connected (Self.Events.Sequence_Timeout_Error (Current_Time, Get_Error_Report (Engine)));

                           -- We need to let ops know something happened so we increment the command fail count. They can check
                           -- the event log for more details.
                           declare
                              Error_Counter : Interfaces.Unsigned_16 renames Self.Engine_Aux_Data.all (Engine.Get_Engine_Id).Command_Error_Counter;
                           begin
                              Error_Counter := @ + 1;
                           end;

                           -- If we were waiting on a command or this engine triggered a load into another engine, then we just execute the next command in our sequence
                           if Engine.Get_Last_Execute_State = Wait_Command or else (Engine.Get_Last_Execute_State = Wait_Load_Seq and then Engine.Get_Load_Destination /= Engine.Get_Engine_Id) then
                              -- Continue on, ignore the timeout, since we are configured to do so.
                              Self.Execute_Engine (Engine);
                           else
                              -- NOTE: I am not sure this code is actually reachable, because the engine should be in the Reserve state
                              -- if a subsequence load occurs. We handle the timeout there too.
                              -- We just failed to receive a subsequence load, i.e. "call" in an appropriate amount of time.
                              -- Put the engine into the error state, because we don't want to just keep waiting forever. We need
                              -- to let the ground know something unrecoverable happened.
                              Engine.Set_Engine_Error (Load_Timeout);
                              -- Now send out info event, since the state of the engine has been updated.
                              Self.Event_T_Send_If_Connected (Self.Events.Sequence_Timeout_Error (Current_Time, Get_Error_Report (Engine)));
                           end if;
                        else
                           -- Put the engine into the error state.
                           Engine.Set_Engine_Error (Command_Timeout);
                           -- Now send out info event, since the state of the engine has been updated.
                           Self.Event_T_Send_If_Connected (Self.Events.Sequence_Timeout_Error (Current_Time, Get_Error_Report (Engine)));
                        end if;
                     end if;

                  -- Grab telemetry value from system and provide it back to the engine for execution. If
                  -- getting the telemetry fails for some reason, we use the internal sequencer timeout mechanism
                  -- to make sure we timeout of this telemetry wait using the timeout value provided in the sequence.
                  when Wait_Telemetry =>
                     -- Get the telemetry for the engine. If this is successful, then we can execute the
                     -- engine. Otherwise, an error event has already been sent.
                     if Self.Get_Telemetry_For_Engine (Engine) /= Error then
                        Self.Execute_Engine (Engine);
                     end if;

                  -- Grab telemetry value from system and provide it back to the engine for execution. If
                  -- telemetry is not available for some reason, we need to start incrementing the global
                  -- timeout for this engine. Since this is not a "wait" on telemetry, it is just a fetch
                  -- the sequence will not be managing the timeout itself. So we need to use the default
                  -- timeout to make sure that we don't get stuck here forever.
                  when Set_Telemetry =>
                     -- Get the telemetry for the engine. If this is successful, then we can execute the
                     -- engine. Otherwise, an error event has already been sent.
                     case Self.Get_Telemetry_For_Engine (Engine) is
                        when Success =>
                           -- Execute the engine.
                           Self.Execute_Engine (Engine);
                        when Not_Available =>
                           -- Check for a timeout:
                           if not Self.Increment_Timeout (Engine) then
                              -- Put the engine into the error state.
                              Engine.Set_Engine_Error (Telemetry_Timeout);
                              -- Now send out info event, since the state of the engine has been updated.
                              Self.Event_T_Send_If_Connected (Self.Events.Sequence_Timeout_Error (Current_Time, Get_Error_Report (Engine)));
                           end if;
                        when Error => null; -- Nothing to do, engine in error state and event already thrown
                     end case;
               end case;
         end case;
      end loop;

      -- Send out summary packet if it is time:
      if Self.Packet_Period /= 0 and then
          Self.Is_Packet_T_Send_Connected
      then
         if (Self.Packet_Counter mod Self.Packet_Period) = 0 then
            -- Build send the packet:
            Self.Send_Summary_Packet;

            -- Reset the packet counter:
            Self.Packet_Counter := 0;
         end if;

         -- Increment the packet counter:
         Self.Packet_Counter := @ + 1;
      end if;
   end Tick_T_Recv_Async;

   -- Command responses from sent commands are received on this connector, allowed subsequent commands in a sequence to be sent out.
   overriding procedure Command_Response_T_Recv_Async (Self : in out Instance; Arg : in Command_Response.T) is
      use Seq;
      use Command_Response_Status;
      use Seq_Enums.Seq_Runtime_State;

      -- Handle the command response. This procedure assumes that the command response is valid for the engine
      -- passed in, and that the engine ID is within range.
      procedure Handle_Command_Response (Response : in Command_Response.T; Engine_Id : in Seq_Types.Sequence_Engine_Id; Expected_Command_Id : in Command_Types.Command_Id) with
         -- The helper assumes we are in the Wait_Command or Wait_Load_New_Seq_Elsewhere state
         Pre => (Self.Seq_Engines.all (Engine_Id).Get_Running_Sequence_State = Wait_Command or else
                     Self.Seq_Engines.all (Engine_Id).Get_Running_Sequence_State = Wait_Load_New_Seq_Elsewhere)
      is
         The_Engine : Seq.Engine renames Self.Seq_Engines.all (Engine_Id);
         Do_Execute_Engine : Boolean := True;
      begin
         -- Check the command ID and make sure it matches the command just sent from this engine.
         if Expected_Command_Id /= Response.Command_Id then
            -- This command response does not match the command that was last sent out of this
            -- engine. In this case, let's just silently ignore. This is likely a command response from
            -- a subsequence load command, but we have already loaded and started running the subsequence.
            null;

            -- Let's report this with an event, but do not continue:
            -- Self.Event_T_Send_If_Connected (Self.Events.Unexpected_Command_Response_Id (Self.Sys_Time_T_Get, (
            --    Response => Response,
            --    Last_Sent_Command_Id => Expected_Command_Id
            -- )));
            -- ^ This event will be produced whenever we have a spawn or call instruction, which will be confusing.
            --    Is it better to just remove?
         else
            -- Command ID looks good!
            -- Check the return status of the command, if it is anything but Success, then the command
            -- failed and we need to report it and maybe halt the sequence.
            if Response.Status /= Success then
               -- A sequence command failed to execute successfully. First let's throw an info event:
               Self.Event_T_Send_If_Connected (Self.Events.Sequence_Command_Failure (Self.Sys_Time_T_Get, (
                  Response => Response,
                  Error_Report => Get_Error_Report (The_Engine)
               )));

               -- Increment the command error counter for this engine:
               declare
                  Error_Counter : Interfaces.Unsigned_16 renames Self.Engine_Aux_Data.all (Engine_Id).Command_Error_Counter;
               begin
                  Error_Counter := @ + 1;
               end;

               -- If we are configured to continue on a command failure then keep going, otherwise
               -- set the engine into an error state.
               if not Self.Continue_On_Command_Failure then
                  The_Engine.Set_Engine_Error (Command_Fail);
                  Do_Execute_Engine := False;
               end if;
            end if;

            -- Execute the engine if the previous command succeeded or we are configured to continue on
            -- command failure.
            if Do_Execute_Engine then
               -- Execute the engine:
               Self.Execute_Engine (The_Engine);
            end if;
         end if;
      end Handle_Command_Response;

      -- Get the ID for the sequence load command.
      function Get_Load_Command_Id return Command_Types.Command_Id is
         use Command_Sequencer_Enums.Sequence_Load_Engine_Request_Type;
         -- Create a temporary load command so that we can get the ID for the load command.
         Load_Command : constant Command.T := Self.Create_Sequence_Load_Command_Function.all (
            Id => 0, -- Doesn't matter
            Engine_Number => 0, -- Doesn't matter
            Engine_Request => Specific_Engine -- Doesn't matter
         );
      begin
         return Load_Command.Header.Id;
      end Get_Load_Command_Id;

   begin
      -- Check if this is a Register_Source command response first.
      if Arg.Status = Register_Source then
         declare
            Source_Id_Set : Boolean := False;
         begin
            -- If the status of the command response is a Register_Source status, then we need to set our command
            -- source id. Otherwise we should perform the action associated with receiving a command response.
            for The_Engine of Self.Seq_Engines.all loop
               -- Engine state can only go from Uninitialized to inactive via the Set_Source_Id procedure.
               if The_Engine.Get_Engine_State = Uninitialized then
                  The_Engine.Set_Source_Id (Arg.Source_Id);
                  Source_Id_Set := True;
                  exit;
               end if;
            end loop;

            -- Make sure a source ID was set, otherwise issue event:
            if not Source_Id_Set then
               Self.Event_T_Send_If_Connected (Self.Events.Unexpected_Register_Source (Self.Sys_Time_T_Get, Arg));
            end if;
         end;
      else
         -- This is a regular command response from a sequence, let's process it.
         declare
            -- Get the engine that this command response is meant for:
            Engine_Id : Seq_Types.Sequence_Engine_Id;
            Ret : constant Boolean := Self.Get_Engine_With_Source_Id (Source_Id => Arg.Source_Id, Engine_Id => Engine_Id);
         begin
            -- Make sure the command response contains a source_id that we recognize, otherwise throw event:
            if Ret then
               declare
                  The_Engine : Seq.Engine renames Self.Seq_Engines.all (Engine_Id);
               begin
                  -- Check the engine state. We want to ignore any command responses we get unless we are in the active or Reserved state
                  case The_Engine.Get_Engine_State is
                     when Active | Reserved =>
                        -- Make sure the sequence state is such that we are ready to handle a command response.
                        case The_Engine.Get_Running_Sequence_State is
                           -- We only execute this engine if we were waiting for a command response to arrive
                           -- or if we just loaded a new sequence elsewhere and we want to continue executing
                           -- the rest of our own sequence now.
                           when Wait_Command =>
                              declare
                                 Last_Sent_Command_Id : constant Command_Types.Command_Id := The_Engine.Get_Last_Command_Id_Sent;
                              begin
                                 -- We have received the expected command response for the last command we sent. Let's handle it.
                                 -- The expected command ID in the response is the last sent command ID from this engine.
                                 Handle_Command_Response (Response => Arg, Engine_Id => Engine_Id, Expected_Command_Id => Last_Sent_Command_Id);
                              end;
                           when Wait_Load_New_Seq_Elsewhere =>
                              -- We have received the expected command response for the last command we sent. Let's handle it.
                              -- The expected command ID is the load command in this case.
                              Handle_Command_Response (Response => Arg, Engine_Id => Engine_Id, Expected_Command_Id => Get_Load_Command_Id);
                           when others =>
                              -- Just ignore the command response. We don't want to error here because its plausible
                              -- that this is a command response from a subsequence load, etc. In that case, the state
                              -- of our sequence could be anything as we do not know if the subsequence has been loaded
                              -- and is currently running, or if that has not happened yet. So we just ignore this
                              -- command response.
                              null;
                        end case;
                     -- We are not in an engine state that can deal with an errant command response. Report and ignore.
                     when Uninitialized | Inactive | Engine_Error | Waiting =>
                        -- This can happen for the load a new sequence command and that is totally fine. We are waiting on the
                        -- sequence load to come in, not the command response for that load command. So we only throw
                        -- this warning event if it is a command response from a command that is not a load new sequence command.
                        if Arg.Command_Id /= Get_Load_Command_Id then
                           Self.Event_T_Send_If_Connected (Self.Events.Unexpected_Command_Response (Self.Sys_Time_T_Get, Arg));
                        end if;
                  end case;
               end;
            else
               -- Throw event:
               Self.Event_T_Send_If_Connected (Self.Events.Unexpected_Command_Response (Self.Sys_Time_T_Get, Arg));
            end if;
         end;
      end if;
   end Command_Response_T_Recv_Async;

   -- The command receive connector. Commands received on this connector are executed by the sequencer itself, i.e. halting a sequence.
   overriding procedure Command_T_Recv_Async (Self : in out Instance; Arg : in Command.T) is
      -- Execute the command:
      Stat : constant Command_Response_Status.E := Self.Execute_Command (Arg);
   begin
      -- Send the return status:
      Self.Command_Response_T_Send_If_Connected ((Source_Id => Arg.Header.Source_Id, Registration_Id => Self.Command_Reg_Id, Command_Id => Arg.Header.Id, Status => Stat));
   end Command_T_Recv_Async;

   -- This connector is used to load a sequence into the command sequencer via memory region. Sequences are not copied to this component's memory, they are run directly from the address provided in the given sequence load memory region.
   overriding procedure Sequence_Load_T_Recv_Async (Self : in out Instance; Arg : in Sequence_Load.T) is
      use Command_Sequencer_Enums.Sequence_Load_Engine_Request_Type;
      use Command_Sequencer_Enums.Sequence_Load_Status;

      function Do_Load_And_Start_Sequence (Engine : in out Seq.Engine; Header : in Sequence_Header.T) return Command_Sequencer_Enums.Sequence_Load_Status.E is
         -- Perform the load and get the state:
         State : constant Seq.Load_Status := Engine.Load (Arg.Sequence_Region);
      begin
         case State is
            when Seq.Success =>
               -- Send info event:
               Self.Event_T_Send_If_Connected (Self.Events.Starting_Sequence (Self.Sys_Time_T_Get, (
                  Load => Arg,
                  Header => Header,
                  Engine_Id => Engine.Get_Engine_Id,
                  Stack_Level => Engine.Get_Stack_Level
               )));

               -- Execute the newly loaded sequence:
               Self.Execute_Engine (Engine);
               return Success;
            when Seq.Failure =>
               -- The load failed for some reason, the engine is in error.
               Self.Event_T_Send_If_Connected (Self.Events.Sequence_Load_Error (Self.Sys_Time_T_Get, (
                  Load => Arg,
                  Header => Header,
                  Stack_Level => Engine.Get_Stack_Level,
                  State => Engine.Get_Running_Sequence_State,
                  Sequence_Error_Code => Engine.Get_Seq_Error_Code
               )));
               return Load_Error;
         end case;
      end Do_Load_And_Start_Sequence;

      function Load_And_Start_Sequence (Engine : in out Seq.Engine) return Command_Sequencer_Enums.Sequence_Load_Status.E is
         use Sequence_Util;

         -- Calculate the CRC over this sequence region:
         Header : Sequence_Header.T;
         Computed_Crc : Crc_16.Crc_16_Type;
         Ret : constant Sequence_Util.Crc_Status := Sequence_Util.Crc_Sequence_Memory_Region (Arg.Sequence_Region, Seq_Header => Header, Computed_Crc => Computed_Crc);
      begin
         -- Check the return status:
         case Ret is
            when Valid =>
               -- If this engine was Reserved for load by another engine, we need to make sure the sequence ID being loaded matches the
               -- Reserved sequence ID.
               if Engine.Get_Engine_State = Reserved then
                  declare
                     use Sequence_Types;
                     -- Get the expected sequence ID to load.
                     Expected_Id : constant Sequence_Types.Sequence_Id := Engine.Get_Reserved_Sequence_Id;
                  begin
                     if Header.Id /= Expected_Id then
                        -- Throw info event:
                        Self.Event_T_Send_If_Connected (Self.Events.Invalid_Sequence_Id (Self.Sys_Time_T_Get, (Load => Arg, Header => Header, Expected_Id => Expected_Id)));
                        return Unexpected_Sequence_Id;
                     end if;
                  end;
               end if;

               -- Check the sequence state, and perform some error checking based on it.
               case Engine.Get_Last_Execute_State is
                  -- Expecting load of a specific sequence ID into this engine.
                  when Wait_Load_Seq =>
                     declare
                        use Sequence_Types;
                        -- Get the expected sequence ID to load.
                        Expected_Id : constant Sequence_Types.Sequence_Id := Engine.Get_Seq_To_Load;
                     begin
                        -- Make sure the sequence we intend to load matches the load we received.
                        if Header.Id = Expected_Id then
                           -- Load and start the sequence:
                           return Do_Load_And_Start_Sequence (Engine, Header);
                        else
                           -- Throw info event. NOTE: I do not think this code is reachable because any failure here will first fail on the "reserve" check
                           -- above this case statement. That said, I don't see harm leaving this in here in case the reserve logic is changed. This redundant
                           -- check is not hurting anything. It would be much worse to accidentally not perform this check.
                           Self.Event_T_Send_If_Connected (Self.Events.Invalid_Sequence_Id (Self.Sys_Time_T_Get, (Load => Arg, Header => Header, Expected_Id => Expected_Id)));
                           return Unexpected_Sequence_Id;
                        end if;
                     end;
                  -- Ready to load any sequence.
                  when Unloaded =>
                     -- Load the sequence.
                     return Do_Load_And_Start_Sequence (Engine, Header);
                  -- We are loading over a sequence that has errored. Make sure to reset the sequence so that this new sequence gets loaded as the parent sequence
                  -- (i.e. the top of the engine stack)
                  when Error =>
                     -- Reset and load the sequence.
                     Engine.Reset;
                     return Do_Load_And_Start_Sequence (Engine, Header);
                  -- Not ready to load a sequence.
                  when Wait_Relative | Wait_Absolute | Wait_Command | Wait_Telemetry | Set_Telemetry | Wait_Telemetry_Relative | Kill_Engines | Print =>
                     -- If the engine is in any other state, then this is an error. The engine is
                     -- in use and not expecting a load.
                     Self.Event_T_Send_If_Connected (Self.Events.Engine_In_Use (Self.Sys_Time_T_Get, (Load => Arg, Header => Header, State => Engine.Get_Running_Sequence_State)));
                     return Engine_In_Use;
               end case;

            when Length_Error =>
               -- Send info event:
               Self.Event_T_Send_If_Connected (Self.Events.Invalid_Sequence_Length (Self.Sys_Time_T_Get, (Load => Arg, Header => Header)));
               return Length_Error;

            when Crc_Error =>
               -- Send info event:
               Self.Event_T_Send_If_Connected (Self.Events.Invalid_Sequence_Crc (Self.Sys_Time_T_Get, (Load => Arg, Header => Header, Computed_Crc => Computed_Crc)));
               return Crc_Error;

         end case;
      end Load_And_Start_Sequence;

      Return_Status : Command_Sequencer_Enums.Sequence_Load_Status.E := Engine_In_Use;
   begin
      -- Does the sequence load request require a specific engine? or any engine?
      case Arg.Engine_Request is
         when Any_Engine =>
            declare
               Engine_Id : Seq_Types.Sequence_Engine_Id;
               Engine_Available : constant Boolean := Self.Find_Available_Engine (Engine_Id);
            begin
               -- If an available engine was found, then load and start a sequence in it:
               if Engine_Available then
                  Return_Status := Load_And_Start_Sequence (Self.Seq_Engines.all (Engine_Id));
               else
                  -- Send out info event if no engine is available for loading.
                  Self.Event_T_Send_If_Connected (Self.Events.No_Engine_Available (Self.Sys_Time_T_Get, Arg));
                  Return_Status := Engine_In_Use;
               end if;
            end;

         when Specific_Engine =>
            -- Make sure the engine ID is valid:
            if Self.Is_Engine_Id_Valid (Arg.Engine_Id) then
               declare
                  Engine : Seq.Engine renames Self.Seq_Engines.all (Arg.Engine_Id);
               begin
                  case Engine.Get_Engine_State is
                     -- Make sure the engine has been initialized.
                     when Uninitialized =>
                        -- Send info event
                        Self.Event_T_Send_If_Connected (Self.Events.Load_To_Uninitialized_Engine (Self.Sys_Time_T_Get, Arg));
                        Return_Status := Engine_Uninitialized;

                     -- Try to load the sequence
                     when Reserved | Active | Engine_Error | Inactive | Waiting =>
                        Return_Status := Load_And_Start_Sequence (Engine);
                  end case;
               end;
            else
               -- Send info event
               Self.Event_T_Send_If_Connected (Self.Events.Load_To_Invalid_Engine_Id (Self.Sys_Time_T_Get, Arg));
               Return_Status := Invalid_Engine_Number;
            end if;
      end case;

      -- Send out a sequence load return status:
      Self.Sequence_Load_Return_T_Send ((Load => Arg, Status => Return_Status));
   end Sequence_Load_T_Recv_Async;

   -- This procedure is called when a Tick_T_Recv_Async message is dropped due to a full queue.
   overriding procedure Tick_T_Recv_Async_Dropped (Self : in out Instance; Arg : in Tick.T) is
   begin
      -- Throw event:
      Self.Event_T_Send_If_Connected (Self.Events.Dropped_Tick (
         Self.Sys_Time_T_Get, Arg
      ));
   end Tick_T_Recv_Async_Dropped;

   -- This procedure is called when a Command_Response_T_Recv_Async message is dropped due to a full queue.
   overriding procedure Command_Response_T_Recv_Async_Dropped (Self : in out Instance; Arg : in Command_Response.T) is
   begin
      -- Throw event:
      Self.Event_T_Send_If_Connected (Self.Events.Dropped_Command_Response (
         Self.Sys_Time_T_Get, Arg
      ));
   end Command_Response_T_Recv_Async_Dropped;

   -- This procedure is called when a Command_T_Recv_Async message is dropped due to a full queue.
   overriding procedure Command_T_Recv_Async_Dropped (Self : in out Instance; Arg : in Command.T) is
   begin
      -- Throw event:
      Self.Event_T_Send_If_Connected (Self.Events.Dropped_Command (
         Self.Sys_Time_T_Get, Arg.Header
      ));
   end Command_T_Recv_Async_Dropped;

   -- This procedure is called when a Sequence_Load_T_Recv_Async message is dropped due to a full queue.
   overriding procedure Sequence_Load_T_Recv_Async_Dropped (Self : in out Instance; Arg : in Sequence_Load.T) is
      use Command_Sequencer_Enums.Sequence_Load_Status;
   begin
      -- Send out the sequence load return with Dropped status:
      Self.Sequence_Load_Return_T_Send ((Load => Arg, Status => Dropped));

      -- Throw event:
      Self.Event_T_Send_If_Connected (Self.Events.Dropped_Sequence_Load (
         Self.Sys_Time_T_Get, Arg
      ));
   end Sequence_Load_T_Recv_Async_Dropped;

   -----------------------------------------------
   -- Command handler primitives:
   -----------------------------------------------
   -- Description:
   --    These are the commands for the Command Sequencer.
   -- This command halts all currently running engines.
   overriding function Kill_All_Engines (Self : in out Instance) return Command_Execution_Status.E is
      use Command_Execution_Status;
   begin
      -- Reset all engines.
      for Engine of Self.Seq_Engines.all loop
         Engine.Reset;
      end loop;

      -- Send info event:
      Self.Event_T_Send_If_Connected (Self.Events.Killed_All_Engines (Self.Sys_Time_T_Get));

      return Success;
   end Kill_All_Engines;

   -- This command halts an engine with the provided engine number.
   overriding function Kill_Engine (Self : in out Instance; Arg : in Packed_Sequence_Engine_Id.T) return Command_Execution_Status.E is
      use Command_Execution_Status;
   begin
      -- Make sure engine id is valid before trying to kill it.
      if Self.Is_Engine_Id_Valid (Arg.Engine_Id) then
         -- Reset engine and send info event:
         Self.Seq_Engines (Arg.Engine_Id).Reset;
         Self.Event_T_Send_If_Connected (Self.Events.Killed_Engine (Self.Sys_Time_T_Get, Arg));
      else
         -- Send error event:
         Self.Event_T_Send_If_Connected (Self.Events.Invalid_Engine_Id (Self.Sys_Time_T_Get, Arg));
         return Failure;
      end if;

      return Success;
   end Kill_Engine;

   -- Set the period of the sequence status packet. A period of zero disables the sending of the packet.
   overriding function Set_Summary_Packet_Period (Self : in out Instance; Arg : in Packed_U16.T) return Command_Execution_Status.E is
      use Command_Execution_Status;
      Current_Time : constant Sys_Time.T := Self.Sys_Time_T_Get;
   begin
      -- Sent the packet period and reset the counter:
      Self.Packet_Period := Arg.Value;
      Self.Packet_Counter := 0;

      -- Send an info event:
      Self.Event_T_Send_If_Connected (Self.Events.Summary_Packet_Period_Set (Current_Time, Arg));

      -- Update data product:
      Self.Data_Product_T_Send_If_Connected (Self.Data_Products.Summary_Packet_Period (Current_Time, (Value => Self.Packet_Period)));

      return Success;
   end Set_Summary_Packet_Period;

   -- The sequence details packet for a particular engine is issued when this command is received.
   overriding function Issue_Details_Packet (Self : in out Instance; Arg : in Packed_Sequence_Engine_Id.T) return Command_Execution_Status.E is
      use Command_Execution_Status;
   begin
      if Self.Is_Engine_Id_Valid (Arg.Engine_Id) then
         -- Only do this if the packet connector is connected, otherwise, treat this command as a noop.
         if Self.Is_Packet_T_Send_Connected then
            -- Send the packet:
            -- What is packet_t_send is disconnected?
            Self.Send_Details_Packet (Arg.Engine_Id);

            -- Send info event:
            Self.Event_T_Send_If_Connected (Self.Events.Details_Packet_Sent (Self.Sys_Time_T_Get, Arg));
         end if;
      else
         -- Send event
         Self.Event_T_Send_If_Connected (Self.Events.Invalid_Engine_Id (Self.Sys_Time_T_Get, Arg));
         return Failure;
      end if;

      return Success;
   end Issue_Details_Packet;

   -- If a sequence requires arguments to be run correctly at the parent level, this command can be used to set the arguments into the engine prior to loading the sequence. This command will only be executed if there is no other sequence loaded in this engine. Arguments can only be set for a sequence that is going to be loaded into the parent stack position. If this command is not run prior to running a sequence in an engine, then the arguments will default to values of zero. If a sequence does not require some or all of the 16 arguments, then those arguments will never be read, and thus do not need to be set by this command.
   overriding function Set_Engine_Arguments (Self : in out Instance; Arg : in Packed_Variable_Array.T) return Command_Execution_Status.E is
      use Command_Execution_Status;
   begin
      if Self.Is_Engine_Id_Valid (Arg.Engine_Id) then
         declare
            -- Extract the destination engine using the ID that has been validated above:
            Engine : Seq.Engine renames Self.Seq_Engines.all (Arg.Engine_Id);
         begin
            -- Check engine state to make sure it is ready to load arguments.
            if Is_Engine_Available (Engine) then
               -- If the engine is in the error state we need to reset it before loading new arguments, otherwise it will
               -- get reset by the sequence load handler, erasing any arguments we set here.
               if Engine.Get_Engine_State = Engine_Error then
                  Engine.Reset;
               end if;

               -- OK, now we are good to load arguments into the engine.
               declare
                  -- Initialize the arguments as zero.
                  Args : Variable_Array := [others => (Value => [others => 0])];
               begin
                  -- This implementation assumes that the variable array is of length 16 and the number
                  -- of arguments provided in Packed_Variable_Array.T is also 16. If this is not true, then
                  -- both types and the copy code below need to be updated to be consistent.
                  pragma Compile_Time_Error (Variable_Array'Length /= 16, "Expected variable length array to be 16.");

                  -- Now load the arguments in one at a time. An assertion
                  Args (Args'First + 0)   := (Value => Packed_U32.Serialization.To_Byte_Array ((Value => Arg.Argument_01)));
                  Args (Args'First + 1)   := (Value => Packed_U32.Serialization.To_Byte_Array ((Value => Arg.Argument_02)));
                  Args (Args'First + 2)   := (Value => Packed_U32.Serialization.To_Byte_Array ((Value => Arg.Argument_03)));
                  Args (Args'First + 3)   := (Value => Packed_U32.Serialization.To_Byte_Array ((Value => Arg.Argument_04)));
                  Args (Args'First + 4)   := (Value => Packed_U32.Serialization.To_Byte_Array ((Value => Arg.Argument_05)));
                  Args (Args'First + 5)   := (Value => Packed_U32.Serialization.To_Byte_Array ((Value => Arg.Argument_06)));
                  Args (Args'First + 6)   := (Value => Packed_U32.Serialization.To_Byte_Array ((Value => Arg.Argument_07)));
                  Args (Args'First + 7)   := (Value => Packed_U32.Serialization.To_Byte_Array ((Value => Arg.Argument_08)));
                  Args (Args'First + 8)   := (Value => Packed_U32.Serialization.To_Byte_Array ((Value => Arg.Argument_09)));
                  Args (Args'First + 9)   := (Value => Packed_U32.Serialization.To_Byte_Array ((Value => Arg.Argument_10)));
                  Args (Args'First + 10) := (Value => Packed_U32.Serialization.To_Byte_Array ((Value => Arg.Argument_11)));
                  Args (Args'First + 11) := (Value => Packed_U32.Serialization.To_Byte_Array ((Value => Arg.Argument_12)));
                  Args (Args'First + 12) := (Value => Packed_U32.Serialization.To_Byte_Array ((Value => Arg.Argument_13)));
                  Args (Args'First + 13) := (Value => Packed_U32.Serialization.To_Byte_Array ((Value => Arg.Argument_14)));
                  Args (Args'First + 14) := (Value => Packed_U32.Serialization.To_Byte_Array ((Value => Arg.Argument_15)));
                  Args (Args'First + 15) := (Value => Packed_U32.Serialization.To_Byte_Array ((Value => Arg.Argument_16)));

                  -- Load arguments into the destination engine:
                  Engine.Set_Arguments (Args);

                  -- Send out info event
                  Self.Event_T_Send_If_Connected (Self.Events.Loaded_Engine_Arguments (Self.Sys_Time_T_Get, (Engine_Id => Engine.Get_Engine_Id)));
               end;
            else
               -- Send out info event if no engine is available for loading.
               Self.Event_T_Send_If_Connected (Self.Events.Unable_To_Load_Engine_Arguments (
                  Self.Sys_Time_T_Get, (Engine_Id => Engine.Get_Engine_Id, Engine_State => Engine.Get_Engine_State)
               ));
               return Failure;
            end if;
         end;
      else
         -- Send event
         Self.Event_T_Send_If_Connected (Self.Events.Invalid_Engine_Id (Self.Sys_Time_T_Get, (Engine_Id => Arg.Engine_Id)));
         return Failure;
      end if;

      return Success;
   end Set_Engine_Arguments;

   -- Invalid command handler. This procedure is called when a command's arguments are found to be invalid:
   overriding procedure Invalid_Command (Self : in out Instance; Cmd : in Command.T; Errant_Field_Number : in Unsigned_32; Errant_Field : in Basic_Types.Poly_Type) is
   begin
      -- Throw event:
      Self.Event_T_Send_If_Connected (Self.Events.Invalid_Command_Received (
         Self.Sys_Time_T_Get,
         (Id => Cmd.Header.Id, Errant_Field_Number => Errant_Field_Number, Errant_Field => Errant_Field)
      ));
   end Invalid_Command;

end Component.Command_Sequencer.Implementation;
