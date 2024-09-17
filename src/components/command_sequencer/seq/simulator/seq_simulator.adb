with Ada.Sequential_IO;
with Ada.IO_Exceptions;
with Seq_Enums; use Seq_Enums;
with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings; use Ada.Strings;
with Command.Representation;
with Telemetry_Record;
with Packed_U32;
with Seq_Print.Representation;
with Sys_Time;

package body Seq_Simulator is
   use Seq_Error;
   use Seq_Execute_State;
   use Sys_Time;

   -- We can handle up to 512 KB sized sequence.
   Max_Sequence_Size : constant Natural := 524_288;

   function Initialize (Self : in out Instance; Num_Engines : in Sequence_Engine_Id; Stack_Size : in Max_Seq_Num; Start_Source_Id : in Command_Source_Id) return Boolean is
      The_Source_Id : Command_Source_Id := Start_Source_Id;
   begin
      Self.Seq_Engines := new Seq_Engine_Array (Sequence_Engine_Id'First .. Sequence_Engine_Id'First + Num_Engines - 1);

      for Id in Self.Seq_Engines.all'Range loop
         -- Provide engine its stack size and engine identifier:
         Self.Seq_Engines (Id).Initialize (Stack_Depth => Stack_Size, Engine_Id => Id);
         Self.Seq_Engines (Id).Set_Source_Id (The_Source_Id);
         The_Source_Id := @ + 1;
      end loop;
      return True;
   exception
      when others =>
         return False;
   end Initialize;

   function Load_Sequence_In_Memory (Path : in String; Buffer : in Basic_Types.Byte_Array_Access; Sequence : out Memory_Region.T) return Boolean is
      package Io is new Ada.Sequential_Io (Basic_Types.Byte);
      use Io;
      File : Io.File_Type;
      Data : Basic_Types.Byte;
      Sequence_Size : Natural := 0;
   begin
      Open (File, In_File, Path);
      while not End_Of_File (File) loop
         Read (File, Data);
         Buffer (Sequence_Size) := Data;
         Sequence_Size := @ + 1;
      end loop;
      Sequence := (Address => (Buffer (0)'Address), Length => Sequence_Size);
      Close (File);
      return True;
   exception
      when Ada.IO_Exceptions.Name_Error =>
         return False;
   end Load_Sequence_In_Memory;

   procedure Simulate (Self : in out Instance; Filepath : in String; To_Load : in Sequence_Engine_Id; Engine_Time_S : in Interfaces.Unsigned_32) is
      Sequence : Memory_Region.T;
      Buffer : Basic_Types.Byte_Array_Access;
      Load_State : Load_Status;
      Exec_State : Seq_Execute_State.E;
      Time : Sys_Time.T := (Seconds => Engine_Time_S, Subseconds => 0);
      Instr_Limit : constant Positive := 10_000;
      Pc : Seq_Position;
   begin
      Buffer := new Basic_Types.Byte_Array (0 .. Max_Sequence_Size - 1);

      -- Attempt to load a sequence into a memory region
      if Load_Sequence_In_Memory (Filepath, Buffer, Sequence) = False then
         Put_Line ("Could not load given sequence: " & Filepath);
         return;
      end if;
      Put_Line ("Sequence was loaded into memory!");
      New_Line;

      Load_State := Self.Seq_Engines (To_Load).Load (Sequence);
      Put_Line ("Engine loaded with state: " & Load_State'Image);
      New_Line;
      while True loop
         Exec_State := Self.Seq_Engines (To_Load).Execute (Instr_Limit, Time);
         Pc := Self.Seq_Engines (To_Load).Get_Lowest_Child_Position;
         Put_Line ("[" & Trim (Pc'Image, Ada.Strings.Left) & "] Engine Id:" & To_Load'Image & ", Stack:" & Self.Seq_Engines (To_Load).Get_Stack_Level'Image & " executed into state: " & Trim (Exec_State'Image, Ada.Strings.Left));
         case Exec_State is
            when Unloaded =>
               Put_Line ("Simulation for engine" & To_Load'Image & " has finished!");
               New_Line;
               return;
            when Wait_Relative =>
               Put_Line (ASCII.HT & "Waiting for: " & Trim (Self.Seq_Engines (To_Load).Get_Wakeup_Time.Seconds'Image, Ada.Strings.Left));
               Put_Line (ASCII.HT & "Using current system time to transform to absolute wait!");
               Self.Seq_Engines (To_Load).Change_Relative_Wait_To_Absolute (Time);
               New_Line;
            when Wait_Absolute =>
               Put_Line (ASCII.HT & "Waiting until: " & Trim (Self.Seq_Engines (To_Load).Get_Wakeup_Time.Seconds'Image, Ada.Strings.Left));
               Put (ASCII.HT & "Enter current time >>> ");
               declare
                  New_Time : constant Sys_Time.T := (Interfaces.Unsigned_32'Value (Get_Line), 0);
               begin
                  if Self.Seq_Engines (To_Load).Is_Done_Waiting (New_Time) = Done then
                     Put_Line (ASCII.HT & "Sequence woke up! Updating system time!");
                     Time.Seconds := New_Time.Seconds;
                     New_Line;
                  else
                     Put_Line (ASCII.HT & "Sequence didn't wake up!");
                     New_Line;
                  end if;
               end;
            when Wait_Command =>
               Put_Line (Command.Representation.To_Tuple_String (Self.Seq_Engines (To_Load).Get_Command));
               Put ("Please acknowledge this command is as expected!");
               declare
                  Ignore : constant String := Get_Line;
               begin
                  New_Line;
               end;
            when Wait_Telemetry =>
               declare
                  use Seq_Runtime_State;
                  Tlm : constant Telemetry_Record.T := Self.Seq_Engines (To_Load).Get_Telemetry_Request;
                  In_Value : Interfaces.Unsigned_32;
                  The_Timeout : constant Sys_Time.T := Self.Seq_Engines (To_Load).Get_Telemetry_Timeout;
                  State : constant Seq_Enums.Seq_Runtime_State.E := Self.Seq_Engines (To_Load).Get_Running_Sequence_State;
               begin
                  Put_Line (ASCII.HT & "Requested Id: " & Trim (Tlm.Id'Image, Ada.Strings.Left));
                  Put_Line (ASCII.HT & "Requested Offset: " & Trim (Tlm.Offset'Image, Ada.Strings.Left));
                  Put_Line (ASCII.HT & "Requested Size: " & Trim (Tlm.Size'Image, Ada.Strings.Left));
                  Put_Line (ASCII.HT & "Is a new value required?: " & Trim (Tlm.New_Value_Required'Image, Ada.Strings.Left));

                  Put (ASCII.HT & "Enter a telemetry value >>> ");
                  In_Value := Interfaces.Unsigned_32'Value (Get_Line);
                  declare
                     Tlm_Record : Packed_U32.T := (Value => In_Value);
                     Tlm_Value : Basic_Types.Poly_32_Type with Import, Convention => Ada, Address => Tlm_Record'Address;
                  begin
                     Self.Seq_Engines (To_Load).Set_Telemetry (Tlm_Value);
                  end;

                  if State = Wait_Telemetry_Value then
                     Put_Line (ASCII.HT & "Timeout at: " & Trim (The_Timeout.Seconds'Image, Ada.Strings.Left));
                     Put (ASCII.HT & "Enter time for this telemetry value >>> ");
                     Time.Seconds := Interfaces.Unsigned_32'Value (Get_Line);
                     New_Line;
                  else
                     Put_Line (ASCII.HT & "No timeout on this action.");
                     New_Line;
                  end if;
               end;
            when Set_Telemetry =>
               Put_Line (ASCII.HT & "TODO not yet implemented in sim.");
            when Wait_Telemetry_Relative =>
               Put_Line (ASCII.HT & "Using current system time to transform to absolute timeout!");
               Self.Seq_Engines (To_Load).Change_Relative_Timeout_To_Absolute (Time);
               New_Line;
            when Wait_Load_Seq =>
               Put_Line (ASCII.HT & "Requesting to load sequence: " & Trim (Self.Seq_Engines (To_Load).Get_Seq_To_Load'Image, Ada.Strings.Left));
               Put_Line (ASCII.HT & "Attempting load into engine: " & Trim (Self.Seq_Engines (To_Load).Get_Load_Destination'Image, Ada.Strings.Left));

               Put (ASCII.HT & "Enter the sequence path to load >>> ");
               declare
                  File_Path : constant String := Get_Line;
               begin
                  -- If loading into another engine
                  if Self.Seq_Engines (To_Load).Get_Load_Destination /= To_Load then
                     if Self.Seq_Engines (To_Load).Get_Load_Destination = 255 then
                        Put (ASCII.HT & "Any engine load, please enter an engine to load into >>> ");
                        declare
                           New_Id : constant Sequence_Engine_Id := Sequence_Engine_Id'Value (Get_Line);
                        begin
                           New_Line;
                           Self.Seq_Engines (New_Id).Set_Arguments (Self.Seq_Engines (To_Load).Get_Arguments);
                           Self.Simulate (File_Path, New_Id, Time.Seconds);
                        end;

                     else
                        New_Line;
                        Self.Seq_Engines (Self.Seq_Engines (To_Load).Get_Load_Destination).Set_Arguments (Self.Seq_Engines (To_Load).Get_Arguments);
                        Self.Simulate (File_Path, Self.Seq_Engines (To_Load).Get_Load_Destination, Time.Seconds);
                     end if;

                  else
                     declare
                        New_Sequence : Memory_Region.T;
                        New_Buffer : Basic_Types.Byte_Array_Access;
                     begin
                        New_Buffer := new Basic_Types.Byte_Array (0 .. Max_Sequence_Size - 1);
                                                -- Attempt to load a sequence into a memory region
                        if Load_Sequence_In_Memory (File_Path, New_Buffer, New_Sequence) = False then
                           Put_Line (ASCII.HT & "Could not load given sequence: " & File_Path);
                           return;
                        end if;
                        Put_Line (ASCII.HT & "Sequence was loaded into memory!");

                        -- Load arguments
                        Self.Seq_Engines (Self.Seq_Engines (To_Load).Get_Load_Destination).Set_Arguments (Self.Seq_Engines (To_Load).Get_Arguments);

                        -- If loading into the same engine
                        Load_State := Self.Seq_Engines (To_Load).Load (New_Sequence);
                        Put_Line (ASCII.HT & "Engine loaded with state: " & Load_State'Image);
                        New_Line;
                     end;
                  end if;
               end;
            when Error =>
               Put_Line (ASCII.HT & "The runtime error code is: " & Trim (Self.Seq_Engines (To_Load).Get_Seq_Error_Code'Image, Ada.Strings.Left));
               Put_Line (ASCII.HT & "The error occurred at pc: " & Trim (Self.Seq_Engines (To_Load).Get_Lowest_Child_Position'Image, Ada.Strings.Left));
               New_Line;
               return;
            when Kill_Engines =>
               Put_Line (ASCII.HT & "Kill_Engine encountered.");
               New_Line;
               return;
            when Print =>
               Put_Line (ASCII.HT & "Print encountered.");
               Put_Line (Seq_Print.Representation.Image (Self.Seq_Engines (To_Load).Get_String_To_Print));
               New_Line;
         end case;
      end loop;
   end Simulate;
end Seq_Simulator;
