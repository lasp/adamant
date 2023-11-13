with Ada.Text_IO; use Ada.Text_IO;
with Seq_Simulator; use Seq_Simulator;
with Seq_Types; use Seq_Types;
with GNAT.Strings; use GNAT.Strings;
with GNAT.Command_Line; use GNAT.Command_Line;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Command_Types; use Command_Types;
with Interfaces;

procedure Main is
   Simulator : Seq_Simulator.Instance;
   Config : Command_Line_Configuration;
   Config_Filename_Access : aliased String_Access := null;
   Ignore : aliased String_Access := null;
   Num_Engines : aliased Integer := 1;
   Stack_Depth : aliased Integer := 1;
   Initial_Load : aliased Integer := 0;
   Start_Source_Id : aliased Integer := 0;
   Start_Time : aliased Integer := 0;

   function Strip (In_String : in String) return String is
   begin
      return Trim (In_String, Ada.Strings.Both);
   end Strip;
begin
   -- Define the command line options:
   Define_Switch (Config, Num_Engines'Access, "-n:", Long_Switch => "--num_engines:", Help => "The number of sequence engines to include in the simulator (default 1).");
   Define_Switch (Config, Stack_Depth'Access, "-s:", Long_Switch => "--stack_depth:", Help => "The stack depth to include in each simulator sequence engine (default 1).");
   Define_Switch (Config, Initial_Load'Access, "-e:", Long_Switch => "--engine_to_load:", Help => "The engine to load the sequence into (default 0).");
   Define_Switch (Config, Start_Source_Id'Access, "-i:", Long_Switch => "--start_source_id:", Help => "The engine command srouce ID to load into the first engine (default 0). Each subsequent engine will have an incremented command source ID starting at this value.");
   Define_Switch (Config, Start_Time'Access, "-t:", Long_Switch => "--start_time:", Help => "The simulator absolute start time in seconds (default 0).");
   Define_Switch (Config, Config_Filename_Access'Access, "-c:", Long_Switch => "--config:", Help => "The sequence builder configuration file that defines commands and telemetry.");
   Define_Switch (Config, Ignore'Access, "-d:", Help => "This switch is unused, but used to maintain compatibility with the original SEQ implementation.");
   Getopt (Config);

   -- Bound commandline inputs:
   if Num_Engines < 1 then
      Num_Engines := 1;
   end if;
   if Num_Engines > Integer (Sequence_Engine_Id'Last) then
      Num_Engines := Integer (Sequence_Engine_Id'Last);
   end if;

   if Stack_Depth < 1 then
      Stack_Depth := 1;
   end if;
   if Stack_Depth > Integer (Max_Seq_Num'Last) then
      Stack_Depth := Integer (Max_Seq_Num'Last);
   end if;

   if Initial_Load < 0 then
      Initial_Load := 0;
   end if;
   if Initial_Load > Num_Engines - 1 then
      Initial_Load := Num_Engines - 1;
   end if;

   if Start_Source_Id < 0 then
      Start_Source_Id := 0;
   end if;
   if Start_Source_Id > Integer (Command_Source_Id'Last) then
      Start_Source_Id := Integer (Command_Source_Id'Last);
   end if;

   if Start_Time < 0 then
      Start_Time := 0;
   end if;

   -- Initialize the simulator:
   Put_Line ("Initializing simulator with " & Strip (Num_Engines'Image) & " sequence engines, each with a stack depth of " & Strip (Stack_Depth'Image) & ". The starting command source ID is " & Strip (Start_Source_Id'Image) & ".");
   if not Simulator.Initialize (Sequence_Engine_Id (Num_Engines), Max_Seq_Num (Stack_Depth), Command_Source_Id (Start_Source_Id)) then
      Put_Line ("An error occured initializing the engines!");
      return;
   end if;

   declare
      -- Get the filename from the command line:
      Seq_Filename : constant String := Get_Argument;
   begin
      Put_Line ("Loading sequence " & Seq_Filename & " into engine " & Strip (Initial_Load'Image) & " at time " & Strip (Start_Time'Image) & ".");
      Simulator.Simulate (Seq_Filename, Sequence_Engine_Id (Initial_Load), Interfaces.Unsigned_32 (Start_Time));
   end;

   pragma Unreferenced (Simulator);
end Main;
