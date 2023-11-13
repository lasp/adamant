with Ada.Text_IO; use Ada.Text_IO;
with Seq_Runtime.Decoder;
with GNAT.Strings; use GNAT.Strings;
with GNAT.Command_Line; use GNAT.Command_Line;

procedure Main is
   Runtime : Seq_Runtime.Decoder.Decoder_Instance;
   Config : Command_Line_Configuration;
   Config_Filename_Access : aliased String_Access := null;
   Ignore : aliased String_Access := null;
begin
   -- Define the command line options:
   Define_Switch (Config, Config_Filename_Access'Access, "-c:", Long_Switch => "--config:", Help => "The sequence builder configuration file that defines commands and telemetry.");
   Define_Switch (Config, Ignore'Access, "-d:", Help => "This switch is unused, but used to maintain compatibility with the original SEQ implementation.");
   Getopt (Config);

   declare
      -- Get the filename from the command line:
      Seq_Filename : constant String := Get_Argument;
   begin
      if Config_Filename_Access = null then
         Seq_Runtime.Decoder.Decode (Self => Runtime, Path => Seq_Filename, Config_Path => "", Output => Standard_Output);
      else
         Seq_Runtime.Decoder.Decode (Self => Runtime, Path => Seq_Filename, Config_Path => Config_Filename_Access.all, Output => Standard_Output);
      end if;
   end;
end Main;
