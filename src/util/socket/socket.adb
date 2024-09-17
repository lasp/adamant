with Interfaces.C;
pragma Unreferenced (Interfaces.C);
with Ada.Task_Identification; use Ada.Task_Identification;
with Ada.Exceptions;

package body Socket is

   -- Function which returns True if the passed in string looks like
   -- a standard IPV4 IP address.
   function Is_Ip_Address (Addr : in String) return Boolean is
      Num_Periods : Natural := 0;
      Last_Char : Character := '.';
   begin
      for Char of Addr loop
         if Char in '0' .. '9' then
            null;
         elsif Char = '.' then
            if Last_Char = '.' then
               return False;
            end if;
            Num_Periods := @ + 1;
         else
            return False;
         end if;
         Last_Char := Char;
      end loop;

      if Num_Periods = 3 and then Last_Char /= '.' then
         return True;
      else
         return False;
      end if;
   end Is_Ip_Address;

   procedure Connect (Self : in out Instance; Addr : in String; Port : in Natural) is
      use Ada.Exceptions;
      Sock : Socket_Type;
      Channel : Stream_Access;
   begin
      Self.Connected := False;
      if Is_Ip_Address (Addr) then
         -- Convert an IP address string like "192.168.0.122"
         Self.Address.Addr := Inet_Addr (Addr);
      else
         -- Resolve a hostname like "www.google.com"
         Self.Address.Addr := Addresses (Get_Host_By_Name (Addr), 1);
      end if;
      Self.Address.Port := Port_Type (Port);

      -- Create the socket:
      Create_Socket (Sock, Family_Inet, Socket_Stream);

      -- Allow reuse of local addresses:
      Set_Socket_Option (Sock, Socket_Level, (Reuse_Address, True));

      -- Connect the socket:
      Connect_Socket (Sock, Self.Address);

      -- Setup send channel:
      Channel := Stream (Sock);

      -- Set the object members:
      Self.Socket := Sock;
      Self.Channel := Channel;
      Self.Connected := True;
   exception
      when Socket_Error | Host_Error =>
         Self.Connected := False;
      when E : others =>
         pragma Assert (False, "Unhandled exception occurred while connecting to socket in " &
           Image (Current_Task) & ASCII.LF & Exception_Name (E) & ": " & Exception_Message (E));
   end Connect;

   procedure Disconnect (Self : in out Instance) is
   begin
      Self.Connected := False;
      Close_Socket (Self.Socket);
   exception
      when Socket_Error => null;
   end Disconnect;

   function Is_Connected (Self : in Instance) return Boolean is
   begin
      return Self.Connected;
   end Is_Connected;

   function Get_Port (Self : in Instance) return Port_Type is
   begin
      return Self.Address.Port;
   end Get_Port;

   function Get_Ip_Address (Self : in Instance) return Inet_Addr_V4_Type is
   begin
      return Self.Address.Addr.Sin_V4;
   end Get_Ip_Address;

   function Stream (Self : in Instance) return Stream_Access is
   begin
      return Self.Channel;
   end Stream;

end Socket;
