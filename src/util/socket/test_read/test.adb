-- Tell the compiler that we are using Ravenscar
pragma Profile (Ravenscar);

with Ada.Text_IO; use Ada.Text_IO;
with Socket;
with Stream_Serializer;
with Ada.Command_Line; use Ada.Command_Line;

procedure Test is
   package Natural_Serializer is new Stream_Serializer (Natural);
   use Natural_Serializer;

   Mysocket : Socket.Instance;
   Data : Natural;
begin
   if Argument_Count >= 2 then
      Put ("Opening socket on " & Argument (1) & ":" & Argument (2) & "... ");
      Mysocket.Connect (Argument (1), Integer'Value (Argument (2)));
      pragma Assert (Mysocket.Is_Connected, "Socket not connected!");
      Put_Line ("passed.");

      Put_Line ("Receiving message... ");
      Deserialize (Mysocket.Stream, Data);
      Put_Line ("Received data: " & Natural'Image (Data));
      pragma Assert (Data = 1);
      Deserialize (Mysocket.Stream, Data);
      Put_Line ("Received data: " & Natural'Image (Data));
      pragma Assert (Data = 2);
      Deserialize (Mysocket.Stream, Data);
      Put_Line ("Received data: " & Natural'Image (Data));
      pragma Assert (Data = 3);
      Deserialize (Mysocket.Stream, Data);
      Put_Line ("Received data: " & Natural'Image (Data));
      pragma Assert (Data = 4);
      Deserialize (Mysocket.Stream, Data);
      Put_Line ("Received data: " & Natural'Image (Data));
      pragma Assert (Data = 5);
      Put_Line ("passed.");

      Put ("Closing socket... ");
      Mysocket.Disconnect;
      Put_Line ("passed.");
   else
      Put_Line (Current_Error, "Usage: test hostname/ip port_number");
      Set_Exit_Status (Failure);
   end if;
end Test;
