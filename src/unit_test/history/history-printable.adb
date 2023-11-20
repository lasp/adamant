with AUnit.Assertions; use AUnit.Assertions;
with Ada.Text_IO; use Ada.Text_IO;

package body History.Printable is

   function To_String (Self : in out Printable_Instance; Index : in Positive) return String is
   begin
      Assert (Index <= Self.Get_Count, "There is no value stored in the history at index " & Positive'Image (Index) & ". You should check the history count before fetching values.");
      return Image (Self.Get (Index));
   end To_String;

   procedure Print (Self : in out Printable_Instance; Index : in Natural := 0) is
   begin
      if Index < 1 then
         if Self.Get_Count = 0 then
            Put_Line ("History is empty.");
         else
            for Idx in 1 .. Self.Get_Count loop
               Put_Line (Natural'Image (Idx) & " => " & Self.To_String (Idx));
            end loop;
         end if;
      else
         Put_Line (Natural'Image (Index) & " => " & Self.To_String (Index));
      end if;
   end Print;

end History.Printable;
