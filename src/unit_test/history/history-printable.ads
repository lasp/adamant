generic
   -- Function which returns string representation of item of type T
   with function Image (Item : in T) return String;
package History.Printable is

   type Printable_Instance is new History.Instance with null record;

   -- Add to string function:
   function To_String (Self : in out Printable_Instance; Index : in Positive) return String;
   -- Specify positive index to print a single item, else the entire
   -- history is printed with Put_Line.
   procedure Print (Self : in out Printable_Instance; Index : in Natural := 0);

end History.Printable;
