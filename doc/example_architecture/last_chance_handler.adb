-- Use the low level GNAT.IO package since the system is in a precarious state.
with GNAT.IO;
with System.Storage_Elements;

package body Last_Chance_Handler is

   procedure Last_Chance_Handler (Msg : System.Address; Line : Integer) is
      use System.Storage_Elements;
      function Peek (Addr : System.Address) return Character is
         C : Character with
           Address => Addr;
      begin
         return C;
      end Peek;
      A : System.Address := Msg;
   begin
      GNAT.IO.Put ("LCH called => ");
      while Peek (A) /= Ascii.Nul loop
         GNAT.IO.Put (Peek (A));
         A := @ + 1;
      end loop;
      GNAT.IO.Put (": ");
      GNAT.IO.Put (Line); -- avoid the secondary stack for Line'Image
      GNAT.IO.New_Line;

       -- Spin indefinitely, don't use sleep
       -- as that can cause a fault.
      loop
         null;
      end loop;
   end Last_Chance_Handler;

end Last_Chance_Handler;
