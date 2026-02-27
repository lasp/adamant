with AUnit.Assertions;
with Ada.Assertions;
with String_Util;
with Ada.Text_IO;

package body Smart_Assert is

   -------------------------------------------------
   -- Low Level generic procedure for code savings
   -------------------------------------------------
   -- Custom assertion method that works even outside the context of an AUnit unit test program.
   procedure Assert (Condition : in Boolean; Message : in String := ""; Filename : in String := Sinfo.File; Line : in Natural := Sinfo.Line) is
      use AUnit.Assertions;
      -- Function which prints a good assertion failure message. We will use this if AUnit is not currently being used.
      procedure Raise_Assertion_Failure is
         pragma Annotate (GNATSAS, Intentional, "subp always fails", "intentional assertion failure in test framework");
      begin
         if Message'Length > 0 then
            Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error, Message);
         end if;
         raise Ada.Assertions.Assertion_Error with " at " & Filename & ":" & String_Util.Trim_Both (Natural'Image (Line));
         pragma Annotate (GNATSAS, Intentional, "raise exception", "intentional assertion failure in test framework");
      end Raise_Assertion_Failure;
   begin
      -- If we are in an active AUnit session then use the AUnit assertions, otherwise just
      -- use our own. Using AUnit assert outside of an AUnit test can cause segmentation faults
      -- and other weirdness that we would like to avoid.
      if AUnit.Assertions.Current_Test = null then
         if not Condition then
            Raise_Assertion_Failure;
         end if;
      else
         AUnit.Assertions.Assert (Condition, Message, Filename, Line);
      end if;
   exception
      -- A storage error gets thrown when smart assert is used outside of
      -- the context of an AUnit powered unit test, and a test fails. I am
      -- not sure why, but it would be really nice if we could use this
      -- smart assert library anywhere. So let's catch the storage error
      -- and just display the error message ourselves.
      when Storage_Error =>
         Raise_Assertion_Failure;
   end Assert;

   -- Low level assertion function, used for implementing comparisons of complex types
   -- Assert function for code savings:
   procedure Call_Assert (Condition : in Boolean; T1 : in T; T2 : in T; Comparison : in String := "compared to"; Message : in String := ""; Filename : in String := Sinfo.File; Line : in Natural := Sinfo.Line) is
      Assert_Message : constant String := "Assertion: " & ASCII.LF & Image (T1) & ASCII.LF & Comparison & ASCII.LF & Image (T2) & ASCII.LF & "failed.";
   begin
      -- Call AUnit assert:
      if Message = "" then
         Assert (Condition, Assert_Message, Filename, Line);
      else
         Assert (Condition, Assert_Message & ASCII.LF & "Message: " & Message, Filename, Line);
      end if;

      -- If the type is out of range, it will fail to print by the assertion
      -- Image function due to a Constraint_Error. In this case, do something safer, but still
      -- alert the user of the failure.
   exception
      when Constraint_Error =>
         declare
            Safe_Assert_Message : constant String := "Assertion: " & ASCII.LF & "Item 1" & ASCII.LF & Comparison & ASCII.LF & "Item 2" & ASCII.LF & "failed due to either Item 1 or Item 2 issuing a Constraint_Error.";
         begin
            if Message = "" then
               Assert (False, Safe_Assert_Message, Filename, Line);
            else
               Assert (False, Safe_Assert_Message & ASCII.LF & "Message: " & Message, Filename, Line);
            end if;
         end;
         -- Make sure constraint error gets thrown if assertions are disabled and the program
         -- has not died yet.
         raise Constraint_Error;
   end Call_Assert;

   -------------------------------------------------
   -- Basic
   -------------------------------------------------
   package body Basic is
      procedure Basic_Assert is new Call_Assert (T, Image_Basic);

      procedure Eq (T1 : in T; T2 : in T; Message : in String := ""; Filename : in String := Sinfo.File; Line : in Natural := Sinfo.Line) is
      begin
         Basic_Assert (T1 = T2, T1, T2, "=", Message, Filename, Line);
      end Eq;

      procedure Neq (T1 : in T; T2 : in T; Message : in String := ""; Filename : in String := Sinfo.File; Line : in Natural := Sinfo.Line) is
      begin
         Basic_Assert (T1 /= T2, T1, T2, "/=", Message, Filename, Line);
      end Neq;
   end Basic;

   -------------------------------------------------
   -- Discrete
   -------------------------------------------------
   package body Discrete is
      procedure Discrete_Assert is new Call_Assert (T, Image_Discrete);

      procedure Eq (T1 : in T; T2 : in T; Message : in String := ""; Filename : in String := Sinfo.File; Line : in Natural := Sinfo.Line) is
      begin
         Basic_Assert_P.Eq (T1, T2, Message, Filename, Line);
      end Eq;

      procedure Neq (T1 : in T; T2 : in T; Message : in String := ""; Filename : in String := Sinfo.File; Line : in Natural := Sinfo.Line) is
      begin
         Basic_Assert_P.Neq (T1, T2, Message, Filename, Line);
      end Neq;

      procedure Gt (T1 : in T; T2 : in T; Message : in String := ""; Filename : in String := Sinfo.File; Line : in Natural := Sinfo.Line) is
      begin
         Discrete_Assert (T1 > T2, T1, T2, ">", Message, Filename, Line);
      end Gt;

      procedure Ge (T1 : in T; T2 : in T; Message : in String := ""; Filename : in String := Sinfo.File; Line : in Natural := Sinfo.Line) is
      begin
         Discrete_Assert (T1 >= T2, T1, T2, ">=", Message, Filename, Line);
      end Ge;

      procedure Lt (T1 : in T; T2 : in T; Message : in String := ""; Filename : in String := Sinfo.File; Line : in Natural := Sinfo.Line) is
      begin
         Discrete_Assert (T1 < T2, T1, T2, "<", Message, Filename, Line);
      end Lt;

      procedure Le (T1 : in T; T2 : in T; Message : in String := ""; Filename : in String := Sinfo.File; Line : in Natural := Sinfo.Line) is
      begin
         Discrete_Assert (T1 <= T2, T1, T2, "<=", Message, Filename, Line);
      end Le;
   end Discrete;

   -------------------------------------------------
   -- Float
   -------------------------------------------------
   package body Float is
      procedure Float_Assert is new Call_Assert (T, Image_Float);

      procedure Eq (T1 : in T; T2 : in T; Epsilon : in T := T'Small; Message : in String := ""; Filename : in String := Sinfo.File; Line : in Natural := Sinfo.Line) is
         Condition : constant Boolean := ((T1 + Epsilon) >= T2) and then ((T1 - Epsilon) <= T2);
      begin
         -- Need to check both sides of floating point value against an epsilon.
         Float_Assert (Condition, T1, T2, "= (with Epsilon => " & Image_Float (Epsilon) & ")", Message, Filename, Line);
      end Eq;

      procedure Neq (T1 : in T; T2 : in T; Epsilon : in T := T'Small; Message : in String := ""; Filename : in String := Sinfo.File; Line : in Natural := Sinfo.Line) is
         Condition : constant Boolean := ((T1 + Epsilon) < T2) or else ((T1 - Epsilon) > T2);
      begin
         -- Need to check both sides of floating point value against an epsilon.
         Float_Assert (Condition, T1, T2, "/= (with Epsilon => " & Image_Float (Epsilon) & ")", Message, Filename, Line);
      end Neq;

      procedure Gt (T1 : in T; T2 : in T; Message : in String := ""; Filename : in String := Sinfo.File; Line : in Natural := Sinfo.Line) is
      begin
         Float_Assert (T1 > T2, T1, T2, ">", Message, Filename, Line);
      end Gt;

      procedure Ge (T1 : in T; T2 : in T; Message : in String := ""; Filename : in String := Sinfo.File; Line : in Natural := Sinfo.Line) is
      begin
         Float_Assert (T1 >= T2, T1, T2, ">=", Message, Filename, Line);
      end Ge;

      procedure Lt (T1 : in T; T2 : in T; Message : in String := ""; Filename : in String := Sinfo.File; Line : in Natural := Sinfo.Line) is
      begin
         Float_Assert (T1 < T2, T1, T2, "<", Message, Filename, Line);
      end Lt;

      procedure Le (T1 : in T; T2 : in T; Message : in String := ""; Filename : in String := Sinfo.File; Line : in Natural := Sinfo.Line) is
      begin
         Float_Assert (T1 <= T2, T1, T2, "<=", Message, Filename, Line);
      end Le;
   end Float;

end Smart_Assert;
