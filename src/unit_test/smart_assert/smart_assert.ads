with GNAT.Source_Info;

package Smart_Assert is
   package Sinfo renames GNAT.Source_Info;

   -- Generic assertion function. Under the hood it calls the AUnit assertion method, but
   -- in a way that makes it safe to use outside the context of an AUnit driven unit test.
   procedure Assert (Condition : in Boolean; Message : in String := ""; Filename : in String := Sinfo.File; Line : in Natural := Sinfo.Line);

   -- Basic smart assert for comparing any type
   generic
      -- Generic type for which to use assertions on
      type T (<>) is private;
      -- Function which returns string representation of item of type T
      with function Image_Basic (Item : in T) return String;
   package Basic is
      -- Public assertions methods which pretty print failures:
      procedure Eq (T1 : in T; T2 : in T; Message : in String := ""; Filename : in String := Sinfo.File; Line : in Natural := Sinfo.Line);
      procedure Neq (T1 : in T; T2 : in T; Message : in String := ""; Filename : in String := Sinfo.File; Line : in Natural := Sinfo.Line);
   end Basic;

   -- Smart assert for comparing discrete types (integer, modular, or enumeration)
   -- Useful when you need to use the >, >=, <, and <= operators.
   generic
      -- Generic type for which to use assertions on
      type T is (<>);
      -- Function which returns string representation of item of type T
      with function Image_Discrete (Item : in T) return String;
   package Discrete is

      package Basic_Assert_P is new Basic (T, Image_Discrete);

      -- Public assertions methods which pretty print failures:
      procedure Eq (T1 : in T; T2 : in T; Message : in String := ""; Filename : in String := Sinfo.File; Line : in Natural := Sinfo.Line);
      procedure Neq (T1 : in T; T2 : in T; Message : in String := ""; Filename : in String := Sinfo.File; Line : in Natural := Sinfo.Line);
      procedure Gt (T1 : in T; T2 : in T; Message : in String := ""; Filename : in String := Sinfo.File; Line : in Natural := Sinfo.Line);
      procedure Ge (T1 : in T; T2 : in T; Message : in String := ""; Filename : in String := Sinfo.File; Line : in Natural := Sinfo.Line);
      procedure Lt (T1 : in T; T2 : in T; Message : in String := ""; Filename : in String := Sinfo.File; Line : in Natural := Sinfo.Line);
      procedure Le (T1 : in T; T2 : in T; Message : in String := ""; Filename : in String := Sinfo.File; Line : in Natural := Sinfo.Line);
   end Discrete;

   -- Smart assert for comparing floating point types (integer, modular, or enumeration)
   -- Useful when you need to use the >, >=, <, and <= operators.
   -- Note: The = and /= operators take an optional epsilon value which will make sure the
   -- values are equal (or not equal) within the bounds of the provided epsilon. If not provided
   -- the epsilon value is assumed to be the smallest possible representable positive floating
   -- point value for that type.
   generic
      -- Generic type for which to use assertions on
      type T is digits <>;
      -- Function which returns string representation of item of type T
      with function Image_Float (Item : in T) return String;
   package Float is
      package Basic_Assert_P is new Basic (T, Image_Float);
      -- Public assertions methods which pretty print failures:
      procedure Eq (T1 : in T; T2 : in T; Epsilon : in T := T'Small; Message : in String := ""; Filename : in String := Sinfo.File; Line : in Natural := Sinfo.Line);
      procedure Neq (T1 : in T; T2 : in T; Epsilon : in T := T'Small; Message : in String := ""; Filename : in String := Sinfo.File; Line : in Natural := Sinfo.Line);
      procedure Gt (T1 : in T; T2 : in T; Message : in String := ""; Filename : in String := Sinfo.File; Line : in Natural := Sinfo.Line);
      procedure Ge (T1 : in T; T2 : in T; Message : in String := ""; Filename : in String := Sinfo.File; Line : in Natural := Sinfo.Line);
      procedure Lt (T1 : in T; T2 : in T; Message : in String := ""; Filename : in String := Sinfo.File; Line : in Natural := Sinfo.Line);
      procedure Le (T1 : in T; T2 : in T; Message : in String := ""; Filename : in String := Sinfo.File; Line : in Natural := Sinfo.Line);
   end Float;

   -- Low level assertion function, used for implementing comparisons of complex types
   -- Assert function for code savings:
   generic
      type T (<>) is private;
      -- Function which returns string representation of item of type T
      with function Image (Item : in T) return String;
   procedure Call_Assert (Condition : in Boolean; T1 : in T; T2 : in T; Comparison : in String := "compared to"; Message : in String := ""; Filename : in String := Sinfo.File; Line : in Natural := Sinfo.Line);

end Smart_Assert;
