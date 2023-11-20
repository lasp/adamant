package AUnit.Reporter.Null_Reporter is
   type My_Null_Reporter is new AUnit.Reporter.Reporter with null record;

   overriding procedure Report (Engine : My_Null_Reporter; R : in out Result'Class; Some_Options : AUnit_Options := Default_Options) is null;
end AUnit.Reporter.Null_Reporter;
