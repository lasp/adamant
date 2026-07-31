package body Aspect_Parent.Child
   with SPARK_Mode => On
is

   function Doubled_Base return Natural is (2 * Base_Value);

end Aspect_Parent.Child;
