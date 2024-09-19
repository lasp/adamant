
package body Spark_Test
   with SPARK_Mode => On
is

   procedure Increment (X : in out Integer) is
   begin
      X := @ + 1;
   end Increment;

end Spark_Test;
