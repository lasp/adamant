package body Spark_Package with
   SPARK_Mode => On
is

   procedure Increment (X : in out Integer) is
   begin
      X := @ + 1;
   end Increment;

end Spark_Package;
