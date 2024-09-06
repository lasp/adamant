with Depends;

package body Spark_Fail
   with SPARK_Mode => On
is

   procedure Increment (X : in out Integer) is
   begin
      X := @ + 1 + Depends.Global_Var;
   end Increment;

end Spark_Fail;
