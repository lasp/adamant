package Spark_Test
   with SPARK_Mode => On
is

   procedure Increment (X : in out Integer)
      with Global   => null,                   -- Increment does not read or write any global variables.
           Depends => (X => X),             -- The value of the parameter X after the call depends only on the (previous) value of X.
           Pre       => X < Integer'Last, -- Increment is only allowed to be called if the value of X prior to the call is less than Integer'Last
           Post      => X = X'Old + 1;      -- The value of X after a call is one greater than its value before the call.

end Spark_Test;
