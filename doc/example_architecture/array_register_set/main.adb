with Example_Register;
with Example_Packed_Register_Array;
with System.Storage_Elements; use System.Storage_Elements;

procedure Main is
   use Example_Register;
   -- Define hardware register array:
   Registers : Example_Packed_Register_Array.Register_T_Le;
   for Registers'Address use To_Address (Integer_Address (16#0070_0014#));
   -- Define register copy:
   Reg_Copy : Example_Register.T_Le;
begin
   -- Read the entire third register atomically:
   Reg_Copy := T_Le (Registers (3));

   -- Write entire register fifth atomically:
   Registers (5) := Register_T_Le (Reg_Copy);

   -- Read and write registers but only access certain components. The
   -- compiler will ensure that the ENTIRE register is read/written
   -- during the following operations.
   if Registers (1).Hw_1_Enabled = Enable and then
      Registers (4).Hw_2_Enabled = Enable
   then
      Registers (7).Threshold := 22;
   end if;
end Main;
