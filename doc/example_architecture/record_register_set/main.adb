with Example_Register;
with Example_Register_Set;
with System.Storage_Elements; use System.Storage_Elements;

procedure Main is
   use Example_Register;
   -- Define hardware register set:
   Registers : Example_Register_Set.Register_T_Le;
   for Registers'Address use To_Address (Integer_Address (16#0060_0014#));
   -- Define a register copy:
   Reg_Copy : Example_Register.T_Le;
begin
   -- Read the entire first register atomically:
   Reg_Copy := T_Le (Registers.Reg_1);

   -- Write entire register atomically:
   Reg_Copy.Threshold := 17;
   Registers.Reg_1 := Register_T_Le (Reg_Copy);

   -- Read and write registers but only access certain components. The
   -- compiler will ensure that the ENTIRE register is read/written
   -- during the following operations.
   if Registers.Reg_2.Hw_1_Enabled = Enable and then
      Registers.Reg_3.Hw_2_Enabled = Enable
   then
      Registers.Reg_1.Threshold := 22;
   end if;
end Main;
