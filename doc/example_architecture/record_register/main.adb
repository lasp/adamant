with Example_Register;
with System.Storage_Elements; use System.Storage_Elements;

procedure Main is
   use Example_Register;
   -- Define a hardware register on a little endian machine:
   Reg : Example_Register.Register_T_Le;
   for Reg'Address use To_Address (Integer_Address (16#0060_0014#));
   -- Define a register copy which is of the normal type:
   Reg_Copy : Example_Register.T_Le;
begin
   -- Read entire register atomically:
   Reg_Copy := T_Le (Reg);

   -- Write entire register atomically:
   Reg_Copy.Threshold := 17;
   Reg := Register_T_Le (Reg_Copy);

   -- Read and write register but only access certain components. The
   -- compiler will ensure that the ENTIRE register is read/written
   -- during the folling operations.
   if Reg.Hw_1_Enabled = Enable and then Reg.Hw_2_Enabled = Enable then
      Reg.Threshold := 22;
   end if;
end Main;
