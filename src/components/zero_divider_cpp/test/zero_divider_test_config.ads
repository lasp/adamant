-- Whether integer division by zero traps on this target. The hosted
-- Linux runtime turns the hardware fault into CONSTRAINT_ERROR. Some
-- bareboard targets define integer division by zero to return a
-- value rather than trap (RISC-V, for example), so there is no
-- exception to observe. Target-selected body.
package Zero_Divider_Test_Config is

   function Integer_Division_Traps return Boolean;

end Zero_Divider_Test_Config;
