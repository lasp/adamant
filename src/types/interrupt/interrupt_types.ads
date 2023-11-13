with Ada.Interrupts;

-- This package contains data types related to Adamant interrupts
package Interrupt_Types is

   type Interrupt_Id_List is array (Natural range <>) of Ada.Interrupts.Interrupt_ID;
   type Interrupt_Id_List_Access is access all Interrupt_Id_List;

end Interrupt_Types;
