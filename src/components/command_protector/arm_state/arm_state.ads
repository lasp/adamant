with Command_Protector_Enums;
with Packed_Arm_Timeout;

-- Package which implements a protected "Arm" state with timeout. This is broken out
-- in case other components besides the command protector want to use this functionality.
package Arm_State is

   -- Protected type which stores the component's arm state and timeout.
   protected type Protected_Arm_State is
      --
      -- Functions that provide read-only access to the private data:
      --
      function Get_State (The_Timeout : out Packed_Arm_Timeout.Arm_Timeout_Type) return Command_Protector_Enums.Armed_State.E;

      --
      -- Procedures requiring full mutual exclusion:
      --
      -- Arm the system and provide a timeout value:
      procedure Arm (New_Timeout : in Packed_Arm_Timeout.Arm_Timeout_Type);
      -- Unarm the system and cancel the timeout:
      procedure Unarm;
      -- Decrement the timeout, and transition to the unarmed state if the
      -- timeout has expired.
      procedure Decrement_Timeout (Timeout_Val : out Packed_Arm_Timeout.Arm_Timeout_Type; New_State : out Command_Protector_Enums.Armed_State.E; Timed_Out : out Boolean);

   private
      State : Command_Protector_Enums.Armed_State.E := Command_Protector_Enums.Armed_State.Unarmed;
      Timeout : Packed_Arm_Timeout.Arm_Timeout_Type := Packed_Arm_Timeout.Arm_Timeout_Type'First;
   end Protected_Arm_State;

end Arm_State;
