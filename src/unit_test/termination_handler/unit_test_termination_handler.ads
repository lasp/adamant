-- Tasks that terminate normally do so silently. The Linux/ body installs
-- a fallback handler (via Ada.Task_Termination) that prints diagnostics
-- for abnormal or unhandled-exception terminations. The bareboard runtime
-- does not expose Cause_Of_Termination, so its body is a no-op.
package Unit_Test_Termination_Handler is
   pragma Elaborate_Body;
end Unit_Test_Termination_Handler;
