-- Bareboard runtime does not expose Ada.Task_Termination, so the
-- termination handler is a no-op. Abnormally terminating tasks will
-- rely on the runtime's default fault handling.
package body Unit_Test_Termination_Handler is
end Unit_Test_Termination_Handler;
