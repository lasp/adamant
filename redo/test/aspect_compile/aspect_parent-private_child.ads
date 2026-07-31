-- A private child package. The leading "private" keyword used to prevent the
-- build system from recognizing this as a package declaration at all, so the
-- implicit dependency on the parent was never recorded.
--
-- Deliberately body-less: every declaration in this directory must be one the
-- old extraction could not parse. A single plain "package body X.Y is" here
-- would recover the parent dependency on its own and mask the regression this
-- directory exists to catch.
private package Aspect_Parent.Private_Child is

   Tripled_Base : constant Natural := 3 * Base_Value;

end Aspect_Parent.Private_Child;
