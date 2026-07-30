-- Child package declared with an aspect specification. The aspect pushes the
-- "is" keyword onto its own line, which is the shape that used to break the
-- build system's implicit parent-package dependency extraction: the parent's
-- directory never made it onto the source path and gprbuild failed to find
-- Aspect_Parent. This test compiles only when that extraction handles a
-- multi-line package declaration.
package Aspect_Parent.Child
   with SPARK_Mode => On
is

   function Doubled_Base return Natural;

end Aspect_Parent.Child;
