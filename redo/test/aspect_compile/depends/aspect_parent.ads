-- Parent package for the aspect_compile test. It lives in a separate
-- directory so that it is only found if the build system correctly extracts
-- the implicit parent dependency from the child package declaration below.
package Aspect_Parent is

   Base_Value : constant Natural := 17;

end Aspect_Parent;
