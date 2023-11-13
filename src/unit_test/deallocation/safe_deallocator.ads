-- Like Ada.Unchecked_Deallocation, but only works on targets that support it. On
-- bareboard targets, it does NOT deallocate memory. It instead does exactly nothing.
package Safe_Deallocator is

   -- Disabling some warnings that appear for the production implementation which is essentially
   -- a "null procedure".
   pragma Warnings (Off, "formal parameter ""X"" is not modified");
   pragma Warnings (Off, "mode could be ""in"" instead of ""in out""");

   --
   -- This procedure mirrors the definition of Ada.Unchecked_Deallocation. It is designed
   -- to call Ada.Unchecked_Deallocation on testing targets where deallocation is allowed,
   -- but is implemented as a null procedure on deployment targets where deallocation is
   -- not allowed, and likely "pragma Restrictions (No_Unchecked_Deallocation)" is enforced.
   --
   -- The purpose of the function is to allow seamless integration of "Finalize" or "Destroy"
   -- subprograms within deployment code, but expect that this code will only be called during
   -- testing. Designing it in this way eliminates the use of the preprocessor to produce
   -- deallocation code on some targets but not on others.
   --
   generic
      type Object (<>) is limited private;
      type Name is access Object;
   procedure Deallocate_If_Testing (X : in out Name);

   -- Reenable warnings:
   pragma Warnings (On, "formal parameter ""X"" is not modified");
   pragma Warnings (On, "mode could be ""in"" instead of ""in out""");

end Safe_Deallocator;
