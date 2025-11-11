with History.Printable;

-- A wrapper around History.Printable which makes it a bit more accessible, and similar
-- to use the regular History package.
generic
   type T is private;
   -- Function which returns string representation of item of type T
   with function T_Image (Item : in T) return String;
package Printable_History is

   package History_Package is new History (T);
   package Printable_History_Package is new History_Package.Printable (T_Image);

   pragma Warnings (Off, "declaration hides ""Instance"" at history.ads");
   subtype Instance is Printable_History_Package.Printable_Instance;
   pragma Warnings (On, "declaration hides ""Instance"" at history.ads");

end Printable_History;
