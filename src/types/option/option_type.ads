generic
   -- Any constrained & non-limited type.
   type Element_Type is private;
package Option_Type is
   -- When the discriminant, Has_Element, is true there is an element field,
   -- when it is false, there are no fields (hence the null keyword).
   type Option (Has_Element : Boolean) is record
      case Has_Element is
         when False =>
            null;
         when True =>
            Element : Element_Type;
      end case;
   end record;
end Option_Type;
