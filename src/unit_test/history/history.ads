generic
   type T is private;
package History is
   type Instance is tagged private;

   procedure Init (Self : in out Instance; Depth : Positive);
   procedure Destroy (Self : in out Instance);
   procedure Push (Self : in out Instance; Value : in T);
   function Get (Self : in Instance; Index : in Positive) return T;
   procedure Clear (Self : in out Instance);
   function Is_Full (Self : in Instance) return Boolean;
   function Is_Empty (Self : in Instance) return Boolean;
   function Get_Depth (Self : in Instance) return Positive;
   function Get_Count (Self : in Instance) return Natural;

private
   type History_Buffer is array (Positive range <>) of T;
   type History_Buffer_Access is access History_Buffer;

   type Instance is tagged record
      Count : Natural := 0;
      Buffer : History_Buffer_Access;
   end record;
end History;
