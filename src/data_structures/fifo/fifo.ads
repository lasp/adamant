-- This is a generic, unprotected FIFO (first in
-- first out) data structure. The user can instantiate this class
-- with any type that they choose, creating a FIFO specialized
-- to holding that particular type.
generic
   -- The type of data stored on the FIFO.
   type T is private;
package Fifo is
   -- The FIFO class instance type:
   type Instance is tagged private;

   -- Status type:
   type Push_Status is (Success, Full);
   type Pop_Status is (Success, Empty);

   --
   -- Initialization/destruction functions:
   --
   procedure Init (Self : in out Instance; Depth : in Positive);
   procedure Destroy (Self : in out Instance);

   --
   -- Add/remove/look at data on the fifo:
   --
   function Push (Self : in out Instance; Value : in T) return Push_Status;
   function Pop (Self : in out Instance; Value : out T) return Pop_Status;
   function Peek (Self : in Instance; Value : out T) return Pop_Status;

   --
   -- Meta data functions:
   --
   -- is the fifo currently full?
   function Is_Full (Self : in Instance) return Boolean;
   -- is the fifo currently empty?
   function Is_Empty (Self : in Instance) return Boolean;
   -- how many items are on the fifo currently?
   function Get_Count (Self : in Instance) return Natural;
   -- what is the maximum number of items ever seen on the fifo since instantiation?
   function Get_Max_Count (Self : in Instance) return Natural;
   -- how many items can the fifo hold in total?
   function Get_Depth (Self : in Instance) return Positive;

private
   type Fifo_Items is array (Natural range <>) of T;
   type Fifo_Items_Access is access Fifo_Items;

   type Instance is tagged record
      Head : Natural := Natural'First;
      Count : Natural := Natural'First;
      Max_Count : Natural := Natural'First;
      Items : Fifo_Items_Access;
   end record;
end Fifo;
