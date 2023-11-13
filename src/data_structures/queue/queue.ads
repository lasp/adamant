with Fifo;
with Ada.Synchronous_Task_Control;

-- This is a generic, protected (thread safe) queue. The user can
-- instantiate this class with any type that they choose, creating
-- a queue specialized to holding that particular type.
generic
   type T is private;
package Queue is
   -- The Queue class instance type:
   type Instance is tagged limited private;

   -- Status type:
   type Push_Status is (Success, Full);
   type Pop_Status is (Success, Empty);
   type Push_Block_Status is (Success, Error);
   type Pop_Block_Status is (Success, Error);

   --
   -- Initialization/destruction functions:
   --
   procedure Init (Self : in out Instance; Depth : in Positive);
   procedure Destroy (Self : in out Instance);

   --
   -- Add/remove/look at data on the queue, non-blocking
   -- operations.
   --
   -- Push a new item onto the queue.
   function Push (Self : in out Instance; Value : in T) return Push_Status;
   -- Pop (remove) the oldest item from the queue.
   function Pop (Self : in out Instance; Value : out T) return Pop_Status;
   -- Look at the oldest item on the queue without removing it.
   function Peek (Self : in Instance; Value : out T) return Pop_Status;

   --
   -- Add/remove/look at data on the queue, blocking
   -- operations.
   --
   -- Note: The options below will either return Success or Error. Error may
   -- be returned on Ravenscar systems only. Ravenscar has a restriction in that
   -- only a single task may wait on a suspension object at any given time. If this
   -- condition is ever violated a Program_Error is raised. In the functions below
   -- we catch this condition and return the Error status, allowing the user to
   -- handle the condition as they please, usually by either moving on, or trying
   -- again. Proper usage of this queue in Adamant should avoid the possibility of
   -- this condition ever occuring. Good design usually avoids more than one task
   -- simultaneously popping or pushing to the same queue in a blocking manner. Use
   -- the "no_wait" version of these functions above whenever possible.
   --
   -- Push item onto the queue. If the queue is full, wait until an item becomes
   -- available on the queue before pushing, then return.
   function Push_Block (Self : in out Instance; Value : in T) return Push_Block_Status;
   -- Pop (remove) item from the queue. If the queue is empty, wait until an item gets put
   -- on the queue before popping, then return.
   function Pop_Block (Self : in out Instance; Value : out T) return Pop_Block_Status;
   -- Peek (look without removing) item from the queue. If the queue is empty, wait until an item gets put
   -- on the queue before peeking, then return.
   function Peek_Block (Self : in out Instance; Value : out T) return Pop_Block_Status;

   --
   -- Meta data functions:
   --
   -- is the queue currently full?
   function Is_Full (Self : in Instance) return Boolean;
   -- is the queue currently empty?
   function Is_Empty (Self : in Instance) return Boolean;
   -- how many items are on the queue currently?
   function Get_Count (Self : in Instance) return Natural;
   -- what is the maximum number of items ever seen on the queue since instantiation?
   function Get_Max_Count (Self : in Instance) return Natural;
   -- how many items can the queue hold in total?
   function Get_Depth (Self : in Instance) return Positive;

private
   -- Include a generic fifo package:
   package T_Fifo is new Fifo (T => T);

   -- Declaration of internal protected fifo:
   protected type Protected_Queue is
      -- Functions that provide read-only access to the private data:
      function Is_Full return Boolean;
      function Is_Empty return Boolean;
      function Get_Count return Natural;
      function Get_Max_Count return Natural;
      function Get_Depth return Positive;
      -- Procedures requiring full mutual exclusion:
      procedure Init (Depth : in Positive);
      procedure Destroy;
      -- Note: We release the suspension objects inside of the protected object. This allows the scheduler
      -- to immediately pop from a just-pushed queue if the popper is the highest priority task. If we did not
      -- release the suspension objects inside of the protected object, then it is possible for another
      -- task to run push before the other end of the queue has time to respond, even if it is higher
      -- a higher priority task. This design ensures the most efficient use of memory on the queue.
      procedure Push (Value : in T; Not_Empty : in out Ada.Synchronous_Task_Control.Suspension_Object; Status : out Push_Status);
      procedure Pop (Value : out T; Not_Full : in out Ada.Synchronous_Task_Control.Suspension_Object; Status : out Pop_Status);
      function Peek (Value : out T) return Pop_Status;
   private
      My_Fifo : T_Fifo.Instance;
   end Protected_Queue;

   type Instance is tagged limited record
      My_Queue : Protected_Queue;
      Not_Full, Not_Empty : Ada.Synchronous_Task_Control.Suspension_Object;
   end record;

end Queue;
