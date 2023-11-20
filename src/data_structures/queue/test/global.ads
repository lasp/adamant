-- Note: A certain test (see below) requires this to be enabled,
-- and the program run in GDB. For long term testing we won't
-- enable this, since it uses ravenscar the test actually never
-- terminates.
-- pragma Profile (Ravenscar);
with Queue;

package Global is
   -- Global (library level declarations):
   package Natural_Queue is new Queue (Natural);
   My_Queue : Natural_Queue.Instance;

   type Action_T is (Push, Pop, Pop_Error, Push_Error, Quit, Nothing);
   protected type Action is
      procedure Get (A : out Action_T);
      procedure Set (A : in Action_T);
   private
      Act : Action_T := Nothing;
   end Action;
   The_Action : Action;

   task Unblock;
end Global;
