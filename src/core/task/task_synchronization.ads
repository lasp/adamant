--
-- This package contains task synchronization patterns commonly employed in Adamant.
-- Each of the patterns is implemented by a protected object with an entry that
-- "blocks" or waits for another task to release its execution.
--
package Task_Synchronization is

   -- A simple wait-release object which provides an entry "wait" that will
   -- block the execution of a task until the "release" procedure is called
   -- by another task.
   protected type Wait_Release_Object is
      -- Block on this entry until released by Release:
      entry Wait;
      -- Release any task blocked on Wait.
      procedure Release;
      -- Resets the object to its default condition. Wait will
      -- block after this call is executed. This can be used to
      -- "clear" any releases that may have happened that should
      -- be ignored according to program logic.
      procedure Reset;
   private
      Do_Release : Boolean := False;
   end Wait_Release_Object;

   -- An extension of the Wait_Release_Object above, this object provides
   -- the same release/wait logic but also implements a timeout. Another task,
   -- possibly a third task, can call "timeout" after it is deemed that a
   -- timeout has occurred. This will cause a waiting task to release and
   -- the "Timed_Out" variable will be set to True. There is also a function
   -- which returns if a task is currently waiting or not, which can be used
   -- to implement more sophisticated timeout logic.
   --
   -- This object does not implement any specific timing features in regards
   -- to the timeout. This must be implemented outside of the protected object.
   -- There is simply a procedure "Timeout" provided which is identical to "Release"
   -- with two exceptions, 1) it causes "Wait" to return Timeout_Out => True and
   -- it only sets the internal variables to True if there is indeed a task waiting,
   -- otherwise it has no effect.
   protected type Wait_Release_Timeout_Object is
      -- Block on this entry until released by Release or Timeout. If
      -- released by Timeout then Timed_Out is set to True otherwise it
      -- is set to False.
      entry Wait (Timed_Out : out Boolean);
      -- Release any task blocked on Wait.
      procedure Release;
      -- Release any task blocked on Wait, but set Timed_Out to True.
      -- This usually needs to be called in a different task from the
      -- task that calls Release.
      procedure Timeout;
      -- Returns True if a task is currently blocked on Wait, and
      -- False otherwise.
      function Is_Waiting return Boolean;
      -- Resets the object to its default condition. Wait will
      -- block after this call is executed. This can be used to
      -- "clear" any releases that may have happened that should
      -- be ignored according to program logic.
      procedure Reset;
   private
      Do_Timeout : Boolean := False;
      Do_Release : Boolean := False;
   end Wait_Release_Timeout_Object;

   -- An extension of the Wait_Release_Timeout_Object above, this object provides
   -- the same release/wait/timeout but implements a counter. When the counter
   -- reaches a limit, the timeout logic is executed.
   --
   -- Unlike the Wait_Release_Timeout_Object, this does implement specific
   -- timing features in regards to the timeout. It assumes three threads of
   -- execution. One thread "Waits" on a condition until a second thread "Releases"
   -- the condition. A third thread periodically calls Increment_Timeout_If_Waiting
   -- which will timeout the waiting thread if it has been waiting for a certain
   -- number of counts.
   protected type Wait_Release_Timeout_Counter_Object is
      -- Initialize the timeout counter object with the timeout
      -- limit. Every time Increment_Timeout_Counter is called
      -- the counter will be checked against the limit. If the
      -- counter >= limit then a timeout condition will
      -- be triggered.
      procedure Set_Timeout_Limit (New_Timeout_Limit : in Natural);
      -- Block on this entry until released by Release or Timeout. If
      -- released by Timeout then Timed_Out is set to True otherwise it
      -- is set to False.
      entry Wait (Timed_Out : out Boolean);
      -- Release any task blocked on Wait.
      procedure Release;
      -- Every time Increment_Timeout_If_Waiting is called
      -- the counter will be checked against the limit. If the
      -- counter >= limit then a timeout condition will
      -- be triggered. A timeout condition consists of
      -- Release any task blocked on Wait, but set Timed_Out to True.
      -- This usually needs to be called in a different task from the
      -- task that calls Release.
      procedure Increment_Timeout_If_Waiting;
      -- Release any task blocked on Wait, but set Timed_Out to True.
      -- This usually needs to be called in a different task from the
      -- task that calls Release.
      procedure Timeout;
      -- Returns True if a task is currently blocked on Wait, and
      -- False otherwise.
      function Is_Waiting return Boolean;
      -- Resets the object to its default condition. Wait will
      -- block after this call is executed. This can be used to
      -- "clear" any releases that may have happened that should
      -- be ignored according to program logic.
      -- This also resets the timeout count to zero.
      procedure Reset;
   private
      Do_Timeout : Boolean := False;
      Do_Release : Boolean := False;
      Timeout_Count : Natural := 0;
      Timeout_Limit : Natural := 0;
   end Wait_Release_Timeout_Counter_Object;

end Task_Synchronization;
