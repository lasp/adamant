-- This package contains many common, simple, protected variable patterns used in
-- components. Each protected variable pattern is contained within a generic
-- package that can be adapted for the particular type needed.

package Protected_Variables is

   -- A generic variable that can be set/fetched in a thread safe way.
   generic
      -- Any nonlimited definite type.
      type T is private;
   package Generic_Variable is
      protected type Variable is
         -- Safely get the variable.
         function Get_Var return T;
         -- Safely set the variable.
         procedure Set_Var (Value : in T);
      private
         Var : T;
      end Variable;
   end Generic_Variable;

   -- A generic counter whose members are set/fetched/incremented in a thread safe way.
   generic
      -- Any modular type
      type T is mod <>;
   package Generic_Protected_Counter is
      protected type Counter is
         -- Set the count.
         procedure Set_Count (Value : in T);
         -- Fetch the count.
         function Get_Count return T;
         -- Set the count to zero.
         procedure Reset_Count;
         -- Increment the count, by 1 by default.
         procedure Increment_Count (To_Add : in T := 1);
         -- Same as Increment_Count but returns the previous value.
         procedure Increment_Count_And_Return_Previous (Prev_Count : out T; To_Add : in T := 1);
      private
         Count : T := 0;
      end Counter;
   end Generic_Protected_Counter;

   -- A generic counter whose members are set/fetched/incremented in a thread safe way.
   generic
      -- Any modular type
      type T is range <>;
   package Generic_Protected_Counter_Decrement is
      protected type Counter is
         -- Set the count.
         procedure Set_Count (Value : in T);
         -- Fetch the count.
         function Get_Count return T;
         -- Set the count to zero.
         procedure Reset_Count;
         -- Increment the count, by 1 by default.
         procedure Decrement_Count (To_Subtract : in T := 1);
         -- Same as Decrement_Count but returns the previous value.
         procedure Decrement_Count_And_Return_Previous (Prev_Count : out T; To_Subtract : in T := 1);
      private
         Count : T := 0;
      end Counter;
   end Generic_Protected_Counter_Decrement;

   -- A generic staging area for handing a value from one task to another in a
   -- thread-safe way. The writer calls Stage to deposit a new value and mark
   -- it pending. The owner calls Is_Staged to check (cheaply) whether a value
   -- is waiting and Copy_From_Staged to atomically read it out and clear the
   -- pending flag. Useful when a component receives a configuration on one
   -- task but applies it on another, at the appropriate time.
   generic
      -- Any nonlimited definite type.
      type T is private;
   package Generic_Staged_Variable is
      protected type Staged_Variable is
         -- Writer side. Replace the staged value and mark it pending. Repeated
         -- Stage calls before the next Copy_From_Staged keep only the most
         -- recent value.
         procedure Stage (Value : in T);
         -- Cheap poll: returns whether a value has been staged. Lets the caller
         -- check without paying the stack cost of a T-sized out parameter.
         function Is_Staged return Boolean;
         -- Atomic read-and-clear. Copies the staged value into Out_Value and
         -- clears the pending flag. Intended to be called only after Is_Staged
         -- has returned True. If called when no value is staged, Out_Value
         -- receives the protected object's default-initialized Staged_Value --
         -- callers should always guard with Is_Staged for performance.
         procedure Copy_From_Staged (Out_Value : out T);
      private
         Staged_Value : T;
         Has_Staged : Boolean := False;
      end Staged_Variable;
   end Generic_Staged_Variable;

   -- A generic counter whose members are set/fetched/incremented in a thread safe way.
   -- The counter contains an incrementing counter and a period. This is usually used
   -- to hold data for a counter that, when reaches the period value, is reset to zero.
   -- The user usually checks to see when the counter has reached zero, using the
   -- Is_Count_At_Period function, and an action is performed if the returned value is true.
   -- These variables often need to be grouped together into a single protected object
   -- to accommodate some operations that need to modify both automatically in a thread
   -- safe manner.
   generic
      -- Any modular type
      type T is mod <>;
   package Generic_Protected_Periodic_Counter is
      protected type Counter is
         -- Fetch the period.
         function Get_Period return T;
         -- Set the period. NOTE: this also resets the count to zero.
         procedure Set_Period_And_Reset_Count (Value : in T);
         -- Fetch the count.
         function Get_Count return T;
         -- Set the count to zero.
         procedure Reset_Count;
         -- Increment the count, by 1 by default.
         procedure Increment_Count (To_Add : in T := 1);
         -- Returns True if period is greater than zero and (count mod period) = 0, otherwise
         -- it returns False. This is often used to trigger some action based on a count reaching
         -- the period. If period is zero, false is always returned, meaning no action.
         function Is_Count_At_Period return Boolean;
      private
         Count : T := 0;
         Period : T := 1;
      end Counter;
   end Generic_Protected_Periodic_Counter;

end Protected_Variables;
