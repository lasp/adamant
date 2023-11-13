package Stack_Margin is

   -- Define stack margin for Linux. A value above 10KB seems to be large enough
   -- to prevent segfaults when filling the stacks with a stack pattern.
   Margin : constant Natural := 12_288; -- 12KB for margin

end Stack_Margin;
