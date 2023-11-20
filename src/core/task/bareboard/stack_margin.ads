with Configuration;

package Stack_Margin is

   -- Define stack margin for the bare board. We grab this value from
   -- the global configuration
   Margin : constant Natural := Configuration.Stack_Margin;

end Stack_Margin;
