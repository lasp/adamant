package body Protected_Variables is

   package body Generic_Variable is
      protected body Variable is
         function Get_Var return T is
         begin
            return Var;
         end Get_Var;

         procedure Set_Var (Value : in T) is
         begin
            Var := Value;
         end Set_Var;
      end Variable;
   end Generic_Variable;

   package body Generic_Protected_Counter is
      protected body Counter is
         procedure Set_Count (Value : in T) is
         begin
            Count := Value;
         end Set_Count;

         function Get_Count return T is
         begin
            return Count;
         end Get_Count;

         procedure Reset_Count is
         begin
            Count := 0;
         end Reset_Count;

         procedure Increment_Count (To_Add : in T := 1) is
         begin
            Count := @ + To_Add;
         end Increment_Count;

         procedure Increment_Count_And_Return_Previous (Prev_Count : out T; To_Add : in T := 1) is
         begin
            Prev_Count := Count;
            Count := @ + To_Add;
         end Increment_Count_And_Return_Previous;
      end Counter;
   end Generic_Protected_Counter;

   package body Generic_Protected_Counter_Decrement is
      protected body Counter is
         procedure Set_Count (Value : in T) is
         begin
            Count := Value;
         end Set_Count;

         function Get_Count return T is
         begin
            return Count;
         end Get_Count;

         procedure Reset_Count is
         begin
            Count := 0;
         end Reset_Count;

         procedure Decrement_Count (To_Subtract : in T := 1) is
         begin
            Count := @ - To_Subtract;
         end Decrement_Count;

         procedure Decrement_Count_And_Return_Previous (Prev_Count : out T; To_Subtract : in T := 1) is
         begin
            Prev_Count := Count;
            Count := @ - To_Subtract;
         end Decrement_Count_And_Return_Previous;
      end Counter;
   end Generic_Protected_Counter_Decrement;

   package body Generic_Protected_Periodic_Counter is
      protected body Counter is
         function Get_Period return T is
         begin
            return Period;
         end Get_Period;

         procedure Set_Period_And_Reset_Count (Value : in T) is
         begin
            Count := 0;
            Period := Value;
         end Set_Period_And_Reset_Count;

         function Get_Count return T is
         begin
            return Count;
         end Get_Count;

         procedure Reset_Count is
         begin
            Count := 0;
         end Reset_Count;

         procedure Increment_Count (To_Add : in T := 1) is
         begin
            if Period > 0 then
               Count := (@ + To_Add) mod Period;
            end if;
         end Increment_Count;

         function Is_Count_At_Period return Boolean is
         begin
            if Period > 0 and then (Count mod Period) = 0 then
               return True;
            else
               return False;
            end if;
         end Is_Count_At_Period;
      end Counter;
   end Generic_Protected_Periodic_Counter;

end Protected_Variables;
