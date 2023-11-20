package body Queue is

   protected body Protected_Queue is

      procedure Init (Depth : in Positive) is
      begin
         My_Fifo.Init (Depth);
      end Init;

      procedure Destroy is
      begin
         My_Fifo.Destroy;
      end Destroy;

      function Is_Full return Boolean is
      begin
         return My_Fifo.Is_Full;
      end Is_Full;

      function Is_Empty return Boolean is
      begin
         return My_Fifo.Is_Empty;
      end Is_Empty;

      function Get_Count return Natural is
      begin
         return My_Fifo.Get_Count;
      end Get_Count;

      function Get_Max_Count return Natural is
      begin
         return My_Fifo.Get_Max_Count;
      end Get_Max_Count;

      function Get_Depth return Positive is
      begin
         return My_Fifo.Get_Depth;
      end Get_Depth;

      procedure Push (Value : in T; Not_Empty : in out Ada.Synchronous_Task_Control.Suspension_Object; Status : out Push_Status) is
         use T_Fifo;
      begin
         if My_Fifo.Push (Value) = Success then
            -- Release the not_empty suspension object:
            Ada.Synchronous_Task_Control.Set_True (Not_Empty);
            Status := Success;
         else
            Status := Full;
         end if;
      end Push;

      procedure Pop (Value : out T; Not_Full : in out Ada.Synchronous_Task_Control.Suspension_Object; Status : out Pop_Status) is
         use T_Fifo;
      begin
         if My_Fifo.Pop (Value) = Success then
            -- Release the not_full suspension object:
            Ada.Synchronous_Task_Control.Set_True (Not_Full);
            Status := Success;
         else
            Status := Empty;
         end if;
      end Pop;

      function Peek (Value : out T) return Pop_Status is
         use T_Fifo;
      begin
         if My_Fifo.Peek (Value) = Success then
            return Success;
         end if;
         return Empty;
      end Peek;

   end Protected_Queue;

   procedure Init (Self : in out Instance; Depth : in Positive) is
   begin
      -- Initialize the suspension objects:
      Ada.Synchronous_Task_Control.Set_False (Self.Not_Empty);
      Ada.Synchronous_Task_Control.Set_True (Self.Not_Full);
      -- Initialize the internal fifo:
      Self.My_Queue.Init (Depth);
   end Init;

   procedure Destroy (Self : in out Instance) is
   begin
      Self.My_Queue.Destroy;
   end Destroy;

   function Is_Full (Self : in Instance) return Boolean is
   begin
      return Self.My_Queue.Is_Full;
   end Is_Full;

   function Is_Empty (Self : in Instance) return Boolean is
   begin
      return Self.My_Queue.Is_Empty;
   end Is_Empty;

   function Get_Count (Self : in Instance) return Natural is
   begin
      return Self.My_Queue.Get_Count;
   end Get_Count;

   function Get_Max_Count (Self : in Instance) return Natural is
   begin
      return Self.My_Queue.Get_Max_Count;
   end Get_Max_Count;

   function Get_Depth (Self : in Instance) return Positive is
   begin
      return Self.My_Queue.Get_Depth;
   end Get_Depth;

   function Push (Self : in out Instance; Value : in T) return Push_Status is
      Status : Push_Status := Success;
   begin
      Self.My_Queue.Push (Value, Self.Not_Empty, Status);
      return Status;
   end Push;

   function Pop (Self : in out Instance; Value : out T) return Pop_Status is
      Status : Pop_Status := Success;
   begin
      Self.My_Queue.Pop (Value, Self.Not_Full, Status);
      return Status;
   end Pop;

   function Peek (Self : in Instance; Value : out T) return Pop_Status is
   begin
      return Self.My_Queue.Peek (Value);
   end Peek;

   function Push_Block (Self : in out Instance; Value : in T) return Push_Block_Status is
      Stat : Push_Status;
   begin
      Stat := Self.Push (Value);
      while Stat = Full loop
         -- Suspend until the queue is no longer full:
         Ada.Synchronous_Task_Control.Suspend_Until_True (Self.Not_Full);
         Stat := Self.Push (Value);
      end loop;
      return Success;
   exception
      when Program_Error =>
         return Error;
   end Push_Block;

   function Pop_Block (Self : in out Instance; Value : out T) return Pop_Block_Status is
      Stat : Pop_Status;
   begin
      Stat := Self.Pop (Value);
      while Stat = Empty loop
         -- Suspend until the queue is no longer empty:
         Ada.Synchronous_Task_Control.Suspend_Until_True (Self.Not_Empty);
         Stat := Self.Pop (Value);
      end loop;
      return Success;
   exception
      when Program_Error =>
         return Error;
   end Pop_Block;

   function Peek_Block (Self : in out Instance; Value : out T) return Pop_Block_Status is
      Stat : Pop_Status;
   begin
      Stat := Self.Peek (Value);
      while Stat = Empty loop
         -- Suspend until the queue is no longer empty:
         Ada.Synchronous_Task_Control.Suspend_Until_True (Self.Not_Empty);
         Stat := Self.Peek (Value);
      end loop;
      return Success;
   exception
      when Program_Error =>
         return Error;
   end Peek_Block;

end Queue;
