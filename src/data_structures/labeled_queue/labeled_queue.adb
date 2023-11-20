package body Labeled_Queue is

   protected body Protected_Queue is

      procedure Init (Size : in Natural) is
      begin
         Queue.Init (Size);
      end Init;

      procedure Init (Bytes : in Basic_Types.Byte_Array_Access) is
      begin
         Queue.Init (Bytes);
      end Init;

      procedure Destroy is
      begin
         Queue.Destroy;
      end Destroy;

      procedure Clear is
      begin
         Queue.Clear;
      end Clear;

      function Get_Count return Natural is
      begin
         return Queue.Get_Count;
      end Get_Count;

      function Get_Max_Count return Natural is
      begin
         return Queue.Get_Max_Count;
      end Get_Max_Count;

      function Num_Bytes_Free return Natural is
      begin
         return Queue.Num_Bytes_Free;
      end Num_Bytes_Free;

      function Num_Bytes_Used return Natural is
      begin
         return Queue.Num_Bytes_Used;
      end Num_Bytes_Used;

      function Max_Num_Bytes_Used return Natural is
      begin
         return Queue.Max_Num_Bytes_Used;
      end Max_Num_Bytes_Used;

      function Num_Bytes_Total return Natural is
      begin
         return Queue.Num_Bytes_Total;
      end Num_Bytes_Total;

      function Current_Percent_Used return Basic_Types.Byte is
      begin
         return Queue.Current_Percent_Used;
      end Current_Percent_Used;

      function Max_Percent_Used return Basic_Types.Byte is
      begin
         return Queue.Max_Percent_Used;
      end Max_Percent_Used;

      function Peek (Label : out Label_Type; Bytes : out Basic_Types.Byte_Array; Length : out Natural; Offset : in Natural) return Pop_Status is
         use Circular_Buffer;
      begin
         case Queue.Peek (Label, Bytes, Length, Offset) is
            when Success =>
               return Success;
            when Empty =>
               return Empty;
         end case;
      end Peek;

      function Peek_Length (Length : out Natural) return Pop_Status is
         use Circular_Buffer;
      begin
         case Queue.Peek_Length (Length) is
            when Success =>
               return Success;
            when Empty =>
               return Empty;
         end case;
      end Peek_Length;

      function Peek_Label (Label : out Label_Type) return Pop_Status is
         use Circular_Buffer;
      begin
         case Queue.Peek_Label (Label) is
            when Success =>
               return Success;
            when Empty =>
               return Empty;
         end case;
      end Peek_Label;

      procedure Push (Label : in Label_Type; Bytes : in Basic_Types.Byte_Array; Not_Empty : in out Ada.Synchronous_Task_Control.Suspension_Object; Status : out Push_Status) is
         use Circular_Buffer;
      begin
         case Queue.Push (Label, Bytes) is
            when Success =>
               -- Release the not_empty suspension object:
               Ada.Synchronous_Task_Control.Set_True (Not_Empty);
               Status := Success;
            when Too_Full =>
               Status := Too_Full;
         end case;
      end Push;

      procedure Pop (Label : out Label_Type; Bytes : out Basic_Types.Byte_Array; Not_Full : in out Ada.Synchronous_Task_Control.Suspension_Object; Length : out Natural; Offset : in Natural; Status : out Pop_Status) is
         use Circular_Buffer;
      begin
         case Queue.Pop (Label, Bytes, Length, Offset) is
            when Success =>
               -- Release the not_full suspension object:
               Ada.Synchronous_Task_Control.Set_True (Not_Full);
               Status := Success;
            when Empty =>
               Status := Empty;
         end case;
      end Pop;

   end Protected_Queue;

   procedure Init (Self : in out Instance; Size : in Natural) is
   begin
      Self.Queue.Init (Size);

      -- Initialize the suspension objects:
      Ada.Synchronous_Task_Control.Set_False (Self.Not_Empty);
      Ada.Synchronous_Task_Control.Set_True (Self.Not_Full);
   end Init;

   procedure Init (Self : in out Instance; Bytes : in Basic_Types.Byte_Array_Access) is
   begin
      Self.Queue.Init (Bytes);

      -- Initialize the suspension objects:
      Ada.Synchronous_Task_Control.Set_False (Self.Not_Empty);
      Ada.Synchronous_Task_Control.Set_True (Self.Not_Full);
   end Init;

   procedure Destroy (Self : in out Instance) is
   begin
      Self.Queue.Destroy;
   end Destroy;

   procedure Clear (Self : in out Instance) is
   begin
      Self.Queue.Clear;

      -- Initialize the suspension objects:
      Ada.Synchronous_Task_Control.Set_False (Self.Not_Empty);
      Ada.Synchronous_Task_Control.Set_True (Self.Not_Full);
   end Clear;

   function Push (Self : in out Instance; Label : in Label_Type; Bytes : in Basic_Types.Byte_Array) return Push_Status is
      Status : Push_Status;
   begin
      Self.Queue.Push (Label, Bytes, Self.Not_Empty, Status);
      return Status;
   end Push;

   function Pop (Self : in out Instance; Label : out Label_Type; Bytes : out Basic_Types.Byte_Array; Length : out Natural; Offset : in Natural := 0) return Pop_Status is
      Status : Pop_Status;
   begin
      Self.Queue.Pop (Label, Bytes, Self.Not_Full, Length, Offset, Status);
      return Status;
   end Pop;

   function Pop (Self : in out Instance; Label : out Label_Type; Bytes : out Basic_Types.Byte_Array; Offset : in Natural := 0) return Pop_Status is
      Ignore : Natural;
   begin
      return Self.Pop (Label, Bytes, Ignore, Offset);
   end Pop;

   function Pop (Self : in out Instance) return Pop_Status is
      Ignore_Label : Label_Type;
      Ignore : Natural;
      Offset : constant Natural := 0;
      Ignore_Bytes : Basic_Types.Byte_Array (1 .. 1);
   begin
      return Self.Pop (Ignore_Label, Ignore_Bytes, Ignore, Offset);
   end Pop;

   function Peek (Self : in out Instance; Label : out Label_Type; Bytes : out Basic_Types.Byte_Array; Length : out Natural; Offset : in Natural := 0) return Pop_Status is
   begin
      return Self.Queue.Peek (Label, Bytes, Length, Offset);
   end Peek;

   function Peek (Self : in out Instance; Label : out Label_Type; Bytes : out Basic_Types.Byte_Array; Offset : in Natural := 0) return Pop_Status is
      Ignore : Natural;
   begin
      return Self.Peek (Label, Bytes, Ignore, Offset);
   end Peek;

   function Peek_Length (Self : in out Instance; Length : out Natural) return Pop_Status is
   begin
      return Self.Queue.Peek_Length (Length);
   end Peek_Length;

   function Peek_Label (Self : in out Instance; Label : out Label_Type) return Pop_Status is
   begin
      return Self.Queue.Peek_Label (Label);
   end Peek_Label;

   function Push_Block (Self : in out Instance; Label : in Label_Type; Bytes : in Basic_Types.Byte_Array) return Push_Block_Status is
      Status : Push_Status;
   begin
      Self.Queue.Push (Label, Bytes, Self.Not_Empty, Status);
      while Status = Too_Full loop
         -- Suspend until the queue is no longer full:
         Ada.Synchronous_Task_Control.Suspend_Until_True (Self.Not_Full);
         Self.Queue.Push (Label, Bytes, Self.Not_Empty, Status);
      end loop;
      return Success;
   exception
      when Program_Error =>
         return Error;
   end Push_Block;

   function Pop_Block (Self : in out Instance; Label : out Label_Type; Bytes : out Basic_Types.Byte_Array; Length : out Natural; Offset : in Natural := 0) return Pop_Block_Status is
      Status : Pop_Status;
   begin
      Self.Queue.Pop (Label, Bytes, Self.Not_Full, Length, Offset, Status);
      while Status = Empty loop
         -- Suspend until the queue is no longer empty:
         Ada.Synchronous_Task_Control.Suspend_Until_True (Self.Not_Empty);
         Self.Queue.Pop (Label, Bytes, Self.Not_Full, Length, Offset, Status);
      end loop;
      return Success;
   exception
      when Program_Error =>
         return Error;
   end Pop_Block;

   function Pop_Block (Self : in out Instance; Label : out Label_Type; Bytes : out Basic_Types.Byte_Array; Offset : in Natural := 0) return Pop_Block_Status is
      Ignore : Natural;
   begin
      return Self.Pop_Block (Label, Bytes, Ignore, Offset);
   end Pop_Block;

   function Peek_Block (Self : in out Instance; Label : out Label_Type; Bytes : out Basic_Types.Byte_Array; Length : out Natural; Offset : in Natural := 0) return Pop_Block_Status is
      Status : Pop_Status;
   begin
      Status := Self.Queue.Peek (Label, Bytes, Length, Offset);
      while Status = Empty loop
         -- Suspend until the queue is no longer empty:
         Ada.Synchronous_Task_Control.Suspend_Until_True (Self.Not_Empty);
         Status := Self.Queue.Peek (Label, Bytes, Length, Offset);
      end loop;
      return Success;
   exception
      when Program_Error =>
         return Error;
   end Peek_Block;

   function Peek_Block (Self : in out Instance; Label : out Label_Type; Bytes : out Basic_Types.Byte_Array; Offset : in Natural := 0) return Pop_Block_Status is
      Ignore : Natural;
   begin
      return Self.Peek_Block (Label, Bytes, Ignore, Offset);
   end Peek_Block;

   function Peek_Length_Block (Self : in out Instance; Length : out Natural) return Pop_Block_Status is
      Status : Pop_Status;
   begin
      Status := Self.Queue.Peek_Length (Length);
      while Status = Empty loop
         -- Suspend until the queue is no longer empty:
         Ada.Synchronous_Task_Control.Suspend_Until_True (Self.Not_Empty);
         Status := Self.Queue.Peek_Length (Length);
      end loop;
      return Success;
   exception
      when Program_Error =>
         return Error;
   end Peek_Length_Block;

   function Peek_Label_Block (Self : in out Instance; Label : out Label_Type) return Pop_Block_Status is
      Status : Pop_Status;
   begin
      Status := Self.Queue.Peek_Label (Label);
      while Status = Empty loop
         -- Suspend until the queue is no longer empty:
         Ada.Synchronous_Task_Control.Suspend_Until_True (Self.Not_Empty);
         Status := Self.Queue.Peek_Label (Label);
      end loop;
      return Success;
   exception
      when Program_Error =>
         return Error;
   end Peek_Label_Block;

   -- Byte arrays don't have a "scalar storage order" since they are an array of single byte
   -- items. So this warning doesn't apply. We can safely overlay a byte array with any type
   -- no matter the underlying scalar storage order.
   --
   -- Which is why you will see this below:
   -- pragma Warnings (Off, "overlay changes scalar storage order");

   function Push_Type (Self : in out Instance; Label : in Label_Type; Src : in T) return Push_Status is
      -- The length in bytes of the serialized type.
      Serialized_Length : constant Natural := T'Object_Size / Basic_Types.Byte'Object_Size; -- in bytes
      -- Byte_Array type for storing the type:
      subtype Byte_Array_Index is Natural range 0 .. (Serialized_Length - 1);
      subtype Byte_Array is Basic_Types.Byte_Array (Byte_Array_Index);
      -- Optimization: create a byte array that overlays the data variable then
      -- pass this byte array into the push function. This avoids a double copy of the data:
      pragma Warnings (Off, "overlay changes scalar storage order");
      Bytes : Byte_Array with Import, Convention => Ada, Address => Src'Address;
      pragma Warnings (On, "overlay changes scalar storage order");
   begin
      return Self.Push (Label, Bytes);
   end Push_Type;

   function Push_Type_Block (Self : in out Instance; Label : in Label_Type; Src : in T) return Push_Block_Status is
      -- The length in bytes of the serialized type.
      Serialized_Length : constant Natural := T'Object_Size / Basic_Types.Byte'Object_Size; -- in bytes
      -- Byte_Array type for storing the type:
      subtype Byte_Array_Index is Natural range 0 .. (Serialized_Length - 1);
      subtype Byte_Array is Basic_Types.Byte_Array (Byte_Array_Index);
      -- Optimization: create a byte array that overlays the data variable then
      -- pass this byte array into the push function. This avoids a double copy of the data:
      pragma Warnings (Off, "overlay changes scalar storage order");
      Bytes : Byte_Array with Import, Convention => Ada, Address => Src'Address;
      pragma Warnings (On, "overlay changes scalar storage order");
   begin
      return Self.Push_Block (Label, Bytes);
   end Push_Type_Block;

   function Push_Variable_Length_Type (Self : in out Instance; Label : in Label_Type; Src : in T) return Push_Variable_Length_Type_Status is
      use Serializer_Types;
      -- Get the serialized length of the source:
      Num_Bytes_Serialized : Natural;
      Status : constant Serialization_Status := Serialized_Length (Src, Num_Bytes_Serialized);
   begin
      -- Make sure source has a valid length:
      if Status /= Success then
         return Serialization_Failure;
      end if;

      declare
         -- Overlay source type with properly sized byte array:
         subtype Sized_Byte_Array_Index is Natural range 0 .. (Num_Bytes_Serialized - 1);
         subtype Sized_Byte_Array is Basic_Types.Byte_Array (Sized_Byte_Array_Index);
         pragma Warnings (Off, "overlay changes scalar storage order");
         Bytes : constant Sized_Byte_Array with Import, Convention => Ada, Address => Src'Address;
         pragma Warnings (On, "overlay changes scalar storage order");
      begin
         case Self.Push (Label, Bytes) is
            when Success =>
               return Success;
            when Too_Full =>
               return Too_Full;
         end case;
      end;
   end Push_Variable_Length_Type;

   function Push_Variable_Length_Type_Block (Self : in out Instance; Label : in Label_Type; Src : in T) return Push_Variable_Length_Type_Block_Status is
      use Serializer_Types;
      -- Get the serialized length of the source:
      Num_Bytes_Serialized : Natural;
      Status : constant Serialization_Status := Serialized_Length (Src, Num_Bytes_Serialized);
   begin
      -- Make sure source has a valid length:
      if Status /= Success then
         return Serialization_Failure;
      end if;

      declare
         -- Overlay source type with properly sized byte array:
         subtype Sized_Byte_Array_Index is Natural range 0 .. (Num_Bytes_Serialized - 1);
         subtype Sized_Byte_Array is Basic_Types.Byte_Array (Sized_Byte_Array_Index);
         pragma Warnings (Off, "overlay changes scalar storage order");
         Bytes : constant Sized_Byte_Array with Import, Convention => Ada, Address => Src'Address;
         pragma Warnings (On, "overlay changes scalar storage order");
      begin
         case Self.Push_Block (Label, Bytes) is
            when Success =>
               return Success;
            when Error =>
               return Error;
         end case;
      end;
   end Push_Variable_Length_Type_Block;

   function Peek_Type (Self : in out Instance; Label : out Label_Type; Dest : out T; Offset : in Natural := 0) return Pop_Type_Status is
      -- The length in bytes of the serialized type.
      Serialized_Length : constant Natural := T'Object_Size / Basic_Types.Byte'Object_Size; -- in bytes
      -- Byte_Array type for storing the type:
      subtype Byte_Array_Index is Natural range 0 .. (Serialized_Length - 1);
      subtype Byte_Array is Basic_Types.Byte_Array (Byte_Array_Index);
      -- Optimization: create a byte array that overlays the data variable then
      -- pass this byte array into the push function. This avoids a double copy of the data:
      pragma Warnings (Off, "overlay changes scalar storage order");
      Bytes : Byte_Array with Import, Convention => Ada, Address => Dest'Address;
      pragma Warnings (On, "overlay changes scalar storage order");
      -- Do the peek:
      Length : Natural;
      Status : constant Pop_Status := Self.Peek (Label, Bytes, Length, Offset);
   begin
      case Status is
         when Success =>
            -- If the returned length is not what was expected, then return error:
            if Length /= Serialized_Length then
               return Deserialization_Failure;
            else
               return Success;
            end if;
         when Empty =>
            return Empty;
      end case;
   end Peek_Type;

   function Peek_Type_Block (Self : in out Instance; Label : out Label_Type; Dest : out T; Offset : in Natural := 0) return Pop_Type_Block_Status is
      -- The length in bytes of the serialized type.
      Serialized_Length : constant Natural := T'Object_Size / Basic_Types.Byte'Object_Size; -- in bytes
      -- Byte_Array type for storing the type:
      subtype Byte_Array_Index is Natural range 0 .. (Serialized_Length - 1);
      subtype Byte_Array is Basic_Types.Byte_Array (Byte_Array_Index);
      -- Optimization: create a byte array that overlays the data variable then
      -- pass this byte array into the push function. This avoids a double copy of the data:
      pragma Warnings (Off, "overlay changes scalar storage order");
      Bytes : Byte_Array with Import, Convention => Ada, Address => Dest'Address;
      pragma Warnings (On, "overlay changes scalar storage order");
      -- Do the peek:
      Length : Natural;
      Status : constant Pop_Block_Status := Self.Peek_Block (Label, Bytes, Length, Offset);
   begin
      case Status is
         when Success =>
            -- If the returned length is not what was expected, then return error:
            if Length /= Serialized_Length then
               return Deserialization_Failure;
            else
               return Success;
            end if;
         when Error =>
            return Error;
      end case;
   end Peek_Type_Block;

   function Peek_Variable_Length_Type (Self : in out Instance; Label : out Label_Type; Dest : out T; Offset : in Natural := 0) return Pop_Type_Status is
      -- The length in bytes of the serialized type.
      Max_Serialized_Length : constant Natural := T'Object_Size / Basic_Types.Byte'Object_Size; -- in bytes
      -- Byte_Array type for storing the type:
      subtype Byte_Array_Index is Natural range 0 .. (Max_Serialized_Length - 1);
      subtype Byte_Array is Basic_Types.Byte_Array (Byte_Array_Index);
      -- Optimization: create a byte array that overlays the data variable then
      -- pass this byte array into the push function. This avoids a double copy of the data:
      pragma Warnings (Off, "overlay changes scalar storage order");
      Bytes : Byte_Array with Import, Convention => Ada, Address => Dest'Address;
      pragma Warnings (On, "overlay changes scalar storage order");
      -- Do the peek:
      Length : Natural;
      Status : constant Pop_Status := Self.Peek (Label, Bytes, Length, Offset);
   begin
      case Status is
         when Success =>
            declare
               use Serializer_Types;
               -- Get the serialized length of the destination:
               Num_Bytes_Serialized : Natural;
               Ser_Status : constant Serialization_Status := Serialized_Length (Dest, Num_Bytes_Serialized);
            begin
               -- If getting the serialized length failed or if the serialized length returned is larger then
               -- the number of bytes returned from the peek then return error:
               if Ser_Status /= Success or else Num_Bytes_Serialized > Length then
                  return Deserialization_Failure;
               else
                  return Success;
               end if;
            end;
         when Empty =>
            return Empty;
      end case;
   end Peek_Variable_Length_Type;

   function Peek_Variable_Length_Type_Block (Self : in out Instance; Label : out Label_Type; Dest : out T; Offset : in Natural := 0) return Pop_Type_Block_Status is
      -- The length in bytes of the serialized type.
      Max_Serialized_Length : constant Natural := T'Object_Size / Basic_Types.Byte'Object_Size; -- in bytes
      -- Byte_Array type for storing the type:
      subtype Byte_Array_Index is Natural range 0 .. (Max_Serialized_Length - 1);
      subtype Byte_Array is Basic_Types.Byte_Array (Byte_Array_Index);
      -- Optimization: create a byte array that overlays the data variable then
      -- pass this byte array into the push function. This avoids a double copy of the data:
      pragma Warnings (Off, "overlay changes scalar storage order");
      Bytes : Byte_Array with Import, Convention => Ada, Address => Dest'Address;
      pragma Warnings (On, "overlay changes scalar storage order");
      -- Do the peek:
      Length : Natural;
      Status : constant Pop_Block_Status := Self.Peek_Block (Label, Bytes, Length, Offset);
   begin
      case Status is
         when Success =>
            declare
               use Serializer_Types;
               -- Get the serialized length of the destination:
               Num_Bytes_Serialized : Natural;
               Ser_Status : constant Serialization_Status := Serialized_Length (Dest, Num_Bytes_Serialized);
            begin
               -- If getting the serialized length failed or if the serialized length returned is larger then
               -- the number of bytes returned from the peek then return error:
               if Ser_Status /= Success or else Num_Bytes_Serialized > Length then
                  return Deserialization_Failure;
               else
                  return Success;
               end if;
            end;
         when Error =>
            return Error;
      end case;
   end Peek_Variable_Length_Type_Block;

   function Pop_Type (Self : in out Instance; Label : out Label_Type; Dest : out T; Offset : in Natural := 0) return Pop_Type_Status is
      -- The length in bytes of the serialized type.
      Serialized_Length : constant Natural := T'Object_Size / Basic_Types.Byte'Object_Size; -- in bytes
      -- Byte_Array type for storing the type:
      subtype Byte_Array_Index is Natural range 0 .. (Serialized_Length - 1);
      subtype Byte_Array is Basic_Types.Byte_Array (Byte_Array_Index);
      -- Optimization: create a byte array that overlays the data variable then
      -- pass this byte array into the push function. This avoids a double copy of the data:
      pragma Warnings (Off, "overlay changes scalar storage order");
      Bytes : Byte_Array with Import, Convention => Ada, Address => Dest'Address;
      pragma Warnings (On, "overlay changes scalar storage order");
      -- Do the pop:
      Length : Natural;
      Status : constant Pop_Status := Self.Pop (Label, Bytes, Length, Offset);
   begin
      case Status is
         when Success =>
            -- If the returned length is not what was expected, then return error:
            if Length /= Serialized_Length then
               return Deserialization_Failure;
            else
               return Success;
            end if;
         when Empty =>
            return Empty;
      end case;
   end Pop_Type;

   function Pop_Type_Block (Self : in out Instance; Label : out Label_Type; Dest : out T; Offset : in Natural := 0) return Pop_Type_Block_Status is
      -- The length in bytes of the serialized type.
      Serialized_Length : constant Natural := T'Object_Size / Basic_Types.Byte'Object_Size; -- in bytes
      -- Byte_Array type for storing the type:
      subtype Byte_Array_Index is Natural range 0 .. (Serialized_Length - 1);
      subtype Byte_Array is Basic_Types.Byte_Array (Byte_Array_Index);
      -- Optimization: create a byte array that overlays the data variable then
      -- pass this byte array into the push function. This avoids a double copy of the data:
      pragma Warnings (Off, "overlay changes scalar storage order");
      Bytes : Byte_Array with Import, Convention => Ada, Address => Dest'Address;
      pragma Warnings (On, "overlay changes scalar storage order");
      -- Do the pop:
      Length : Natural;
      Status : constant Pop_Block_Status := Self.Pop_Block (Label, Bytes, Length, Offset);
   begin
      case Status is
         when Success =>
            -- If the returned length is not what was expected, then return error:
            if Length /= Serialized_Length then
               return Deserialization_Failure;
            else
               return Success;
            end if;
         when Error =>
            return Error;
      end case;
   end Pop_Type_Block;

   function Pop_Variable_Length_Type (Self : in out Instance; Label : out Label_Type; Dest : out T; Offset : in Natural := 0) return Pop_Type_Status is
      -- The length in bytes of the serialized type.
      Max_Serialized_Length : constant Natural := T'Object_Size / Basic_Types.Byte'Object_Size; -- in bytes
      -- Byte_Array type for storing the type:
      subtype Byte_Array_Index is Natural range 0 .. (Max_Serialized_Length - 1);
      subtype Byte_Array is Basic_Types.Byte_Array (Byte_Array_Index);
      -- Optimization: create a byte array that overlays the data variable then
      -- pass this byte array into the push function. This avoids a double copy of the data:
      pragma Warnings (Off, "overlay changes scalar storage order");
      Bytes : Byte_Array with Import, Convention => Ada, Address => Dest'Address;
      pragma Warnings (On, "overlay changes scalar storage order");
      -- Do the pop:
      Length : Natural;
      Status : constant Pop_Status := Self.Pop (Label, Bytes, Length, Offset);
   begin
      case Status is
         when Success =>
            declare
               use Serializer_Types;
               -- Get the serialized length of the destination:
               Num_Bytes_Serialized : Natural;
               Ser_Status : constant Serialization_Status := Serialized_Length (Dest, Num_Bytes_Serialized);
            begin
               -- If getting the serialized length failed or if the serialized length returned is larger then
               -- the number of bytes returned from the pop then return error:
               if Ser_Status /= Success or else Num_Bytes_Serialized > Length then
                  return Deserialization_Failure;
               else
                  return Success;
               end if;
            end;
         when Empty =>
            return Empty;
      end case;
   end Pop_Variable_Length_Type;

   function Pop_Variable_Length_Type_Block (Self : in out Instance; Label : out Label_Type; Dest : out T; Offset : in Natural := 0) return Pop_Type_Block_Status is
      -- The length in bytes of the serialized type.
      Max_Serialized_Length : constant Natural := T'Object_Size / Basic_Types.Byte'Object_Size; -- in bytes
      -- Byte_Array type for storing the type:
      subtype Byte_Array_Index is Natural range 0 .. (Max_Serialized_Length - 1);
      subtype Byte_Array is Basic_Types.Byte_Array (Byte_Array_Index);
      -- Optimization: create a byte array that overlays the data variable then
      -- pass this byte array into the push function. This avoids a double copy of the data:
      pragma Warnings (Off, "overlay changes scalar storage order");
      Bytes : Byte_Array with Import, Convention => Ada, Address => Dest'Address;
      pragma Warnings (On, "overlay changes scalar storage order");
      -- Do the pop:
      Length : Natural;
      Status : constant Pop_Block_Status := Self.Pop_Block (Label, Bytes, Length, Offset);
   begin
      case Status is
         when Success =>
            declare
               use Serializer_Types;
               -- Get the serialized length of the destination:
               Num_Bytes_Serialized : Natural;
               Ser_Status : constant Serialization_Status := Serialized_Length (Dest, Num_Bytes_Serialized);
            begin
               -- If getting the serialized length failed or if the serialized length returned is larger then
               -- the number of bytes returned from the pop then return error:
               if Ser_Status /= Success or else Num_Bytes_Serialized > Length then
                  return Deserialization_Failure;
               else
                  return Success;
               end if;
            end;
         when Error =>
            return Error;
      end case;
   end Pop_Variable_Length_Type_Block;

   function Num_Bytes_Free (Self : in out Instance) return Natural is
   begin
      return Self.Queue.Num_Bytes_Free;
   end Num_Bytes_Free;

   function Num_Bytes_Used (Self : in out Instance) return Natural is
   begin
      return Self.Queue.Num_Bytes_Used;
   end Num_Bytes_Used;

   function Max_Num_Bytes_Used (Self : in out Instance) return Natural is
   begin
      return Self.Queue.Max_Num_Bytes_Used;
   end Max_Num_Bytes_Used;

   function Size_In_Bytes (Self : in out Instance) return Natural is
   begin
      return Self.Queue.Num_Bytes_Total;
   end Size_In_Bytes;

   function Current_Percent_Used (Self : in out Instance) return Basic_Types.Byte is
   begin
      return Self.Queue.Current_Percent_Used;
   end Current_Percent_Used;

   function Max_Percent_Used (Self : in out Instance) return Basic_Types.Byte is
   begin
      return Self.Queue.Max_Percent_Used;
   end Max_Percent_Used;

   function Num_Elements (Self : in out Instance) return Natural is
   begin
      return Self.Queue.Get_Count;
   end Num_Elements;

   function Max_Num_Elements (Self : in out Instance) return Natural is
   begin
      return Self.Queue.Get_Max_Count;
   end Max_Num_Elements;

end Labeled_Queue;
