with Crc_16;
with Sys_Time;

-- A store for a record that must survive a reboot, such as counters kept in MRAM.
--
-- The store keeps two copies of the record in memory that persists across a reboot,
-- each with its save time and a CRC. Save writes the older copy, data first and CRC
-- last, so a reboot in the middle of a save leaves the other copy intact. Restore
-- returns the newest valid copy.
--
-- To use it, instantiate the generic on the record type, declare two Persistent_Copy
-- objects over the persistent memory, and hand access values to them to Init once at
-- boot. That memory must be uncached, or writes must otherwise reach it in program
-- order, for CRC last to protect a save.
generic
   -- Every bit pattern of T must be a valid value, as in a packed record of unsigned
   -- integers. The CRC is computed over an unchecked conversion of T, and GNATprove
   -- rejects the instantiation otherwise. GNATprove cannot check a generic instantiated
   -- as a library unit, so instantiate inside a package, or prove a nested twin the way
   -- Double_Buffer_Store_Prover does.
   type T is private;
package Double_Buffer_Store with SPARK_Mode => On is

   -- The contracts and ghost code below are for GNATprove only. They are ignored at
   -- run time, so the compiled code is the same as without them.
   pragma Assertion_Policy
      (Pre => Ignore,
       Pre'Class => Ignore,
       Post => Ignore,
       Post'Class => Ignore,
       Ghost => Ignore);

   type Restore_Status is (Restored, No_Valid_Copy);

   -- The part of a copy that the CRC covers:
   type Payload_Type is record
      Save_Time : Sys_Time.T;
      Data : T;
   end record;

   -- One copy of the store as it sits in memory. The aspects mark it as volatile
   -- memory that the next boot reads, so every write reaches it, in order, and none is
   -- dropped or merged.
   type Persistent_Copy is record
      -- CRC-16 over the payload. Complemented to invalidate the copy.
      Crc : Crc_16.Crc_16_Type;
      Payload : Payload_Type;
   end record
      with Volatile, Async_Readers => True, Async_Writers => False, Effective_Reads => False, Effective_Writes => True;

   -- SPARK requires an access to volatile memory to carry the same aspects:
   type Persistent_Copy_Access is access all Persistent_Copy
      with Volatile, Async_Readers => True, Async_Writers => False, Effective_Reads => False, Effective_Writes => True;

   -- A store bound to two copies. Keep one per pair of copies.
   type Instance is tagged private;

   -- True once Init has bound the store. For the contracts below.
   function Is_Initialized (Self : in Instance) return Boolean
      with Ghost, Global => null;

   -- Return the newest valid copy: the valid one, or the one with the later save time
   -- if both are valid, with Status Restored. If neither is valid, Status is
   -- No_Valid_Copy and Value is all zero bits, which is a valid T since every bit
   -- pattern is. Restore does not change memory.
   procedure Restore (Self : in Instance; Value : out T; Status : out Restore_Status)
      with Global => null,
           Pre'Class => Is_Initialized (Self);
   pragma Annotate (GNATSAS, False_Positive, "validity check",
      "The precondition is a ghost predicate ignored at run time by the Assertion_Policy above, so Restore always assigns Status.");

   -- Bind the store to its two copies and restore, as Restore does. Then make the
   -- other copy the target of the next save, invalidating it if it was valid, or copy
   -- A when neither copy was valid. Call once at boot, before Save or Restore. The two
   -- copies must not overlap, and Init rejects copies that do.
   procedure Init (
      Self : out Instance;
      Region_A : in not null Persistent_Copy_Access;
      Region_B : in not null Persistent_Copy_Access;
      Value : out T;
      Status : out Restore_Status
   )
      with Global => null,
           Post'Class => Is_Initialized (Self);

   -- Write Value and Save_Time to the target copy, CRC last, then make the other copy
   -- the target.
   procedure Save (Self : in out Instance; Value : in T; Save_Time : in Sys_Time.T)
      with Global => null,
           Pre'Class => Is_Initialized (Self),
           Post'Class => Is_Initialized (Self);

private

   -- Where a restore came from, or Neither when no copy is valid:
   type Source_Type is (Copy_A, Copy_B, Neither);
   subtype Copy_Type is Source_Type range Copy_A .. Copy_B;

   -- SPARK only allows an access value to volatile memory inside a volatile record,
   -- so the Instance carries the same aspects as the memory:
   type Instance is tagged record
      Region_A : Persistent_Copy_Access := null;
      Region_B : Persistent_Copy_Access := null;
      -- The copy the next save writes:
      Target : Copy_Type := Copy_A;
   end record
      with Volatile, Async_Readers => True, Async_Writers => False, Effective_Reads => False, Effective_Writes => True;

   function Is_Initialized (Self : in Instance) return Boolean is
      (Self.Region_A /= null and then Self.Region_B /= null);

end Double_Buffer_Store;
