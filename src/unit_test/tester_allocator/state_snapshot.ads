-- Test-only capture and restore of an object's freshly-elaborated
-- state, by byte image. Save stores the object's bytes the first time
-- it is called; Restore copies them back, returning the object to its
-- just-elaborated state.
--
-- Target-split bodies: no-ops on Linux, where test fixtures get a
-- fresh heap Tester every scenario anyway. Real on bareboard, where
-- the fixture reuses one static Tester across scenarios and state
-- would otherwise leak between them. The image is only ever restored
-- onto the same object it was captured from, at the same address and
-- with no tasks queued, so identity-encoding state (access
-- discriminants, protected object bookkeeping) restores to identical
-- bytes. Requires a by-reference (tagged or limited) Object_Type.
generic
   type Object_Type is limited private;
package State_Snapshot is

   -- Capture Object's byte image. Only the first call captures; later
   -- calls are no-ops.
   procedure Save (Object : in Object_Type);

   -- Overwrite Object with the captured image. Must not be called
   -- before Save.
   procedure Restore (Object : in out Object_Type);

end State_Snapshot;
