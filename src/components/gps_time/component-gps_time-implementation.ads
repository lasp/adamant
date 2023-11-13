--------------------------------------------------------------------------------
-- Gps_Time Component Implementation Spec
--------------------------------------------------------------------------------

-- Standard Includes:
-- Invokee Connector Includes:
with Sys_Time;

-- The System Time component is a servicing component which provides the system time in GPS format to any component who requests it. Internally, the system time is provided by the Ada.Real_Time library.
package Component.Gps_Time.Implementation is

   -- The component class instance record:
   type Instance is new Gps_Time.Base_Instance with private;

private

   -- The component class instance record:
   type Instance is new Gps_Time.Base_Instance with record
      null; -- No internal state
   end record;

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- The system time is provided via this connector.
   overriding function Sys_Time_T_Return (Self : in out Instance) return Sys_Time.T;

end Component.Gps_Time.Implementation;
