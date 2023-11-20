with GNAT.Sockets; use GNAT.Sockets;

package Socket is

   subtype Inet_Addr_Ipv4 is Inet_Addr_Type (Family => Family_Inet);
   type Instance is tagged private;

   procedure Connect (Self : in out Instance; Addr : in String; Port : in Natural);
   procedure Disconnect (Self : in out Instance);
   function Is_Connected (Self : in Instance) return Boolean;
   function Get_Port (Self : in Instance) return Port_Type;
   function Get_Ip_Address (Self : in Instance) return Inet_Addr_V4_Type;
   -- Note: Send and receive methods are provided
   -- via the serialize and deserialize methods
   -- in the Stream_Serializer package.
   --
   -- Return the stream for use with the Stream_Serializer
   -- package.
   function Stream (Self : in Instance) return Stream_Access;

private

   type Instance is tagged record
      Address : Sock_Addr_Type;
      Socket : Socket_Type;
      Channel : Stream_Access;
      Connected : Boolean := False;
   end record;

end Socket;
