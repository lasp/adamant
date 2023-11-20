-- This package contains types common to type serializers.
package Serializer_Types is
   -- Status determining whether a serialization or
   -- deserialization was successful.
   type Serialization_Status is (Success, Failure);
end Serializer_Types;
