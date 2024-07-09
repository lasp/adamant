-- Standard includes:
with Ccsds_Downsampler_Types; use Ccsds_Downsampler_Types;

{% if description %}
{{ printMultiLine(description, '-- ') }}
{% endif %}
package {{ name }} is
   -- Size of the initial list of the downsampler
   Downsample_List_Size_In_Bytes : constant Natural := {{ size }};
   -- The initial list for the downsampler
   Downsample_List : aliased Ccsds_Downsample_Packet_List := [
{% for apid in filter_products.values() %}
      -- {{ apid.name }}
   {% if apid.description %}
   {{ printMultiLine(apid.description, '-- ') }}
   {% endif %}
   {{ loop.index0 }} => (
         Apid => {{ apid.apid }},
         Filter_Factor => {{ apid.filter_factor}}
      ){{ "," if not loop.last }}
{% endfor %}
   ];

   -- Access for the downsample list
   Downsample_List_Access : Ccsds_Downsample_Packet_List_Access := Downsample_List'Access;

end {{ name }};
