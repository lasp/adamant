
; This script handles setting up the packet specific pages
{% for id, packet in packets.items() %}
create_page Ccsds-{{ packet.full_name|replace(".","-") }}
{% endfor %}
echo "All packet pages are complete."
