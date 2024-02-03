require 'openc3/config/config_parser'
require 'openc3/interfaces/protocols/protocol'
require 'openc3/utilities/crc'
require 'thread'

module OpenC3
  # Creates a checksum on write and inserts into packet
  class CmdChecksum < Protocol

    # @param write_item_name [String/nil] Item to fill with calculated CRC value for outgoing packets (nil = don't fill)
    # @param allow_empty_data [true/false/nil] See Protocol#initialize
    def initialize(
      write_item_name = nil,
      allow_empty_data = nil
    )
      super(allow_empty_data)
      @write_item_name = ConfigParser.handle_nil(write_item_name)
      @allow_empty_data = ConfigParser.handle_true_false(allow_empty_data)

    end

    def write_packet(packet, seed = 0xFF)
      if @write_item_name
        crc = packet.buffer.bytes.reduce(seed, :^)
        packet.write(@write_item_name, crc)
      end
      packet
    end
  end
end
