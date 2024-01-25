require 'openc3/config/config_parser'
require 'openc3/interfaces/protocols/protocol'
require 'openc3/utilities/crc'
require 'thread'

module OpenC3
  # Creates a checksum on write and inserts into packet
  class BitChecksum < Protocol

    # @param write_item_name [String/nil] Item to fill with calculated CRC value for outgoing packets (nil = don't fill)
    # @param bit_offset [Integer] Bit offset of the CRC in the data.  Can be negative to indicate distance from end of packet
    # @param bit_size [Integer] Bit size of the CRC - Must be 8, 16, 32, or 64
    # @param allow_empty_data [true/false/nil] See Protocol#initialize
    def initialize(
      write_item_name = nil,
      bit_size = 8,
      allow_empty_data = nil
    )
      super(allow_empty_data)
      @write_item_name = ConfigParser.handle_nil(write_item_name)
      @allow_empty_data = ConfigParser.handle_true_false(allow_empty_data)

      begin
        @bit_offset = Integer(bit_offset)
      rescue
        raise "Invalid bit offset of #{bit_offset}. Must be a number."
      end
      raise "Invalid bit offset of #{bit_offset}. Must be divisible by 8." if @bit_offset % 8 != 0

      begin
        @bit_size = Integer(bit_size)
      rescue
        raise "Invalid bit sise of #{bit_size}. Must be a number."
      end
      raise "Invalid bit size of #{bit_size}. Must be divisible by 8." if @bit_size % 8 != 0

      @bit_size = bit_size.to_i

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
