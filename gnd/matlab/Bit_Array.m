classdef Bit_Array
    %Bit_Array Class which provides ability to extract bits of the
    % underlying data into various types, and set bits of the underlying
    % data from various types.

    properties
        Data
        last_byte_set
    end

    methods
        function obj = Bit_Array(data)
            % Store the data passed in as a byte array without modifying the
            % underlying bits of the passed data type in any way. Usually
            % an array of uint8 bytes should be passed in.
            if nargin == 1
              obj.Data = typecast(data, 'uint8');
            end
        end

        function obj = zeros(obj, num_bytes)
            obj.Data = zeros(1, num_bytes, 'uint8');
        end

        function out = bytes(obj)
            % Return the internal uint8 array:
            out = obj.Data;
        end

        function num_bits = size(obj)
            % Return size in bits
            num_bits = length(obj.Data)*8;
        end

        function data = extract(obj, start_bit, end_bit, type)
            % Extract type from internal bit array from 'start_bit' to
            % 'end_bit' and return the extraction in the desired 'type'. This
            % function assumes the underlying data is a big endian
            % representation of 'type'.

            % Validate input arguments:
            [num_bits, start_byte_idx, start_bit_idx, end_byte_idx, end_bit_idx] = ...
                obj.check_bit_indexes(start_bit, end_bit);

            % Handling for output type:
            orig_type = type;
            [is_float, type, is_signed] = obj.check_type(type);

            % Make sure indexes are within the output object:
            data_bit_length = obj.type_length_in_bits(type);
            if data_bit_length < num_bits
                error("number of bits to extract (" + num_bits + ") exceeds the number of bits specified in the output type '" + type + "' (" + data_bit_length + ")");
            end

            % Extract bits from internal data:
            bits = [];
            if end_byte_idx == start_byte_idx
                % Special case if we are extracting from a single byte in
                % the source data.
                bits = [bits, bitget(obj.Data(start_byte_idx),9-start_bit_idx:-1:9-end_bit_idx, 'uint8')];
            else
                % Go through each byte of the source and extract the
                % appropriate bits from that byte and append them to a bit
                % array to return to the caller.
                for byte_idx=start_byte_idx:end_byte_idx
                    switch byte_idx
                        case end_byte_idx
                            bits = [bits, bitget(obj.Data(byte_idx),8:-1:9-end_bit_idx, 'uint8')];
                        case start_byte_idx
                            bits = [bits, bitget(obj.Data(byte_idx),9-start_bit_idx:-1:1, 'uint8')];
                        otherwise
                            bits = [bits, bitget(obj.Data(byte_idx),8:-1:1, 'uint8')];
                    end
                end
            end

            % Set bits in output:
            data_idx = 1;
            data = zeros(type);
            for idx=length(bits):-1:1
                data = bitset(data, data_idx, bits(idx));
                data_idx = data_idx + 1;
            end

            % Convert to two's complement if necessary:
            if is_signed
              if data < 2^(num_bits - 1)
                data = typecast(data, orig_type);
              else
                data = -1*(2^num_bits - typecast(data, orig_type));
              end
            end

            % Convert to float if necessary:
            if is_float
               data = typecast(data, orig_type);
            end
        end

        function obj = set(obj, start_bit, end_bit, data)
            % Set internal bit array from 'start_bit' to 'end_bit' with the
            % right most bits in 'data'. This function assumes the underlying
            % data is big endian.

            % Make sure data is not arrayed.
            if length(data) ~= 1
              error("data can not be an arrayed type; it has a length of: " + length(data));
            end

            % Validate input arguments:
            [num_bits, start_byte_idx, start_bit_idx, end_byte_idx, ~] = ...
                obj.check_bit_indexes(start_bit, end_bit);

            % Make sure we can handle the type of the input data:
            orig_type = class(data);
            [is_float, type, is_signed] = obj.check_type(orig_type);

            % Make sure we can set the bits appropriately.
            type_bits = obj.type_length_in_bits(type);
            max_bits = obj.size();
            if num_bits > type_bits
                error("cannot set " + num_bits + " bits because source type '" + orig_type + "' is only " + type_bits + " in size.");
            end
            if start_bit + num_bits - 1 > max_bits
                error("cannot set data of type '" + orig_type + "' and size " + num_bits + " bits at start_bit " + start_bit + " because underlying bit array is only " + max_bits + " bits in size");
            end

            % Convert float to integer representation for bit operations:
            if is_float
               data = typecast(data, type);
            end

            % Convert two's complement to unsigned representation for bit operations:
            if is_signed
              if data >= 0
                data = typecast(data, type);
              else
                data = typecast(data + 2^num_bits, type);
              end
            end

            % Extract bits from source:
            src_offset = type_bits - num_bits;
            src_bits = bitget(data, type_bits-src_offset:-1:1, type);

            % Set the bits in the internal bit array:
            src_bits_idx = 1;
            dest_bits_idx = start_bit_idx;
            for byte_idx=start_byte_idx:end_byte_idx
                while src_bits_idx <= length(src_bits) && dest_bits_idx <= 8
                    obj.Data(byte_idx) = bitset(obj.Data(byte_idx), 9 - dest_bits_idx, src_bits(src_bits_idx));
                    src_bits_idx = src_bits_idx + 1;
                    dest_bits_idx = dest_bits_idx + 1;
                    obj.last_byte_set = byte_idx;
                end
                dest_bits_idx = 1;
            end
        end
    end

    methods (Access = private)
        function [is_float, uint_type, is_signed] = check_type(obj, type)
            % Handling for output type:
            is_float = false;
            uint_type = type;
            is_signed = false;
            switch type
                case 'single'
                    is_float = true;
                    uint_type = 'uint32';
                case 'double'
                    is_float = true;
                    uint_type = 'uint64';
                case 'int8'
                    is_signed = true;
                    uint_type = 'uint8';
                case 'int16'
                    is_signed = true;
                    uint_type = 'uint16';
                case 'int32'
                    is_signed = true;
                    uint_type = 'uint32';
                case 'int64'
                    is_signed = true;
                    uint_type = 'uint64';
                case 'uint8'
                case 'uint16'
                case 'uint32'
                case 'uint64'
                otherwise
                    error("unrecognized type '" + type + "' - must be one of 'uint64' | 'uint32' | 'uint16' | 'uint8' | 'int64' | 'int32' | 'int16' | 'int8' | 'single' | 'double'");
            end
        end

        function num_bits = type_length_in_bits(obj, type)
            data = zeros(type);
            num_bits = length(typecast(data, 'uint8'))*8;
        end

        function [num_bits, start_byte_idx, start_bit_idx, end_byte_idx, end_bit_idx] = check_bit_indexes(obj, start_bit, end_bit)
            num_bits = end_bit - start_bit + 1;
            % Validate input arguments:
            if start_bit < 1
                error('start_bit must be a positive integer');
            end
            if end_bit < 1
                error('end_bit must be a positive integer');
            end
            if num_bits <= 0
                error("end_bit must be >= start_bit");
            end

            % Calculate useful indexes:
            start_byte_idx = idivide(uint64(start_bit + 7), uint64(8));
            start_bit_idx = mod(start_bit - 1, 8) + 1;
            end_byte_idx = idivide(uint64(end_bit + 7), uint64(8));
            end_bit_idx = mod(end_bit - 1, 8) + 1;

            % Make sure indexes are within the data stored in the object:
            if start_byte_idx > length(obj.Data)
                error("start_bit is too large for internal data, in bytes: " + start_byte_idx + " > " + length(obj.Data));
            end
            if end_byte_idx > length(obj.Data)
                error("end_bit is too large for internal data, in bytes: " + end_byte_idx + " > " + length(obj.Data));
            end
        end
    end
end

