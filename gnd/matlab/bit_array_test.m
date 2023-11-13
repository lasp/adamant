%
% The purpose of this script is to unit test the Bit_Array class.
%

% Test empty bit array creation:
ba = Bit_Array();
assert(isempty(ba.bytes()));
assert(ba.size() == 0);

% Test zeros:
ba = ba.zeros(5);
assert(all(ba.bytes() == uint8([0, 0, 0, 0, 0])));
assert(ba.size() == 5*8);

ba = ba.zeros(1);
assert(all(ba.bytes() == uint8(0)));
assert(ba.size() == 1*8);

ba = ba.zeros(4);
assert(all(ba.bytes() == uint8([0, 0, 0, 0])));
assert(ba.size() == 4*8);

% Test initialization with data:
ba = Bit_Array(uint8(7));
assert(all(ba.bytes() == uint8(7)));
assert(ba.size() == 1*8);

ba = Bit_Array(swapbytes(uint16(7)));
assert(all(ba.bytes() == uint8([0, 7])));
assert(ba.size() == 2*8);

ba = Bit_Array(swapbytes(uint16(0x0a0b)));
assert(all(ba.bytes() == uint8([10, 11])));
assert(ba.size() == 2*8);

ba = Bit_Array(swapbytes(uint32(0x0a0b0c0d)));
assert(all(ba.bytes() == uint8([10, 11, 12, 13])));
assert(ba.size() == 4*8);

ba = Bit_Array(swapbytes(single(2.22199988365)));
assert(all(ba.bytes() == uint8([0x40, 0x0e, 0x35, 0x3f])));
assert(ba.size() == 4*8);

ba = Bit_Array(uint8([1, 2, 3, 4, 5]));
assert(all(ba.bytes() == uint8([1, 2, 3, 4, 5])));
assert(ba.size() == 5*8);

% Test serialization & deserialization:
ba = ba.zeros(4);
ba = ba.set(1, 32, uint32(0x0a0b0c0d));
assert(all(ba.bytes() == uint8([0x0a, 0x0b, 0x0c, 0x0d])));
assert(ba.extract(1, 32, 'uint32') == uint32(0x0a0b0c0d));
assert(ba.extract(9, 32, 'uint32') == uint32(0x0b0c0d));
assert(ba.extract(17, 32, 'uint32') == uint32(0x0c0d));
assert(ba.extract(25, 32, 'uint32') == uint32(0x0d));
assert(ba.extract(9, 24, 'uint16') == uint16(0x0b0c));
assert(ba.extract(17, 32, 'uint16') == uint16(0x0c0d));
assert(ba.extract(25, 32, 'uint16') == uint16(0x0d));
assert(ba.extract(1, 8, 'uint8') == uint8(0x0a));
assert(ba.extract(9, 16, 'uint8') == uint8(0x0b));
assert(ba.extract(17, 24, 'uint8') == uint8(0x0c));
assert(ba.extract(25, 32, 'uint8') == uint8(0x0d));

ba = ba.zeros(4);
ba = ba.set(9, 16, uint8(10));
assert(all(ba.bytes() == uint8([0x0, 0xa, 0x0, 0x0])));
assert(ba.extract(9, 16, 'uint8') == uint8(0xa));

ba = ba.zeros(4);
ba = ba.set(14, 21, uint8(255));
assert(all(ba.bytes() == uint8([0x0, 0b00000111, 0b11111000, 0x0])));
assert(ba.extract(14, 21, 'uint8') == uint8(255));

ba = ba.zeros(4);
ba = ba.set(1, 32, single(2.22199988365));
assert(all(ba.bytes() == uint8([0x40, 0x0e, 0x35, 0x3f])));
assert(ba.extract(1, 32, 'single') == single(2.22199988365));

ba = ba.zeros(4);
ba = ba.set(32, 32, uint8(1));
assert(all(ba.bytes() == uint8([0x00, 0x00, 0x00, 0x01])));
assert(ba.extract(32, 32, 'uint8') == uint8(1));
ba = ba.set(31, 31, uint8(1));
assert(all(ba.bytes() == uint8([0x00, 0x00, 0x00, 0x03])));
assert(ba.extract(32, 32, 'uint8') == uint8(1));
assert(ba.extract(31, 31, 'uint8') == uint8(1));
assert(ba.extract(31, 32, 'uint8') == uint8(3));
ba = ba.set(30, 30, uint8(1));
assert(all(ba.bytes() == uint8([0x00, 0x00, 0x00, 0x07])));
assert(ba.extract(30, 30, 'uint8') == uint8(1));
assert(ba.extract(31, 31, 'uint8') == uint8(1));
assert(ba.extract(30, 32, 'uint8') == uint8(7));

ba = Bit_Array(uint8(255));
assert(ba.extract(1, 1, 'uint8') == uint8(1));

ba = Bit_Array(uint8([255, 255]));
assert(ba.extract(9, 9, 'uint8') == uint8(1));
assert(ba.extract(6, 10, 'uint8') == uint8(0x1f));

ba = Bit_Array(uint8([255, 255, 255, 255]));
assert(ba.extract(12, 23, 'uint16') == uint16(0x0fff));

ba = Bit_Array(uint8([233, 255, 235, 255]));
assert(ba.extract(11, 22, 'uint16') == uint16(0x0ffa));

ba = Bit_Array(uint8(0x2));
assert(ba.extract(7, 8, 'uint8') == uint8(0x2));

ba = Bit_Array(uint8([0x55, 0xaa, 0x55, 0xaa]));
assert(ba.extract(5, 12, 'uint8') == uint8(0x5a));
assert(ba.extract(5, 20, 'uint16') == uint16(0x5aa5));
assert(ba.extract(4, 19, 'uint16') == uint16(0xad52));
assert(ba.extract(3, 19, 'uint32') == uint16(0xad52));
assert(ba.extract(4, 20, 'uint32') == uint32(0x15aa5));
assert(ba.extract(1, 9, 'uint16') == uint16(0xab));