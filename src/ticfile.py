import struct
import zlib

from src.common import log_deep, log_error, byte_length


class Chunk:
    HEADER_LENGTH = 1 + 2 + 1

    CODE_UNCOMPRESSED = 0x5
    CODE_COMPRESSED = 0x10
    DEFAULT_CHUNK = 0x11

    TYPE_MASK = 0x1f

    def __init__(self, chunk_type=None):
        self.valid = False
        self.chunk_type = chunk_type

    def create(self, chunk_id, data=None):
        self.chunk_id = chunk_id
        self.data = data
        return self

    def read(self, chunk):
        if len(chunk) < Chunk.HEADER_LENGTH:
            log_error("Expected chunk header length {}, got {}.".format(
                Chunk.HEADER_LENGTH, len(chunk)))
            return

        self.chunk_id = struct.unpack('B', chunk[:1])[0]
        data_length = struct.unpack('<H', chunk[1:3])[0]
        max_remaining_length = len(chunk) - Chunk.HEADER_LENGTH
        if data_length > max_remaining_length:
            log_error("Expected chunk data length {}, got {}.".format(
                data_length, max_remaining_length))
            return

        begin_offset = Chunk.HEADER_LENGTH
        end_offset = begin_offset + data_length

        self.data = chunk[begin_offset:end_offset]

        return self

    def is_code(self):
        return self.type() in [Chunk.CODE_UNCOMPRESSED, Chunk.CODE_COMPRESSED]

    def length(self):
        return Chunk.HEADER_LENGTH + len(self.data)

    def id(self):
        return self.chunk_id

    def type(self):
        return self.chunk_id & Chunk.TYPE_MASK

    types = {CODE_UNCOMPRESSED: 'Uncompressed code',
             CODE_COMPRESSED: 'Compressed code',
             DEFAULT_CHUNK: 'CHUNK_DEFAULT (Sweetie 16 palette)', }


class Tic:
    def __init__(self, args, data):
        self.valid = False
        self.args = args
        self.with_full_default_chunk = args.pedantic
        self.verbose = 0
        self.chunks = {}
        self.sources = {}

        try:
            data.decode('utf-8')
            self.valid = True
            chunk_id = Chunk.CODE_UNCOMPRESSED
            code_chunk = Chunk().create(chunk_id, data)
            self.chunks[chunk_id] = code_chunk
            self.sources[chunk_id] = code_chunk.data
            self.first_code_chunk_id = chunk_id
            return
        except UnicodeDecodeError:
            pass

        length = len(data)

        offset = 0
        while offset + 4 <= length:

            chunk = Chunk().read(data[offset:])
            if not chunk:
                log_error("Couldn't read chunk")
                return

            assert chunk.id() not in self.chunks
            self.chunks[chunk.id()] = chunk
            offset += chunk.length()
            if offset > length:
                log_error("Truncated TIC file")
                return

            if chunk.is_code():
                if not self.sources:
                    self.first_code_chunk_id = chunk.id()

                if chunk.type() == Chunk.CODE_UNCOMPRESSED:
                    self.sources[chunk.id()] = chunk.data
                else:
                    self.sources[chunk.id()] = zlib.decompress(
                        chunk.data[2:], -15)

        if not self.sources:
            log_error("Couldn't find code chunk")
            return

        self.valid = True

    def source(self):
        return self.sources[self.first_code_chunk_id]

    def write_chunk(self, chunk):
        # https://github.com/nesbox/TIC-80/wiki/tic-File-Format
        # header is 4 bytes large
        # Header[0] -> 16 -> 0x10 -> (BBBCCCCC) 00010000d ->
        # Chunktype: 16 (0x10) - ZLIB Compressed Code (0.80)

        full_chunk = [chunk.id()]

        chunk_type = chunk.type()
        if (chunk_type != Chunk.DEFAULT_CHUNK
           or self.with_full_default_chunk):
            length = len(chunk.data) if chunk.data else 0
            full_chunk += [length & 0xff, length >> 8, 0]

        if chunk.data:
            full_chunk += chunk.data

        if self.verbose:
            s = "    - {0}{1} chunk ({2})".format(
                "Truncated " if len(full_chunk) == 1 else "",
                Chunk.types[chunk_type] if chunk_type in Chunk.types else "Unknown".format(chunk.id()),
                byte_length(len(full_chunk)))

            log_deep(s)

        self.file.write(bytes(full_chunk))

    def write(self, filename, uncompressed_data, compressed_data, verbose=1):
        uncomp_length = len(uncompressed_data)
        final_length = uncomp_length
        if compressed_data:
            final_length = min(final_length, len(compressed_data))
        self.verbose = verbose

        max_code_size = 64 * 1024
        if final_length >= max_code_size:
            if verbose:
                length_unit = "KiB" if self.args.pedantic else 'KB'
                log_error("Code chunk {} exceeds {} {}"
                          .format(final_length,
                                  int((max_code_size + 1023) / 1024),
                                  length_unit))
            return

        with open(filename, 'wb') as self.file:
            if final_length < uncomp_length:
                code_chunk_data = compressed_data
                code_chunk_type = Chunk.CODE_COMPRESSED
            else:
                code_chunk_data = uncompressed_data
                code_chunk_type = Chunk.CODE_UNCOMPRESSED

            if self.verbose:
                print("Writing " + filename)

            skip_chunk_types = [Chunk.DEFAULT_CHUNK]
            wrote_code_chunk = False
            for chunk_id in self.chunks:
                # print("chunk", chunk_id)
                chunk = self.chunks[chunk_id]

                if chunk.type() in skip_chunk_types:
                    continue

                if chunk_id == self.first_code_chunk_id:
                    wrote_code_chunk = True
                    chunk.chunk_id = ((chunk_id & ~Chunk.TYPE_MASK)
                                      + code_chunk_type)
                    chunk.data = code_chunk_data

                self.write_chunk(chunk)

            if not wrote_code_chunk:
                code_chunk = Chunk().create(code_chunk_type, code_chunk_data)
                self.write_chunk(code_chunk)

            if self.args.default_chunk:
                chunk_id = Chunk.DEFAULT_CHUNK
                default = Chunk().create(chunk_id)
                self.chunks[chunk_id] = default
                self.write_chunk(default)

        # [TODO] debug
        # with open('output.lua', 'wb') as file:
        #     file.write(uncompressed_data)
