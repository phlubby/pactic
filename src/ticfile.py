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
        self.header_length = Chunk.HEADER_LENGTH

    def create(self, chunk_id, data=None):
        self.chunk_id = chunk_id
        self.header_length = Chunk.HEADER_LENGTH
        self.data = data
        return self

    def read(self, chunk):
        self.header_length = 0
        self.data = bytearray()

        def eof(num_bytes):
            if len(chunk) < self.header_length + num_bytes:
                return True

            return False

        def advance(num_bytes):
            self.header_length += num_bytes

        truncated_error = "Truncated TIC-80 cart file"
        if eof(1):
            log_error(truncated_error)
            return

        self.chunk_id = struct.unpack('B', chunk[:1])[0]
        advance(1)

        # self.header_length = min(self.HEADER_LENGTH, len(chunk))
        if eof(1):
            return self

        if eof(2):
            log_error(truncated_error)
            assert False
            return

        data_length = struct.unpack('<H', chunk[1:3])[0]
        advance(2)

        if eof(1):
            return self

        advance(1)

        assert self.header_length == Chunk.HEADER_LENGTH

        if data_length:
            max_remaining_length = len(chunk) - self.header_length

            begin_offset = Chunk.HEADER_LENGTH
            end_offset = begin_offset + min(max_remaining_length, data_length)

            self.data = chunk[begin_offset:end_offset]

        return self

    def is_code(self):
        return self.type() in [Chunk.CODE_UNCOMPRESSED, Chunk.CODE_COMPRESSED]

    def length(self):
        return self.header_length + len(self.data)

    def id(self):
        return self.chunk_id

    def type(self):
        return self.chunk_id & Chunk.TYPE_MASK

    types = {CODE_UNCOMPRESSED: 'Uncompressed code',
             CODE_COMPRESSED: 'Compressed code',
             DEFAULT_CHUNK: 'CHUNK_DEFAULT (Sweetie 16 palette)', }


class Tic:
    def __init__(self, args):
        self.valid = False
        self.args = args
        self.with_full_default_chunk = args.pedantic
        self.verbose = 0
        self.chunks = {}
        self.sources = {}
        self.error_msg = ""

    def read(self, data, requires_source=True):
        ok = self.do_read(data)

        if requires_source and not self.sources:
            try:
                data.decode('utf-8')
                self.valid = True
                chunk_id = Chunk.CODE_UNCOMPRESSED
                code_chunk = Chunk().create(chunk_id, data)
                self.chunks = {}
                self.chunks[chunk_id] = code_chunk
                self.sources[chunk_id] = code_chunk.data
                self.first_code_chunk_id = chunk_id
                return self
            except UnicodeDecodeError:
                if ok:
                    log_error("Couldn't decode source")
                    return

        if not ok:
            log_error(self.error_msg)
            return

        return self

    def do_read(self, data):

        length = len(data)

        offset = 0
        while offset + 1 <= length:

            chunk = Chunk().read(data[offset:])
            if not chunk:
                return self.error("Couldn't read chunk")

            assert chunk.id() not in self.chunks
            self.chunks[chunk.id()] = chunk
            offset += chunk.length()
            if offset > length:
                return self.error("Truncated TIC file")

            if chunk.is_code():
                if not self.sources:
                    self.first_code_chunk_id = chunk.id()

                if chunk.type() == Chunk.CODE_UNCOMPRESSED:
                    self.sources[chunk.id()] = chunk.data
                else:
                    self.sources[chunk.id()] = zlib.decompress(
                        chunk.data[2:], -15)

        if not self.sources:
            return self.error("Couldn't find code chunk")

        self.valid = True
        return True

    def error(self, s):
        self.error_msg = s
        return False

    def source(self):
        return self.sources[self.first_code_chunk_id]

    def create_chunk(self, chunk):
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

        return bytearray(full_chunk)

    def write_chunks(self, filename, tic_file_chunks):
        with open(filename, 'wb') as self.file:
            self.file.write(bytearray(tic_file_chunks))

    def write(self, filename, uncompressed_data, compressed_data, verbose=1):
        if verbose:
            print("Writing " + filename)
            print("")
        tic_file = self.create(uncompressed_data, compressed_data, verbose)
        self.write_chunks(filename, tic_file)

    def create(self, uncompressed_data, compressed_data, verbose=0):
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

        if final_length < uncomp_length:
            code_chunk_data = compressed_data
            code_chunk_type = Chunk.CODE_COMPRESSED
        else:
            code_chunk_data = uncompressed_data
            code_chunk_type = Chunk.CODE_UNCOMPRESSED

        wrote_code_chunk = False
        write_default_chunk = self.args.default_chunk

        tic_file_chunks = bytearray()

        for chunk_id in self.chunks:
            chunk = self.chunks[chunk_id]

            if chunk.type() == Chunk.DEFAULT_CHUNK:
                write_default_chunk = True
                continue

            if chunk_id == self.first_code_chunk_id:
                wrote_code_chunk = True
                chunk.chunk_id = ((chunk_id & ~Chunk.TYPE_MASK)
                                    + code_chunk_type)
                chunk.data = code_chunk_data

            tic_file_chunks += self.create_chunk(chunk)
        if not wrote_code_chunk:
            code_chunk = Chunk().create(code_chunk_type, code_chunk_data)
            tic_file_chunks += self.create_chunk(code_chunk)

        if write_default_chunk:
            chunk_id = Chunk.DEFAULT_CHUNK
            default = Chunk().create(chunk_id)
            self.chunks[chunk_id] = default
            tic_file_chunks += self.create_chunk(default)

        # [TODO] debug
        # with open('output.lua', 'wb') as file:
        #     file.write(uncompressed_data)
        return tic_file_chunks
