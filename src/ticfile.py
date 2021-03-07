from src.common import log_deep, log_error, byte_length


class Tic:
    chunk_types = {0x05: 'Uncompressed code',
                   0x10: 'Compressed code',
                   0x11: 'CHUNK_DEFAULT (Sweety16 palette)', }

    def __init__(self,
                 with_default_chunk=False,
                 with_full_default_chunk=True,
                 with_checksum=False):
        self.with_default_chunk = with_default_chunk
        self.with_full_default_chunk = with_full_default_chunk
        self.with_checksum = with_checksum
        self.verbose = 0

    def write_chunk(self, chunk_type, data=None):
        # https://github.com/nesbox/TIC-80/wiki/tic-File-Format
        # header is 4 bytes large
        # Header[0] -> 16 -> 0x10 -> (BBBCCCCC) 00010000d ->
        # Chunktype: 16 (0x10) - ZLIB Compressed Code (0.80)

        chunk = []
        chunk.append(chunk_type)

        if chunk_type != 0x11 or self.with_full_default_chunk:
            length = len(data) if data else 0
            chunk += [length & 0xFF, length >> 8, 0]

        if data:
            chunk += data

        if self.verbose:
            s = "    - {0}{1} chunk ({2})".format(
                "Truncated " if len(chunk) == 1 else "",
                self.chunk_types[chunk_type],
                byte_length(len(chunk)))

            log_deep(s)

        self.file.write(bytes(chunk))

    def write(self, filename, uncompressed_data, compressed_data, verbose=1):
        uncomp_length = len(uncompressed_data)
        final_length = uncomp_length
        if compressed_data:
            final_length = min(final_length, len(compressed_data))
        self.verbose = verbose

        max_code_size = 64 * 1024 + 1023
        if final_length >= max_code_size:
            if verbose:
                length_unit = "KiB" if byte_length(0).count('byte') else 'KB'
                log_error("Code chunk {} exceeds {} {}"
                          .format(final_length,
                                  int((max_code_size + 1023) / 1024),
                                  length_unit))
            return

        with open(filename, 'wb') as self.file:
            if final_length < uncomp_length:
                chunk_data = compressed_data
                chunk_type = 0x10
            else:
                chunk_data = uncompressed_data
                chunk_type = 0x05

            if self.verbose:
                print("Writing " + filename)

            self.write_chunk(chunk_type, chunk_data)

            if self.with_default_chunk:
                self.write_chunk(0x11)

        # [TODO] debug
        # with open('output.lua', 'wb') as file:
        #     file.write(uncompressed_data)
