from ctypes import byref, CDLL, c_char, c_int, c_size_t, \
    c_void_p, pointer, Structure
import os
import platform


class ZopfliOptions(Structure):
    _fields_ = [("verbose", c_int),
                ("verbose_more", c_int),
                ("num_iterations", c_int),
                ("blocksplitting", c_int),
                ("unused_blocksplittinglast", c_int),
                ("blocksplittingmax", c_int)]


compressor = None


def zopfli_compress(bytes_in, use_extreme=False):
    global compressor

    if not compressor:
        if compressor == 0:
            return None

        append = '.so'
        system = platform.system()
        if system == 'Darwin':
            append = '.dylib'
        elif system == 'Windows':
            # append = ('32' if sizeof(c_void_p) == 4 else '64') + '.dll'
            append = '.dll'

        path = os.path.join(os.path.dirname(__file__),
                            '..', 'lib', 'libzopfli' + append)
        try:
            compressor = CDLL(path)
        except OSError:
            print("WARNING: Not using Zopfli"
                  " (shared library not found at {}).".format(path))
            compressor = 0
            return None

    options = ZopfliOptions()
    compressor.ZopfliInitOptions.restype = None
    compressor.ZopfliInitOptions(byref(options))

    # options.verbose = 1
    # options.verbose_more = 1

    if use_extreme:
        options.num_iterations = 20
        options.blocksplitting = 1
        options.blocksplittingmax = 0
    else:
        # Faster compression settings that should result in same compression
        # ratio as extreme settings. If not these need to be tweaked!
        options.num_iterations = 10
        options.blocksplitting = 1
        options.blocksplittingmax = 2

    num_in = len(bytes_in)

    bytes_out = c_void_p(None)
    num_out = c_size_t(0)

    compressor.ZopfliZlibCompress.restype = None
    compressor.ZopfliZlibCompress(
        byref(options),
        bytes_in, c_size_t(num_in),
        pointer(bytes_out), pointer(num_out))

    if not num_out:
        assert False, "zopfli compression failed?!"
        return None

    return bytes((c_char * num_out.value).from_address(bytes_out.value))
