import ctypes
import os
import platform


class ZopfliOptions(ctypes.Structure):
    _fields_ = [("verbose", ctypes.c_int),
                ("verbose_more", ctypes.c_int),
                ("num_iterations", ctypes.c_int),
                ("blocksplitting", ctypes.c_int),
                ("unused_blocksplittinglast", ctypes.c_int),
                ("blocksplittingmax", ctypes.c_int)]


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
            # append = str(ctypes.sizeof(ctypes.c_void_p) * 8) + '.dll'
            append = '.dll'

        path = os.path.join(os.path.dirname(__file__),
                            '..', 'lib', 'libzopfli' + append)
        try:
            compressor = ctypes.CDLL(path)
        except OSError:
            print("WARNING: Not using Zopfli"
                  " (shared library not found at {}).".format(path))
            compressor = 0
            return None

    options = ZopfliOptions()
    compressor.ZopfliInitOptions.restype = None
    compressor.ZopfliInitOptions(ctypes.byref(options))

    # options.verbose = 1
    # options.verbose_more = 1

    if use_extreme:
        options.num_iterations = 400
        options.blocksplitting = 1
        options.blocksplittingmax = 0
    else:
        # Faster compression settings that should result in same compression
        # ratio as extreme settings. If not these need to be tweaked!
        options.num_iterations = 14
        options.blocksplitting = 1
        options.blocksplittingmax = 3

    num_in = len(bytes_in)

    bytes_out = ctypes.c_void_p(None)
    num_out = ctypes.c_size_t(0)

    compressor.ZopfliZlibCompress.restype = None
    compressor.ZopfliZlibCompress(
        ctypes.byref(options),
        bytes_in, ctypes.c_size_t(num_in),
        ctypes.pointer(bytes_out), ctypes.pointer(num_out))

    if not num_out:
        # assert False, "zopfli compression failed?!"
        return None

    return bytes((ctypes.c_char * num_out.value).from_address(bytes_out.value))
