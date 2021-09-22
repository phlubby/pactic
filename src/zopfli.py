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


def zopfli_compress(bytes_in, iter_count=-1):
    global compressor

    if not compressor:
        if compressor == 0:
            return None

        append = '.so'
        system = platform.system()
        if system == 'Darwin':
            append = '.dylib'
        elif system == 'Windows':
            append = str(ctypes.sizeof(ctypes.c_void_p) * 8) + '.dll'
        search_paths = []

        if append != '.dll':
            search_paths += ['/usr/lib/']

        search_paths += [os.path.join(os.path.dirname(__file__), '..', 'lib')]

        lib_name = 'libzopfli' + append

        found = False
        for path in search_paths:
            try:
                path = os.path.join(path, lib_name)
                compressor = ctypes.CDLL(path)
                found = True
                break
            except OSError:
                pass

        if not found:
            print("\nWARNING: Not using Zopfli"
                  " (shared library {} not found at {}).\n".format(
                    lib_name, str(search_paths)[1:-1]))
            if append == '.so':
                s = "Consider using e.g. your favourite package manager to"
                s += " install libzopfli. Because it's worth it.\n"
                print(s)

            compressor = 0
            return None

    options = ZopfliOptions()
    compressor.ZopfliInitOptions.restype = None
    compressor.ZopfliInitOptions(ctypes.byref(options))

    # options.verbose = 1
    # options.verbose_more = 1

    if iter_count != -1:
        options.num_iterations = iter_count
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

    return bytes(bytearray((ctypes.c_char * num_out.value)
                           .from_address(bytes_out.value)))
