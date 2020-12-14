# TIC80 packer
#  Uses the zlib code chunk to crunch down your source
#  https://github.com/nesbox/TIC-80/wiki/tic-File-Format
import sys
import zlib

with open(sys.argv[1], mode='rb') as file:
  uncomp = file.read()
print("Uncompressed length: {} bytes".format(len(uncomp)))
comp = zlib.compress(uncomp)
print("Compressed length:   {} bytes".format(len(comp)))
print("With header:         {} bytes".format(len(comp)+4))
with open(sys.argv[1]+".tic", 'wb') as file:
  file.write(bytes([16]))
  file.write(bytes([len(comp) & 0xFF]))
  file.write(bytes([len(comp) >> 8]))
  file.write(bytes([0]))
  file.write(comp)