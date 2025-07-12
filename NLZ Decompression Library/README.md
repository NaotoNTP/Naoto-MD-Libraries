# NaotoLZ (NLZ) Decompression Library
A decompression library for a custom LZSS variant, written in Motorola 68000 assembly language.

# Contents
- Decompressor and queueing system source code (Motorola 68K assembly).
- A pre-built fork of the 'clownlzss' compression utility which supports this format. (Source code for this fork can be found here: https://github.com/NaotoNTP/clownlzss/tree/NLZ-Support).
- Zero-clause BSD (0BSD) license.
- This readme.

# Notes
This compression algorithm was designed and optimized for the SEGA Mega Drive's 4bpp tile-based art format. As such, please primarily think of this as a Mega Drive art compression format. While it's still possible to use algorithm to compress other types of data, there are other algorithms which are better suited to more generalized use cases.
