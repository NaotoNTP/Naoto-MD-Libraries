# NaotoLZ (NLZ) Decompression Library
A decompression library for a custom LZSS variant, written in Motorola 68000 assembly language.

# Contents
- Decompressor and queueing system source code (Motorola 68K assembly).
- A pre-built fork of the 'clownlzss' compression utility which supports this format. (Source code for this fork can be found here: https://github.com/NaotoNTP/clownlzss/tree/NLZ-Support).
- Zero-clause BSD (0BSD) license.
- This readme.

# Notes
This compression format was designed and optimized for the SEGA Mega Drive's 4bpp tile-based art dat. That said, it performs respectably on other, more general forms of data as well.

As of v1.1.0, the decompressor offers the ability to asynchronously decompress data archives to arbitrary system RAM locations as well, rather than reserving that functionality for art loading only.
