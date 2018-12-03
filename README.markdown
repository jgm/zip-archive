zip-archive
===========

The zip-archive library provides functions for creating, modifying,
and extracting files from zip archives.  The zip archive format
is documented in
<http://www.pkware.com/documents/casestudies/APPNOTE.TXT>.

Certain simplifying assumptions are made about the zip archives:
in particular, there is no support for strong encryption, zip
files that span multiple disks, ZIP64, OS-specific file
attributes, or compression methods other than Deflate.  However,
the library should be able to read the most common zip archives,
and the archives it produces should be readable by all standard
unzip programs.

Archives are built and extracted in memory, so manipulating
large zip files will consume a lot of memory.  If you work with
large zip files or need features not supported by this library,
a better choice may be [zip](http://hackage.haskell.org/package/zip),
which uses a memory-efficient streaming approach.  However, zip
can only read and write archives inside instances of MonadIO, so
zip-archive is a better choice if you want to manipulate zip
archives in "pure" contexts.

As an example of the use of the library, a standalone zip archiver
and extracter is provided in the source distribution.
