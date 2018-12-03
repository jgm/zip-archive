zip-archive
===========

The zip-archive library provides functions for creating, modifying,
and extracting files from zip archives.

Certain simplifying assumptions are made about the zip archives:
in particular, there is no support for strong encryption, zip
files that span multiple disks, ZIP64, OS-specific file
attributes, or compression methods other than Deflate.  However,
the library should be able to read the most common zip archives,
and the archives it produces should be readable by all standard
unzip programs.

Archives are extracted in memory, so this library is not
suitable for use with large zip files that might exhaust
available memory.  An advantage of this approach is that it does
not require IO: zip archives can be constructed and read in
"pure" contexts.

As an example of the use of the library, a standalone zip archiver
and extracter, Zip.hs, is provided in the source distribution.

For more information on the format of zip archives, consult
<http://www.pkware.com/documents/casestudies/APPNOTE.TXT>

For a more feature-complete pure-Haskell library for manipulating
zip archives, see <http://hackage.haskell.org/package/zip>.
