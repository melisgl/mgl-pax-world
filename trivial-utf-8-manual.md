<a id="x-28TRIVIAL-UTF-8-3A-40TRIVIAL-UTF-8-MANUAL-20MGL-PAX-3ASECTION-29"></a>
<a id="TRIVIAL-UTF-8:@TRIVIAL-UTF-8-MANUAL%20MGL-PAX:SECTION"></a>

# Trivial UTF-8 Manual

## Table of Contents

- [1 Introduction][870d]
- [2 Links and Systems][3450]
- [3 Reference][f5756]

###### \[in package TRIVIAL-UTF-8\]
<a id="x-28TRIVIAL-UTF-8-3A-40INTRODUCTION-20MGL-PAX-3ASECTION-29"></a>
<a id="TRIVIAL-UTF-8:@INTRODUCTION%20MGL-PAX:SECTION"></a>

## 1 Introduction

Trivial UTF-8 is a small library for doing UTF-8-based in- and
output on a Lisp implementation that already supports Unicode -
meaning [`char-code`][4720] and [`code-char`][772c] deal with Unicode character codes.

The rationale for the existence of this library is that while
Unicode-enabled implementations usually do provide some kind of
interface to dealing with character encodings, these are typically
not terribly flexible or uniform.

The [Babel][babel] library solves a similar problem while
understanding more encodings. Trivial UTF-8 was written before Babel
existed, but for new projects you might be better off going with
Babel. The one plus that Trivial UTF-8 has is that it doesn't depend
on any other libraries.

[babel]: https://common-lisp.net/project/babel/


<a id="x-28TRIVIAL-UTF-8-3A-40LINKS-AND-SYSTEMS-20MGL-PAX-3ASECTION-29"></a>
<a id="TRIVIAL-UTF-8:@LINKS-AND-SYSTEMS%20MGL-PAX:SECTION"></a>

## 2 Links and Systems

Here is the [official repository][trivial-utf-8-repo] and the
[HTML documentation][trivial-utf-8-doc] for the latest version.

[trivial-utf-8-repo]: https://gitlab.common-lisp.net/trivial-utf-8/trivial-utf-8

[trivial-utf-8-doc]: http://melisgl.github.io/mgl-pax-world/trivial-utf-8-manual.html


<a id="x-28-22trivial-utf-8-22-20ASDF-2FSYSTEM-3ASYSTEM-29"></a>
<a id="%22trivial-utf-8%22%20ASDF%2FSYSTEM:SYSTEM"></a>

- [system] **"trivial-utf-8"**

    - _Description:_ A small library for doing UTF-8-based input and output.
    - _Licence:_ ZLIB
    - _Author:_ Marijn Haverbeke <marijnh@gmail.com>
    - _Maintainer:_ Gábor Melis <mega@retes.hu>
    - _Homepage:_ <https://common-lisp.net/project/trivial-utf-8/>
    - _Bug tracker:_ <https://gitlab.common-lisp.net/trivial-utf-8/trivial-utf-8/-/issues>
    - _Source control:_ [GIT](https://gitlab.common-lisp.net/trivial-utf-8/trivial-utf-8.git)
    - *Depends on:* mgl-pax-bootstrap

<a id="x-28TRIVIAL-UTF-8-3A-40REFERENCE-20MGL-PAX-3ASECTION-29"></a>
<a id="TRIVIAL-UTF-8:@REFERENCE%20MGL-PAX:SECTION"></a>

## 3 Reference

<a id="x-28TRIVIAL-UTF-8-3AUTF-8-BYTE-LENGTH-20FUNCTION-29"></a>
<a id="TRIVIAL-UTF-8:UTF-8-BYTE-LENGTH%20FUNCTION"></a>

- [function] **utf-8-byte-length** *string*

    Calculate the amount of bytes needed to encode `string`.

<a id="x-28TRIVIAL-UTF-8-3ASTRING-TO-UTF-8-BYTES-20FUNCTION-29"></a>
<a id="TRIVIAL-UTF-8:STRING-TO-UTF-8-BYTES%20FUNCTION"></a>

- [function] **string-to-utf-8-bytes** *string &key null-terminate*

    Convert `string` into an array of unsigned bytes containing its UTF-8
    representation. If `null-terminate`, add an extra 0 byte at the end.

<a id="x-28TRIVIAL-UTF-8-3AUTF-8-GROUP-SIZE-20FUNCTION-29"></a>
<a id="TRIVIAL-UTF-8:UTF-8-GROUP-SIZE%20FUNCTION"></a>

- [function] **utf-8-group-size** *byte*

    Determine the amount of bytes that are part of the character whose
    encoding starts with `byte`. May signal [`utf-8-decoding-error`][ad1e].

<a id="x-28TRIVIAL-UTF-8-3AUTF-8-BYTES-TO-STRING-20FUNCTION-29"></a>
<a id="TRIVIAL-UTF-8:UTF-8-BYTES-TO-STRING%20FUNCTION"></a>

- [function] **utf-8-bytes-to-string** *bytes &key (start 0) (end (length bytes))*

    Convert the `start`, `end` subsequence of the array of `bytes` containing
    UTF-8 encoded characters to a [`string`][b93c]. The element type of
    `bytes` may be anything as long as it can be [`coerce`][6d29]d into
    an `(unsigned-bytes 8)` array. May signal [`utf-8-decoding-error`][ad1e].

<a id="x-28TRIVIAL-UTF-8-3AREAD-UTF-8-STRING-20FUNCTION-29"></a>
<a id="TRIVIAL-UTF-8:READ-UTF-8-STRING%20FUNCTION"></a>

- [function] **read-utf-8-string** *input &key null-terminated stop-at-eof (char-length -1) (byte-length -1)*

    Read UTF-8 encoded data from `input`, a byte stream, and construct a
    string with the characters found. When `null-terminated` is given,
    stop reading at a null character. If `stop-at-eof`, then stop at
    [`end-of-file`][fe09] without raising an error. The `char-length` and
    `byte-length` parameters can be used to specify the max amount of
    characters or bytes to read, where -1 means no limit. May signal
    [`utf-8-decoding-error`][ad1e].

<a id="x-28TRIVIAL-UTF-8-3AWRITE-UTF-8-BYTES-20FUNCTION-29"></a>
<a id="TRIVIAL-UTF-8:WRITE-UTF-8-BYTES%20FUNCTION"></a>

- [function] **write-utf-8-bytes** *string byte-stream &key null-terminate*

    Write `string` to `byte-stream`, encoding it as UTF-8. If
    `null-terminate`, write an extra 0 byte at the end.

<a id="x-28TRIVIAL-UTF-8-3AUTF-8-DECODING-ERROR-20CONDITION-29"></a>
<a id="TRIVIAL-UTF-8:UTF-8-DECODING-ERROR%20CONDITION"></a>

- [condition] **utf-8-decoding-error** *[simple-error][cac1]*

  [3450]: #TRIVIAL-UTF-8:@LINKS-AND-SYSTEMS%20MGL-PAX:SECTION "Links and Systems"
  [4720]: http://www.lispworks.com/documentation/HyperSpec/Body/f_char_c.htm "CHAR-CODE (MGL-PAX:CLHS FUNCTION)"
  [6d29]: http://www.lispworks.com/documentation/HyperSpec/Body/f_coerce.htm "COERCE (MGL-PAX:CLHS FUNCTION)"
  [772c]: http://www.lispworks.com/documentation/HyperSpec/Body/f_code_c.htm "CODE-CHAR (MGL-PAX:CLHS FUNCTION)"
  [870d]: #TRIVIAL-UTF-8:@INTRODUCTION%20MGL-PAX:SECTION "Introduction"
  [ad1e]: #TRIVIAL-UTF-8:UTF-8-DECODING-ERROR%20CONDITION "TRIVIAL-UTF-8:UTF-8-DECODING-ERROR CONDITION"
  [b93c]: http://www.lispworks.com/documentation/HyperSpec/Body/t_string.htm "STRING (MGL-PAX:CLHS CLASS)"
  [cac1]: http://www.lispworks.com/documentation/HyperSpec/Body/e_smp_er.htm "SIMPLE-ERROR (MGL-PAX:CLHS CONDITION)"
  [f5756]: #TRIVIAL-UTF-8:@REFERENCE%20MGL-PAX:SECTION "Reference"
  [fe09]: http://www.lispworks.com/documentation/HyperSpec/Body/e_end_of.htm "END-OF-FILE (MGL-PAX:CLHS CONDITION)"
