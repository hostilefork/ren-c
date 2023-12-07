> This file's intent is to centralize credit to organizations, individuals,
> and to code + libraries + tools used.  For all new source contributions
> please give copyright attribution to "Ren-C Open Source Contributors".
> Include any new credits in pull requests as a modification to this file.
>
> **NOTE** The project has also benefited from significant supporting work
> outside this code repository by members of the community--too numerous to
> list here!


CREDITS
=======

Originators of REBOL
--------------------

Carl Sassenrath, Rebol Technologies
* http://www.rebol.com
* [@carls](https://github.com/carls) on GitHub

_REBOL is a trademark of REBOL Technologies_

Rebol 3 Alpha was [released to the open-source community][1] under an Apache 2
license on 12-Dec-2012:

[1]: http://www.rebol.com/cgi-bin/blog.r?view=0519#comments


Code Contributors
-----------------

Contributors to this project are encouraged to add/edit entries here, with a
one-line summary and a link to a landing webpage of their choice:

**Andreas Bolka**
- [@earl](https://github.com/earl) on GitHub
- http://rebolsource.net
- 64-bit and other porting, build farm, core design, core patches, test suite...

**Barry Walsh**
- [@draegtun](https://github.com/draegtun) on GitHub
- http://draegtun.com
- CHANGES.md automation, Console skinner, command-line & other start-up changes.

**Brian Dickens**
- [@hostilefork](https://github.com/hostilefork) on GitHub
- http://hostilefork.com
- "Ren-C" branch founder, core evaluator rethinking, and design...

**Brett Handley**
- [@codebybrett](https://github.com/codebybrett) on GitHub
- http://codeconscious.com
- Libraries to parse and process Rebol's C code using Rebol, file conversions.

**Brian Hawley**
- [@BrianHawley](https://github.com/brianh) on GitHub
- Mezzanine design and module system, core patches, PARSE design for Rebol3.

**Christian Ensel**
- [@gurzgri](https://github.com/gurzgri) on GitHub
- original ODBC driver code for R3-Alpha

**Giulio Lunati**
- [@giuliolunati](https://github.com/giuliolunati) on GitHub
- MAP! and hashing updates, Android builds, source serialization improvements.

**Joshua Shireman**
- [@kealist](https://github.com/kealist) on GitHub
- Serial port driver work (based on code by Carl Sassenrath)

**Ladislav Mecir**
- [@ladislav](https://github.com/ladislav) on GitHub
- Advanced math and currency support, test suite, core patches, core design...

**Richard Smolak**
- [@cyphre](https://github.com/cyphre) on GitHub
- TLS and HTTPS, Diffie-Hellman and crypto, extension model, GUI support...

**Shixin Zeng**
* [@zsx](https://github.com/zsx) on GitHub
- FFI library, CALL implementation, UNIX signals, native math, GUI support...


Corporate Support
-----------------

**Atronix Engineering, Inc**
- http://www.atronixengineering.com/downloads
- David den Haring, Director of Engineering

**Saphirion AG**
- http://development.saphirion.com/rebol/
- Robert M.Münch, CEO, Prototype sponsoring


Third-Party Components
----------------------

This aims to list all the third-party components of this distribution but may
not be complete.  Please amend with any corrections.

**debugbreak**
- Copyright (c) 2011-2021, Scott Tsai
- `%src/include/debugbreak.h`

**dtoa**
- Copyright (c) 1991, 2000, 2001 by Lucent Technologies.
- `%src/core/f-dtoa.c`

**JPEG**
- Copyright 1994-1996, Thomas G. Lane.
- `%src/core/u-jpg.c`
- `%src/include/sys-jpg.h`

**LodePNG**
- Copyright (c) 2005-2013 Lode Vandevenne
- `%src/codecs/png/lodepng.h`
- `%src/codecs/png/lodepng.c`

**mbedTLS**
- Copyright The Mbed TLS Contributors
- `%extensions/crypt/mbedtls/*`

**qsort**
- Copyright (c) 1992, 1993 The Regents of the University of California.
- `%src/core/f-qsort.c`

**Unicode**
- Copyright 2001-2004 Unicode, Inc.
- Portions used in `%src/include/cells/cell-char.h`
- Author: Mark E. Davis, 1994.
- Rev History: Rick McGowan, fixes & updates May 2001.
  Fixes & updates, Sept 2001.

**ZLIB**
- Copyright 1995-2017 Jean-loup Gailly and Mark Adler
- `%src/core/u-zlib.c`
- `%src/include/sys-zlib.h`
