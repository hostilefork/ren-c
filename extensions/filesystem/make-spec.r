REBOL []

name: 'Filesystem
source: %filesystem/mod-filesystem.c
includes: [
    %prep/extensions/filesystem
]

depends: compose [
    %filesystem/p-file.c
    %filesystem/p-dir.c

    (switch system-config/os-base [
        'Windows [
            [%filesystem/file-windows.c]
        ]

        default [
            ; Linux and OS/X stick to POSIX for file I/O for now
            ; Other options exist, e.g. "aio.h"
            ; https://fwheel.net/aio.html
            ;
            [%filesystem/file-posix.c]
        ]
    ])

    (if "1" = get-env "USE_FCNTL_NOT_FCNTL64" [
        [%filesystem/fcntl-patch.c]
    ])
]

ldflags: compose [
    (if "1" = get-env "USE_FCNTL_NOT_FCNTL64" [
        {-Wl,--wrap=fcntl64}
    ])
]
