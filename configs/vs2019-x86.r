Rebol [
    file: %vs2019-x86.r
]

os-id: 0.3.1

target: 'nmake

compiler: 'cl
compiler-path: %cl.exe

with-ffi: [
    definitions: ["FFI_BUILDING"]  ; The prebuilt library is static

    includes: [%../external/ffi-prebuilt/msvc/lib32/libffi-3.2.1/include]

    ; Change to .../Debug for debugging build
    ;
    searches: [%../external/ffi-prebuilt/msvc/lib32/Release]

    libraries: reduce [make rebmake.ext-static-class [output: %libffi.lib]]
]

rebol-tool: %r3-make.exe
