//
//  File: %call-windows.c
//  Summary: "Implemention of CALL native for Windows"
//  Section: Extension
//  Project: "Rebol 3 Interpreter and Run-time (Ren-C branch)"
//  Homepage: https://github.com/metaeducation/ren-c/
//
//=////////////////////////////////////////////////////////////////////////=//
//
// Copyright 2012 Atronix Engineering
// Copyright 2012-2024 Ren-C Open Source Contributors
// REBOL is a trademark of REBOL Technologies
//
// See README.md and CREDITS.md for more information.
//
// Licensed under the Lesser GPL, Version 3.0 (the "License");
// you may not use this file except inbuf compliance with the License.
// You may obtain a copy of the License at
//
// https://www.gnu.org/licenses/lgpl-3.0.html
//
//=////////////////////////////////////////////////////////////////////////=//
//
// Windows has no clear standard on when piped processes return UTF-16 vs.
// ASCII, or UTF-8, or anything else.  It's just a pipe.  What programs do
// in general (including Rebol) is detect if they are hooked to a console
// with `GetFileType(GetStdHandle(STD_OUTPUT_HANDLE)) == FILE_TYPE_CHAR`.
// If so, they send UTF-16.
//
// https://docs.microsoft.com/en-us/windows/desktop/api/fileapi/nf-fileapi-getfiletype
//
// If you call CMD.EXE itself and as it to perform a shell function, such as
// say `ECHO`, it will default to giving back ASCII.  This can be overridden
// with `CMD.EXE /U` ("when piped or redirected, gives "UCS-2 little endian")
//
// Given Windows itself wishing to set the standard for pipes and redirects
// to use plain bytes, it seems good to go with it.  Rather than give the
// appearance of endorsement of UCS-2/UTF-16 by offering a switch for it,
// a process returning it may be handled by requesting BLOB! output and
// then doing the conversion themselves.
//

#define WIN32_LEAN_AND_MEAN  // trim down the Win32 headers
#include <windows.h>

#include <process.h>
#include <shlobj.h>

#ifdef OUT
    #undef OUT  // %minwindef.h defines this, we have a better use for it
#endif
#ifdef VOID
    #undef VOID  // %winnt.h defines this, we have a better use for it
#endif

#include "tmp-mod-process.h"

#include "reb-process.h"


//
//  Try_Init_Startupinfo_Sink: C
//
// Output and Error code is nearly identical and is factored into a
// subroutine.
//
// Note: If it returns `false` GetLastError() is tested to return a message,
// so the caller assumes the Windows error state is meaningful upon return.
//
static bool Try_Init_Startupinfo_Sink(
    Sink(HANDLE) hsink,  // will be set (is &si.hStdOutput or &si.hStdError)
    Sink(HANDLE) hwrite,  // set to match hsink unless it doesn't need closing
    Sink(HANDLE) hread,  // write may have read side if pipe captures variables
    DWORD std_handle_id,  // e.g. STD_OUTPUT_HANDLE, STD_ERROR_HANDLE
    const Value* arg  // argument e.g. :OUTPUT or :ERROR for behavior
){
    SECURITY_ATTRIBUTES sa;
    sa.nLength = sizeof(SECURITY_ATTRIBUTES);
    sa.lpSecurityDescriptor = nullptr;
    sa.bInheritHandle = TRUE;

    *hread = 0;
    *hwrite = 0;
    *hsink = INVALID_HANDLE_VALUE;  // this function must set unless error

    if (Is_Nulled(arg)) {  // write normally (usually to console)
        *hsink = GetStdHandle(std_handle_id);
    }
    else if (Is_Word(arg)) {
        char mode = Get_Char_For_Stream_Mode(arg);
        if (mode == 'i') {
            //
            // !!! This said true was "inherit", but hwrite was not being
            // set to anything...?  So is this supposed to be able to deal
            // with shell-based redirection of the parent in a way that is
            // distinct, saying if the r3 process was redirected to a file
            // then the default is *not* to also redirect the child?  There
            // was no comment on this.
            //
            /*if (not SetHandleInformation(
                *hwrite, HANDLE_FLAG_INHERIT, HANDLE_FLAG_INHERIT
            )){
                return false;
            }
            *hsink = *hwrite;*/

           fail ("Unimplemented branch for INHERIT in Startupinfo_Sink()");
        }
        else {
            // Not documented, but this is how to make a /dev/null on Windows
            // https://stackoverflow.com/a/25609668
            //
            assert(mode == 'n');
            *hwrite = CreateFile(
                L"NUL",
                GENERIC_WRITE,
                0,
                &sa,  // just says inherithandles = true
                OPEN_EXISTING,
                0,
                NULL
            );
            if (*hwrite == INVALID_HANDLE_VALUE)
                return false;
            *hsink = *hwrite;
        }
    }
    else switch (Type_Of(arg)) {
      case TYPE_TEXT:  // write to pre-existing TEXT!
      case TYPE_BLOB:  // write to pre-existing BLOB!
        if (not CreatePipe(hread, hwrite, NULL, 0))
            return false;

        // make child side handle inheritable
        //
        if (not SetHandleInformation(
            *hwrite, HANDLE_FLAG_INHERIT, HANDLE_FLAG_INHERIT
        )){
            return false;
        }
        *hsink = *hwrite;
        break;

      case TYPE_FILE: {  // write to file
        WCHAR *local_wide = rebSpellWide("file-to-local @", arg);

        // !!! This was done in two steps, is this necessary?

        *hwrite = CreateFile(
            local_wide,
            GENERIC_WRITE,  // desired mode
            0,  // shared mode
            &sa,  // security attributes
            CREATE_NEW,  // creation disposition
            FILE_ATTRIBUTE_NORMAL,  // flag and attributes
            nullptr  // template
        );

        if (
            *hwrite == INVALID_HANDLE_VALUE
            and GetLastError() == ERROR_FILE_EXISTS
        ){
            *hwrite = CreateFile(
                local_wide,
                GENERIC_WRITE,  // desired mode
                0,  // shared mode
                &sa,  // security attributes
                OPEN_EXISTING,  // creation disposition
                FILE_ATTRIBUTE_NORMAL,  // flag and attributes
                nullptr  // template
            );
        }
        rebFree(local_wide);

        if (*hwrite == INVALID_HANDLE_VALUE)
            return false;

        *hsink = *hwrite;
        break; }

      default:
        panic (arg);  // CALL's type checking should have screened the types
    }

    assert(*hsink != INVALID_HANDLE_VALUE);
    assert(*hwrite == 0 or *hwrite == *hsink);

    return true;  // succeeded
}


//
//  Call_Core: C
//
Bounce Call_Core(Level* level_) {
    INCLUDE_PARAMS_OF_CALL_INTERNAL_P;

    UNUSED(REF(CONSOLE));  // !!! This is not paid attention to (?)

    // Make sure that if the output or error series are STRING! or BLOB!,
    // they are not read-only, before we try appending to them.
    //
    if (Is_Text(ARG(OUTPUT)) or Is_Blob(ARG(OUTPUT)))
        Ensure_Mutable(ARG(OUTPUT));
    if (Is_Text(ARG(ERROR)) or Is_Blob(ARG(ERROR)))
        Ensure_Mutable(ARG(ERROR));

    bool flag_wait;
    if (
        REF(WAIT)
        or (
            Is_Text(ARG(INPUT)) or Is_Blob(ARG(INPUT))
            or Is_Text(ARG(OUTPUT)) or Is_Blob(ARG(OUTPUT))
            or Is_Text(ARG(ERROR)) or Is_Blob(ARG(ERROR))
        )  // I/O redirection implies /WAIT
    ){
        flag_wait = true;
    }
    else
        flag_wait = false;

    // We synthesize the argc and argv from the "command".  This does dynamic
    // allocations of argc strings through the API, which need to be freed
    // before we return.
    //
    REBWCHAR *cmd;
    int argc;
    const REBWCHAR **argv;

    if (Is_Text(ARG(COMMAND))) {  // Windows takes command-lines by default

      text_command:

        if (REF(SHELL)) {
            //
            // Do not pass /U for UCS-2, see notes at top of file.
            //
            // !!! This seems it would be better to be detected and done in
            // usermode, perhaps as an AUGMENT on platforms that wish to offer
            // the facility.
            //
            cmd = rebSpellWide(
                "unspaced [--{cmd.exe /C \"}--", ARG(COMMAND), "--{\"}--]"
            );
        }
        else {
            cmd = rebSpellWide(ARG(COMMAND));
        }

        argc = 1;
        argv = rebAllocN(const REBWCHAR*, (argc + 1));

        // !!! Make two copies because it frees cmd and all the argv.  Review.
        //
        argv[0] = rebSpellWide(ARG(COMMAND));
        argv[1] = nullptr;
    }
    else if (Is_Block(ARG(COMMAND))) {
        //
        // In order for argv-call to work with Windows reliably, it has to do
        // proper escaping of its arguments when forming a string.  We
        // do this with a usermode helper.
        //
        // https://github.com/rebol/rebol-issues/issues/2225

        Value* text = rebValue("argv-block-to-command*", ARG(COMMAND));
        Copy_Cell(ARG(COMMAND), text);
        rebRelease(text);
        goto text_command;
    }
    else
        fail (PARAM(COMMAND));

    REBU64 pid = 1020;  // avoid uninitialized warning, garbage value
    DWORD exit_code = 304;  // ...same...

    REBINT result = -1;
    REBINT ret = 0;

    SECURITY_ATTRIBUTES sa;
    sa.nLength = sizeof(SECURITY_ATTRIBUTES);
    sa.lpSecurityDescriptor = nullptr;
    sa.bInheritHandle = TRUE;

    STARTUPINFO si;
    si.cb = sizeof(si);
    si.lpReserved = nullptr;
    si.lpDesktop = nullptr;
    si.lpTitle = nullptr;
    si.dwFlags = STARTF_USESHOWWINDOW;
    si.dwFlags |= STARTF_USESTDHANDLES;
    si.wShowWindow = SW_SHOWNORMAL;
    si.cbReserved2 = 0;
    si.lpReserved2 = nullptr;

    // We don't want to close standard handles.  So we only close handles that
    // *we open* with CreateFile.  So we track this separate list of handles
    // rather than using the ones in the STARTUPINFO to know to close them.
    // https://devblogs.microsoft.com/oldnewthing/20130307-00/?p=5033
    //
    HANDLE hOutputRead = 0;
    HANDLE hOutputWrite = 0;
    HANDLE hInputWrite = 0;
    HANDLE hInputRead = 0;
    HANDLE hErrorWrite = 0;
    HANDLE hErrorRead = 0;

    UNUSED(REF(INFO));

    unsigned char* inbuf = nullptr;
    size_t inbuf_size = 0;
    unsigned char* outbuf = nullptr;
    size_t outbuf_used = 0;
    unsigned char* errbuf = nullptr;
    size_t errbuf_used = 0;

    //=//// INPUT SOURCE SETUP ////////////////////////////////////////////=//

    if (not REF(INPUT)) {  // get stdin normally (usually from user console)
        si.hStdInput = GetStdHandle(STD_INPUT_HANDLE);
    }
    else if (Is_Word(ARG(INPUT))) {
        char mode = Get_Char_For_Stream_Mode(ARG(INPUT));
        if (mode == 'i') {  // !!! make inheritable (correct?)
            if (not SetHandleInformation(
                hInputRead,
                HANDLE_FLAG_INHERIT,
                HANDLE_FLAG_INHERIT
            )){
                goto stdin_error;
            }
            si.hStdInput = hInputRead;
        }
        else {
            assert(mode == 'n');
            // Not documented, but this is how to make a /dev/null on Windows
            // https://stackoverflow.com/a/25609668
            //
            si.hStdInput = hInputRead = CreateFile(
                L"NUL",
                GENERIC_READ,
                0,
                &sa,  // just says inherithandles = true
                OPEN_EXISTING,
                0,
                NULL
             );  // don't offer any stdin
        }
    }
    else switch (Type_Of(ARG(INPUT))) {
      case TYPE_TEXT: {  // feed standard input from TEXT!
        //
        // See notes at top of file about why UTF-16/UCS-2 are not used here.
        // Pipes and file redirects are generally understood in Windows to
        // *not* use those encodings, and transmit raw bytes.
        //
        inbuf_size = rebSpellInto(nullptr, 0, ARG(INPUT));
        inbuf = rebAllocBytes(inbuf_size + 1);
        size_t check = rebSpellInto(cast(char*, inbuf), inbuf_size, ARG(INPUT));
        assert(check == inbuf_size);
        UNUSED(check);
        goto input_via_buffer; }

      case TYPE_BLOB:  // feed standard input from BLOB! (full-band)
        inbuf = rebBytes(&inbuf_size, ARG(INPUT));

      input_via_buffer:

        if (not CreatePipe(&hInputRead, &hInputWrite, nullptr, 0))
            goto stdin_error;

        if (not SetHandleInformation(
            hInputRead,  // make child side handle inheritable
            HANDLE_FLAG_INHERIT,
            HANDLE_FLAG_INHERIT
        )){
            goto stdin_error;
        }
        si.hStdInput = hInputRead;
        break;

      case TYPE_FILE: {  // feed standard input from file contents
        WCHAR *local_wide = rebSpellWide("file-to-local", ARG(INPUT));

        hInputRead = CreateFile(
            local_wide,
            GENERIC_READ,  // desired mode
            0,  // shared mode
            &sa,  // security attributes
            OPEN_EXISTING,  // creation disposition
            FILE_ATTRIBUTE_NORMAL | FILE_FLAG_SEQUENTIAL_SCAN,  // flags
            nullptr  // template
        );
        si.hStdInput = hInputRead;

        rebFree(local_wide);

        inbuf = nullptr;
        inbuf_size = 0;
        break; }

      default:
        panic (ARG(INPUT));
    }

    //=//// OUTPUT SINK SETUP /////////////////////////////////////////////=//

    if (not Try_Init_Startupinfo_Sink(
        &si.hStdOutput,
        &hOutputWrite,
        &hOutputRead,
        STD_OUTPUT_HANDLE,
        ARG(OUTPUT)
    )){
        goto stdout_error;
    }

    //=//// ERROR SINK SETUP ////./////////////////////////////////////////=//

    if (not Try_Init_Startupinfo_Sink(
        &si.hStdError,
        &hErrorWrite,
        &hErrorRead,
        STD_ERROR_HANDLE,
        ARG(ERROR)
    )){
        goto stderr_error;
    }

  //=//// COMMAND AND ARGUMENTS SETUP /////////////////////////////////////=//

    // 1. Once we've passed in the handles to CreateProcess, we don't have
    //    to worry about keeping those open--because the child process reads
    //    the input and writes the output/error.  If we're involved in it
    //    at all, we write the input and read the output/error.

    PROCESS_INFORMATION pi;
    result = CreateProcess(
        nullptr,  // executable name
        cmd,  // command to execute
        nullptr,  // process security attributes
        nullptr,  // thread security attributes
        TRUE,  // inherit handles, must be TRUE for I/O redirection
        NORMAL_PRIORITY_CLASS | CREATE_DEFAULT_ERROR_MODE,  // creation flags
        nullptr,  // environment
        nullptr,  // current directory
        &si,  // startup information (has hStdInput, hStdOutput, hStdError) [1]
        &pi  // process information
    );

    rebFree(cmd);

    pid = pi.dwProcessId;

    if (hInputRead != nullptr)
        CloseHandle(hInputRead);  // we only need write side of input [1]

    if (hOutputWrite != nullptr)
        CloseHandle(hOutputWrite);  // we only need read side of output [1]

    if (hErrorWrite != nullptr)
        CloseHandle(hErrorWrite);  // we only need read side of error [1]

    if (result != 0 and flag_wait) {  // Wait for termination
        HANDLE handles[4];
        int count = 0;
        DWORD outbuf_capacity = 0;
        DWORD errbuf_capacity = 0;

        // The original Atronix code only waited on the file handles, and then
        // waited on the process after the handle wait loop.  But it seems to
        // be cleaner if we add the process handle to the waited-on set, so
        // that process exit will break the wait loop.
        //
        handles[count++] = pi.hProcess;

        if (hInputWrite and inbuf_size > 0) {
            handles[count++] = hInputWrite;
        }
        if (hOutputRead != NULL) {
            outbuf_capacity = BUF_SIZE_CHUNK;
            outbuf_used = 0;

            outbuf = rebAllocBytes(outbuf_capacity);
            handles[count++] = hOutputRead;
        }
        if (hErrorRead != NULL) {
            errbuf_capacity = BUF_SIZE_CHUNK;
            errbuf_used = 0;

            errbuf = rebAllocBytes(errbuf_capacity);
            handles[count++] = hErrorRead;
        }

        DWORD inbuf_pos = 0;

        while (count > 0) {
            DWORD wait_result = WaitForMultipleObjects(
                count, handles, FALSE, INFINITE
            );

            // If we test wait_result >= WAIT_OBJECT_0 it will tell us "always
            // true" with -Wtype-limits, since WAIT_OBJECT_0 is 0.  Take that
            // comparison out, but add assert in case you're on an abstracted
            // Windows and it isn't 0 for that implementation.
            //
            assert(WAIT_OBJECT_0 == 0);
            if (wait_result < WAIT_OBJECT_0 + count) {
                int i = wait_result - WAIT_OBJECT_0;
                DWORD n = 0;

                if (handles[i] == pi.hProcess) {
                    //
                    // Process terminated.
                    //
                    GetExitCodeProcess(pi.hProcess, &exit_code);
                    CloseHandle(pi.hThread);
                    CloseHandle(pi.hProcess);
                    goto cleanup;
                }
                else if (handles[i] == hInputWrite) {
                    if (not WriteFile(
                        hInputWrite,
                        inbuf + inbuf_pos,
                        inbuf_size - inbuf_pos,
                        &n,
                        NULL
                    )){
                        if (i < count - 1) {
                            memmove(
                                &handles[i],
                                &handles[i + 1],
                                (count - i - 1) * sizeof(HANDLE)
                            );
                        }
                        count--;
                    }
                    else {
                        inbuf_pos += n;
                        if (inbuf_pos >= inbuf_size) {  // done with input
                            //
                            // If we close the handle of the pipe we are using
                            // to provide input to the child before it is done
                            // reading the data we wrote, it will get an
                            // ERROR_BROKEN_PIPE.  So hopefully the client has
                            // looked at the data it has already read and
                            // decided it does not need more...because if it
                            // reads past the protocol that will happen.
                            //
                            CloseHandle(hInputWrite);
                            hInputWrite = NULL;

                            rebFree(inbuf);
                            inbuf = nullptr;
                            if (i < count - 1) {
                                memmove(
                                    &handles[i],
                                    &handles[i + 1],
                                    (count - i - 1) * sizeof(HANDLE)
                                );
                            }
                            count--;
                        }
                    }
                }
                else if (handles[i] == hOutputRead) {
                    if (not ReadFile(
                        hOutputRead,
                        outbuf + outbuf_used,
                        outbuf_capacity - outbuf_used,
                        &n,
                        nullptr
                    )){
                        if (i < count - 1) {
                            memmove(
                                &handles[i],
                                &handles[i + 1],
                                (count - i - 1) * sizeof(HANDLE)
                            );
                        }
                        count--;
                    }
                    else {
                        outbuf_used += n;
                        if (outbuf_used >= outbuf_capacity) {
                            outbuf_capacity += BUF_SIZE_CHUNK;
                            outbuf = rebReallocBytes(outbuf, outbuf_capacity);
                            if (outbuf == NULL)  // !!! never with rebRealloc
                                goto kill;
                        }
                    }
                }
                else if (handles[i] == hErrorRead) {
                    if (not ReadFile(
                        hErrorRead,
                        errbuf + errbuf_used,
                        errbuf_capacity - errbuf_used,
                        &n,
                        NULL
                    )){
                        if (i < count - 1) {
                            memmove(
                                &handles[i],
                                &handles[i + 1],
                                (count - i - 1) * sizeof(HANDLE)
                            );
                        }
                        count--;
                    }
                    else {
                        errbuf_used += n;
                        if (errbuf_used >= errbuf_capacity) {
                            errbuf_capacity += BUF_SIZE_CHUNK;
                            errbuf = rebReallocBytes(errbuf, errbuf_capacity);
                            if (errbuf == NULL)  // !!! never with rebRealloc
                                goto kill;
                        }
                    }
                }
                else {
                    //printf("Error READ");
                    if (ret == 0)
                        ret = GetLastError();
                    goto kill;
                }
            }
            else if (wait_result == WAIT_FAILED) {
                //printf("Wait Failed\n");
                if (ret == 0)
                    ret = GetLastError();
                goto kill;
            }
            else {
                //printf("Wait returns unexpected result: %d\n", wait_result);
                if (ret == 0)
                    ret = GetLastError();
                goto kill;
            }
        }

        assert(!"WaitForMultipleObjects() loop ended but process didn't end");
    }
    else if (result) {
        //
        // No wait, close handles to avoid leaks
        //
        CloseHandle(pi.hThread);
        CloseHandle(pi.hProcess);
    }
    else {
        // CreateProcess failed
        ret = GetLastError();
    }

    goto cleanup;

  kill:

    if (TerminateProcess(pi.hProcess, 0)) {
        WaitForSingleObject(pi.hProcess, INFINITE);

        GetExitCodeProcess(pi.hProcess, &exit_code);
    }
    else if (ret == 0) {
        ret = GetLastError();
    }

    CloseHandle(pi.hThread);
    CloseHandle(pi.hProcess);

  cleanup:

    if (hInputWrite != nullptr)
        CloseHandle(hInputWrite);

    if (hOutputRead != nullptr)
        CloseHandle(hOutputRead);

    if (hErrorRead != nullptr)
        CloseHandle(hErrorRead);

  stderr_error:
  stdout_error:
  stdin_error:

    // Call may not succeed if r != 0, but we still have to run cleanup
    // before reporting any error...

    assert(argc > 0);

    int i;
    for (i = 0; i != argc; ++i)
        rebFree(m_cast(REBWCHAR*, argv[i]));

    rebFree(m_cast(REBWCHAR**, argv));

    // We can actually recover the rebAlloc'd buffers as BLOB!.  If the
    // target is TEXT!, we DELINE it first to eliminate any CRs.  Note the
    // remarks at the top of file about how piped data is not generally
    // assumed to be UCS-2.
    //
    if (Is_Text(ARG(OUTPUT))) {
        Value* output_val = rebRepossess(outbuf, outbuf_used);
        rebElide("insert", ARG(OUTPUT), "deline", output_val);
        rebRelease(output_val);
    }
    else if (Is_Blob(ARG(OUTPUT))) {
        Value* output_val = rebRepossess(outbuf, outbuf_used);
        rebElide("insert", ARG(OUTPUT), output_val);
        rebRelease(output_val);
    }
    else
        assert(outbuf == nullptr);

    if (Is_Text(ARG(ERROR))) {
        Value* error_val = rebRepossess(errbuf, errbuf_used);
        rebElide("insert", ARG(ERROR), "deline", error_val);
        rebRelease(error_val);
    }
    else if (Is_Blob(ARG(ERROR))) {
        Value* error_val = rebRepossess(errbuf, errbuf_used);
        rebElide("append", ARG(ERROR), error_val);
        rebRelease(error_val);
    }
    else
        assert(errbuf == nullptr);

    rebFreeMaybe(inbuf);

    if (ret != 0)
        rebFail_OS (ret);

    if (REF(INFO)) {
        VarList* info = Alloc_Varlist(TYPE_OBJECT, 2);

        Init_Integer(Append_Context(info, CANON(ID)), pid);
        if (REF(WAIT))
            Init_Integer(Append_Context(info, CANON(EXIT_CODE)), exit_code);

        return Init_Object(OUT, info);
    }

    // We may have waited even if they didn't ask us to explicitly, but
    // we only return a process ID if /WAIT was not explicitly used
    //
    if (REF(WAIT))
        return Init_Integer(OUT, exit_code);

    return Init_Integer(OUT, pid);
}
