//
//  File: %mod-console.c
//  Summary: "Read/Eval/Print Loop (REPL) Skinnable Console for Revolt"
//  Section: Extension
//  Project: "Revolt Language Interpreter and Run-time Environment"
//  Homepage: https://github.com/metaeducation/ren-c/
//
//=////////////////////////////////////////////////////////////////////////=//
//
// Copyright 2016-2019 Revolt Open Source Contributors
// REBOL is a trademark of REBOL Technologies
//
// See README.md and CREDITS.md for more information.
//
// Licensed under the Lesser GPL, Version 3.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
// https://www.gnu.org/licenses/lgpl-3.0.html
//
//=////////////////////////////////////////////////////////////////////////=//
//

#ifdef TO_WINDOWS

    #undef _WIN32_WINNT  // https://forum.rebol.info/t/326/4
    #define _WIN32_WINNT 0x0501  // Minimum API target: WinXP
    #define WIN32_LEAN_AND_MEAN  // trim down the Win32 headers
    #include <windows.h>

    #undef IS_ERROR  // %windows.h defines this, but so does %sys-core.h

#elif defined(TO_EMSCRIPTEN)
    //
    // Nothing needed here yet...
    //
#else

    #include <signal.h>  // needed for SIGINT, SIGTERM, SIGHUP

#endif


#include "sys-core.h"

#include "tmp-mod-console.h"


//=//// USER-INTERRUPT/HALT HANDLING (Ctrl-C, Escape, etc.) ///////////////=//
//
// There's clearly contention for what a user-interrupt key sequence should
// be, given that "Ctrl-C" is copy in GUI applications.  Yet handling escape
// is not necessarily possible on all platforms and situations.
//
// For console applications, we assume that the program starts with user
// interrupting enabled by default...so we have to ask for it not to be when
// it would be bad to have the Revolt stack interrupted--during startup, or
// when in the "kernel" of the host console.
//
// (Note: If halting is done via Ctrl-C, technically it may be set to be
// ignored by a parent process or context, in which case conventional wisdom
// is that we should not be enabling it ourselves.  Review.)
//

bool halting_enabled = true;

#if defined(TO_EMSCRIPTEN)  //=//// EMSCRIPTEN ///////////////////////////=//

// !!! Review how an emscripten console extension should be hooking something
// like a keyboard shortcut for breaking.  With the pthread model, there may
// be shared memory for the GUI to be able to poke a value in that the running
// code can see to perceive a halt.

void Disable_Halting(void) {}
void Enable_Halting(void) {}


#elif defined(TO_WINDOWS)  //=//// WINDOWS ////////////////////////////////=//

// Windows handling is fairly simplistic--this is the callback passed to
// `SetConsoleCtrlHandler()`.  The most annoying thing about cancellation in
// windows is the limited signaling possible in the terminal's readline.
//
BOOL WINAPI Handle_Break(DWORD dwCtrlType)
{
    switch (dwCtrlType) {
      case CTRL_C_EVENT:
      case CTRL_BREAK_EVENT:
        rebHalt();
        return TRUE;  // TRUE = "we handled it"

      case CTRL_CLOSE_EVENT:
        //
        // !!! Theoretically the close event could confirm that the user
        // wants to exit, if there is possible unsaved state.  As a UI
        // premise this is probably less good than persisting the state
        // and bringing it back.
        //
      case CTRL_LOGOFF_EVENT:
      case CTRL_SHUTDOWN_EVENT:
        //
        // They pushed the close button, did a shutdown, etc.  Exit.
        //
        // !!! Review arbitrary "100" exit code here.
        //
        exit(100);

      default:
        return FALSE;  // FALSE = "we didn't handle it"
    }
}

BOOL WINAPI Handle_Nothing(DWORD dwCtrlType)
{
    if (dwCtrlType == CTRL_C_EVENT)
        return TRUE;

    return FALSE;
}

void Disable_Halting(void)
{
    assert(halting_enabled);

    SetConsoleCtrlHandler(Handle_Break, FALSE);
    SetConsoleCtrlHandler(Handle_Nothing, TRUE);

    halting_enabled = false;
}

void Enable_Halting(void)
{
    assert(not halting_enabled);

    SetConsoleCtrlHandler(Handle_Break, TRUE);
    SetConsoleCtrlHandler(Handle_Nothing, FALSE);

    halting_enabled = true;
}

#else  //=//// POSIX, LINUX, MAC, etc. ////////////////////////////////////=//

// SIGINT is the interrupt usually tied to "Ctrl-C".  Note that if you use
// just `signal(SIGINT, Handle_Signal);` as R3-Alpha did, this means that
// blocking read() calls will not be interrupted with EINTR.  One needs to
// use sigaction() if available...it's a slightly newer API.
//
// http://250bpm.com/blog:12
//
// !!! What should be done about SIGTERM ("polite request to end", default
// unix kill) or SIGHUP ("user's terminal disconnected")?  Is it useful to
// register anything for these?  R3-Alpha did, and did the same thing as
// SIGINT.  Not clear why.  It did nothing for SIGQUIT:
//
// SIGQUIT is used to terminate a program in a way that is designed to
// debug it, e.g. a core dump.  Receiving SIGQUIT is a case where
// program exit functions like deletion of temporary files may be
// skipped to provide more state to analyze in a debugging scenario.
//
// SIGKILL is the impolite signal for shutdown; cannot be hooked/blocked

static void Handle_Signal(int sig)
{
    UNUSED(sig);
    rebHalt();
}

struct sigaction old_action;

void Disable_Halting(void)
{
    assert(halting_enabled);

    sigaction(SIGINT, nullptr, &old_action); // fetch current handler
    if (old_action.sa_handler != SIG_IGN) {
        struct sigaction new_action;
        new_action.sa_handler = SIG_IGN;
        sigemptyset(&new_action.sa_mask);
        new_action.sa_flags = 0;
        sigaction(SIGINT, &new_action, nullptr);
    }

    halting_enabled = false;
}

void Enable_Halting(void)
{
    assert(not halting_enabled);

    if (old_action.sa_handler != SIG_IGN) {
        struct sigaction new_action;
        new_action.sa_handler = &Handle_Signal;
        sigemptyset(&new_action.sa_mask);
        new_action.sa_flags = 0;
        sigaction(SIGINT, &new_action, nullptr);
    }

    halting_enabled = true;
}

#endif  //=///////////////////////////////////////////////////////////////=//


//
//  export console: native [
//
//  {Runs customizable Read-Eval-Print Loop}
//
//      return: "Exit code, RESUME instruction, or handle to evaluator hook"
//          [integer! sym-group! handle!]
//      /resumable "Allow RESUME instruction (will return a SYM-GROUP!)"
//      /skin "File containing console skin, or MAKE CONSOLE! derived object"
//          [file! object!]
//  ]
//
REBNATIVE(console)
//
// !!! The idea behind the console is that it can be called with skinning;
// so that if BREAKPOINT wants to spin up a console, it can...but with a
// little bit of injected information like telling you the current stack
// level it's focused on.  How that's going to work is still pretty up
// in the air.
//
// What it will return will be either an exit code (INTEGER!), a signal for
// cancellation (BLANK!), or a debugging instruction (BLOCK!).
{
    CONSOLE_INCLUDE_PARAMS_OF_CONSOLE;

    // !!! The initial usermode console implementation was geared toward a
    // single `system/console` object.  But the debugger raised the issue of
    // nested sessions which might have a different skin.  So save whatever
    // the console object was if it is being overridden.

    REBVAL *old_console = rebValue(":system/console", rebEND);
    if (REF(skin))
        rebElide("system/console: _", rebEND);  // !!! needed for now

    // We only enable halting (e.g. Ctrl-C, or Escape, or whatever) when user
    // code is running...not when the HOST-CONSOLE function itself is, or
    // during startup.  (Enabling it during startup would require a special
    // "kill" mode that did not call rebHalt(), as basic startup cannot
    // meaningfully be halted--the system would be in an incomplete state.)
    //
    bool was_halting_enabled = halting_enabled;
    if (was_halting_enabled)
        Disable_Halting();

    // The DO and APPLY hooks are used to implement things like tracing
    // or debugging.  If they were allowed to run during the host
    // console, they would create a fair amount of havoc (the console
    // is supposed to be "invisible" and not show up on the stack...as if
    // it were part of the C codebase, even though it isn't written in C)
    //
    REBEVL *saved_eval_hook = PG_Trampoline_Throws;
    REBNAT saved_dispatch_hook = PG_Dispatch;

    // !!! While the new mode of TRACE (and other code hooking function
    // execution) is covered by `saved_eval_hook/saved_apply_hook`, there
    // is independent tracing code in PARSE which is also enabled by TRACE ON
    // and has to be silenced during console-related code.  Review how hooks
    // into PARSE and other services can be avoided by the console itself
    //
    REBINT Save_Trace_Level = Trace_Level;
    REBINT Save_Trace_Depth = Trace_Depth;

    REBVAL *responses = rebValueQ("make-chan/capacity 1", rebEND);
    REBVAL *requests = rebValueQ("make-chan/capacity 1", rebEND);

    rebElideQ(
        "go/kernel [",
            "console-impl",
            requests,
            responses,
            rebL(did REF(resumable)),
            REF(skin),
        "]",
        rebEND
    );

    REBVAL *code;

    while (true) {
        assert(not halting_enabled);  // not if HOST-CONSOLE is on the stack

        // This runs the HOST-CONSOLE, which returns *requests* to execute
        // arbitrary code by way of its return results.  The ENTRAP is thus
        // here to intercept bugs *in HOST-CONSOLE itself*.  Any evaluations
        // for the user (or on behalf of the console skin) are done in
        // Run_Sandboxed_Group().
        //
        code = rebValueQ("receive-chan", requests, rebEND);

        if (rebDidQ("integer?", code, rebEND))
            break;  // when HOST-CONSOLE returns INTEGER! it means exit code

        if (rebDidQ("match [sym-group! handle!]", code, rebEND)) {
            assert(REF(resumable));
            break;
        }

        bool debuggable = rebDidQ("block?", code, rebEND);
        REBVAL *block;

        if (debuggable) {
            block = rebValueQ(code, rebEND);  // release w/o affecting `code`

            // Restore custom DO and APPLY hooks, but only if it was a GROUP!
            // initially (indicating running code initiated by the user).
            //
            // (We do not want to trace/debug/instrument Revolt code that the
            // console is using to implement *itself*, which it does with
            // BLOCK! Same for Trace_Level seen by PARSE.
            //
            PG_Trampoline_Throws = saved_eval_hook;
            PG_Dispatch = saved_dispatch_hook;
            Trace_Level = Save_Trace_Level;
            Trace_Depth = Save_Trace_Depth;
        }
        else {
            block = rebValueQ("as block!", code, rebEND);  // to run w/o DO
        }

        rebRelease(code);

        // Both console-initiated and user-initiated code is cancellable with
        // Ctrl-C (though it's up to HOST-CONSOLE on the next iteration to
        // decide whether to accept the cancellation or consider it an error
        // condition or a reason to fall back to the default skin).
        //
        Enable_Halting();

        REBVAL *result;
        if (debuggable)
            result = rebValue(
                "receive-chan go/channel", block, rebEND
            );
        else
            result = rebValue(
                "receive-chan go/channel/kernel", block, rebEND
            );

        rebRelease(block);  // Note: does not release `code`
        Disable_Halting();

        rebElideQ("send-chan", responses, result, rebEND);
        rebRelease(result);

        // If the custom DO and APPLY hooks were changed by the user code,
        // then save them...but restore the unhooked versions for the next
        // iteration of HOST-CONSOLE.  Same for Trace_Level seen by PARSE.
        //
        if (debuggable) {
            saved_eval_hook = PG_Trampoline_Throws;
            saved_dispatch_hook = PG_Dispatch;
            PG_Trampoline_Throws = &Trampoline_Throws;
            PG_Dispatch = &Dispatch_Internal;
            Save_Trace_Level = Trace_Level;
            Save_Trace_Depth = Trace_Depth;
            Trace_Level = 0;
            Trace_Depth = 0;
        }
    }

    // Exit code is now an INTEGER! or a resume instruction PATH!

    if (was_halting_enabled)
        Enable_Halting();

    rebElideQ("system/console:", rebR(old_console), rebEND);

    // !!! Go lore says "don't close a channel from the receiver side".  This
    // means we should not close `requests`?
    //
    rebElideQ("close-chan", responses, rebEND);

    rebRelease(responses);
    rebRelease(requests);

    return code;  // http://stackoverflow.com/q/1101957/
}
