Development Notes on Performance
================================

**Building HTML elements**

*Date: 2016-05-16*

Test to find out whether building HTML elements can be sped up by batching FFI calls.

I have created a small test program `TestSpeed.hs` that creates 200 HTML `span` elements with the code

    getBody window #+ replicate 200 (UI.string "Haskell-")

This will send ~3*200 `"RunEval"` messages to the browser. To test whether the browser/server communication is the limiting factor, I have changed the FFI internals to perform different workloads when a `"RunEval"` message is processed. The workloads and timings are:

1. Use `eval` to execute the message body: 1423.73 ms
2. Always create a span element without using `eval`: 1492.88 ms
3. Always execute `document.write("<span>Haskell-</span>")`: 1266.225 ms

In contrast, creating a single, but long span with the Haskell code

    getBody window #+ [UI.string $ concat $ replicate 200 "Haskell-"]

takes 33.14 ms. Also, creating span elements in a way similar to (3), but in a single `"RunEval"` message by using the Haskell code

    runFunction $
        ffi $ concat $ replicate 200 "document.write('<span>Haskell-</span>');"

takes something between 17.55 ms and 69.26 ms

This demonstrates that the processing time is dominated by the number of messages sent, and not by the amount of JavaScript work done during each message. This means that batching FFI calls has the potential to dramatically cut execution time.

