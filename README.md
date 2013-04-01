Use the web browser as a GUI, controllable from Haskell.

## Basics

Threepenny is a GUI library for Haskell designed for easy setup across
all major OS variants.  It leverages the web browser as the foundation on 
which to build a desktop GUI.  Web-centric tasks such as message passing, 
routing, and serving files are handled automatically with little set up 
from the user.  As such, users unfamiliar with web technologies will likely
be able to pick up Threepenny quickly, although familiarity with HTML helps.
What is more, Threepenny is controlled entirely from within Haskell code, 
relieving the user of writing client-side Javascript by hand.

Threepenny comes with a simple web server that is preconfigured to host a 
client-side JS file called threepenny-gui.js.  The Threepenny API communicates
with this JS to create new elements, respond to events, and more.  This frequent
communication precludes Threepenny from use in high-latency environments.

Users can, however, write their own Javascript if they wish, and invoke that
JS from Threepenny.  This is useful for cases in which keeping the server in
the loop contributes too much delay.

This project was originally called Ji in its earliest iterations.

## Examples

* [Some simple buttons](http://chrisdone.com/ji/buttons/)
* [Missing dollars question](http://chrisdone.com/ji/missing-dollars/)
* [A chat app](http://chrisdone.com/ji/chat/)
* [Use words](http://chrisdone.com/ji/use-words/) (Apologies for the male-centric story)
* [moogle.tv](http://moogle.tv/) (WIP)

## Difference to HJScript and Fay

HJScript and Fay are a libraries that generate JavaScript, which runs on the
client, with JavaScript semantics.

So, e.g. if you want to write expressions in HJScript you write:

    x <- varWith (int 10)
    alert (mathMax (val 10 * x) 45)
    ajax (string "get-user") (10,"admin") $ \user -> do
       udiv <- j "<div></div>"
       setText (toString user)
       append body udiv

Threepenny is a set of Haskell functions that manipulate a remote DOM, which
runs on the server and sends code to the client, with Haskell
semantics.

So for the above in Threepenny you might write:

    let x = 10
    alert (max (10 * x) 45)
    user <- getUser 10 Admin
    udiv <- new
    setText (show user) udiv
    append body udiv

Although the HJScript can be abstracted somewhat to look more like the
below, by using Haskell like a macro language for JavaScript, it's
still JavaScript.

## More detail

Threepenny transparently runs on the server or the client depending on what
needs to be done, but most time is spent on the server. In this sense
one could write code like this:

     body <- getBody
     ul <- new # addTo body
     forM_ [1..10] $ \i -> do
       new # addTo ul # setText (show i)
       threadDelay (1000*1000)

Which would add a new line with each number to the browser's body at
least every second, network latency allowing.

## Challenges

### Latency

The above example has a latency. If this is a problem, e.g. then using
Threepenny for this task is wrong. The solution is to produce some JavaScript
that will run on the client. Consider this approach similar to [a
shading language.](http://en.wikipedia.org/wiki/Shading_language)

Some means of producing JavaScript from Haskell might be:

* Fay
* HJScript
* GHCJS
* UHC

Alternatively, the JS can be written by the user directly and invoked from 
Threepenny.

### Garbage collection

Every DOM element referenced from the server needs a number generated
for it per session in a map, and once the session timesout it's
deleted. It would be good to optimize this to an IntMap, for space
reasons. It we can attach finalizers to Element values then we can
automatically free up "reference" numbers to be used again.

## Other ideas

Switch to HTML rendering mode — it might be nice in the case of search
engines to merely generate a DOM and render it, so that search engines
can read the pages.

## Other helpful libraries of interest

* [qooxdoo](http://qooxdoo.org/demo) — provides a feature-complete
  widget set. One could wrap this in a type-safe API from Threepenny and get a
  complete, stable UI framework for free. Most of the "immediate
  feedback" like dragging things here, switching tabs there, are taken
  care of by the framework. All that would be left would be to provide
  the domain configuration and business/presentation logic.

There are plenty more like this, but this is the first that springs to
mind that is good.
