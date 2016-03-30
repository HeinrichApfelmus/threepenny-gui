Design Guide for Widgets
========================
*Heinrich Apfelmus* -- *September 2013*

<a rel="license" href="http://creativecommons.org/licenses/by-sa/3.0/"><img alt="Creative Commons License" style="border-width:0" src="http://i.creativecommons.org/l/by-sa/3.0/88x31.png" /></a> This text is licensed under a <a rel="license" href="http://creativecommons.org/licenses/by-sa/3.0/">Creative Commons Attribution-ShareAlike 3.0 Unported License</a>.

*Table of Contents*

* [Introduction](#introduction)
* [Concepts](#concepts)
    * [Model-View-Controller](#model-view-controller)
    * [Functional Reactive Programming](#functional-reactive-programming)
    * [FRP in an imperative language?](#frp-in-an-imperative-language)
    * [Three Principles for Representing MVC Concepts with FRP](#three-principles-for-representing-mvc-concepts-with-frp)
* [Implementation](#implementation)

Introduction
------------

The [threepenny-gui][] library provides you with all the basic UI elements, but the real challenge is to combine them in new ways to create a simple and pleasant user experience.

For example, a basic text input widget allows the user to input arbitrary text into your program. But often, we want the user to adhere to a certain format. For instance, he may only enter a valid email address and should be provided with visual feedback in case he enters an invalid email address. Such a "validating input widget" would be very useful in many contexts, and deserves to be cast into a library module.

This guide sets forth a few principles for **widget design** using the threepenny-gui library. The focus is on technical aspects: we want to make sure that the widget code is easy to implement and pleasant to reuse. Ideas from Functional Reactive Programming will help us with that.

  [threepenny-gui]: http://www.haskell.org/haskellwiki/Threepenny-gui

Concepts
========

First, it may be helpful to clarify what a widget actually is and explain how it fits into an overall conceptual framework for programming user interfaces.

Model-View-Controller
---------------------

  [mvc]: http://st-www.cs.illinois.edu/users/smarch/st-docs/mvc.html

The traditional paradigm for writing GUI applications is the [*Model-View-Controller*][mvc] (MVC) pattern, and it applies to Threepenny as well. However, we will use it mainly on a conceptual level, whereas the [original description of MVC][mvc] also focuses on implementation details that are no longer relevant to us.

Here is how the MVC paradigm works: The whole purpose of a user interface is to allow a user to manipulate data on a computer. This data is the **model**. For instance, the data in question may be the address of a friend that the user wants to remember. To be able to inspect the data, the user has to see a representation of the data on the computer screen; this is the **view**. In our example, the computer draws glyphs corresponding to the friend's address on the screen. Finally, the user not only wants to passively view the data, but also to actively change it. He does so by operating a **controller**. For example, he can press keys on his keyboard to change the address. The following figure illustrates these concepts:

![](mvc.png)

Note that controller and view are often closely linked together; the most pleasant user experiences are those where the user can interact directly with the visual representation, where he can almost "touch" it. **Widgets** embody this combination of controller and view. Of course, there are widgets that consist only of view (for example a status text) or only of controller (for example a clickable button), but these should be used in supporting roles only, because they lead to a separation between "thing I see" and "thing I touch", which results in a clumsy user experience.

From an implementation point of view, the concepts of model, controller and view are *relative* notions, they depend on your level of abstraction. For instance, a city map with scrollbars will be perceived mainly as a view by the user, but it actually consists of a model (rectangle displayed), a controller (scrollbars) and a view (showing the partial map). In turn, the scrollbar itself consists of a model (position), a controller (responds to mouse drags) and a view (position indicator). In other words, the MVC concepts can be nested arbitrarily. Moving up and down the abstraction ladder, we can easily create rich widgets by combining simple ones. The figure illustrates nesting:

![](mvc-nested.png)

Functional Reactive Programming
-------------------------------

Having established the concepts of model, view and controller, we can now ask which programming constructs we should use to represent them.

The main point is that the data in the model may change over time, usually on request by a controller, and the view needs to be updated to reflect the new data. A traditional paradigm for updating data and dealing with such changes is the so-called [event-driven programming][event-driven], where you register callback functions to be called whenever a change happens. Unfortunately, it turns out that this style is rather clumsy when it comes to ensuring that updates happen in the right order and yield consistent results.

But since we are using [Haskell][haskell], the programming language that is most powerful when it comes to creating new abstractions, we can look for other paradigms to represent MVC concepts. In particular, we will make use of [functional reactive programming][frp] (FRP).

The core idea of FRP is to use abstract date types that already include a notion of time. There are several different variations on this theme; in Threepenny, we will use the FRP variant laid out in the [`Reactive.Threepenny`][reactive-threepenny] module. It consists of two core types, the **Behavior** and the **Event**.

A Behavior is simply a *value that varies in time*. You can think of it as a function that maps each moment in time to the corresponding value.

    type Behavior a = Time -> a

An Event is a sequence of *event occurrences*. Think of it as an (infinite) list of occurrences, which are pairs: the first component indicates when the occurrences happen and the second component is a value tagged to this occurrence.

    type Event    a = [(Time,a)]

The following figures illustrate the meaning of these types.

![](frp-behavior.png)
![](frp-event.png)

In words, a Behavior is like a continuously varying value, say the position of a ball that is being thrown by a basketball player, while an Event is a sequence of occurrences, for instance keeping track of whenever said ball falls through the basket.

Several combinators allow us to make new Behaviors and Events from old ones, thus giving us a new way to program with time-dependent data. Unfortunately, a detailed explanation of FRP is out of scope for this document, [see elsewhere][reactive-banana-slides] for a more thorough introduction.

FRP in an imperative language?
------------------------------

The important point about Behavior and Event is that their implementations perform update notification internally. However, even if you were using a programming language that does not support FRP as actual data types, it is still very useful  to use **FRP as a concept** for thinking about your code. For instance, in an imperative language, a Behavior roughly corresponds to a read-only variable (with built-in change notification), while an Event is an object where you can register event listeners. 

In fact, using FRP is completely optional when programming with the Threepenny library. You can view the type `Event` as a means to register event handlers and `Behavior` as a read-only variable. 

Whether you use FRP as actual data types or as a concept only, the key **advantage** of FRP is the syntactic style it encourages:

> Programming in the style of functional reactive programming means to specify the dynamic behavior of a value completely at the time of declaration.

For instance, take the example of a counter: you have two buttons labelled “Up” and “Down” which can be used to increment or decrement the counter. Imperatively, you would first specify an initial value and then change it whenever a button is pressed; something like this:

    counter := 0                               -- initial value
    on buttonUp   = (counter := counter + 1)   -- change it later
    on buttonDown = (counter := counter - 1)

The point is that at the time of declaration, only the initial value for the counter is specified; the dynamic behavior of counter is implicit in the rest of the program text. A remote part of the program may change the counter, and this "action at a distance" is not visible at the point where the counter is declared.

In contrast, FRP specifies the whole dynamic behavior at the time of declaration, like this:

    counter :: Behavior Int
    counter = accumulate ($) 0
                (fmap (+1) eventUp
                 `union` fmap (subtract 1) eventDown)

Now, it is no longer possible for a remote part of the program to change the counter; the only way to influence the counter is to do so at declaration time. This is the key property of FRP that helps you simplify your code, and it is worth keeping in mind whenever you program with FRP.

  [reactive-banana-slides]: http://apfelmus.nfshost.com/blog/2012/07/15-frp-tutorial-slides.html
  [reactive-threepenny]: ../src/Reactive/Threepenny.hs
  [haskell]: http://www.haskell.org
  [event-driven]: http://en.wikipedia.org/wiki/Event-driven_programming
  [frp]: http://stackoverflow.com/questions/1028250/what-is-functional-reactive-programming


Three Principles for Representing MVC Concepts with FRP
-------------------------------------------------------

After a short introduction to FRP, we are now in a position to give a representation of the concepts of model, view and controller in terms of actual programming constructs. However, we have seen that MVC can be nested, so there cannot be a single direct mapping between concepts and data types. But we can formulate three *principles* which serve as a guideline for structuring your code.

Here are the three principles:

1. The **Model** is represented by a continuous time-varying value, i.e. a **Behavior**. The point is that while Behaviors do change notification under the hood, this is transparent to the programmer. The contents of a view should **only** depend on the present (and past) values of the model, and **not** on how often the model has "changed internally". This eliminates a large source of potential errors and helps you keep your code clean.
2. Only the **user** may trigger **controller** **Events**. In other words, a programmatic change to the model never leads to the widget emitting an event. On the other hand, user input will usually lead to events being triggered. In fact, it is recommended, though not strictly enforced, that user input is represented in terms of Events only. This is in line with the first principle and helps you to cleanly separate between updates that the model *still needs* to incorporate, and updates that the model *already has* incorporated.
3. Often, there is the issue of **feedback cycles**: the user makes a change to the widget, which is propagated to the model, which in turn changes the view contents of the widget and may heavily interfere with the user's editing operations. The solution is to use a **temporary copy**: as long as the user manipulates the widget, the view contents may diverge from the model, so he is essentially manipulating a copy of the data represented by the widget, instead of the data taken from the model.

    For instance, the hand of a clock will stop following time while the user adjusts it, and will only resume ticking when the user releases the hand. The connection between the current time (model) and the clocks hands (view) is suspended while the user is manipulating the clock.

These principles can be summarized in the following illustration:

![](mvc-frp.png)

So much for a conceptual overview. We will see how to represent this in actual code using Threepenny's `WriteAttr`, `ReadAttr` and `Event` types below.


Implementation
==============

Having laid the conceputal groundwork for programming user interfaces, we can now focus on how to realize these concepts in Haskell using the threepenny-gui library.

We will use the validated input widget as an example. Please have a look at the [complete source code for this example][validated-input].

  [validated-input]: 

First Steps: a Type for your Widget
-----------------------------------

Each widget should have a separate **type**. After all, we are programming in Haskell.

For instance, the validated input widget mentioned previously would be represented by a type `ValidatedInput a` where the type variable `a` is the type of the value being stored.

A widget will typically consist of one or more HTML elements (or even widgets) and additional properties. It is convenient to put this data into a record.

    data ValidatedInput a = ValidatedInput
        { vInput :: Element
        , ...
        }

Of course, the widget is abstract, the individual record elements are not exported. Note that the `RecordWildCards` language extension is very handy for avoiding boilerplate while programming with Haskell records.

Next, we need a function to **create the widget**. Traditionally, it has the same name as the widget type, but in lowercase letters.

    validatedInput :: IO (ValidatedInput a)

Then, we need to obtain a **visual representation** of the widget. Typically, the widget is visualized by a single DOM element. Hence, you should make the widget a member of the `Widget` type class, which offers a uniform interface for manipulating the visual representation.

    class Widget w where
        getElement :: w -> Element
        ..
    
    instance Widget (ValidatedInput a) where
        getElement = vInput

Last but not least, we need to adhere to an API for the new functionality of our widget. We will discuss this in the next section.

*FIXME: destroying widgets, garbage collection?*


Connect to a Model
------------------

    value     :: WriteAttr (ValidatedInput a) a
    userInput :: ValidatedInput a -> Event a


Visual Styling
--------------

*FIXME: This section is yet to be written. Question:*

1. Styling via CSS classes? Example: Bootstrap?
2. How to include external resources like .css properties or images?

