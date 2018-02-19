Design Guide for Widgets
========================
*Heinrich Apfelmus* -- *September 2013*

<a rel="license" href="http://creativecommons.org/licenses/by-sa/3.0/"><img alt="Creative Commons License" style="border-width:0" src="http://i.creativecommons.org/l/by-sa/3.0/88x31.png" /></a> This text is licensed under a <a rel="license" href="http://creativecommons.org/licenses/by-sa/3.0/">Creative Commons Attribution-ShareAlike 3.0 Unported License</a>.

*This document is supposed to describe how to implement the ideas described in ["The Model-View-Controller Pattern and Functional Reactive Programming" — Three useful principles for structuring GUI code][mvc] with the Threepenny GUI library. Unfortunately, this text is still unfinished...*

 [mvc]: https://github.com/HeinrichApfelmus/frp-guides/blob/master/apfelmus/mvc.md


... So much for a conceptual overview. We will see how to represent this in actual code using Threepenny's `WriteAttr`, `ReadAttr` and `Event` types below.


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

