---
layout: post
title: Functional inheritance in Android
---

I've spent the past few works working on what I decided to call [froid](https://github.com/mchav/froid), a library/framework for writing Android applications in Frege. Because I want the library to be usable to write entire Android applications with ease, my method of developing the library has proceeded as follows:

* pick a text/set of tutorials for writing Android applications.
* read through the instruction and the code for the app
* translate the code and concepts to somewhat functional semantics
* add enough features to froid to be able to write the app
* test and run the app

My text of choice has been Big Nerd Ranch's Android programming book. Having used the book before to learn Android development, I thought it was a great, all encompassing work that focuses on building somewhat useful applications and covers most of the Android API.

One and a half applications in, I think I've learnt a blog post's worth of lessons on translating the semantics of Java to Frege (a functional language the compiles to Java). This post will describe my work in dealing with the problem of inheritance and subsequent posts will explore other problems.

## Inheritance


### Sublcassing Activities in Android

The problem of subclassing has come up a couple of times during the development of froid. The first time it popped up was when I had to write the very first class needed to run Android, an activity. Frege has support for subclassing. The declaration `native module type X where { ... }` will make the module a subclass of X and hoist the definitions placed in `{ ... }` to the top of the Java file at compile time. This is the solution I leverage in the [previous post](http://mchav.github.io/frege-on-android/) however this solution required too much complicated boilerplate and knowledge of the inner workings of Frege. Crucially, the solution didn't scale very well. Some virtual methods can have up to 4 arguments and long type names. So I needed a way to abstract all this from the user. The key contraint for this problem was that `onCreate` had to be the entry point for the application. `onCreate` is an Android activity's de facto main and passing state through the activity constructor not only introduces strange bugs but wouldn't work here because we aren't calling the constructor ourselves, the Android OS is.

My inital solution involved method hiding. Since Frege compiles functions to static methods, including a function, `onCreateF`, calling it from the overriden `onCreate`, and having the user write an `onCreateF` that hides the super class implementation runtime would suffice. However, this solution didn't work. This would only hide the method when called in the subclass not in the super class, where it mattered. Furthermore it would be buggy even if it worked. For example if I added an extra parameter to `onCreateF` or got the signature wrong it would call the dummy `onCreateF` in the super class. Having that vulnerability for all methods in the activity life cycle isn't very useful. Plus it would be hard to maintain any notion of global state without using `performUnsafeIO`.

So the problem now is to make `onCreate` act as a "main" and through it override other methods in the activity lifecycle. My final solution was to use reflection to dynamically call an `onCreate` with a given signature. Again this isn't type safe but it means the system only has one vulnerability that the user can be informed of at runtime. Not the best solution but a good compromise. I proceeded by subclassing `Activity` and then using the subclassed `Activity` as the default `Activity` class in the library as follows:

```
data Activity = native froid.app.Activity.FregeActivity

public class FregeActivity extends android.app.Activity {
        
        // lambda instance variables
        frege.run7.Func.U<frege.runtime.Phantom.RealWorld, Short> onPauseLambda = null;
        ...

        // setter for lambda
        public void setOnPause(frege.run7.Func.U<frege.runtime.Phantom.RealWorld, Short> lambda) {
            this.onPauseLambda = lambda;
        }

        ...

        private Object run(Object invokedMethod) {
            if (invokedMethod == null) return null;
            
            final frege.run7.Func.U<Object,Short> res = frege.run.RunTM.<frege.run7.Func.U<Object,Short>>cast(
                    invokedMethod).call();
            return frege.prelude.PreludeBase.TST.run(res).call();
        }

        @Override
        public void onCreate(android.os.Bundle savedInstanceState) {
            super.onCreate(savedInstanceState);
            // wrap bundle in optional type
            frege.prelude.PreludeBase.TMaybe<android.os.Bundle> sis = savedInstanceState == null ?
             frege.prelude.PreludeBase.TMaybe.DNothing.<android.os.Bundle>mk(): 
             frege.prelude.PreludeBase.TMaybe.DJust.<android.os.Bundle>mk(frege.run7.Thunk.<android.os.Bundle>lazy(savedInstanceState));
            // method that does reflection and takes the original function signature for error handling
            Object invokedOnCreate = invokeStaticActivityMethod("onCreate", new Object[] {this, sis}, "onCreate :: MutableIO Activity -> IO ()");
            run(invokedOnCreate);   
        }
```

Now a simple activity can be written as follows:

```
module io.github.mchav.fregeandroid.FregeActivity where

import froid.app.Activity
import froid.content.Context
import froid.widget.TextView

native module type Activity where {}

onCreate :: MutableIO Activity -> IO ()
onCreate this = do
    context <- this.getApplicationContext
    tv <- TextView.new context
    tv.setText "Hello, Android - Love, Frege"
    this.setContentView tv
```

This is a great improvement in the usability of the library because it not only reduces boilerplate, it also allows for some notion of global state. Since all the activity lifecycle methods can be set in `onCreate` you can define some `MVars` to pass across function closures. A great example would be the variable `cheated` in GeoQuiz which is used by the Activity methods to determine which questions in the quiz a user has cheated on. You neededn't know the API in depth for this example to make sense, the comments should explain the rationale well.

```
onCreate :: MutableIO Activity -> Maybe (MutableIO Bundle) -> IO ()
onCreate this savedInstanceState = do
    ui <- setupUI this
    firstQuestion <- (initialQuestion savedInstanceState)
    ui.textView.setText firstQuestion
    -- get string of cheated questions from bundle then decide whether to show answer buttons or not
    c <- getQuestionsCheated savedInstanceState
    let hide = if c.contains firstQuestion then View.invisible else View.visible
    showHideButtons ui hide
    -- set cheated MVar as the string of already cheated questions
    cheated <- newMVar $ c
    -- update the cheated variable asynchronously when the activity gets a result back
    this.setOnActivityResult (onActivityResult ui (getTextString ui.textView) cheated)
    -- maintain cheat instance when screen is flipped
    this.setOnSavedInstanceState (onSaveInstanceState cheated (ui.textView))
    setButtonOnClick ui.buttonFalse (checkAnswer ui False)
    setButtonOnClick ui.buttonTrue (checkAnswer ui True)
    setButtonOnClick ui.buttonCheat (cheat this ui)
    buttons <- mapM (asButton this) [btnNextId, btnPrevId]
    --hide answer buttons when the user has cheated on the questionto navigate to
    zipWithM_ setButtonOnClick buttons [(navigate cheated ui Forward), (navigate cheated ui Back)]
```

Thus, with the constraint that `onCreate` is the de facto main, we have solved the problem of subclassing. As a quick aside an `MVar` is a mutable variable container used as a concurrency mechanism. To read the variable you have to take it out of the container operate on it and then put it back. If anyone else tries to read the same variable from the MVar they would have to wait until whoever first took the variable out of the MVar puts it back in. 


### Using delegators to mimick objects

But the more general case still stands. What if we have full calling power over the class and we need to subclass it? I argue that an object in OO is just a collection of functions with some internal state which we can mimick with [records](http://learnyouahaskell.com/making-our-own-types-and-typeclasses) and [closures](https://simple.wikipedia.org/wiki/Closure_(computer_science)). Of course we can use type classes and the like to give a loose notion of inheritance but at this point I'm not too concerned with the general problem of inheritance - only subclassing some API methods and making the subclassing general enough to work with most user needs.

Here is how you create objects using records and closures.

Having records of functions to represent virtual methods (in the API I call these records delegators) turns out to be a pretty powerful mechanism I'll give the example of using Fragments in Android. Suppose I need to define a subclass of `Fragment` that overrides some methods and I control when and how Fragments are constructed. I can define a top level function that assigns each record field to a function and then makes the object using that delegator. This is how it looks in the `Fragment` class.

```
data Fragment = native android.support.v4.app.Fragment


data FragmentDelegator = FragmentDelegator { ...
                                           , onCreateView           :: Maybe (MutableIO Fragment -> MutableIO LayoutInflater -> MutableIO ViewGroup -> Maybe (MutableIO Bundle) -> IO (MutableIO View))
                                           ...
                                       }


defaultFragmentDelegator :: FragmentDelegator
defaultFragmentDelegator = FragmentDelegator { ...
                                               , onCreateView           = Nothing
                                               ..
                                           }
```

We make all the fields Optional/Maybe types so the default delegator is a record of `Nothing`s. Now all we have to do is make the construtor and bridge the Java code over to Frege.

The constructor should make a new fragment then assign a delegator to it as follows:

```
native delegateFragment "froid.support.v4.app.Fragment.delegate" :: FragmentDelegator -> STMutable RealWorld Fragment

native module where {

    public static android.support.v4.app.Fragment delegate(TFragmentDelegator delegator) {
        FregeFragment frag = new FregeFragment();
        frag.delegator = delegator;
        return (android.support.v4.app.Fragment) frag;
    }

    public static class FregeFragment extends android.support.v4.app.Fragment {

        TFragmentDelegator delegator = null;

        @Override
        public android.view.View onCreateView(android.view.LayoutInflater inflater, android.view.ViewGroup container, android.os.Bundle savedInstanceState) {
            // call function from delegator and the apply all arguments given in onCreateView
            Func.U res = delegator.onCreateView.asJust(inflater, container, savedInstanceState);
            return (android.view.View) frege.prelude.PreludeBase.TST.run(res).call();
        }
    }
}
```

We don't expose the constructor to the user, we only expose `delegateFragment` that way an object is constructed for the user and there is no chance of the  delegator ever being null.

An example of this would be in CriminalIntent where we provide an implementation of `onCreateView`:

```
onCreateView :: Crime -> MutableIO Fragment -> MutableIO LayoutInflater ->
                MutableIO ViewGroup -> Maybe (MutableIO Bundle) -> IO (MutableIO View)
onCreateView c this inf vg b = do
    v <- inf.inflate fragmentCrime vg False
    titleField <- asEditText v.findViewById crimeTitleId
    titleField.setText c.crimeTitle
    return v

newCrimeFragment :: STMutable RealWorld Fragment
newCrimeFragment = do
    crime <- newCrime "" False
    let delegator = defaultFragmentDelegator.{onCreateView = Just (onCreateView crime)}
    delegateFragment delegator
```

This method is extensible to other classes such as the `BaseAdapter` class. It allows the user to avoid instance variables and always mutate objects in functions.

I'm yet to come across an inheritance problem that doesn't generalise to the aforementioned abstraction and I am actively working to see if there are better abstractions until then delegators seem to be the simplest and most general solution to inheritance in OO-Functional bindings.
