---
layout: post
title: Frege on Android
---

After a weekend of clicking through forums and readin about previous attempts I've finally gotten Frege to work on Android. The implementation can be found [here](https://github.com/mchav/FregeAndroid)

The implementation is quite hacky but I have faith in its extensibility after I get a better grasp of the Android build system. To run the build on your machine you'll need to download Frege 3.24.100.1-jdk7, open it with an archive manager like 7-zip, delete the run8 folder (Android studio won't be able to parse Java 8), add it as a source dependency in your app's gradle module, then copy the gradle settings in build.gradle to your build. The implementation borrows heavily from [ppelleti](https://github.com/ppelleti/frege-on-android) with some adjustments to execute with a newer version of Frege.

The code calls the Frege compiler to compile `FregeActivity` without creating class files and then places it where Android would have expected to find Java files. The gradle options can be copied almost verbatim with some adjustments to where your "custom" Frege compiler is located.

Let's talk about the code. 


We want to extend the Activity class and then define some methods corresponding to the activity lifecycle. In a fashion similar to my [previous post](http://mchav.github.io/a-simple-server-in-frege/), we declare extend a class with the code:

```
data FregeActivity = native android.app.Activity
native module type FregeActivity where {
	super.onCreate(savedInstanceState);
    android.widget.TextView tv = new android.widget.TextView(this);
    tv.setText("Hello, Android - Love, Frege");
    setContentView(tv);
```

This will programatically create a view and place the text Hello, Android - Love, Frege" in a TextView at the top. But we want to move the code to FregeLand so we aren't just wrapping java code in a single Frege module. Again, similar to last blog post, we can make onCreate call a Frege Function. We don't, however, have to pass this function into the code, we can references it from inside the Frege since the "native" part is hoisted to the top of the compiled Java. Let's call our FregeVersion of `onCreate` `oncCreateF` and pass in a reference to the Activity class.

```
onCreateF :: MutableIO FregeActivity -> IO ()
onCreateF !this = ...
```

The reference must be strict so the method call in our native portion can be `onCreateF(this)` instead of wrapping it in the `Lazy` type. After defining some boilerplate and placing all the `onCreate` code in FregeLand we have:

```
module io.github.mchav.fregeandroid.FregeActivity where
data Bundle = native android.os.Bundle

data Context = native android.content.Context

data FregeActivity = native android.app.Activity where
	native getApplicationContext :: MutableIO FregeActivity -> IO (MutableIO Context)
	native setContentView :: MutableIO FregeActivity -> MutableIO TextView -> IO ()

data TextView = native android.widget.TextView where
	native new :: MutableIO Context -> STMutable RealWorld TextView
	native setText :: MutableIO TextView -> String -> IO ()

onCreateF :: MutableIO FregeActivity -> IO ()
onCreateF !this = do
	context <- this.getApplicationContext
	tv <- TextView.new context
	tv.setText "Hello, Android - Love, Frege"
	this.setContentView tv

native module type FregeActivity where {
	@Override
    public void onCreate(android.os.Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        final frege.run7.Func.U<Object,Short> res = RunTM.<frege.run7.Func.U<Object,Short>>cast(onCreateF(this)).call();
		frege.prelude.PreludeBase.TST.run(res).call();
    }
}
```

Halala! A complete implementation of an Android activity in Frege. I hope to continue work on Frege in Android to make the process a little less hacky and less boilerplaty. Any help and ideas are appreciated.
