---
layout: post
title: Try Frege Android
---

When I initially started learning Haskell I'd always want to run ad hoc code on my phone from books like RWH and LYAH. The only mobile application that was available was TryHaskell which provided a thin veneer over the haskell website TryHaskell and ironically didn't let you define functions. I discovered the Frege REPL and used it for a lot of my discrete math assignments on ChromeBook and thought it would be helpful to provide and Android wrapper.

Then I had an idea - what if I could write a mobile app for the TryFrege in Frege. So I started learning and asking, and learning and asking and after a while knew enough to write some Android bindings for Frege. I then used those bindings to write an application that wraps around the Frege site and the work is preliminarily done. The application is live on the App Store. The application has a few bugs that I'm still working on - screen rotation, syncing edits on some devices, and some gotchas with session ids. But for now here it is:

[Download it from the Google Play Store](https://play.google.com/store/apps/details?id=io.github.mchav.tryfrege&hl=en).


[Source code](https://github.com/mchav/try-frege-android).
