---
layout: post
title: Easy concurrency with functional programming
---

My most recent project was an Android REPL application that reads an expression, sends a GET request to a server and then receives the result of evaluating that expression from an interpreter on the server. Network programming is admittedly very error prone (a lot can go wrong), and network requests sometimes take a long time to process. For this reason, Android prevents applications from running network tasks on the UI thread. However, the Android API provides some concurrency mechanisms for dealing with network I/O, the most common being `AsyncTask`. `AsyncTask` is a class for running tasks in the background and defining what happens before, during, and after task execution. The `AsyncTask` class, crucially, allows the user to edit/change a view after the operation in a way that running a normal thread doesn't. The syntax is quite involved and considering I only wanted to define two behaviours - downloading and getting results - `AsyncTask` feels like overkill.

Here is what the `AsyncTask` to send some code to the REPL would look like:

```
 private class ReplResult extends AsyncTask<String, Void, String> {
     protected Long doInBackground(String... exprs) {
         StringBuilder sb = new StringBuilder();
         int count = urls.length;
         for (int i = 0; i < count; i++) {
             sb.append(evalutate(exprs[i]));
         }
         return sb.toString();
     }

     protected void onProgressUpdate(Void... progress) {}

     protected void onPostExecute(String res) {
         TextView result = (TextView) this.findViewById(R.id.result_txt);
         result.append(res);
     }
 }
```

The code above contains a lot of heavy machinery for a simple download. But it is nonetheless justified to ensure safe concurrency.

A big advantage you'll always hear from the Functional Programming camp about why you should use/learn functional programming is that it provides easier/better concurrency mechanisms and data structures than imperative languages like Java. For my task I found this to be true.

### Concurrency in functional programming languages

Languages such as Haskell are purely functional. The separation of I/O and pure functions is baked into languages like Haskell. In general, functional data structures are immutable, so concurrency works well. One might think that's a bit of a cop out - the API that people usually deal with is impure (does a lot of I/O).

Functional programming, does, however, allow mutation but it must be done explicitly. There [four mechanisms for implementing mutable state in Haskell](http://blog.jakubarnold.cz/2014/07/20/mutable-state-in-haskell.html). The linked blog explains each quite well. `MVar`s seemed to be a natural and easy solution to handling concurrent IO for my application. An `MVar` is a mutable variable that supports two basic operations `putMVar` and `takeMVar`. The names are self-explanatory. The functions put and remove a value from the variable, respectively. If the `MVar` is full, `putMVar` waits until the value in the `MVar` is consumed before it can put another value into the variable. Similarly, `takeMVar` waits until a value is placed in the variable before it takes the variable. `MVar`s can be edited across threads.

So to evaluate an expression in the REPL, I can just run a thread to do the downloading, put the results in an `MVar` and read the value whenever I'm done.

Here is an simple Frege/Haskell example:

```
onCreate :: AppCompatActivity -> Maybe Bundle -> IO ()
onCreate this bundle = do
    ...
    resultVar <- newEmptyMVar :: IO (MVar String)
    forkIO (evaluate expr >>= putMVar resultVar)
    result <- takeMVar resultVar
    txtView.setText result
```

This code will run the `evaluate` function in a forked thread (forkIO creates and thread and runs the given function in that thread) and then put the result into `resultVar` (read `>>=` as "and then"). But this has a slight problem; the UI thread will block waiting for `resultVar` to be filled. But Android provides a `View.post` method that allows async updates to a view. We can use the post function to update the `TextView` as follows:

```
onCreate :: AppCompatActivity -> Maybe Bundle -> IO ()
onCreate this bundle = do
    ...
    resultVar <- newEmptyMVar :: IO (MVar String)
    forkIO (evaluate expr >>= putMVar resultVar)
    update <- Runnable.new (takeMVar resultVar >>= txtView.setText)
    txtView.post update
```

Now, we update the TextView with the result without blocking the main thread. I'm still yet to look into how this API could be used to send progress updates to the UI as well but for my purposes, `forkIO` and `MVar`s proved to be a very simple and useful solution.

Check out the complete application, written in Frege:

[Source code](https://github.com/mchav/try-frege-android).
