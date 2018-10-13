---
layout: post
title: A simple server in Frege
---

I've always liked working in Java because the packages are extensive and are usually well documented. However, I prefer working in functional languages because functional languages do a better job of enforcing modularity. [Frege](https://github.com/Frege/frege) seems to be the sweet spot between these two concerns (I haven't looked into [ETA](https://github.com/typelead/eta) yet but it seems promising too). So I took it upon myself to try and write a simple HTTP server in Frege using Java SE (if you count the sun packages as part of Java SE proper).

The goal will be to serve the text "<h1>It worked</h1>" to localhost:8000 using Frege. Let's start by defining the module that our application will exist in, the response string and a dummy main function.

```haskell
module Server where

response :: String
response = "<h1>It worked</h1>"

main = println "Serving HTTP on 0.0.0.0 port 8000"
```

At a high level, we want to establish a socket connection to localhost, create a server at that address, define a root context and a handler for the context, then start the server.

In Haskell-like pseudocode, the operations would be a series of `IO` actions as follows:

```haskell
handler :: IO ()
handler = send "<h1>It worked!</h1>"

main = do
	connection <- connectToAddress 8000
	server <- startServer connection handler
	server.start
```

Rougly speaking, the Frege solution should look somewhat similar. So we have a tenative Frege section and we want to use some Java libraries to build the server. Let's first digress and see how the same solution would be implemented in pure Java. 

```java
package server;

import java.io.IOException;
import java.io.OutputStream;
import java.net.InetSocketAddress;

import com.sun.net.httpserver.HttpExchange;
import com.sun.net.httpserver.HttpHandler;
import com.sun.net.httpserver.HttpServer;

public class Test {

    public static void main(String[] args) throws Exception {
        HttpServer server = HttpServer.create(new InetSocketAddress(8000), 0);
        server.createContext("/test", new MyHandler());
        server.setExecutor(null); // creates a default executor
        server.start();
    }

    static class MyHandler implements HttpHandler {
        @Override
        public void handle(HttpExchange t) throws IOException {
            String response = "This is the response";
            t.sendResponseHeaders(200, response.length());
            OutputStream os = t.getResponseBody();
            os.write(response.getBytes());
            os.close();
        }
    }

}
```

This isn't a clear case of interfacing with Java as you may notice. We have an interface that we need to implement. The interface must define a handler that we can assign to a context. But from the example above we have a rough idea of what the types should look like, so let's go ahead and define a few of them and revisit the main function.

```haskell
data HttpExchange = native com.sun.net.httpserver.HttpExchange where
	native getResponseBody :: MutableIO HttpExchange -> IO OutputStream
	native sendResponseHeaders :: MutableIO HttpExchange -> Int -> Long -> IO () throws IOException

data Executor = native java.util.concurrent.Executor

data InetSocketAddress = native java.net.InetSocketAddress where
	native new :: Int -> STMutable s InetSocketAddress

data HttpServer = native com.sun.net.httpserver.HttpServer where
	native create "com.sun.net.httpserver.HttpServer.create" :: Mutable s InetSocketAddress -> Int -> STMutable s HttpServer throws IOException
	native createContext :: MutableIO HttpServer -> String -> MutableIO HttpHandler -> IO ()
	native setExecutor :: MutableIO HttpServer -> Maybe Executor -> IO ()
	native start :: MutableIO HttpServer -> IO ()

main = do
	inet <- InetSocketAddress.new 8000
	server <- HttpServer.create inet 0
	handler <- HttpHandler.new handle
	server.createContext "/" handler
	server.start
	println "Serving HTTP on 0.0.0.0 port 8000"
```

A note on defining Java classes in Frege. Java classes correspond to Frege data types. In general, you define Java classes in Frege using the native keyword as follows: `data X = native java.dummy.Type`. This will make a data type X corresponding to the Java class `Type`. To expose the class's methods simply give the name and type signature of each method as show above. There are some caveats but those are dealt with extensively in the Frege documentation.

Say we wanted to define a Handler class that contains a `handle` function that will be called by the server. In Frege you can inline a Java module that will be hoisted to the top of the compiled Java file.

```java
native module where {
  public static class Handler implements com.sun.net.httpserver.HttpHandler {
    @Override
    public void handle(com.sun.net.httpserver.HttpExchange t) throws java.io.IOException {
      String response = "This is the response";
      t.sendResponseHeaders(200, response.length());
      OutputStream os = t.getResponseBody();
      os.write(response.getBytes());
      os.close();
    }
  }
}
```

This is where it gets a bit tricky. My first thought was that the data definition for this class would be:

```haskell
data Handler = native com.sun.net.httpserver.HttpHandler where
	native new Server.Handler.newInstance :: (MutableIO HttpExchange -> IO ()) -> STMutable s HttpHandler 
```

This will not compile because the Frege compiler will use Handler as a substitute for `com.sun.net.httpserver.HttpHandler`. When it tries to call the object constructor via new it will complain and tell you that HttpHandler is an abstract class or interface and therefore cannot be constructed. A workaround would be to provide a `newInstance` method that wraps the constructor then use `new` as a substitue for that native function. Now we have:

```
data Handler = native com.sun.net.httpserver.HttpHandler where
	native new Fervor.Handler.newInstance :: () -> STMutable s HttpHandler 

native module where {
  public static Handler newInstance() {
    Handler h = new Handler();
    return h;
  }
  
  public static class Handler implements com.sun.net.httpserver.HttpHandler {
    @Override
    public void handle(com.sun.net.httpserver.HttpExchange t) throws java.io.IOException {
      String response = "This is the response";
      t.sendResponseHeaders(200, response.length());
      OutputStream os = t.getResponseBody();
      os.write(response.getBytes());
      os.close();
    }
  }
}
```

That's it! We have a complete definiton of an HttpServer in Frege. But wait, what if we wanted to move the implementation of the interface to FregeLand rather than relying on Java still? This turns out to be quite a hairy problem that requires a dive into the Frege source code. We would need to communicate with the Frege code from within the java class to delegate the job of handling the request. My solution, after a few attempts,was to pass the handler function into the Handler constructor and then call it from within the Java code. First, let's make the constructor take in a function from HttpExchange to Unit.

```haskell
data HttpHandler = native com.sun.net.httpserver.HttpHandler where
  -- substitute for a constructor
  native new Server.Handler.newInstance :: (MutableIO HttpExchange -> IO ()) -> STMutable s HttpHandler 
```

After some snooping and readin through the source code I found that the function type in Frege is `Func.U` found in `frege.run7<A, B>`where A is the input type and B is the return value type. This definition will only work if the compile target is Java 7. Java 8 or higher would fail. However, it should be easy to migrate to 8. So we want our native code to take in an argument of type `frege.run7.Func.U` and then pass it to the handler which then runs it. But what does running it look like? The problem with the handler function as returned by Frege is that it is an unapplied function that is waiting for an HttpExchange. We evaluate the function by calling `apply` with a lazy value, casting it back to a function then calling run as follows:

```haskell
native module where {
 public static class Handler implements com.sun.net.httpserver.HttpHandler {
   final Func.U<HttpExchange, Func.U<RealWorld,Short>> handlerFunction;
 
   public Handler(Func.U<HttpExchange, Func.U<RealWorld,Short>> function){
     this.handlerFunction = function;
   }
 
  public static Handler newInstance(Func.U<HttpExchange, Func.U<RealWorld,Short>> function) {
    Handler h = new Handler(function);
    return h;
  }
 
  @Override
  public void handle(HttpExchange t) throws java.io.IOException {
    try 
      final Lazy<Func.U<RealWorld,Short>> args = handlerFunction.apply(Thunk.<cHttpExchange>lazy(t)).call();
      final frege.run7.Func.U<Object,Short> res = RunTM.<frege.run7.Func.U<Object,Short>>cast(args).call();
      PreludeBase.TST.run(res).call();
    } catch (Exception e) {
      System.out.println("Failed to execute handler");
    }			
  }
}
```

And now we have our handler function as:

```haskell
handle :: MutableIO HttpExchange -> IO ()
handle t = do
	t.sendResponseHeaders 200 (length response).long
	os <- t.getResponseBody
	bytes <- StringAsBytes.getBytes response
	os.write bytes
	os.close
```

There you have it! A simple server with all important parts in Frege land. This seems a little extraneous but I assure you with a more involved handler it is worth trudging through the boilerplate. Chances are with the adoption of Frege and the availability of external libraries in Frege a lot of the boilerplate may disappear. Hopefully this motivates someone to contribute to a wonderful language community!

[The project is available on Github gist.](https://gist.github.com/mchav/ec2a5527d0d43f649aee6b2692a3628a)
