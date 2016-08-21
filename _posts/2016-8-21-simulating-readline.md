---
layout: post
title: Towards shell tab completion - Simulating readline to capture intermediate input
---

Any program that claims to be useful [REPL](https://en.wikipedia.org/wiki/Read-eval-print_loop)
must support three basic features:

* Tab completion,
* History, and,
* Support for shell commands in the REPL.

I wrote the [with tool](https://github.com/mchav/With) with these features in mind.
`with` uses a `while` loop and bash's builtin command `read` to simulate a REPL.
Adding history to the program was a simple command call, and supporting execution of arbitrary
shell commands required a little (eval trickery)[https://github.com/mchav/with/issues/23].


The third feature, what has since become my holy grail, is tab completion in bash.
The `read` command supports either file tab completion or command tab completion and the 
two are [mutually exclusively](http://stackoverflow.com/questions/12044574/getting-complete-and-menu-complete-to-work-together).
Even then, command completion was useless to me because the it doesn't complete arguments on commmands. That is,
`gi<TAB>` gives you a list containing all installed programs beginning with "gi" and `git a` gives you a list
containing all the programs starting with "a" and ignores the previous word.

This difficulty arises from the fact that `read` in the bash source code sets the completion function to `null`
and then tries binds it to either file or command completion only as mentioned (here)[http://stackoverflow.com/questions/4726695/bash-and-readline-tab-completion-in-a-user-input-loop]. Ultimately what this means is that tab completion past those two features
is impossible. 

Surely there must be a workaround, right?

After, searching through pages of Google hits and Stack Overflow questions it seemed as though there wasn't a way to do so.
So until bash decides to include the feature, it is an open problem that is the subject of many forum discussions.
I decided I had the following options:

* [change the bash source code and distribute it with the with tool](https://media.giphy.com/media/O8lbnqdFAgunm/giphy.gif)
* [write a C program that simulated read and then ported over some of the completion work](https://41dcdfcd4dea0e5aba20-931851ca4d0d7cdafe33022cf8264a37.ssl.cf1.rackcdn.com/5971464_kanye-west-caught-smiling-and-then-frowning_6a0a7fd1_m.jpg?bg=51353A)
* [make the application hook back to the main bash process and try to access completions from there](http://superuser.com/questions/175799/does-bash-have-a-hook-that-is-run-before-executing-a-command), or,
* control the way IO happened in the script.

The third solution seems like overkkill but I haven't looked into it. 

The fourth doesn't seem to solve the problem - or does it?
The fourth does something a quite beautiful. If we can incrementally get the input then we can use it to start a script or process that
generates completions for us and the control what is printed and how using some [cursor trickery](http://www.idem-singapore.com/sites/live.idemsg2015.site.gsi.sg/files/No%20Under%2018.png). It reduces the problem of trying to get completions from a comand that doesn't support
 then (seemingly NP-complete and the pun is intended) to generating a list of completions using some external tool and controlling how it is printed (P-complete :)). 

Generating completions itself proved to be hard but there are hacks that have worked for me. The most notable one is using [xdotool](http://www.semicomplete.com/projects/xdotool/) in a separate bash process to simulate pressing tab on some input then writing the completions
generated onto a file, reading them and then cyclying through them. Of course this is slightly inefficient but one if you can count on 
having tab completion being used once in a while, some caching and a command history it seems fine to allow this sort of inefficiency.

But how do we simulate read to accept completions? One thought is a [C program](http://2.bp.blogspot.com/-rofkX14qGUA/UB04M3NJ2wI/AAAAAAAACEk/w-BDMoI7sFU/s320/kanye-west-laugh.gif) - can you tell how much I like writing C? - but I deally I want to distribute this as a purely
scripted program with possibly a third party dependency. I want to be able to essentialy intercept each key press and then using the current input and all the input so far
do something. So this boils down to reading a single character, which bash can do, swallowing the input, doing some processing,
and finally printing some result on the same line. This little script does just that:

```
my_read ()
{
  while true
  do
    read -s -n 1 w
    echo -e -n $w
    if [[ -z "$w" ]]; then
      echo -e -n $'\n'
      break
    fi
    word="$word$w"
  done
}

my_read
echo "Done: the word was $word"
```

But there are still a few open problems with this solution as well. Reading a character in this way doesn't seem to differentiate between
spaces - newlines and tabs, as far as I know seem to both be considered empty strings. But that seems to be a minor issue given that all else is in place. Given this implementation we can get completions and fill them in ourselves using some cursor manipulation and clever printing.

I have a full example on the way for part two of this series but hopefully this either is useful to someone or reveals some other insights that will help with the problem.