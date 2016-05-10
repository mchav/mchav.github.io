---
layout: post
title: On learning to use new tools
---

At the beginning of the year I only knew of Scala. I hardly knew its syntax or design principles. I just knew it was a functional language based on the JVM. In anticipation of GSoC I reached out to some Scala devs to learn about their projects and hopefully start contributing. I was immediately assigned tasks and bug fixes to get me comfortable with the code base and the workflow. I found that this was the fastest way I've ever learnt to use a tool. I didn't slave through tutorials reading on principles of the language or making toy projects. I had ad hoc reading sessions to learn how to implement some high level ideas in code. This, I argue, is the best way to learn any new software tool (with some caveats to be stated at the end). And here's why:

## 1) You become productive quicker
While being productive is important in and off itself, I think, in terms of learning, it provides motivation to keep using a tool set. I very quickly became very curious about what else I could do and what other features I could implement. That sense of ownership comes sooner than from book learning. It also motivates book learning (albeit in a pretty disorganised way).

## 2) You'll only ever use a small subset of the tool anyway
I find this is true of functional programming and more generally speaking, feature rich labguages. You can survive using composition and higher-order functions, not knowing a thing about monads, applicatives, futures and all the rest of it. Coversely though, it helps to know these things before hand so they can influence your design choices. That being said - feedback such as, "That type is clearly an applicative and you could made your code much more readable by making it an instance of applicative." It's easier to learn the use cases and principles of applicative once you've come across

## 3) Hands on development means you read more real-world code
Assuming you're doing PRs on an open source repository or a large code base, going in head first means that you are forced to read a more experienced developer's code and learn from their style. Admittedly, this is more of an argument for doing open source development rather than a learning by doing but I have found these two to intersect so frequently that it is fruitful to cite this point as an argument for hands on development. Even when you're just copy pasting code and making edits, you are more likley to freely update the code and check if it work if you're not bogged down by details.

All this advice assumes you have a prior knowledge of programming in general. If you're an aboslute beginner then reading probably helps a decent amount. But as a programmer, productivity is key so put that tutorial down, think of something to make for yourself, and make it.
