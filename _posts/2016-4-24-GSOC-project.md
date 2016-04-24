---
layout: post
title: Google Summer of Code with Scala
---

I recently got accepted into the Google Summer of Code program under the Scala organisation to work on
a build tool called CBT (I think what the C stands for is still an open question).

My mentor, Christopher Vogt, is someone I had worked with prior to my acceptance. He helped me write
a [Maven Search utility](https://github.com/ChavXO/MavenSearch) earlier this year and we have plans to include it in CBT some
time in the future.

My summer of code project, as I said, entails adding some features to a fairly new build tool called CBT.
CBT is a build tool in its infancy with a lot of room for optimisation and enhancement. As such, development is still very fast moving. The goal of my project is primarily threefold: decrease build time by having CBT work completely in memory with a virtual file system; adding support for download centers such as Sonatype and BinTray allowing for upload and deployment; and implementing a cloud compilation feature to exploit the benefits of cluster computing.
# Summary
CBT is a fast, easy build tool that allows builds to be written in idiomatic Scala. My project seeks to enhance it specifically in those areas. Thus my project will cover the following:
Making CBT to work in-memory to increase build speed times.
Supporting for cluster, cloud compilation.
Cleaning up the code base and making some usability changes.
Adding support for download centers.

# Project details
# Running CBT in-memory
One of CBT’s main priorities is to support fast builds. Part of CBT’s current bottleneck is the time it takes to load files from disk. Loading jar and class files to memory and subsequently working from an in memory file system would allow short lived files to be written and accessed without generating disk or network I/O and subsequently increase the tool’s build speed. A virtual file system maximises file manipulation speed while preserving UNIX file semantics meaning most of the project is easily portable to the in-memory file system.
To implement this part of the project I will use an in-memory file system such as [JIMFS](https://github.com/google/jimfs) to load and cache files.

# Support for cluster, cloud compilation
A cluster is a logical grouping of container instances that you can place tasks on. Supporting cluster compilation would mean that developers can increase performance by sending tasks to a cloud service such as AWS’s lambda platform or Apache Mesos. This functionality is in line with CBT’s speed goal. Setting up functionality for cloud compilation would entail allowing safe, multi-user authorisation to the web platform and interfacing Scala with whichever cloud service I work on. I will be co-mentored by Steven She for this part of the project.

# Cleaning up code and making usability changes
Since CBT is supposed to be a community tool. Thus maintaining a well-documented and readable code base is essential to keeping the project active and making it more popular. This part of the project is continuous and would involve documenting the code base, commenting the code and including a developer wiki. It would also entail fixing small bugs that are reported as the tool is used. I also hope to tighten up some of the work on Github dependencies as that could be written as small pieces of a stand-alone project.

# Support for download centers
Finally, I will to add support for deployment and uploading into download centers such as bintray or sonatype. This part of the project will entail setting up user authentication, encrypting this information on user disk using GPG and then adding the download and deploy features.

I'll keep all research into these topics posted on the blog.