---
layout: post
title: Installing docker on a Chromebook
---

I couldn't find any instructions online so I thought I'd post them here for anyone who goes through a similar struggle.

```bash
$ sudo apt-get update
$ sudo apt-get install ca-certificates curl gnupg
$ sudo rm -f /etc/apt/keyrings/docker.gpg # Remove any broken stuff that might be around
$ sudo mkdir -p /etc/apt/keyrings
$ curl -fsSL https://download.docker.com/linux/debian/gpg | sudo gpg --dearmor -o /etc/apt/keyrings/docker.gpg
$ sudo chmod a+r /etc/apt/keyrings/docker.gpg
$ sudo apt-get update
$ sudo apt-get install docker-ce docker-ce-cli containerd.io docker-buildx-plugin docker-compose-plugin
```
