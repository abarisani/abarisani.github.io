Title
=====

Fully arbitrary 802.3 packet injection: maximizing the Ethernet attack surface

Authors
=======

Andrea Barisani <andrea@inversepath.com>  
Daniele Bianco <daniele@inversepath.com>  

First presentation
==================

July 2013 - Black Hat USA

Video
=====

https://www.youtube.com/watch?v=j_sqwo1xjIA

Abstract
========

It is generally assumed that sending and sniffing arbitrary Fast Ethernet
packets can be performed with standard Network Interface Cards (NIC) and
generally available packet injection software. However, full control of frame
values such as the Frame Check Sequence (FCS) or Start-of-Frame delimiter (SFD)
has historically required the use of dedicated and costly hardware. Our
presentation will dissect Fast Ethernet layer 1 & 2 presenting novel attack
techniques supported by an affordable hardware setup with customized firmware
which will be publicly released.

This research expands the ability to test and analyse the full attack surface
of networked embedded systems, with particular attention on automation,
automotive and avionics industries. Application of attacks against NICs with
hard and soft Media Access Control (MAC) on industrial embedded systems will be
explored.

We will illustrate how specific frame manipulations can trigger SFD parsing
anomalies and Ethernet Packet-In-Packet injection. These results are analyzed
in relation to their security relevance and scenarios of application. Finally,
conditions for a successful remote Ethernet Packet-In-Packet injection will be
discussed and demonstrated for what is believed to be the first time in public.

Updates
=======

The XMOS XC-2 Ethernet Kit, mentioned in the research, can now be replaced with
the author's own hardware design:

  https://github.com/f-secure-foundry/jobun
