Title
=====

TamaGo - bare metal Go framework for ARM SoCs.

Reducing the attack surface with pure embedded Go.

Authors
=======

Andrea Barisani <andrea.barisani@f-secure.com>  
Andrej Rosano   <andrej.rosano@f-secure.com>  

First presentation
==================

December 2019 - 36c3

Video
-----

https://www.youtube.com/watch?v=4QircrJjEtQ&t=1s

Updated presentation
====================

October 2020 - GoLab

Video
-----

https://www.youtube.com/watch?v=aOEl5BCfHv0

Abstract
========

TamaGo is an Open Source operating environment framework which aims to allow
deployment of firmware for embedded ARM devices by using 0% C and 100% Go code.
The goal is to dramatically reduce the attack surface posed by complex OSes
while allowing unencumbered Go applications.

TamaGo is a compiler modification and driver set for ARM SoCs, which allows
bare metal drivers and applications to be executed with pure Go code and
minimal deviations from the standard Go runtime.

The presentation explores the inspiration, challenges and implementation of
TamaGo as well as providing sample applications that benefit from a pure Go
bare metal environment.

TamaGo allows a considerable reduction of embedded firmware attack surface,
while maintaining the strength of Go runtime standard (and external) libraries.
This enables the creation of HSMs, cryptocurrency stacks and many more
applications without the requirement for complex OSes and libraries as
dependencies.

Repository
==========

https://github.com/f-secure-foundry/tamago
