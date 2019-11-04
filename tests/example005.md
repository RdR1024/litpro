---
title: Example markdown output for Pandoc
author: RdR
date: 2019-07-07

---

# Introduction
This is example markdown text that can be handled by Pandoc.

Note that the label for the code block below is a full path name: <tt>tests/mycode.pro</tt>

If you use the extract option ( <tt>-x</tt> ) on the commandline for <tt>lit</tt>, then the
code will be extracted into that file, instead of the default file.  In other words, you
can use this approach to put code into distinct files.

~~~{ .prolog label=mycode caption="Example code" numbers=left .tag1 .tag2 }
person(1,lily).
person(2,john).


~~~


Note that if you want to refer to the code listing in the text, then
use the base name of the path (i.e. the name without path or extension).

For example, refer to Listing \ref{mycode}.

