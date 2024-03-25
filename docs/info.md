<!---

This file is used to generate your project datasheet. Please fill in the information below and delete any unused
sections.

You can also include images in this folder and reference them in the markdown. Each image must be less than
512 kb in size, and the combined size of all images must be less than 1 MB.
-->

## How it works

Uses a set of registers to divide the clock, and then some combinational logic
to convert from binary to decimal for the display.

Puts the bottom 8 bits of the counter on the bidirectional outputs.

With all the inputs set to 0, the internal 24 bit compare is set to 10,000,000. This means the
counter will increment by one each second.

If any inputs are non zero, then the input will be used as an bits 11 to 18 of the 24 bit compare register.
Example: setting the inputs to 00010000 will program 16384 into the compare register.
With a 10MHz clock the counter will increment ~610 times per second.

## How to test

After reset, the counter should increase by one every second with a 10MHz input clock.
Experiment by changing the inputs to change the counting speed.
