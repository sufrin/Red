The variant of AppleRed-server in this folder was an experiment in
using nc from within a running App to make communication with a
running server faster.

It was unsuccessful for the same reason the original simple
AppleRed.app was: apps run in a sandbox that prevents them
sending/receiving communications without explicit permissions being
set in the OS.

BAS. 5th October 2022

