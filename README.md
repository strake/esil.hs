# Esil — the Explicit-Stack Intermediate Language

Notice: This is rough, unfinished, experimental. It will not emit working assembly code for you yet.

It is a general low-level/backend IR for compilers. It essentially models a register machine with (countable) infinitely many registers, but also has an explicit stack to allow dynamically-sized locals with arbitrary control flow.
