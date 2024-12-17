# Mantis

Rust inspired Memory safe flexible programming language

- Borrow Checking
- Optional Garbage Collection ( compatible with borrow checker )
- Optional Smart Referencing, compiler automatically chooses a variable to be ref counted or owned or garbage collected
- High Level and Low Level differentiation ( modify the compiler itself for special cases )
- More flexible Lifetimes

Are we there yet? Not even close

What's supported?

- Everything is unstable
- But Can import functions from glibc or any other libaries if needed
- C String supported (broken again)
- Loops that require manual "continue", otherwise just are regular blocks


TODO:
- Make Loop loop without manual continue keyword
- Structs
- Generics
- Borrow Checker
