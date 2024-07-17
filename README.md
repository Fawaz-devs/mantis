# Mantis

Rust inspired Memory safe flexible programming language

- Borrow Checking
- Optional Garbage Collection ( compatible with borrow checker )
- Optional Smart Referencing, compiler automatically chooses a variable to be ref counted or owned or garbage collected
- High Level and Low Level differentiation ( modify the compiler itself for special cases )
- More flexible Lifetimes

Are we there yet? No where even close


What's supported?

- Everything is unstable
- Can't declare local functions, only main is the function
- But Can import functions from glibc or any other libaries if needed
- I64 operations supported
- No loops yet
- C String supported
