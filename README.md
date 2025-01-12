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
- But Can import functions from libc or any other libaries if needed


TODO:
- Generics
- Borrow Checker
- Function Templates
- Trait Templates


## Example Hello World

```

type MyStruct = struct {
  name String,
}

type MyEnum = enum {
  None,
  MyStruct(MyStruct),
}



fn main() {
  let s = "Hello World";
  println(s);

  let my_string = String.fromStr(s);
  let my_enum = MyEnum.MyStruct(my_string);

  if MyEnum.MyStruct(ms) = my_enum {
    print("My Enum has My Struct ");
    println(ms.as_str());
  } else {
    println("My Enum doesn't have My Struct");
  }

  
}


  
```
