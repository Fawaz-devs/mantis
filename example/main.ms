

fn fwrite(buffer_ptr i64, size i64, count i64, stream_ptr i64) i64 extern;
fn write(fd i64, buffer_ptr i64, len i64) i64 extern;

fn strlen(ptr i64) i64 extern;

type ptr[T] = i64;

type ArraySlice[T] = struct {
    pointer ptr[T],
    len u64
}

type StrSlice = ArraySlice[u8];

fn main(argc i32, argv i64) i32 { 
    let stdout = 1;
    let s = "hello world";

    write(stdout, s.pointer, s.len);

    mut i = 0;
    loop {
        if i >= argc as i64 {
            break;
        }

        let arg_char_ptr = *(argv + i * 8);
        let arg_len = strlen(arg_char_ptr);
        write(stdout, arg_char_ptr, arg_len);
        i = i + 1;
    }

    return 0 as i32; 
}


