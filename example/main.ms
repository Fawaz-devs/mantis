

fn fwrite(buffer_ptr i64, size i64, count i64, stream_ptr i64) i64 extern;
fn write(fd i64, buffer_ptr i64, len i64) i64 extern;
fn malloc(size i64) i64 extern;
fn free(size i64) extern;
fn memcpy(dest i64, src i64, count i64) i64 extern;

fn strlen(ptr i64) i64 extern;

type ptr[T] = i64;

type ArraySlice[T] = struct {
    pointer ptr[T],
    len u64
}

type StrSlice = ArraySlice[u8];


type String = struct {
    capacity u64,
    slice StrSlice,
}


impl Drop for String {
    fn drop(self @mut Self) {
        free(self.slice.ptr);
    }
}


impl String {
    fn with_capacity(cap u64) -> String {
        if cap > 0 {
            return String {
                slice: StrSlice {
                    pointer: malloc(cap),
                    len: 0
                },
                capacity: cap
            };
        } else {
            return String {
                slice: "",
                capacity: cap,
            }
        }
    }

    fn new() String {
        return String.with_capacity(0);
    }

    fn grow(self @mut String, new_cap u64) {
        let new_string = String.with_capacity(new_cap);
        memcpy(new_string.slice.pointer, self.slice.pointer, self.slice.len);
        *self = new_string;
    }
    
    fn push(self @mut String, c i8) {
        if self.capacity < self.slice.len + 1 {
            self.grow(self.capacity * 2);
        } 
        *(self.slice.ptr + self.slice.len) = c;
        self.slice.len = self.slice.len + 1;
    }

    fn push_str(self @mut String, s StrSlice) {
        if self.capacity < self.slice.len + s.len {
            self.grow(self.capacity * 2);
        } 
        memcpy(self.slice.pointer + self.slice.len, slice.pointer, slice.len);
        self.slice.len = self.slice.len + s.len;
    }

    fn truncate(self @mut String, new_len u64) {
        self.slice.len = new_len;
    }

    fn clear(self @mut String) {
        self.truncate(0);
    }

    fn fromStr(slice StrSlice) String {
        mut s = String.with_capacity(s);
        s.push_str(slice);
        return s;
    }
}




fn main(argc i32, argv i64) i32 { 
    let stdout = 1;
    let s = "hello world\n";

    mut string_test = String.fromStr(s);
    string_test.push_str(s);

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


