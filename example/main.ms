

fn fwrite(buffer_ptr i64, size i64, count i64, stream_ptr i64) i64 extern;

fn strlen(ptr i64) i64 extern;


fn main(argc i32, argv @mut @i64) i32 { 
    mut a = argc as i64;

	let arg_len = strlen(*argv);
	fwrite(*argv, 1, arg_len, 1);

    return 0 as i32; 
}


