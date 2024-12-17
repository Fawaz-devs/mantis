
// fn extern fwrite(ptr: i64, size: i64, nmemb: i64, stream: i64) i64;
// fn extern malloc(size: i64) i64;
// fn extern fopen(file_path_ptr: i64, mode_ptr: i64) i64;
// fn extern fread(buffer_ptr: i64, size: i64, count: i64, file_ptr: i64) i64;
// fn extern puts(ptr: i64) i32;
// fn extern fclose(file_ptr: i64) i32;
// fn extern write(fd: i64, data_ptr: i64, count: i64) i64;
// fn extern free(ptr: i64);
fn extern rand() i32;

// struct Foo {
// 	a: i32,
// 	b: i64,
// 	c: f32,
// }


fn foo() i64 {
	return 20;
}



fn main(argc: i32, argv: i64) i32 {
	let a: i64 = 2;
	let b: i32 = 7 as i32;

	let i: i64 = 0;

	// if i > 10 {
	// 	i = i + 2;
	// } else {
	// 	i = i + 12;
	// }

	loop {
		i = i + 2;

		if i > 10 {
			i = i - 1;
			break;
		} else {
			
		}
	}
	
	return i as i32; 
}
