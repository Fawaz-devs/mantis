
fn extern fwrite(ptr: i64, size: i64, nmemb: i64, stream: i64) i64;
fn extern malloc(size: i64) i64;
fn extern fopen(file_path_ptr: i64, mode_ptr: i64) i64;
fn extern fread(buffer_ptr: i64, size: i64, count: i64, file_ptr: i64) i64;
fn extern puts(ptr: i64) i32;
fn extern fclose(file_ptr: i64) i32;
fn extern write(fd: i64, data_ptr: i64, count: i64) i64;
fn extern free(ptr: i64);


fn main(argc: i32, argv: i64) i32 {
	let a = 2;
	let b = a + 1;
	b = b * 2;

	puts("Hello World");

	let f = fopen("/tmp/file", "ab");
	fwrite("Fwrite Test sheesh", 1, 15, f);
	fclose(f);


	let f = fopen("/tmp/input", "r");
	let buffer = malloc(1024);
	let read = fread(buffer, 1, 1024, f);
	fclose(f);

	write(1, buffer, read);

	return argc; 
}
