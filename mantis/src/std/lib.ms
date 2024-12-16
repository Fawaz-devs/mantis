type File = void;

extern "C" File* stdout, stdin, stderr;

fn extern fwrite(start: char*, chunk_size: i64, number_of_chunks: i64, file: File*);
fn extern fflush(file: File*) : i32;
fn extern strlen(s: char*) : isize;


fn print(s: char*) {
	let len: isize = strlen(s);
	fwrite(s, len, 0, stdout);
	fflush(stdout);
}
