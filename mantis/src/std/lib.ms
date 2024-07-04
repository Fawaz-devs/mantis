type File = void;

extern "C" File* stdout, stdin, stderr;

extern "C" fn fwrite(start: char*, chunk_size: i64, number_of_chunks: i64, file: File*);
extern "C" fn fflush(file: File*) : i32;

extern "C" fn strlen(s: char*) : isize;


fn print(s: char*) {
	let len: isize = strlen(s);
	fwrite(s, len, 0, stdout);
	fflush(stdout);
}
