fn extern malloc(size: i64) i64;

struct Array[T] {
	ptr: u64,
	size: u64,
}


fn main(argc: i32, argv: i64) i32 {
	let size: u64 = 20 as u64;
	let ptr: i64 =  malloc(4 * size);

	let array = #init(Array[u64]);
	array.ptr = ptr;
	array.size = size;
	

	return 0;
}
