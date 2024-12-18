fn extern malloc(size: i64) i64;
fn extern free(ptr: i64);
fn extern write(fd: i64, data_ptr: i64, count: i64) i64;


struct Array[T] {
	ptr: pointer[T],
	size: u64
}

fn array_new[T](size: u64) -> pointer[Array[T]] {
	let ptr: pointer[T] = malloc(size * #size_of(f64)) as pointer[T];
	let arr: pointer[Array[T]] = malloc(#size_of(Array[T])) as pointer[Array[T]];
	arr.ptr = ptr;
	arr.size = size;
	return arr;
}

fn array_free[T](arr: pointer[Array[T]]) {
	free(arr.ptr);
	free(arr);
}

fn array_get_at[T](arr: pointer[Array[T]], index: u64) {
	return @(arr.ptr + index * #size_of(T));
}

fn array_set_at[T](arr: pointer[Array[T]], index: u64, value: T) {
	@(arr.ptr + index * #size_of(T)) = value;
}

fn main(argv: i32, argc: i64) i32 {
	let arr: pointer[Array[u8]] = array_new[u8](11);

	array_set_at[u8](arr, 0, 104 as u8);
	array_set_at[u8](arr, 1, 101 as u8);
	array_set_at[u8](arr, 2, 108 as u8);
	array_set_at[u8](arr, 3, 108 as u8);
	array_set_at[u8](arr, 4, 111 as u8);

	write(1, arr.ptr, 5);

	array_free[u8](arr);

	return 0;
}


