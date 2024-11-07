


fn split_strings(s: String, c: Char) Vec[String] {

	mut result = Vec[String].new();
	mut prev = 0;
	
	for i in 0..s.len() {
		if (s[i] == c) {
			result.push(s.substring(prev, i));
			prev = i + 1;
		}
	}

	return result;

} (mut ref allocator = GlobalAllocator)



fn foo() {

	mut arena = std.mem.ArenaAllocator.new();

	with (allocator = arena) {
		let s = String.from("Hello World Hello World");
		let result = without split_strings(s, ' '); // Type of result = Vec[String]@ref GlobalAllocator

		return result;
	}

	
	let s = with (allocator = arena) String.from("Hello World Hello World");
	let result = with (alloactor = arena) split_strings(s, ' '); // Type of result = Vec[String]@[ref arena]

	arena.free();

	result // can't be used;


	
}
