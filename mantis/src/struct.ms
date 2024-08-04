
struct Foo {
	a: i64,
	b: i64,
	c: f64,
}


fn main(argv: i32, argc: i64) i32 {
	let f = Foo;

	// f.a = 10;
	// f.b = 20;
	// f.c = 3.14;


	// f.a = f.a + 1;
	// let af = f.a as f64;
	// f.c = f.c + af;

	// let fc = f as f32;

	let o = f.a;

	return o as i32;
}
