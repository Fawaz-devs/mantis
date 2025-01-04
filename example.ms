use std.Lifetime


struct Foo;
Struct Doo;

Lifetime;
Allocated;


fn main() void {
	let s = "Hello World";
	#print(s);
}


fn create_string_but_return_slice() -] Ref[str, String] {
	let s = String::from("Hello world");
	let s2 = String::from("Hello world");

	return s2.slice(0..10).slice(0..1);

	// Ref[str, Ref[[u8], Ref[str, T]]] -> Ref[[u8], T]

	{
		let ref_v = {
			let v = Vec.with_capacity[u8](10 * 1024 * 1024);

			let ref_v: Ref[[u8], Vec] = v.slice(0..10);
			ref_v

		}

	}
}


DeveloperCode | Preprocessing | MyCompilerTypesEvalution | CompilerTypesEvalution | MantisCompiler | Cranelift IR | Executable

--------------------------- compiler.ms -----------------------------------------

