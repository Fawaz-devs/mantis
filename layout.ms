
fn fn_name(arg0 type, arg1 type, arg2 type) return_type {
	#macro(arg0, arg1, arg2);

	let value = arg0.method(arg1, arg2);

	return value;
}


fn main() void {
	let s = "Hello World";

	print(s);
}

fn print(s str) void {
	#print(s);
}
