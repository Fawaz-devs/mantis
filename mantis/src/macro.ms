
fn #vec[T](args: Vec[Arg[U]]) $Node
 U: Any + const Into[T] {
	let v = Vec.with_capacity(args.len());

	let idx = 0;
	loop {
		v.push(v.at(idx).into());
		idx += 1;
		if idx == args.len() {
			break;
		}
	}

	return $Node.var(v);
}


fn main() {
	let v = #vec[u32](0, 1, 2);
}
