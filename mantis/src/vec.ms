
struct std {
	net: Net 
}

struct Net {
	TcpSocket,
	SocketAddr
}

struct SocketAddr {
	host: Array[u8, 4],
	port: u8,
}


struct Array[T, N] {
	
}

struct Slice[T, &L] {
	ptr: *T,
	len: usize,
}


struct Vec[T] {
	ptr: *T,
	len: usize,
	cap: usize,

	fn new() Self {
		return Self {
			ptr: 0,
			len: 0,
			cap: 0,	
		}
	}

	fn with_cap(cap: usize) Self {
		let ptr = malloc(size_of[T] * cap).cast[*T]()
		return Self {
			ptr,
			len: 0,
			cap,
		}
	}

	fn extend_from(slice: Slice[T]) {
		
	}


	fn get(self, idx: usize) Option[T] {
		if (self.len < idx) {
			return Option.Some(*(self.ptr + self.len))
		}
		return Option.None
	}

	fn push(self, element: T) {
		if (self.len + 1 <= self.cap) {
			
		}
		
	}

	fn pop(self) Option[T] {
		if (self.len > 0) {
			let element = self.get(self.len - 1);
			self.len -= 1;
			return element;
		}
		return Option.None;
	}

}


fn SocketAddr.new(host: String, port: u8) SocketAddr {
	return SocketAddr {
		host, port
	};
}

struct TcpSocket {
	buffer: Vec[u8]
}

fn TcpSocket.new(addr: SocketAddr) TcpSocket {
}

fn TcpSocket.listen(self) {
}


struct SyncRead {
	fn read_into(self, buffer: Slice[u8]) usize;
	fn read_all(self) Vec[u8]; 
}

struct SyncWrite {
	fn write(self, buffer: Slice[u8]);
}


fn main() i32 {
	let HOST = "127.0.0.1"
	let PORT = 8080;
	let socket = std::net::TcpSocket::new(std::net::SocketAddr::new(HOST, PORT));
	let socket = std.net.TcpSocket.new(std.net.SocketAddr.new(HOST, PORT));
}
