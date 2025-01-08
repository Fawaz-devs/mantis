

#include <stdio.h>

typedef struct {
  char *pointer;
  size_t len;
} StrSlice;

typedef struct {
  StrSlice slice;
  size_t capacity;
} String;


void String_grow(String* s, size_t new_cap) {
  
}

void String_push_str(String* s, StrSlice slice) {
  if (s->capacity < s->slice.len + slice.len) {
    String_grow(s, capacity)
  }
}




int main() {
  
}
