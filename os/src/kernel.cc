#include <cstdint>

template <int N>
void display(char const (&str)[N], char color, int position) {
  char *vga = reinterpret_cast<char*>(0xb8000);
  for (int i = 0; i < N; ++i) {
    vga[position + 2*i] = str[i];
    vga[position + 2*i + 1] = color;
  }
}

extern "C" {
  void cc_main() {
    display("Hello, World!", 0x1f, 1050);
  }
}

