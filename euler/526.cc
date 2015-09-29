#include <iostream>
#include <string>
#include <vector>
#include <array>
#include <cstdint>

uint64_t f(uint64_t n) {
  uint64_t factor = (n & 1) ? 1 : 2;
  if (2 == factor) {
    while (!(n & 1)) n = n / 2;
  }
  for (uint64_t d = 3; d*d <= n; d += 2 ) {
    if (0 == n % d) {
      factor = d;
      while (0 == n % d) n = n / d;
    }
  }
  return n > 1 ? n : factor;
}

struct g_t {
  uint64_t k;
  int i;
  std::array<uint64_t, 9> fs;
  uint64_t g;

  g_t() : k{2}, i{0}, fs{f(2), f(3), f(4), f(5), f(6), f(7), f(8), f(9), f(10)}, g{32} {}

  g_t& operator++() {
    ++k;
    g -= fs[i];
    fs[i] = f(k + 8);
    g += fs[i];
    i = ++i % 9;
    return *this;
  }

  uint64_t operator*() const { return g; }
};

uint64_t h(uint64_t n) {
  auto m = uint64_t{0};
  for (auto g = g_t{}; g.k <= n; ++g) {
    if (*g > m) m = *g;
  }
  return m;
}

void print_fs_of_g(uint64_t n) {
  uint64_t g = 0;
  for (uint64_t i = 0; i < 9; ++i) {
    auto const fni = f(n + i);
    g += fni;
    std::cout << fni << " ";
  }
  std::cout << ": " << g << std::endl;
}

int main() {
  std::cout << f(98) << "\n";
  std::cout << f(99) << "\n";
  std::cout << f(100) << "\n";
  std::cout << f(101) << "\n";

  print_fs_of_g(98);
  print_fs_of_g(99);
  print_fs_of_g(100);

  std::cout << "h(100): " << h(100) << "\n";
  std::cout << "h(200): " << h(200) << "\n";
  std::cout << "h(1000): " << h(1000) << "\n";
  std::cout << "h(10000): " << h(10000) << "\n";
  std::cout << "h(1000000): " << h(1000000) << "\n";
  //this is about the limit of the naive quadratic algorithm
  // the line below will not finish running
  // std::cout << "h(10^9): " << h(1000000000) << "\n";
  return 0;
}
