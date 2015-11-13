#include <iostream>
#include <string>
#include <cassert>

using namespace std;

int main() {
  auto s = std::string("abc");

  auto i = s.find('d');
  assert(i == std::string::npos);

  size_t i2 = s.find('d');
  assert(i2 == std::string::npos);

  unsigned long i3 = s.find('d');
  assert(i3 == std::string::npos);

  unsigned int i4 = s.find('d');
  assert(i4 != std::string::npos);

  int i5 = s.find('d');
  assert(i5 == std::string::npos);

  using ul_t = unsigned long;
  auto i6 = ul_t{s.find('d')};
  assert(i6 == std::string::npos);

  using ui_t = unsigned int;
  auto i7 = ui_t{s.find('d')};
  assert(i7 != std::string::npos);

  auto i8 = int{s.find('d')};
  assert(i8 == std::string::npos);


  return 0;
}
