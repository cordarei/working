// based on www.keithschwarz.com/interesting/code/?dir=zeckendorf-logarithm
// it should be possible to unify the iterator types and make them actual iterators
// of an infinite range with a nice abstraction for statically selecting arbitrary groups


#include <iostream>

class integer_group {
public:
  using value_type = int;
  static value_type id();
  static value_type op(value_type a, value_type b);
};

struct int_mult_group_wrapper {
  using value_type = int;
  static value_type const identity = 1;

  int_mult_group_wrapper(value_type w) : v{w} {}

  int_mult_group_wrapper operator+(int_mult_group_wrapper other) { return int_mult_group_wrapper{v + other.v}; }

  value_type v;
};

class fibonacci_iterator {
  int a;
  int b;
public:
  fibonacci_iterator()
    : a{0}, b{1}
  {}

  int operator*() const { return a; }
  int next() const { return b; }

  fibonacci_iterator& operator++() {
    int c = a + b;
    a = b;
    b = c;
    return *this;
  }

  fibonacci_iterator& operator--() {
    int c = b - a;
    b = a;
    a = c;
    return *this;
  }
};

class fiblog_iterator {
  int a;
  int b;
public:
  fiblog_iterator(int c)
    : a{1}, b{c}
  {}

  int operator*() const { return a; }
  int next() const { return b; }

  fiblog_iterator& operator++() {
    int c = a * b;
    a = b;
    b = c;
    return *this;
  }

  fiblog_iterator& operator--() {
    int c = b / a;
    b = a;
    a = c;
    return *this;
  }
};



int main() {
  int const n = 878371;
  int const a = 32;

  auto baseit = fibonacci_iterator{};
  auto expit = fiblog_iterator{2};

  for (; expit.next() < n; ++baseit, ++expit);
  int k = n;
  int result = 0;
  for (; *baseit > 0; --baseit, --expit) {
    if (*expit <= k) {
      k /= *expit;
      result += *baseit;
    }
  }

  std::cout << "Result: " << result << std::endl;

  return 0;
}
