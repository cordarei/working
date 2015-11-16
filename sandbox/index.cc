#include <cstddef>
#include <climits>
#include <cassert>

#include <vector>
#include <string>
#include <iostream>



namespace kysk
{
  struct index_t
  {
    using value_type = ptrdiff_t;

    explicit constexpr index_t(value_type v) : value_{v} {}
    explicit constexpr index_t(size_t n) : value_{static_cast<value_type>(n)}
    {
      assert(n/2 <= PTRDIFF_MAX);
    }

    index_t operator++() {
      return index_t{++value_};
    }
    index_t operator--() {
      return index_t{--value_};
    }

    constexpr value_type value() const { return value_; }

    // constexpr operator value_type() const { return value_; }
    constexpr operator size_t() const {
      assert(value_ > 0);
      return static_cast<size_t>(value_);
    }

  private:
    value_type value_;
  };

  bool operator<(index_t i, index_t j) {
    return i.value() < j.value();
  }
  bool operator>(index_t i, index_t j) {
    return i.value() > j.value();
  }
  bool operator<=(index_t i, index_t j) {
    return i.value() <= j.value();
  }
  bool operator>=(index_t i, index_t j) {
    return i.value() >= j.value();
  }
  bool operator>=(index_t i, int j) {
    return i.value() >= j;
  }


  index_t operator"" _ix(unsigned long long v) {
    return index_t{static_cast<size_t>(v)};
  }
}
using namespace kysk;

int main()
{
  auto vec = std::vector<int>{1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 9, 42, 99};

  for (auto i = 0_ix; i < vec.size(); ++i)
    std::cout << vec[i] << ", ";
  std::cout << std::endl;

  for (auto i = index_t{vec.size() - 1}; i >= 0; --i) //assertion failure
    std::cout << vec[i] << ", ";
  std::cout << std::endl;

  auto j = 0_ix;
  auto k = 1_ix;
  std::cout << (j < k) << std::endl;

  return 0;
}
