#include <vector>
#include <string>
#include <iostream>
#include <algorithm>

int main()
{
  auto vec = std::vector<int>{1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 9, 42, 99};

  // print in order:

  for (int i = 0; i < vec.size(); ++i)
    std::cout << vec[i] << ", ";
  std::cout << std::endl;
  // ^ we get a warning about comparing signed and unsigned integers

  for (size_t i = 0; i < vec.size(); ++i)
    std::cout << vec[i] << ", ";
  std::cout << std::endl;
  // ^ no warning

  for (auto i = vec.begin(); i != vec.end(); ++i)
    std::cout << *i << ", ";
  std::cout << std::endl;
  // ^ no warning

  for (auto const & v : vec)
    std::cout << v << ", ";
  std::cout << std::endl;
  // ^ no warning


  // print in reverse order:

  for (int i = vec.size() - 1; i >= 0; --i) // btw I wrote 2 separate bugs on this line
    std::cout << vec[i] << ", ";
  std::cout << std::endl;
  // ^ no warning for truncating vec.size(),

  //for (size_t i = vec.size(); i >= 0; --i)
  // ^ infinite loop

  for (size_t i = vec.size() - 1; i > 0; --i)
    std::cout << vec[i] << ", ";
  std::cout << std::endl;
  // ^ missing the first elt

  for (auto i = vec.rbegin(); i != vec.rend(); ++i)
    std::cout << *i << ", ";
  std::cout << std::endl;
  // ^ no warning

  // and of course we can always reverse the vector in place first:
  std::reverse(vec.begin(), vec.end()); // and of course I tried to pass `vec` as the sole argument, thanks range-v3!
  for (size_t i = 0; i < vec.size(); ++i)
    std::cout << vec[i] << ", ";
  std::cout << std::endl;


  // another test (from http://stackoverflow.com/a/10168569/32683):
  std::cout << (std::string( "Hi" ).length() < -3 ? "yes" : "no") << std::endl;
  // ^ prints "yes" because shit is fucked yo, but at least we get a warning

  return 0;
}
