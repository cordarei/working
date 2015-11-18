#include <fstream>
#include <iostream>
#include <string>


struct lines_gen {
  lines_gen(std::istream &sin) : sin_{&sin} {}
  std::string operator()() {
    std::getline(*sin_, line_);
    return line_;
  }
  explicit operator bool() { return !!*sin_; }
private:
  std::string line_;
  std::istream *sin_;
};

template <typename F>
auto with_file(std::string filename, F &&f) {
  std::fstream fs{filename};
  return f(fs);
}

int main(int argc, char **argv) {
  if (argc != 2)
    return -1;

  // std::ifstream sin{argv[1]};
  auto x =
  with_file(argv[1], [](auto &&fs) {
      auto lines = lines_gen{fs};
      while (lines)
        std::cout << lines() << "\n";
      return fs.tellg();
    });

  std::cout << "Read: " << x << " bytes\n";
  return 0;
}
