// #include <string>

void dostuff(int &n) {
  n = 0;
  for (int i = 0; i < 10; ++i)
    n += i;
  n /= 10;
}

extern "C" {
  void cc_main() {
    int n=0;
    dostuff(n);

    // const std::string s = "Hello World!";
  }
}

