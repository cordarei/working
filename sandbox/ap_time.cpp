#include <iostream>
#include <chrono>

using namespace std::literals;

constexpr auto AP_MIN = 5min;

int main()
{
	const auto points = { 5, 10, 20, 30, 40, 125 };

	std::cout << "AP\t\tTime" << std::endl;
	std::cout << "--\t\t----\n";
	for (auto pt : points) {
		auto t_min = AP_MIN * pt;
		auto t_h = std::chrono::duration_cast<std::chrono::hours>(t_min);
		auto t_min_rem = t_min - t_h;
		std::cout
			<< pt 
			<< "\t\t"
			//<< t_min.count()
			//<< "min\t"
			<< t_h.count()
			<< ":"
			<< t_min_rem.count()
			<< std::endl;
	}

	return 0;
}