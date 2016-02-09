#include <fstream>
#include <topcoder>
#include "DigitHoles.cpp"
namespace tc = TopCoder;

int main(int argc, char const *argv[]) {
  try {
    std::ifstream ifs(argv[1]);
    int number; tc::read(ifs, number);
    ifs.close();

    std::ofstream ofs(argv[2]);
    DigitHoles solver;
    tc::show(ofs, solver.numHoles(number));
    ofs.close();
  } catch (std::exception &e) {
    std::cerr << e.what() << std::endl;
  }
  return 0;
}
