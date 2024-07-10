#include "Parser.hpp"

// Use tutorials in: https://llvm.org/docs/tutorial/

int main (int argc, char *argv[])
{
    Parser parser;

    if (!parser.Parse()) {
        return 1;
    }

    std::error_code ERR;
    llvm::raw_fd_ostream outLL(argv[2], ERR);

    parser.Generate()->print(outLL, nullptr);

    return 0;
}
