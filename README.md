Brief description
-----------------

Clang-based standalone tool that finds string literals in c/c++ source
and converts it to ascii

Building
------------------

1. Clone the repository to 'llvm/tools/clang/tools/extra'.
2. Add 'add_subdirectory(ebcdic2ascii)' to 'CmakeLists.txt' in the same directory.
3. Run 'ninja ebcdic2ascii' inside your LLVM's 'build/' directory.
