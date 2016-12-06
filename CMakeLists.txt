set (LLVM_LINK_COMPONENTS support)

add_clang_executable(ebcdic2ascii
  ConvertEbcdicToAscii.cpp
  )

target_link_libraries(ebcdic2ascii
  clangTooling
  clangBasic
  clangASTMatchers
  )
