#!/bin/sh

# #include <iostream>
# 
# class FooBar {
#     int asdf;
# };
# 
# int main(int argc, const char * argv[]) {
#     FooBaz foobar;
#     return 0;
# }

# $ cat ~/dev/cpp/sample/sample/main.cpp | gcc -Wall -Wextra -x c++ -
# <stdin>: In function ‘int main(int, const char**)’:
# <stdin>:8:5: error: ‘FooBaz’ was not declared in this scope
# <stdin>:8:5: note: suggested alternative: ‘FooBar’
# <stdin>:7:14: warning: unused parameter ‘argc’ [-Wunused-parameter]
# <stdin>:7:38: warning: unused parameter ‘argv’ [-Wunused-parameter]

echo "<stdin>: In function ‘int main(int, const char**)’:"
echo "<stdin>:8:5: error: ‘FooBaz’ was not declared in this scope"
echo "<stdin>:8:5: note: suggested alternative: ‘FooBar’"
echo "<stdin>:7:14: warning: unused parameter ‘argc’ [-Wunused-parameter]"
echo "<stdin>:7:38: warning: unused parameter ‘argv’ [-Wunused-parameter]"
