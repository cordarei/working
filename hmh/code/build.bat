@call ..\misc\shell.bat

mkdir ..\build
pushd ..\build
cl /Zi ..\code\win32_handmade.cpp User32.lib
popd
