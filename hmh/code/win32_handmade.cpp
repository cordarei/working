#include <windows.h>

int CALLBACK
WinMain(HINSTANCE hInstance,
        HINSTANCE hPrevInstance,
        LPSTR lpCmdLine,
        int nCmdShow)
{
  MessageBoxA(NULL, "Hello, world.", "HMH", MB_OK|MB_ICONINFORMATION);
  return 0;
}
