#include <windows.h>


LRESULT CALLBACK
MainWindowCallback(HWND hwnd,
                   UINT msg,
                   WPARAM wParam,
                   LPARAM lParam)
{
  LRESULT result = 0;
  switch (msg) {
  case WM_PAINT:
    {
      auto ps = PAINTSTRUCT{};
      auto dc = BeginPaint(hwnd, &ps);
      auto x = ps.rcPaint.left;
      auto y = ps.rcPaint.top;
      auto w = ps.rcPaint.right - ps.rcPaint.left;
      auto h = ps.rcPaint.bottom - ps.rcPaint.top;
      static DWORD op = WHITENESS;
      PatBlt(dc, x, y, w, h, op);
      if (op == WHITENESS)
        op = BLACKNESS;
      else
        op = WHITENESS;
      EndPaint(hwnd, &ps);
    }
  default:
    result = DefWindowProc(hwnd, msg, wParam, lParam);
  }
  return result;
}

int CALLBACK
WinMain(HINSTANCE app_instance,
        HINSTANCE /*hPrevInstance*/,
        LPSTR /*lpCmdLine*/,
        int /*nCmdShow*/)
{
  WNDCLASS wc = {};
  wc.style = CS_OWNDC|CS_HREDRAW|CS_VREDRAW;
  wc.lpfnWndProc = MainWindowCallback;
  wc.hInstance = app_instance;
  // wc.hIcon = ;
  wc.lpszClassName = "HandmadeHeroWindowClass";

  auto result = RegisterClass(&wc);
  auto hwnd = CreateWindowEx(
                             0,
                             wc.lpszClassName,
                             "Handmade Hero",
                             WS_OVERLAPPEDWINDOW|WS_VISIBLE,
                             CW_USEDEFAULT,
                             CW_USEDEFAULT,
                             CW_USEDEFAULT,
                             CW_USEDEFAULT,
                             0,
                             0,
                             app_instance,
                             0);

  MSG msg;
  for(;;) {
    auto result = GetMessage(&msg, 0, 0, 0);
    if (result > 0) {
      TranslateMessage(&msg);
      DispatchMessage(&msg);
    } else {
      break;
    }
  }
  
  return 0;
}
