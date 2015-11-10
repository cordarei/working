#include <windows.h>

static bool running;

static BITMAPINFO bitmap_info;
static HBITMAP bitmap;
static void *bitmap_mem;
void W32ResizeDIBSection(int width, int height)
{
  if (bitmap) {
    DeleteObject(bitmap);
  }

  bitmap_info.bmiHeader.biSize = sizeof(bitmap_info.bmiHeader);
  bitmap_info.bmiHeader.biWidth = width;
  bitmap_info.bmiHeader.biHeight = height;
  bitmap_info.bmiHeader.biPlanes = 1;
  bitmap_info.bmiHeader.biBitCount = 32;
  bitmap_info.bmiHeader.biCompression = BI_RGB;

  auto dc = CreateCompatibleDC(0);

  void *mem = nullptr;
  bitmap = CreateDIBSection(dc,
                            &bitmap_info,
                            DIB_RGB_COLORS,
                            &mem,
                            0,
                            0);
  if (mem)
    bitmap_mem = mem;
}

void W32UpdateWindow(HDC dc, int x, int y, int w, int h)
{
  StretchDIBits(dc,
                x, y, w, h,
                x, y, w, h,
                bitmap_mem,
                &bitmap_info,
                DIB_RGB_COLORS,
                SRCCOPY);
}

LRESULT CALLBACK
MainWindowCallback(HWND hwnd,
                   UINT msg,
                   WPARAM wParam,
                   LPARAM lParam)
{
  LRESULT result = 0;
  switch (msg) {
  case WM_SIZE:
    {
      RECT rect;
      GetClientRect(hwnd, &rect);
      auto w = rect.right - rect.left;
      auto h = rect.bottom - rect.top;
      W32ResizeDIBSection(w, h);
    }
    break;
  case WM_CLOSE:
  case WM_DESTROY:
    running = false;
    break;
  case WM_PAINT:
    {
      auto ps = PAINTSTRUCT{};
      auto dc = BeginPaint(hwnd, &ps);
      auto x = ps.rcPaint.left;
      auto y = ps.rcPaint.top;
      auto w = ps.rcPaint.right - ps.rcPaint.left;
      auto h = ps.rcPaint.bottom - ps.rcPaint.top;
      W32UpdateWindow(dc, x, y, w, h);
      EndPaint(hwnd, &ps);
    }
    break;
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

  running = true;
  MSG msg;
  while(running) {
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
