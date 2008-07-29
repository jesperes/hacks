#include <windows.h>
#include <stdlib.h>
#include <stdio.h>
#include <assert.h>

void watch_directory(char *d) 
{
  HANDLE handles[1];

  handles[0] = FindFirstChangeNotification(d, TRUE, 
                                           FILE_NOTIFY_CHANGE_LAST_WRITE);

  if (handles[0] == INVALID_HANDLE_VALUE) {
    printf("Failed to set change notification on %s.\n", d);
    return;
  }

  assert(handles[0] != NULL);

  while (TRUE) {
    int result = WaitForMultipleObjects(1, handles, FALSE, INFINITE);
    switch (result) {
    case WAIT_OBJECT_0:
      printf("Directory '%s' changed.\n", d);
      FindNextChangeNotification(handles[0]);
      break;
    }
  }
}

int main(int argc, char *argv[]) 
{
  if (argc != 2) {
    printf("Usage: %s <dir>\n", argv[0]);
  }

  watch_directory(argv[1]);
  return 0;
}

