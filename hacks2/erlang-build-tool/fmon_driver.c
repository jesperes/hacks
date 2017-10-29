#ifdef _WIN32
#include <windows.h>
#endif

#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#ifdef unix
#include <pthread.h>
#endif

#ifdef _WIN32
void win32_watch_directory(char *d) 
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
      
      FILE_NOTIFY_INFORMATION fni;
      DWORD bytes_read;

      result = ReadDirectoryChangesW(handles[0], &fni, sizeof(fni), 
                                     TRUE, -1, &bytes_read, 
                                     NULL, NULL);

      assert(result != 0);

      FindNextChangeNotification(handles[0]);
      break;
    }
  }
}
#endif

void *stdin_reader(void *arg)
{
  return NULL;
}

int main(int argc, char *argv[]) 
{
  if (argc != 2) {
    printf("Usage: %s <dir>\n", argv[0]);
  }

#ifdef _WIN32  
  win32_watch_directory(argv[1]);
#endif
  return 0;
}

