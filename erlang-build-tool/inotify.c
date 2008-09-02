#include <stdio.h>
#include <assert.h>
#include <unistd.h>
#include <stdlib.h>

#include <sys/inotify.h>

int main() 
{
  int fd = inotify_init();
  assert(fd != -1);

  int watch_fd = inotify_add_watch(fd, "/etc/", IN_ACCESS);
  assert(watch_fd != -1);

  printf("Added watch: %d\n", watch_fd);
  
  struct inotify_event event;
  ssize_t count;

  do {
    count = read(fd, &event, 4096);
    if (count == -1) {
      perror("read");
      exit(1);
    }

    if (count > 0) {
      printf("name = %s\n", event.name);
    }
  } while (count > 0);
  
  return 0;
}
