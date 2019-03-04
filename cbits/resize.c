#include <sys/ioctl.h>
#include <string.h>
#include "resize.h"

int resize(int fd, unsigned short x, unsigned short y){
    struct winsize ws;

    /* Set the terminal size and settings. */
    memset(&ws, 0, sizeof ws);
    ws.ws_col = x;
    ws.ws_row = y;

    return ioctl(fd, TIOCSWINSZ, &ws);
}
