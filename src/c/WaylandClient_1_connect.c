/*
   From https://wayland-book.com/xdg-shell-basics/example-code.html
   Compile with: 
   $ gcc -o connect WaylandClient_1_connect.c -lwayland-client
*/
#include <stdio.h>
#include <wayland-client.h>

int main(void)
{
    struct wl_display *display = wl_display_connect(NULL);
    if (display) {
        printf("Connected!\n");
    } else {
        printf("Error connecting ;(\n");
        return 1;
    }

    wl_display_disconnect(display);
    return 0;
}


