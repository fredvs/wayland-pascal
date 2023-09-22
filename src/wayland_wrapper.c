// wayland_wrapper.c
// compile with: gcc -fPIC -shared -o libwayland_wrapper.so wayland_wrapper.c -lwayland-client


#include <wayland-client.h>

// Wrapper for wl_display_get_registry
struct wl_registry* wrap_wl_display_get_registry(struct wl_display* display) {
    return wl_display_get_registry(display);
}

// Wrapper for wl_registry_add_listener
int wrap_wl_registry_add_listener(struct wl_registry* registry,
                                const struct wl_registry_listener* listener,
                                void* data) {
    return wl_registry_add_listener(registry, listener, data);
}


