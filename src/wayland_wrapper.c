// wayland_wrapper.c
// compile with: gcc -fPIC -shared -o libwayland_wrapper.so wayland_wrapper.c -lwayland-client


#include <wayland-client.h>
#include <stdlib.h> 


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

// Wrapper for wl_registry_add_listener


// Wrapper for wl_registry_bind
void *wrap_wl_registry_bind(struct wl_registry *registry,
                               uint32_t name,
                               const struct wl_interface *interface,
                               uint32_t version) {
    return wl_registry_bind(registry, name, interface, version);
}

// Wrapper for wl_compositor_create_surface
struct wl_surface *wrap_wl_compositor_create_surface(struct wl_compositor *compositor) {
    return wl_compositor_create_surface(compositor);
}

// Wrapper for wl_shell_get_shell_surface
struct wl_shell_surface *wrap_wl_shell_get_shell_surface(struct wl_shell *shell, struct wl_surface *surface) {
    return wl_shell_get_shell_surface(shell, surface);
}

// Wrapper for wl_shell_surface_set_toplevel
void wrap_wl_shell_surface_set_toplevel(struct wl_shell_surface *shell_surface) {
    wl_shell_surface_set_toplevel(shell_surface);
}


// Wrapper for wl_shm_create_pool
struct wl_shm_pool *wrap_wl_shm_create_pool(struct wl_shm *shm, int32_t fd, int32_t size) {
    return wl_shm_create_pool(shm, fd, size);
}

// Wrapper for wl_shm_pool_create_buffer
struct wl_buffer *wrap_wl_shm_pool_create_buffer(struct wl_shm_pool *pool,
    int32_t offset, int32_t width, int32_t height, int32_t stride, uint32_t format) {
    return wl_shm_pool_create_buffer(pool, offset, width, height, stride, format);
}

// Wrapper for wl_surface_attach
void wrap_wl_surface_attach(struct wl_surface *surface, struct wl_buffer *buffer, int32_t x, int32_t y) {
    wl_surface_attach(surface, buffer, x, y);
}

// Wrapper for wl_surface_commit
void wrap_wl_surface_commit(struct wl_surface *surface) {
    wl_surface_commit(surface);
}


void wrap_wl_shm_pool_destroy(struct wl_shm_pool* wl_shm_pool_) {
    wl_shm_pool_destroy(wl_shm_pool_);
}
