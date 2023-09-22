# wayland-pascal
Demos of wayland client using fpc Pascal compiler.

It uses the excellent fpc-wayland Bindings Generator for freepascal from Andrews Haines.
https://github.com/andrewd207/fpc-wayland

The demos are inspired by C code of Sergey Bugaev from here:
https://bugaevc.gitbooks.io/writing-wayland-clients/

To compile-link the demos, you will need to create a library.
The code  is in /src/wayland_wrapper.c and instruction how to compile is in begin of code.
Copy that libwayland_wrapper.so in your libraries directory. That library is only needed for linking.
