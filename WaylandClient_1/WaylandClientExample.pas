
program WaylandClientExample;

{$mode objfpc}

uses
  ctypes,
  wayland_client,
  wayland_client_core,
  SysUtils;

var
  display: pwl_display;

begin
  display := wl_display_connect(nil);
  if display <> nil then
  begin
    writeln('Connected!');
  end
  else
  begin
    writeln('Error connecting ;(');
    ExitCode := 1;
    Exit;
  end;

  wl_display_disconnect(display);
  ExitCode := 0;
end.

