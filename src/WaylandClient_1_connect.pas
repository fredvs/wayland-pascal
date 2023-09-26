// By Fred vS 2023.

program WaylandClient_1_connect;

{$mode objfpc}{$H+}
{$packrecords c}
{$interfaces corba}

uses
  ctypes,
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
