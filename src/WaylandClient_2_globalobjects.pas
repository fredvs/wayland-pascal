// By Fred vS 2023.

program WaylandClient_2_globalobjects;

{$mode objfpc}{$H+}
{$packrecords c}
{$interfaces corba}

uses
  ctypes,
  wayland_client_core,
  wayland_client_wrapper,
  SysUtils;
  
type
  pwl_registry_listener = ^wl_registry_listener;
  wl_registry_listener = record
    global: procedure(data: pointer; registry: pwl_registry; name: LongWord;
                      interface_: PChar; version: LongWord); cdecl;
    global_remove: procedure(data: pointer; registry: pwl_registry; name: LongWord); cdecl;
  end;  

procedure registry_global_handler(data: pointer; registry: pwl_registry;
  name: LongWord; interface_: PChar; version: LongWord); cdecl;
begin
  writeln('interface: ''', interface_, ''', version: ', version, ', name: ', name);
end;

procedure registry_global_remove_handler(data: pointer; registry: pwl_registry;
  name: LongWord); cdecl;
begin
  writeln('removed: ', name);
end;

var
  display: pwl_display;
  registry: pwl_registry;
  registry_listener: wl_registry_listener;

begin
if ww_Load(ExtractFilePath(ParamStr(0)) + '/libwayland_wrapper.so') then
begin

  display := wl_display_connect(nil);
  if display <> nil then
  begin
    registry := wrap_wl_display_get_registry(display);

    registry_listener.global := @registry_global_handler;
    registry_listener.global_remove := @registry_global_remove_handler;

    wrap_wl_registry_add_listener(registry, @registry_listener, nil);

    while True do
    begin
      wl_display_dispatch(display);
    end;
  end
  else
  begin
    writeln('Error connecting ;(');
    ExitCode := 1;
  end;
  ww_Unload();
end else writeln('libwayland_wrapper.so did not load.');   
end.
