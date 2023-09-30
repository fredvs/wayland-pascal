// By Fred vS 2023.

program WaylandClient_2_globalobjects;

{$mode objfpc}{$H+}
{$packrecords c}
{$interfaces corba}

uses
  ctypes,
  Classes,
  wayland_client_core,
  SysUtils;

type
  pwl_registry_listener = ^wl_registry_listener;

  wl_registry_listener = record
    global: procedure(Data: Pointer; registry: pwl_registry; Name: longword; interface_: PChar; version: longword); cdecl;
    global_remove: procedure(Data: Pointer; registry: pwl_registry; Name: longword); cdecl;
  end;

  procedure registry_global_handler(Data: Pointer; registry: pwl_registry; Name: longword; interface_: PChar; version: longword); cdecl;
  begin
    writeln('interface: ''', interface_, ''', version: ', version, ', name: ', Name);
  end;

  procedure registry_global_remove_handler(Data: Pointer; registry: pwl_registry; Name: longword); cdecl;
  begin
    writeln('removed: ', Name);
  end;

var
  display: pwl_display;
  registry: pwl_registry;
  registry_listener: wl_registry_listener;
  Sres: TResourceStream;  // for resource library.
  Fres: TFileStream;
  dirlib: string;

begin
    display := wl_display_connect(nil);
    if display <> nil then
    begin
      registry := wl_display_get_registry(display);

      registry_listener.global        := @registry_global_handler;
      registry_listener.global_remove := @registry_global_remove_handler;

      wl_registry_add_listener(registry, @registry_listener, nil);

      while True do
        wl_display_dispatch(display);
    end
    else
    begin
      writeln('Error connecting ;(');
      ExitCode := 1;
    end;
  end.

