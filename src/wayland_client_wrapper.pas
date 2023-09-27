// Dynamic loading of Wrapped methods by Fred vS 2023.

unit wayland_client_wrapper;

{$mode objfpc}{$H+}
{$packrecords c}

interface

uses
  dynlibs,
  CTypes,
  wayland_client_core,
  wayland_util;

const
  libww =
    'libwayland_wrapper.so';

var

  wrap_wl_display_get_registry: function(wl_display_: pwl_display): pwl_registry; cdecl;

  wrap_wl_registry_add_listener: function(wl_registry: pwl_registry; const listener: Pwl_registry_listener; Data: Pointer): cint; cdecl;

  wrap_wl_registry_bind: function(registry: Pwl_registry; Name: longword; interface_: Pwl_interface; version: longword): Pointer; cdecl;

  wrap_wl_compositor_create_surface: function(compositor: Pwl_compositor): Pwl_surface; cdecl;

  wrap_wl_shell_get_shell_surface: function(shell: Pwl_shell; surface: Pwl_surface): Pwl_shell_surface; cdecl;

  wrap_wl_shell_surface_set_toplevel: procedure(shell_surface: Pwl_shell_surface); cdecl;

  wrap_wl_shm_create_pool: function(shm: Pwl_shm; fd: integer; size: integer): Pwl_shm_pool; cdecl;

  wrap_wl_shm_pool_create_buffer: function(pool: Pwl_shm_pool; offset, Width, Height, stride, format: integer): Pwl_buffer; cdecl;

  wrap_wl_surface_attach: procedure(surface: Pwl_surface; buffer: Pwl_buffer; x, y: integer); cdecl;

  wrap_wl_surface_commit: procedure(surface: Pwl_surface); cdecl;

  wrap_wl_shm_pool_destroy: procedure(wl_shm_pool: Pwl_shm_pool); cdecl;

{Special function for dynamic loading of lib ...}

var
  ww_Handle: TLibHandle = dynlibs.NilHandle; 
  ReferenceCounter: cardinal = 0;  

function ww_IsLoaded: Boolean; inline;

function ww_Load(const libfilename: string): Boolean; // load the lib

procedure ww_Unload(); // unload and frees the lib from memory : do not forget to call it before close application.

implementation

function ww_IsLoaded: Boolean;
begin
  Result := (ww_Handle <> dynlibs.NilHandle);
end;

function ww_Load(const libfilename: string): Boolean;
var
  thelib: string;
begin
  Result := False;
  if ww_Handle <> 0 then
  begin
    Inc(ReferenceCounter);
    Result := True; {is it already there ?}
  end
  else
  begin {go & load the library}
    if Length(libfilename) = 0 then
      thelib := libww
    else
      thelib := libfilename;
    ww_Handle := DynLibs.SafeLoadLibrary(thelib); // obtain the handle we want
    if ww_Handle <> DynLibs.NilHandle then
    begin {now we tie the functions to the VARs from above}

      Pointer(wrap_wl_display_get_registry)    := DynLibs.GetProcedureAddress(ww_Handle, PChar('wrap_wl_display_get_registry'));
      Pointer(wrap_wl_registry_add_listener)   := DynLibs.GetProcedureAddress(ww_Handle, PChar('wrap_wl_registry_add_listener'));
      Pointer(wrap_wl_registry_bind)           := DynLibs.GetProcedureAddress(ww_Handle, PChar('wrap_wl_registry_bind'));
      Pointer(wrap_wl_compositor_create_surface) := DynLibs.GetProcedureAddress(ww_Handle, PChar('wrap_wl_compositor_create_surface'));
      Pointer(wrap_wl_shell_get_shell_surface) := DynLibs.GetProcedureAddress(ww_Handle, PChar('wrap_wl_shell_get_shell_surface'));
      Pointer(wrap_wl_shell_surface_set_toplevel) := DynLibs.GetProcedureAddress(ww_Handle, PChar('wrap_wl_shell_surface_set_toplevel'));
      Pointer(wrap_wl_shm_create_pool)         := DynLibs.GetProcedureAddress(ww_Handle, PChar('wrap_wl_shm_create_pool'));
      Pointer(wrap_wl_shm_pool_create_buffer)  := DynLibs.GetProcedureAddress(ww_Handle, PChar('wrap_wl_shm_pool_create_buffer'));
      Pointer(wrap_wl_surface_attach)          := DynLibs.GetProcedureAddress(ww_Handle, PChar('wrap_wl_surface_attach'));
      Pointer(wrap_wl_surface_commit)          := DynLibs.GetProcedureAddress(ww_Handle, PChar('wrap_wl_surface_commit'));
      Pointer(wrap_wl_shm_pool_destroy)        := DynLibs.GetProcedureAddress(ww_Handle, PChar('wrap_wl_shm_pool_destroy'));

    end;
    Result           := ww_IsLoaded;
    ReferenceCounter := 1;
  end;

end;

procedure ww_Unload;
begin
  // < Reference counting
  if ReferenceCounter > 0 then
    Dec(ReferenceCounter);
  if ReferenceCounter > 0 then
    Exit;
  // >
  if ww_IsLoaded then
  begin
    DynLibs.UnloadLibrary(ww_Handle);
    ww_Handle := DynLibs.NilHandle;
  end;
end;

end.

 