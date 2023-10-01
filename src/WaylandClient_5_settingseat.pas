// By Fred vS 2023.

program WaylandClient_5_settingseat;

{$mode objfpc}{$H+}
{$packrecords c}
{$interfaces corba}

uses
  cmem,
  Classes,
  BaseUnix,
  wayland_client_core,
  wayland_protocol,
  xdg_shell_protocol,
  SysUtils;

var
  display: Pwl_display;
  registry: Pwl_registry;
  shm: Pwl_shm;
  compositor: Pwl_compositor;
  xdg_wm_base: Pxdg_wm_base;
  seat: Pwl_seat;
  surface: Pwl_surface;
  xdg_surface: Pxdg_surface;
  xdg_toplevel: Pxdg_toplevel;
  keyboard: Pwl_keyboard;
  touch: Pwl_touch;
  
  xdg_wm_base_listener: Txdg_wm_base_listener;
  wl_surface_frame_listener: Twl_callback_listener;
  
  offset: cfloat = 0;
  last_frame: longword;

  pinginc: integer = 0;
  
  procedure wl_seat_capabilities(data: Pointer; wl_seat: Pwl_seat; capabilities: LongWord); cdecl;
 // var
 // state: Pclient_state;
begin
 // state := Pclient_state(data);
  // TODO: Implement your logic for seat capabilities
end;

procedure wl_seat_name(data: Pointer; wl_seat: Pwl_seat; name: PAnsiChar); cdecl;
begin
  WriteLn('This is the call --> seat name: ', name);
end;

const
  wl_seat_listener: Twl_seat_listener = (
    capabilities: @wl_seat_capabilities;
    name: @wl_seat_name;
  );
  

  function draw_frame(state: Pwl_display): Pwl_buffer;
  cdecl;
  var
    size, stride, fd, x, y: integer;
    s: string = '*';
    Data: ^longword;
    pool: Pwl_shm_pool;
    buffer: Pwl_buffer;
    Width, Height: integer;
  begin

    //writeln('draw_frame init');

    Width  := 400;
    Height := 400;

    stride := Width * 4;
    // Adjust based on pixel format
    size   := stride * Height;

    //writeln('size = ' + inttostr(size));
    
    fd := CreateAnonymousFile(size);

   if fd = -1 then
    begin
      writeln('allocate_shm_file = BAD');
      Exit(nil);
      // Return nil in case of failure
    end;

      // writeln('before FpMmap') ; 

    Data := (FpMmap(nil, size + 1, PROT_READ or PROT_WRITE, MAP_SHARED, fd, 0));

    if Data = MAP_FAILED then
    begin
      writeln('data = MAP_FAILED');
      fpclose(fd);
      Exit(nil);
      // Return nil in case of failure
    end;
      //else  writeln('data = OK');

      // writeln('before pool') ; 
    pool := nil;

    pool := wl_shm_create_pool(shm, fd, size);

    // if pool = nil then writeln('wl_shm_create_pool = NOT OK') else writeln('wl_shm_create_pool = OK');

    buffer := wl_shm_pool_create_buffer(pool, 0, Width, Height, stride, WL_SHM_FORMAT_XRGB8888);

    //if buffer = nil then writeln('wl_shm_pool_create_buffer = NOT OK') else writeln('wl_shm_pool_create_buffer = OK');

    wl_shm_pool_destroy(pool);

    fpclose(fd);

    { Draw chessboard background }

    // writeln('Offset = ' + floattostr(offset));

    for y := 0 to Height - 1 do
      for x := 0 to Width - 1 do
        if ((x + trunc(offset)) div 50 + (y + trunc(offset)) div 50) mod 2 = 0 then

          Data[y * Width + x] := $FF834555 // Set pixel color
        else
          Data[y * Width + x] := $FFEEEEEE;

    FpMunmap(Data, size);

    wl_surface_attach(surface, buffer, 0, 0);
    wl_surface_commit(surface);
    // writeln('done draw');
    Result := buffer;
  end;

  // Surface

  function xdg_wm_base_get_xdg_surface(xdg_wm_base: Pxdg_wm_base; surface: Pwl_surface): Pxdg_surface; cdecl;
  var
    id: Pwl_proxy;
    inte: integer;
  begin
    id := nil;

    inte := wl_proxy_get_version(Pwl_proxy(xdg_wm_base));
    writeln('xdg_wm_base version ' + IntToStr(inte));

    id := wl_proxy_marshal_flags_get_xdg_surface(
      Pwl_proxy(xdg_wm_base),
      XDG_WM_BASE_GET_XDG_SURFACE_, @xdg_surface_interface,
      wl_proxy_get_version(Pwl_proxy(xdg_wm_base)),
      0,
      nil,
      surface
      );

    Result := Pxdg_surface(id);

    if Result = nil then
      writeln('Pxdg_surface NOT OK ')
    else
      writeln('Pxdg_surface OK ');
  end;


  procedure wl_surface_frame_callback(Data: Pointer; cb: Pwl_callback; time: longword); cdecl;
  var
    //state: Pclient_state;
    elapsed: integer;
    buffer: Pwl_buffer;
  begin

    // writeln('Yep callback');

    { Destroy this callback }
    wl_callback_destroy(cb);

    { Request another frame }
    cb := wl_surface_frame(surface);
    wl_callback_add_listener(cb, @wl_surface_frame_listener, Data);

    { Update scroll amount at 24 pixels per second }
    if (last_frame <> 0) then
    begin
      elapsed := time - last_frame;
      offset  := offset + elapsed / 1000 * 24;
    end;

    { Submit a frame for this event }
    buffer := draw_frame(display);
    wl_surface_attach(surface, buffer, 0, 0);
    wl_surface_damage_buffer(surface, 0, 0, MaxInt, MaxInt);
    wl_surface_commit(surface);

    last_frame := time;

  end;


  procedure xdg_surface_ack_configure(xdg_surface: Pxdg_surface; serial: DWord); cdecl;
  begin
    // if xdg_surface <> nil then writeln('xdg_surface OK') else
    //  writeln('xdg_surface NOT OK') ;

    // if Pwl_proxy(xdg_surface) <> nil then writeln('Pwl_proxy(xdg_surface) OK') else
    //  writeln('Pwl_proxy(xdg_surface) NOT OK') ;

    wl_proxy_marshal_flags_ack_configure(
      Pwl_proxy(xdg_surface),
      XDG_SURFACE_ACK_CONFIGURE_,
      nil,
      wl_proxy_get_version(Pwl_proxy(xdg_surface)),
      0,
      serial);
  end;

  procedure xdg_surface_configure(Data: Pointer; xdg_surface: Pxdg_surface; serial: cuint); cdecl;
  var
    state: Pwl_display = nil;
    buffer: Pwl_buffer = nil;
  begin
    state := Pwl_display(Data);

    //  if state <> nil then writeln('state OK') else
    //  writeln('state NOT OK') ;

    // if xdg_surface <> nil then writeln('xdg_surface OK') else
    //writeln('xdg_surface NOT OK') ;

    //writeln('serial = ' + inttostr(serial)) ;

    xdg_surface_ack_configure(xdg_surface, serial);

    buffer := draw_frame(state);

    if buffer <> nil then
    begin
      wl_surface_attach(surface, buffer, 0, 0);
      wl_surface_commit(surface);
      //writeln('buffer xdg_surface_configure OK') ;
    end
    else
      writeln('buffer xdg_surface_configure NOT OK');
  end;

  function xdg_surface_add_listener(xdg_surface: Pxdg_surface; listener: Pxdg_surface_listener; Data: Pointer): longint; cdecl;
  begin
    Result := wl_proxy_add_listener(Pwl_proxy(xdg_surface), Pointer(listener), Data);
    writeln('xdg_surface_add_listener = ' + IntToStr(Result));
  end;

  // xdg_wm_base

  procedure xdg_wm_base_ping(Data: Pointer; xdg_wm_base: Pxdg_wm_base; serial: cuint); cdecl;
  begin
    wl_proxy_marshal_flags_ping(
      Pwl_proxy(xdg_wm_base),
      XDG_WM_BASE_PONG_,
      nil,
      wl_proxy_get_version(Pwl_proxy(xdg_wm_base)),
      0,
      serial
      );

    Inc(pinginc);

    writeln(IntToStr(pinginc) + ' ping done of 50');
  end;

  procedure xdg_wm_base_add_listener(xdg_wm_base: Pxdg_wm_base; listener: Pxdg_wm_base_listener; Data: Pointer); cdecl;
  var
    resu: integer;
  begin
    if Pwl_proxy(xdg_wm_base) = nil then
      writeln('Pwl_proxy(xdg_wm_base) = nil')
    else
      writeln('Pwl_proxy(xdg_wm_base) = ok');

    if listener^.ping <> nil then
    begin
      resu := wl_proxy_add_listener(Pwl_proxy(xdg_wm_base), Pointer(listener), Data);
      writeln('xdg_wm_base_add_listener = ' + IntToStr(resu));
    end
    else
      writeln('@listener.ping = nil');
  end;

  procedure registry_global(Data: Pointer; wl_registry: Pwl_registry; Name: cuint; iface: PChar; version: cuint); cdecl;
  var
    state: Pwl_display;
  begin
    state := Pwl_display(Data);

    //writeln('registry_global init');

    //writeln('version = ' + inttostr(version));

    if AnsiCompareStr(iface, wl_shm_interface.Name) = 0 then
      shm        := wl_registry_bind(wl_registry, Name, @wl_shm_interface, 1)
    else if AnsiCompareStr(iface, wl_compositor_interface.Name) = 0 then
      compositor := wl_registry_bind(wl_registry, Name, @wl_compositor_interface, 4)

    else if AnsiCompareStr(iface, xdg_wm_base_interface.Name) = 0 then
    begin
      xdg_wm_base := wl_registry_bind(wl_registry, Name, @xdg_wm_base_interface, 1);

      // Initialize listener
      xdg_wm_base_listener.ping := @xdg_wm_base_ping;

      // test it.
      // xdg_wm_base_listener.ping(data, pointer(xdg_wm_base), version)  ;

      xdg_wm_base_add_listener(xdg_wm_base, @xdg_wm_base_listener, state);

    end else if AnsiCompareStr(iface, wl_seat_interface.name) = 0 then
    begin
    seat := wl_registry_bind(wl_registry, name, @wl_seat_interface, 7);
    wl_seat_add_listener(seat,   @wl_seat_listener, state);
    end;

  end;

  function xdg_surface_get_toplevel(xdg_surface: Pxdg_surface): Pxdg_toplevel; cdecl;
  begin
    Result := nil;
    Result := Pxdg_toplevel(
      wl_proxy_marshal_flags_get_toplevel(
      Pwl_proxy(xdg_surface),
      XDG_SURFACE_GET_TOPLEVEL_, @xdg_toplevel_interface,
      wl_proxy_get_version(Pwl_proxy(xdg_surface)),
      0,
      nil));

    if Result = nil then
      writeln('xdg_surface_get_toplevel = nil')
    else
      writeln('xdg_surface_get_toplevel = ok');

  end;

  procedure xdg_toplevel_set_title(xdg_toplevel: Pxdg_toplevel; title: PChar); cdecl;
  begin
    wl_proxy_marshal_flags_set_title(
      Pwl_proxy(xdg_toplevel),
      XDG_TOPLEVEL_SET_TITLE_,
      nil,
      wl_proxy_get_version(Pwl_proxy(xdg_toplevel)),
      0,
      title
      );
  end;

  procedure registry_global_remove(Data: Pointer; wl_registry: Pwl_registry; Name: cuint); cdecl;
  begin
    { This space deliberately left blank }
  end;

var
  listener: Twl_registry_listener;
  xdg_surface_listener: Txdg_surface_listener;
  cb: Pwl_callback;

begin
  { Initialize Wayland objects }
  shm         := nil;
  compositor  := nil;
  xdg_wm_base := nil;
  surface     := nil;

  { Connect to the Wayland display }
  display := wl_display_connect(nil);
  if display <> nil then
    writeln('Display connected')
  else
    writeln('Display not connected');

  registry := wl_display_get_registry(display);
  if registry <> nil then
    writeln('registry connected')
  else
    writeln('registry not connected');

  listener.global := @registry_global;
  writeln('listener.global');

  listener.global_remove := @registry_global_remove;
  writeln('listener.global_remove');

  wl_registry_add_listener(registry, @listener, display);
  writeln('wl_registry_add_listener');

  wl_display_roundtrip(display);
  writeln('wl_display_roundtrip');

  surface := wl_compositor_create_surface(compositor);
  if surface <> nil then
    writeln('surface connected')
  else
    writeln('surface not connected');

  xdg_surface := xdg_wm_base_get_xdg_surface(xdg_wm_base, surface);
  if xdg_surface <> nil then
    writeln('xdg_surface connected')
  else
    writeln('xdg_surface not connected');

  xdg_surface_listener.configure := @xdg_surface_configure;
  writeln('xdg_surface_listener.configure');

  xdg_surface_add_listener(xdg_surface, @xdg_surface_listener, display);
  writeln('xdg_surface_add_listener');

  xdg_toplevel := xdg_surface_get_toplevel(xdg_surface);
  if xdg_toplevel <> nil then
    writeln('xdg_toplevel connected')
  else
    writeln('xdg_toplevel not connected');

  xdg_toplevel_set_title(xdg_toplevel, 'Example client');
  writeln('xdg_toplevel_set_title');

  wl_surface_commit(surface);
  writeln('wl_surface_commit');

  cb := wl_surface_frame(surface);

  wl_surface_frame_listener.done := @wl_surface_frame_callback;

  wl_callback_add_listener(cb, @wl_surface_frame_listener, display);

  writeln('wl_callback_add_listener');

  writeln('>>>>>>>>>>>>> init loop');

  while (wl_display_dispatch(display) <> -1) and (pinginc < 50) do
    { This space deliberately left blank };

  wl_display_disconnect(display);

  writeln();
  writeln('wl_display_disconnect');

  writeln();
  writeln('Bye!');

end.

