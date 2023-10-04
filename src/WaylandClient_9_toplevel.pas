// By Fred vS 2023.

program WaylandClient_9_toplevel;

{$mode objfpc}{$H+}
{$packrecords c}
{$interfaces corba}

uses
  cmem,
  Classes,
  BaseUnix,
  wayland_client_core,
  wayland_protocol,
  wayland_util,
  xdg_shell_protocol,
  libxkbcommon,
  SysUtils;

type
  TPointerEventMask = (
    POINTER_EVENT_ENTER = 1 shl 0,
    POINTER_EVENT_LEAVE = 1 shl 1,
    POINTER_EVENT_MOTION = 1 shl 2,
    POINTER_EVENT_BUTTON = 1 shl 3,
    POINTER_EVENT_AXIS = 1 shl 4,
    POINTER_EVENT_AXIS_SOURCE = 1 shl 5,
    POINTER_EVENT_AXIS_STOP = 1 shl 6,
    POINTER_EVENT_AXIS_DISCRETE = 1 shl 7
    );

  TPointerEventAxes = record
    valid: Boolean;
    Value: integer; // Assuming wl_fixed_t is represented as int
    discrete: Int32;
  end;

type
  PointerAxes = record
    valid: Boolean;
    Value: Int32;
    discrete: Int32;
  end;

  PPointerEvent = ^TPointerEvent;

  TPointerEvent = record
    event_mask: set of TPointerEventMask;
    surface_x, surface_y: Int32;
    button, state, time, serial, axis_source: UInt32;
    axes: array [0..1] of PointerAxes;
  end;

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
  wlpointer: Pwl_pointer;
  touch: Pwl_touch;

  xdg_wm_base_listener: Txdg_wm_base_listener;
  wl_surface_frame_listener: Twl_callback_listener;
  wl_pointer_listener: Twl_pointer_listener;
  wl_keyboard_listener: Twl_keyboard_listener;

  offset: cfloat = 0;
  last_frame: longword;
  Width_, Height_: integer;
  closed: Boolean;
  pointer_event: TPointerEvent;
  xkbstate: Pxkb_state;
  xkbcontext: Pxkb_context;
  xkbkeymap: Pxkb_keymap;

  pinginc: integer = 0;

  procedure wl_pointer_enter(Data: Pointer; wl_pointer: Pwl_pointer; serial: DWord; surface: Pwl_surface; surface_x, surface_y: longint); cdecl;
  begin
    Include(pointer_event.event_mask, POINTER_EVENT_ENTER);
    pointer_event.serial    := serial;
    pointer_event.surface_x := surface_x;
    pointer_event.surface_y := surface_y;
  end;

  procedure wl_pointer_leave(Data: Pointer; wl_pointer: Pwl_pointer; serial: UInt32; surface: Pwl_surface); cdecl;
  begin
    // writeln('wl_pointer_leave');
    pointer_event.serial := serial;
    Include(pointer_event.event_mask, POINTER_EVENT_LEAVE);
  end;

  procedure wl_pointer_motion(Data: Pointer; wl_pointer: Pwl_pointer; time: UInt32; surface_x, surface_y: integer); cdecl;
  begin
    // writeln('wl_pointer_motion');
    Include(pointer_event.event_mask, POINTER_EVENT_MOTION);
    pointer_event.time      := time;
    pointer_event.surface_x := surface_x;
    pointer_event.surface_y := surface_y;
  end;

  procedure wl_pointer_button(Data: Pointer; wl_pointer: Pwl_pointer; serial, time, button, state: UInt32); cdecl;
  begin
    // writeln('wl_pointer_button');
    Include(pointer_event.event_mask, POINTER_EVENT_BUTTON);
    pointer_event.time   := time;
    pointer_event.serial := serial;
    pointer_event.button := button;
    pointer_event.state  := state;
  end;

  procedure wl_pointer_axis(Data: Pointer; wl_pointer: Pwl_pointer; time, axis: UInt32; Value: integer); cdecl;
  begin
    Include(pointer_event.event_mask, POINTER_EVENT_AXIS);
    pointer_event.time := time;
    pointer_event.axes[axis].valid := True;
    pointer_event.axes[axis].Value := Value;
  end;

  procedure wl_pointer_axis_source(Data: Pointer; wl_pointer: Pwl_pointer; axis_source: UInt32); cdecl;
  begin
    Include(pointer_event.event_mask, POINTER_EVENT_AXIS_SOURCE);
    pointer_event.axis_source := axis_source;
  end;

  procedure wl_pointer_axis_stop(Data: Pointer; wl_pointer: Pwl_pointer; time, axis: UInt32); cdecl;
  begin

    pointer_event.time := time;
    Include(pointer_event.event_mask, POINTER_EVENT_AXIS_STOP);
    pointer_event.axes[axis].valid := True;

  end;

  procedure wl_pointer_frame(Data: Pointer; wl_pointer: Pwl_pointer); cdecl;
  var
    event: PPointerEvent;
    i: integer;
    axisEvents: set of TPointerEventMask;
    axisName: array [0..1] of string;
    axisSource: array [0..3] of string;
  begin
    event := @pointer_event;

    WriteLn('pointer frame @ ', event^.time, ': ');

    if POINTER_EVENT_ENTER in event^.event_mask then
      WriteLn('entered ', FloatToStrF(wl_fixed_to_double(event^.surface_x), ffGeneral, 8, 4), ', ',
        FloatToStrF(wl_fixed_to_double(event^.surface_y), ffGeneral, 8, 4), ' ');

    if POINTER_EVENT_LEAVE in event^.event_mask then
      WriteLn('leave ');

    if POINTER_EVENT_MOTION in event^.event_mask then
      WriteLn('motion ', FloatToStrF(wl_fixed_to_double(event^.surface_x), ffGeneral, 8, 4), ', ',
        FloatToStrF(wl_fixed_to_double(event^.surface_y), ffGeneral, 8, 4), ' ');

    if POINTER_EVENT_BUTTON in event^.event_mask then
      if event^.state = WL_POINTER_BUTTON_STATE_RELEASED then
        WriteLn('button ', event^.button, ' released')
      else
        WriteLn('button ', event^.button, ' pressed');

    axisEvents := [POINTER_EVENT_AXIS, POINTER_EVENT_AXIS_SOURCE,
      POINTER_EVENT_AXIS_STOP, POINTER_EVENT_AXIS_DISCRETE];

    axisName[0] := 'vertical';
    axisName[1] := 'horizontal';

    axisSource[0] := 'wheel';
    axisSource[1] := 'finger';
    axisSource[2] := 'continuous';
    axisSource[3] := 'wheel tilt';

    for i := 0 to 1 do
    begin
      if not event^.axes[i].valid then
        Continue;

      WriteLn(axisName[i], ' axis ');

      if POINTER_EVENT_AXIS in event^.event_mask then
        WriteLn('value ', wl_fixed_to_double(event^.axes[i].Value), ' ');

      if POINTER_EVENT_AXIS_DISCRETE in event^.event_mask then
        WriteLn('discrete ', event^.axes[i].discrete, ' ');

      if POINTER_EVENT_AXIS_SOURCE in event^.event_mask then
        WriteLn('via ', axisSource[event^.axis_source], ' ');

      if POINTER_EVENT_AXIS_STOP in event^.event_mask then
        WriteLn('(stopped) ');
    end;

    WriteLn;
    FillChar(event^, SizeOf(event^), 0);
  end;

  procedure wl_pointer_axis_discrete(Data: Pointer; wl_pointer: Pwl_pointer; axis: UInt32; discrete: Int32); cdecl;
  begin
    Include(pointer_event.event_mask, POINTER_EVENT_AXIS_DISCRETE);
    pointer_event.axes[axis].valid    := True;
    pointer_event.axes[axis].discrete := discrete;
  end;

    // Keyboard
  procedure wl_keyboard_keymap(Data: Pointer; wl_keyboard: Pwl_keyboard; format: DWord; fd: longint; size: DWord); cdecl;
 var
  map_shm: PChar;
  
 begin
  Assert(format = WL_KEYBOARD_KEYMAP_FORMAT_XKB_V1);

  map_shm := fpmmap(nil, size, PROT_READ, MAP_PRIVATE, fd, 0);
  Assert(map_shm <> MAP_FAILED);

  xkbkeymap := xkb_keymap_new_from_string(xkbcontext, map_shm,
    XKB_KEYMAP_FORMAT_TEXT_V1, XKB_KEYMAP_COMPILE_NO_FLAGS);
  
  fpmunmap(map_shm, size);
  fpclose(fd);

  xkbstate := xkb_state_new(xkbkeymap);
 
end;


  procedure wl_keyboard_enter(Data: Pointer; wl_keyboard: Pwl_keyboard; serial: DWord; surface: Pwl_surface; keys: Pwl_array); cdecl;
  var
    key: PLongWord;
    buf: array[0..127] of char;
    sym: xkb_keysym_t;
    i: integer;
 begin
   WriteLn('Keyboard enter, key pressed.');

{
  for i := 0 to keys^.size div SizeOf(LongWord) - 1 do
  begin
    key := PLongWord(PtrUInt(keys^.data) + SizeOf(LongWord) * i);
    sym := xkb_state_key_get_one_sym(xkbstate, key^ + 8);
    xkb_keysym_get_name(sym, buf, SizeOf(buf));
    Write('sym: ', buf, ' (', sym, '), ');

    xkb_state_key_get_utf8(xkbstate, key^ + 8, buf, SizeOf(buf));
    WriteLn('utf8: ''', buf, '''');
  end;
}  

end;

procedure wl_keyboard_key(Data: Pointer; wl_keyboard: Pwl_keyboard; serial, time, key, state: DWord); cdecl;
  var
  buf: array[0..127] of char;
  keycode: LongWord;
  sym: xkb_keysym_t;
  action: string;
begin
   keycode := key + 8;
   
   //if xkbstate = nil then  Writeln('xkbstate = nil') else Writeln('xkbstate not nil') ;
   
   sym := xkb_state_key_get_one_sym(xkbstate, keycode);
  
  WriteLn('Sym: ', inttostr(sym));
  xkb_keysym_get_name(sym, buf, SizeOf(buf));
  if state = WL_KEYBOARD_KEY_STATE_PRESSED then
    action := 'press'
  else
    action := 'release';
  
  WriteLn('Key: ', inttostr(key-1),', Action: ', action,  ', Symbol: ', buf, ' (', sym, ')');

  xkb_state_key_get_utf8(xkbstate, keycode, buf, SizeOf(buf));
  Writeln('utf8: ''', buf, '''');
end;
 

  procedure wl_keyboard_leave(Data: Pointer; wl_keyboard: Pwl_keyboard; serial: longword; surface: Pwl_surface); cdecl;
  begin
    WriteLn('keyboard leave');
  end;

  procedure wl_keyboard_modifiers(Data: Pointer; wl_keyboard: Pwl_keyboard; serial, mods_depressed, mods_latched, mods_locked, group: DWord); cdecl;
  begin
    xkb_state_update_mask(xkbstate,
    mods_depressed, mods_latched, mods_locked, 0, 0, group);
  end;

  procedure wl_keyboard_repeat_info(Data: Pointer; wl_keyboard: Pwl_keyboard; rate, delay: longint); cdecl;
  begin
    // Left as an exercise for the reader
  end;

// Seat 
  procedure wl_seat_capabilities(Data: Pointer; wl_seat: Pwl_seat; capabilities: longword); cdecl;
  var
    have_pointer: Boolean;
    have_keyboard: Boolean;
  begin
    // writeln('wl_seat_capabilities init');
    have_pointer := capabilities and WL_SEAT_CAPABILITY_POINTER <> 0;
    if have_pointer then
      writeln('have_pointer true')
    else
      writeln('have_pointer false');
    if have_pointer and (wlpointer = nil) then
    begin

      // init the methods
      wlpointer := wl_seat_get_pointer(wl_seat);
      //if (wlpointer = nil) then writeln('wlpointer = nil') else  writeln('wlpointer not nil');

      wl_pointer_listener.enter := @wl_pointer_enter;

      // test
      //wl_pointer_listener.enter(data, wlpointer,  wl_proxy_get_version(Pwl_proxy(wlpointer)), surface, 1,1);

      // if (wl_pointer_listener.enter = nil) then writeln('wl_pointer_listener.enter = nil') 
      //                                       else  writeln('wl_pointer_listener.enter not nil');

      wl_pointer_listener.button        := @wl_pointer_button;
      wl_pointer_listener.enter         := @wl_pointer_enter;
      wl_pointer_listener.leave         := @wl_pointer_leave;
      wl_pointer_listener.motion        := @wl_pointer_motion;
      wl_pointer_listener.button        := @wl_pointer_button;
      wl_pointer_listener.axis          := @wl_pointer_axis;
      wl_pointer_listener.frame         := @wl_pointer_frame;
      wl_pointer_listener.axis_source   := @wl_pointer_axis_source;
      wl_pointer_listener.axis_stop     := @wl_pointer_axis_stop;
      wl_pointer_listener.axis_discrete := @wl_pointer_axis_discrete;

      wl_pointer_add_listener(wlpointer, @wl_pointer_listener, Data);

    end
    else if not have_pointer and (wlpointer <> nil) then
    begin
      wl_pointer_release(wlpointer);
      wlpointer := nil;
    end;

    have_keyboard := capabilities and WL_SEAT_CAPABILITY_KEYBOARD <> 0;
    if have_keyboard then
      writeln('have_keyboard true')
    else
      writeln('have_keyboard false');

    if have_keyboard and (keyboard = nil) then
    begin
      keyboard := wl_seat_get_keyboard(wl_seat);

      if (keyboard = nil) then
        writeln('keyboard = nil')
      else
        writeln('keyboard not nil');

      wl_keyboard_listener.keymap      := @wl_keyboard_keymap;
      wl_keyboard_listener.enter       := @wl_keyboard_enter;
      wl_keyboard_listener.key         := @wl_keyboard_key;
      wl_keyboard_listener.leave       := @wl_keyboard_leave;
      wl_keyboard_listener.modifiers   := @wl_keyboard_modifiers;
      wl_keyboard_listener.repeat_info := @wl_keyboard_repeat_info;

      wl_keyboard_add_listener(keyboard, @wl_keyboard_listener, Data);

    end
    else if not have_keyboard and (keyboard <> nil) then
    begin
      wl_keyboard_release(keyboard);
      keyboard := nil;
    end;

  end;

  procedure wl_seat_name(Data: Pointer; wl_seat: Pwl_seat; Name: PAnsiChar); cdecl;
  begin
    WriteLn('This is the call --> seat name: ', Name);
  end;

const
  wl_seat_listener: Twl_seat_listener = (
    capabilities: @wl_seat_capabilities;
    Name: @wl_seat_name;
    );

  function draw_frame(state: Pwl_display): Pwl_buffer;
  cdecl;
  var
    size, stride, fd, x, y: integer;
    s: string = '*';
    Data: ^longword;
    pool: Pwl_shm_pool;
    buffer: Pwl_buffer;
  begin

    //writeln('draw_frame init');

      stride := Width_ * 4;
    // Adjust based on pixel format
    size   := stride * Height_;

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

    buffer := wl_shm_pool_create_buffer(pool, 0, Width_, Height_, stride, WL_SHM_FORMAT_XRGB8888);

    //if buffer = nil then writeln('wl_shm_pool_create_buffer = NOT OK') else writeln('wl_shm_pool_create_buffer = OK');

    wl_shm_pool_destroy(pool);

    fpclose(fd);

    { Draw chessboard background }

    // writeln('Offset = ' + floattostr(offset));
    
    for y := 0 to Height_ - 1 do
      for x := 0 to Width_ - 1 do
        if ((x + trunc(offset)) div 50 + (y + trunc(offset)) div 50) mod 2 = 0 then
        begin
          if (Data^) >= (y * Width_ + x) then Data[y * Width_ + x] := $FF834555; // Set pixel color
        end
        else
        begin
          if (Data^) >= (y * Width_ + x) then Data[y * Width_ + x] := $FFEEEEEE;
        end;
        
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

  procedure xdg_toplevel_configure(Data: Pointer; xdg_toplevel: Pxdg_toplevel; Width, Height: Int32; states: Pwl_array); cdecl;
  begin
    if (Width = 0) or (Height = 0) then
      Exit{ Compositor is deferring to us };

    width_  := Width;
    height_ := Height;
  end;

  procedure xdg_toplevel_close(Data: Pointer; toplevel: Pxdg_toplevel); cdecl;
  begin
    closed := True;
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

  function xdg_toplevel_add_listener(xdg_toplevel: Pxdg_toplevel; listener: Pxdg_toplevel_listener; Data: Pointer): longint; cdecl;
  begin
    Result := wl_proxy_add_listener(Pwl_proxy(xdg_toplevel), Pointer(listener), Data);
    writeln('xdg_toplevel_add_listener = ' + IntToStr(Result));
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

    writeln(IntToStr(pinginc) + ' ping done of 100');
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
  begin
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

      xdg_wm_base_add_listener(xdg_wm_base, @xdg_wm_base_listener, Pwl_display(Data));

    end
    else if AnsiCompareStr(iface, wl_seat_interface.Name) = 0 then
    begin
      seat := wl_registry_bind(wl_registry, Name, @wl_seat_interface, 7);
      wl_seat_add_listener(seat, @wl_seat_listener, Pwl_display(Data));
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
  xdg_toplevel_listener: Txdg_toplevel_listener;
  cb: Pwl_callback;

begin
  { Initialize Wayland objects }
  shm         := nil;
  compositor  := nil;
  xdg_wm_base := nil;
  surface     := nil;

  width_  := 400;
  height_ := 400;

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

  xkbcontext := xkb_context_new(XKB_CONTEXT_NO_FLAGS);
  if xkbcontext <> nil then
    writeln('xkbcontext connected')
  else
    writeln('xkbcontext not connected');

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

  xdg_toplevel_listener.configure := @xdg_toplevel_configure;
  xdg_toplevel_listener.Close     := @xdg_toplevel_close;

  xdg_toplevel_add_listener(xdg_toplevel, @xdg_toplevel_listener, display);

  xdg_toplevel_set_title(xdg_toplevel, 'Example client');
  writeln('xdg_toplevel_set_title');

  wl_surface_commit(surface);
  writeln('wl_surface_commit');

  cb := wl_surface_frame(surface);

  wl_surface_frame_listener.done := @wl_surface_frame_callback;

  wl_callback_add_listener(cb, @wl_surface_frame_listener, display);

  writeln('wl_callback_add_listener');

  writeln('>>> Init loop');
  writeln('');
  writeln('Move mouse and press button on the chessboard');
  writeln('');

  while (wl_display_dispatch(display) <> -1) and (pinginc < 100) do
    { This space deliberately left blank };

  wl_display_disconnect(display);
  
  xkb_keymap_unref(xkbkeymap);
  xkb_state_unref(xkbstate); 

  writeln();
  writeln('wl_display_disconnect');

  writeln();
  writeln('Bye!');

end.

