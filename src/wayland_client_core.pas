// Pascal translation by Andrews Haines.
// This unit is part of fpc-wayland project
// From https://github.com/andrewd207/fpc-wayland

// Added methods and classes by Fred vS 2023.

unit wayland_client_core;

{$mode objfpc}{$H+}
{$packrecords c}
{$linklib wayland-client}

interface

uses
  Classes, SysUtils, unixtype, wayland_util, ctypes;

const
  XDG_TOPLEVEL_SET_TITLE_ = 2;
  XDG_WM_BASE_GET_XDG_SURFACE_ = 2;
  XDG_SURFACE_ACK_CONFIGURE_ = 4;
  XDG_SURFACE_GET_TOPLEVEL_ = 1;
  XDG_WM_BASE_PONG_ = 3;
  WL_DISPLAY_GET_REGISTRY_ = 1;
  WL_REGISTRY_BIND_ = 0;
  WL_COMPOSITOR_CREATE_SURFACE_ = 0;
  WL_SHELL_GET_SHELL_SURFACE_ = 0;
  WL_SHELL_SURFACE_SET_TOPLEVEL_ = 3;
  WL_SHM_CREATE_POOL_ =	0;
  WL_SHM_POOL_CREATE_BUFFER_ =	0;
  WL_SURFACE_ATTACH_ = 1;
  WL_SURFACE_COMMIT_ =	6;
  WL_SHM_POOL_DESTROY_ = 1;
  WL_SURFACE_FRAME_ = 3;
  WL_SURFACE_DAMAGE_BUFFER_ = 9;
  WL_SEAT_GET_POINTER_ = 0;
  WL_POINTER_RELEASE_ =	1;
  WL_SEAT_GET_KEYBOARD_ = 1;
  WL_KEYBOARD_RELEASE_ = 0;
   
type
  Pwl_display = pointer;  
  Pwl_event_queue = pointer ; 
  Pwl_proxy = pointer;  
  Pwl_proxy_wrapper = pointer; 
  pwl_registry = pointer; 
  Pwl_compositor = Pointer;
  Pwl_surface = Pointer;
  Pwl_shell = Pointer;
  Pwl_shell_surface = Pointer;
  Pwl_shm  = Pointer;
  Pwl_shm_pool  = Pointer;
  Pwl_buffer  = Pointer;
  Pwl_callback = Pointer;
  Pwl_seat = Pointer;
  Pwl_pointer = Pointer;
     
  Pwl_callback_listener = ^Twl_callback_listener;
  Twl_callback_listener = record
    done : procedure(data: Pointer; AWlCallback: Pwl_callback; ACallbackData: DWord); cdecl;
  end;
  
  type
  Pxdg_toplevel = Pointer;
  
  Pxdg_toplevel_listener = ^Txdg_toplevel_listener;
  Txdg_toplevel_listener = record
    configure : procedure(data: Pointer; AXdgToplevel: Pxdg_toplevel; AWidth: LongInt; AHeight: LongInt; AStates: Pwl_array); cdecl;
    close : procedure(data: Pointer; AXdgToplevel: Pxdg_toplevel); cdecl;
  end;
  
  Pwl_keyboard = Pointer;
 
  type
  Pwl_keyboard_listener = ^Twl_keyboard_listener;
  Twl_keyboard_listener = record
    keymap : procedure(data: Pointer; AWlKeyboard: Pwl_keyboard; AFormat: DWord; AFd: LongInt{fd}; ASize: DWord); cdecl;
    enter : procedure(data: Pointer; AWlKeyboard: Pwl_keyboard; ASerial: DWord; ASurface: Pwl_surface; AKeys: Pwl_array); cdecl;
    leave : procedure(data: Pointer; AWlKeyboard: Pwl_keyboard; ASerial: DWord; ASurface: Pwl_surface); cdecl;
    key : procedure(data: Pointer; AWlKeyboard: Pwl_keyboard; ASerial: DWord; ATime: DWord; AKey: DWord; AState: DWord); cdecl;
    modifiers : procedure(data: Pointer; AWlKeyboard: Pwl_keyboard; ASerial: DWord; AModsDepressed: DWord; AModsLatched: DWord; AModsLocked: DWord; AGroup: DWord); cdecl;
    repeat_info : procedure(data: Pointer; AWlKeyboard: Pwl_keyboard; ARate: LongInt; ADelay: LongInt); cdecl;
  end;

  { TWLProxyObject }

  TWLProxyObject = class;

  PWLUserData = ^TWLUserData;
  TWLUserData = record
    ListenerUserData: Pointer; //wl_xxx_set_userdata and add listener share this.
    PascalObject: TWLProxyObject; { private }
    UserData: Pointer;
  end;

  TWLProxyObjectClass = class of TWLProxyObject;
  TWLProxyObject = class
  private
    FOwnsProxy: Boolean;
    function GetUserData: Pointer;
    procedure SetUserData(AValue: Pointer);
  protected
    FProxy: Pwl_proxy;
    FUserDataRec: TWLUserData;
  public
    class function WLToObj(AProxy: Pwl_proxy): TWLProxyObject;
  public
    constructor Create(AProxy: Pwl_proxy; AOwnsProxy: Boolean = True); virtual;
    destructor  Destroy; override;
    function GetVersion: LongInt;
    property Proxy: Pwl_proxy read FProxy;
    property UserData: Pointer read GetUserData write SetUserData;
    property OwnsProxy: Boolean read FOwnsProxy write FOwnsProxy;
  end;

  { TWLDisplayBase }

  TWLDisplayBase = class(TWLProxyObject)
    class function  Connect(AName: String; AClass: TWLProxyObjectClass=nil{TWLDisplay}): TWLDisplayBase;
    class function  ConnectToFd(AFd: LongInt; AClass: TWLProxyObjectClass=nil{TWLDisplay}): TWLDisplayBase;
    procedure Disconnect;
    function  GetFd:LongInt;
    function  Dispatch: LongInt;
    function  DispatchQueue(AQueue: Pwl_event_queue): LongInt;
    function  DispatchQueuePending(AQueue: Pwl_event_queue): LongInt;
    function  DispatchPending: LongInt;
    function  GetError: LongInt;
    function  GetProtocolError(AInterface: PPwl_interface; AId: PLongWord): LongWord;
    function  Flush: LongInt;
    function  RoundtripQueue(AQueue: Pwl_event_queue): LongInt;
    function  Roundtrip: LongInt;
    function  CreateQueue: Pwl_event_queue;
    function  PrepareReadQueue(AQueue: Pwl_event_queue): LongInt;
    function  PrepareRead: LongInt;
    procedure CancelRead;
    function  ReadEvents:LongInt;
    destructor Destroy; override;
  end;

  TWLShmBase = class;

  { TWLShmPoolBase }

  TWLShmPoolBase = class(TWLProxyObject)
  private
    FData: Pointer;
    FFd: LongInt;
    FSize: LongWord;
    FShm: TWLShmBase;
    procedure SetParams(AFd: LongInt; AData: Pointer; ASize: LongWord; AShm: TWLShmBase);
  public
    function   Data(AOffset: Integer): Pointer;
    procedure  Reallocate(ANewSize: LongWord);
    destructor Destroy; override;
    property   Allocated: LongWord read FSize;
  end;

  { TWLShmBase }

  TWLShmBase = class(TWLProxyObject)
  protected
  public
    function CreatePool(ASize: LongWord; AClass: TWLProxyObjectClass=nil{TWlShmPool}): TWLShmPoolBase;
  end;

function CreateAnonymousFile(ASize: PtrUint): cint; {fd}

  { Twl_event_queue }
procedure wl_event_queue_destroy(queue: Pwl_event_queue); cdecl; external;

procedure wl_proxy_marshal(p: Pwl_proxy; opcode: cint32); cdecl; external; varargs;
procedure wl_proxy_marshal_array(p: Pwl_proxy; opcode: cint32; args: Pwl_argument); cdecl; external;
function  wl_proxy_create(factory: Pwl_proxy; &interface: Pwl_interface): Pwl_proxy; cdecl; external;
function  wl_proxy_create_wrapper(proxy: pointer): Pwl_proxy_wrapper; cdecl; external;
procedure wl_proxy_wrapper_destroy(proxy_wrapper: Pointer); cdecl; external;
function  wl_proxy_marshal_constructor(p: Pwl_proxy; opcode: cint32; &interface: Pwl_interface): Pwl_proxy; cdecl; external; varargs;
function  wl_proxy_marshal_constructor_versioned(p: Pwl_proxy; opcode: cint32; &interface: Pwl_interface; version: cuint32): Pwl_proxy; cdecl; external; varargs;
function  wl_proxy_marshal_array_constructor(p: Pwl_proxy; opcode: cint32; args: Pwl_argument; &interface: Pwl_interface): Pwl_proxy; cdecl; external;
function  wl_proxy_marshal_array_constructor_versioned(p: Pwl_proxy; opcode: cint32; args: Pwl_argument; &interface: Pwl_interface; version: cuint32): Pwl_proxy; cdecl; external;
procedure wl_proxy_destroy(proxy: Pwl_proxy); cdecl; external;
function  wl_proxy_add_listener(proxy: Pwl_proxy; impl: pointer; data: pointer): cint; cdecl; external;
function  wl_proxy_get_listener(proxy: Pwl_proxy): Pointer; cdecl; external;
function  wl_proxy_add_dispatcher(proxy: Pwl_proxy; dispatcher_func: wl_dispatcher_func_t; dispatcher_data: pointer; data: pointer): cint; cdecl; external;
procedure wl_proxy_set_user_data(proxy: Pwl_proxy; user_data: pointer); cdecl; external;
function  wl_proxy_get_user_data(proxy: Pwl_proxy): Pointer; cdecl; external;
function  wl_proxy_get_version(proxy: Pwl_proxy): cuint32; cdecl; external;
function  wl_proxy_get_id(proxy: Pwl_proxy): cint; cdecl; external;
function  wl_proxy_get_class(proxy: Pwl_proxy): PChar; cdecl; external;
procedure wl_proxy_set_queue(proxy: Pwl_proxy; queue: Pwl_event_queue); cdecl; external;

function  wl_display_connect(name: PChar): Pwl_display; cdecl; external;
function  wl_display_connect_to_fd(fd: cint): Pwl_display; cdecl; external;
procedure wl_display_disconnect(display :Pwl_display); cdecl; external;
function  wl_display_get_fd(display :Pwl_display): cint; cdecl; external;
function  wl_display_dispatch(display :Pwl_display): cint; cdecl; external;
function  wl_display_dispatch_queue(display :Pwl_display; queue: Pwl_event_queue): cint; cdecl; external;
function  wl_display_dispatch_queue_pending(display :Pwl_display; queue: Pwl_event_queue): cint; cdecl; external;
function  wl_display_dispatch_pending(display :Pwl_display): cint; cdecl; external;
function  wl_display_get_error(display :Pwl_display): cint; cdecl; external;
function  wl_display_get_protocol_error(display :Pwl_display; &interface: PPwl_interface; id: Pcuint32): cuint32; cdecl; external;
function  wl_display_flush(display :Pwl_display): cint; cdecl; external;
function  wl_display_roundtrip_queue(display :Pwl_display; queue: Pwl_event_queue): cint; cdecl; external;
function  wl_display_roundtrip(display :Pwl_display): cint; cdecl; external;
function  wl_display_create_queue(display :Pwl_display): Pwl_event_queue; cdecl; external;
function  wl_display_prepare_read_queue(display :Pwl_display; queue: Pwl_event_queue): cint; cdecl; external;
function  wl_display_prepare_read(display :Pwl_display): cint; cdecl; external;
procedure wl_display_cancel_read(display :Pwl_display); cdecl; external;
function  wl_display_read_events(display :Pwl_display): cint; cdecl; external;
procedure wl_log_set_handler_client(handler: wl_log_func_t); cdecl; external;

//function wl_proxy_marshal_flags(proxy: Pwl_proxy; opcode: DWord; interface_: Pwl_interface;
// flags: DWord; p1, p2, p3: Pointer): Pwl_proxy; cdecl; external 'wayland-client';
 
function wl_proxy_marshal_flags_get_xdg_surface(
  proxy: Pwl_proxy;
  opcode: longint;
  interface_: Pwl_interface;
  version: DWord;
  flags: DWord;
   param1: pointer;
  surface: Pwl_surface
   
): Pwl_proxy; cdecl; external 'wayland-client' name 'wl_proxy_marshal_flags';


procedure wl_proxy_marshal_flags_ack_configure(
  proxy: Pwl_proxy;
  opcode: DWord;
  interface_: Pwl_interface;
  version: DWord;
  flags: DWord;
  serial: DWord
); cdecl; external 'wayland-client' name 'wl_proxy_marshal_flags';


function wl_proxy_marshal_flags_get_toplevel(
  proxy: Pwl_proxy;
  opcode: DWord;
  interface_: Pwl_interface;
  version: DWord;
  flags: DWord;
  param: pointer
): Pxdg_toplevel ; cdecl; external 'wayland-client' name 'wl_proxy_marshal_flags';

procedure wl_proxy_marshal_flags_set_title(
  proxy: Pwl_proxy;
  opcode: DWord;
  interface_: Pwl_interface;
  version: DWord;
  flags: DWord;
  title: PChar
); cdecl; external 'wayland-client' name 'wl_proxy_marshal_flags';

procedure wl_proxy_marshal_flags_ping(
  proxy: Pwl_proxy;
  opcode: DWord;
  interface_: Pwl_interface;
  version: DWord;
  flags: DWord;
  serial: DWord
); cdecl; external 'wayland-client' name 'wl_proxy_marshal_flags';


implementation
uses
 wayland_protocol, BaseUnix, syscall;
 
// c functions
function mkstemp(filename: PChar):longint;cdecl;external 'libc' name 'mkstemp';
function mkostemp(filename: PChar; flags: LongInt):longint;cdecl;external 'libc' name 'mkostemp';

{ TWLShmPoolBase }

procedure TWLShmPoolBase.SetParams(AFd: LongInt; AData: Pointer;
  ASize: LongWord; AShm: TWLShmBase);
begin
  Ffd := AFd;
  FData:=AData;
  FSize:=ASize;
  FShm := AShm;
end;

function TWLShmPoolBase.Data(AOffset: Integer): Pointer;
begin
  Result := FData+AOffset;
end;

procedure TWLShmPoolBase.Reallocate(ANewSize: LongWord);
const
  MREMAP_MAYMOVE = 1;
begin
  if ANewSize <= FSize then
    Exit;
  FpFtruncate(FFd, ANewSize);
  FData := Pointer(Do_SysCall(syscall_nr_mremap, TSysParam(FData), TSysParam(FSize), TSysParam(ANewSize), TsysParam(MREMAP_MAYMOVE)));
  TWlShmPool(Self).Resize(ANewSize);
  FSize:=ANewSize;
end;

destructor TWLShmPoolBase.Destroy;
begin
  inherited Destroy;
  FpClose(FFd);
end;


{ TWLShmBase }

function CreateAnonymousFile(ASize: PtrUint): cint; {fd}
const
  O_CLOEXEC = $80000;
var
  lName: String;
  flags: cint;
begin
  lName := GetEnvironmentVariable('XDG_RUNTIME_DIR') + '/weston-shared-XXXXXX';

  Result := mkostemp(PChar(lName), O_CLOEXEC);
  FpUnlink(lName);

  if (FpFtruncate(Result, ASize) < 0) then
  begin
    FpClose(Result);
    Result := -1;
  end;
end;

function TWLShmBase.CreatePool(ASize: LongWord; AClass: TWLProxyObjectClass): TWLShmPoolBase;
var
  fd: LongInt;
  data: Pointer;
begin
  Result := nil;
  fd := CreateAnonymousFile(ASize);
  if fd < 0 then
    Exit;

  data := Fpmmap(nil, ASize, PROT_READ or PROT_WRITE, MAP_SHARED, fd, 0);
  if data = MAP_FAILED then
  begin
    ASize := errno;
    fpclose(fd);
    Exit;
  end;

  if AClass = nil then
    AClass := TWlShmPool;

  Result := TWlShm(Self).CreatePool(fd, Asize, AClass);
  Result.SetParams(fd, data, ASize, Self);

end;

{ TWLDisplayBase }

class function TWLDisplayBase.Connect(AName: String; AClass: TWLProxyObjectClass
  ): TWLDisplayBase;
var
  lDisplay: wayland_client_core.Pwl_display;
  lName: PChar;
begin
  Result := nil;

  if AClass = nil then
    AClass:=TWlDisplay;

  if AName = '' then
    lName:=nil
  else
    lName := PChar(AName);

  lDisplay := wl_display_connect(lName);
  if lDisplay <> nil then
  begin
    Result := TWLDisplayBase(AClass.Create(lDisplay));
  end;
end;

class function TWLDisplayBase.ConnectToFd(AFd: LongInt; AClass: TWLProxyObjectClass): TWLDisplayBase;
var
  lDisplay: wayland_client_core.Pwl_display;
begin
  if AClass = nil then
    AClass:=TWlDisplay;
  Result := nil;
  lDisplay := wl_display_connect_to_fd(AFd);
  if lDisplay <> nil then
  begin
    Result := TWLDisplayBase(AClass.Create(lDisplay));
  end;
end;

procedure TWLDisplayBase.Disconnect;
begin
  if Assigned(FProxy) then
    wl_display_disconnect(FProxy);
  FProxy:=nil;
end;

function TWLDisplayBase.GetFd: LongInt;
begin
  Result := wl_display_get_fd(FProxy);
end;

function TWLDisplayBase.Dispatch: LongInt;
begin
  Result := wl_display_dispatch(FProxy);
end;

function TWLDisplayBase.DispatchQueue(AQueue: Pwl_event_queue): LongInt;
begin
  Result := wl_display_dispatch_queue(FProxy, AQueue);
end;

function TWLDisplayBase.DispatchQueuePending(AQueue: Pwl_event_queue): LongInt;
begin
  Result := wl_display_dispatch_queue_pending(FProxy, AQueue);
end;

function TWLDisplayBase.DispatchPending: LongInt;
begin
  Result := wl_display_dispatch_pending(FProxy);
end;

function TWLDisplayBase.GetError: LongInt;
begin
  Result := wl_display_get_error(FProxy);
end;

function TWLDisplayBase.GetProtocolError(AInterface: PPwl_interface;
  AId: PLongWord): LongWord;
begin
  Result := wl_display_get_protocol_error(FProxy, AInterface, AId);

end;

function TWLDisplayBase.Flush: LongInt;
begin
  Result := wl_display_flush(FProxy);

end;

function TWLDisplayBase.RoundtripQueue(AQueue: Pwl_event_queue): LongInt;
begin
  Result := wl_display_roundtrip_queue(FProxy, AQueue);
end;

function TWLDisplayBase.Roundtrip: LongInt;
begin
  Result := wl_display_roundtrip(FProxy);
end;

function TWLDisplayBase.CreateQueue: Pwl_event_queue;
begin
  Result := wl_display_create_queue(FProxy);
end;

function TWLDisplayBase.PrepareReadQueue(AQueue: Pwl_event_queue): LongInt;
begin
  Result := wl_display_prepare_read_queue(FProxy, AQueue);
end;

function TWLDisplayBase.PrepareRead: LongInt;
begin
  Result := wl_display_prepare_read(FProxy);
end;

procedure TWLDisplayBase.CancelRead;
begin
  wl_display_cancel_read(FProxy);

end;

function TWLDisplayBase.ReadEvents: LongInt;
begin
  Result := wl_display_read_events(FProxy);
end;

destructor TWLDisplayBase.Destroy;
begin
  Disconnect;
  //inherited Destroy;
end;

{ TWLProxyObject }

function TWLProxyObject.GetUserData: Pointer;
begin
  Result := FUserDataRec.UserData;
end;

procedure TWLProxyObject.SetUserData(AValue: Pointer);
begin
  if FUserDataRec.UserData=AValue then Exit;
  FUserDataRec.UserData:=AValue;
end;

class function TWLProxyObject.WLToObj(AProxy: Pwl_proxy): TWLProxyObject;
var
  lData: PWLUserData;
begin
  lData := wl_proxy_get_user_data(AProxy);
  Result := lData^.PascalObject;
end;

constructor TWLProxyObject.Create(AProxy: Pwl_proxy; AOwnsProxy: Boolean);
begin
  FUserDataRec.PascalObject := Self;
  FProxy:=AProxy;
end;

destructor TWLProxyObject.Destroy;
begin
  inherited Destroy;
  if Assigned(FProxy) and FOwnsProxy then
    wl_proxy_destroy(FProxy);
end;

function TWLProxyObject.GetVersion: LongInt;
begin
  Result := wl_proxy_get_version(FProxy);
end;

end.
