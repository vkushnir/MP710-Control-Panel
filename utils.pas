unit utils;

interface

uses
  JvHidControllerClass, WinApi.Windows , System.Classes, System.SysUtils;

type
  TBUFF = array [0..9] of byte;

  TEventHandlers = class
    procedure HIDArrival(HidDev: TJvHidDevice);
    procedure HIDRemoval(HidDev: TJvHidDevice);
  end;

  THidDevice = record
    ID: Integer;
    HID: TJvHidDevice;
  end;

  TJvHidMP710PlugEvent = procedure(UID: Integer) of object;
  TJvHidMP710UnplugEvent = TJvHidMP710PlugEvent;
  // Classes
  TJvHidMP710ControllersList = class(TJvHidDeviceController)
    private
      BUFI, BUFO: Array [0..9] of Byte;
      FDevicesList: TStringList;
      FMP710ArrivalEvent: TJvHidMP710PlugEvent;
      FMP710RemovalEvent: TJvHidMP710UnplugEvent;
      // Parent Events
      procedure jvHidDeviceArrival(HidDev: TJvHidDevice);
      procedure jvHidDeviceRemoval(HidDev: TJvHidDevice);
      // Utilites
      procedure ClearBuffers;
      function GetUID(HidDev: TJvHidDevice; var UID: Integer): Boolean; overload;
      function SetPORTkk(HidDev: TJvHidDevice; N, REG, COM: Byte; CMD, PRG: Word): Boolean;
      function GetPORTkk(HidDev: TJvHidDevice; N: Byte; var REG, COM: Byte; var CMD, PRG: Word): Boolean;
      // Property support
      function GetCount: Integer;
      function GetUID(Index: Integer): Integer; overload;
      function GetMP710Device(Index: Integer): TJvHidDevice;
      function GetIndexOfUID(UID: Integer): Integer;
      function GetMP710DeviceByUID(UID: Integer): TJvHidDevice;
      function GetUIDsList: String;
      function GetMP710PORTkkEnabled(UID: Integer; N: Byte): Boolean;
      procedure SetMP710PORTkkEnabled(UID: Integer; N: Byte; Value: Boolean);
    protected
      procedure DoMP710Arrival(UID: Integer);
      procedure DoMP710Removal(UID: Integer);
    public
      // Properties
      property Count: Integer read GetCount;
      property UIDsList: String read GetUIDsList;
      property UIDs[Index: Integer]: Integer read GetUID;
      property MP710Devices[Index: Integer]: TJvHidDevice read GetMP710Device;
      property IndexOfUID[UID: Integer]: Integer read GetIndexofUID;
      property MP710DeviceByUID[UID: Integer]: TJvHidDevice read GetMP710DeviceByUID;
      property MP710PORTkkEnabled[UID: Integer; N: Byte]: Boolean read GetMP710PORTkkEnabled write SetMP710PORTkkEnabled;
      // Constructors / Destructors
      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;
    published
      property OnMP710Arrival: TJvHidMP710PlugEvent read FMP710ArrivalEvent write FMP710ArrivalEvent;
      property OnMP710Removal: TJvHidMP710UnplugEvent read FMP710RemovalEvent write FMP710RemovalEvent;
  end;

  procedure ClearBuffers;

  function SetMP710PORTkk(ID: DWORD; N, REG, COM: Byte; CMD, PRG: Word): Boolean;
  function GetMP710PORTkk(ID: DWORD; N: Byte; var REG, COM: Byte; var CMD, PRG: Word): Boolean;
  function GetMP710PORTkkEnabled(ID: DWORD; N: Byte; var Value: Boolean): Boolean;
  function SetMP710PORTkkEnabled(ID: DWORD; N: Byte; Value: Boolean): Boolean;

var
  EvHandler: TEventHandlers;
  HID: TJvHidDeviceController;
  MP724: TJvHidDevice;
  MP710: array of THidDevice;
  BUFI, BUFO: TBUFF;
  i: Integer;

const
  MPl24_FID             = $30;
  MP710_FID             = $2D;
  CMD_ID_GET            = $1D;
  CMD_PORT_GET          = $36;
  CMD_PORT_SET          = $63;

implementation

uses
  Main;

// Event Handlers
procedure TEventHandlers.HIDArrival(HidDev: TJvHidDevice);
var
  res : boolean;
  SV: WORD;
  ID: DWORD;
  i: Integer;
begin
  if (HidDev.VendorName='www.masterkit.ru') and
     (HidDev.ProductName = 'MP710') then begin
    ClearBuffers;
    HidDev.OpenFile;
    BUFO[1]:=CMD_ID_GET;
    res:=HidDev.SetFeature(BUFO, 9);
    res:=res and HidDev.GetFeature(BUFI, 9);
    res:=res and (BUFI[1]=CMD_ID_GET);
    res:=res and (BUFI[2]=MP710_FID);
    if res then begin
      asm
        lea edi, BUFI
        mov ax, [ edi + 3 ]
        mov SV, ax
        mov eax, [ edi + 5 ]
        bswap eax
        mov ID, eax
      end;
      frmMain.sInfo('Устройство MP710['+IntToStr(ID)+'] (SV: '+IntToStr(SV)+') подключено.', 3);
      i := Length(MP710);
      SetLength(MP710, i + 1);
      i := High(MP710);
      MP710[i].ID := ID;
      MP710[i].HID := HidDev;
      MP710[i].HID.OpenFile;
    end;
  end;
end;
procedure TEventHandlers.HIDRemoval(HidDev: TJvHidDevice);
var
  i: Integer;
  ALength, TailElements: Cardinal;
begin
  if (HidDev.VendorName='www.masterkit.ru') and
     (HidDev.ProductName = 'MP710') then begin
    for i := Low(MP710) to High(MP710) do begin
      if MP710[i].HID = HidDev then begin
        frmMain.sInfo('Устройство MP710['+IntToStr(MP710[i].ID)+'] извлечено.');
        ALength := Length(MP710);
        Assert(ALength > 0);
        Assert(i < ALength);
        Finalize(MP710[i]);
        TailElements := ALength - i;
        if TailElements > 0 then
          Move(MP710[i + 1], MP710[i], SizeOf(THidDevice) * TailElements);
        Initialize(MP710[ALength - 1]);
        SetLength(MP710, ALength - 1);
        break;
      end;
    end;
  end;
end;

// Feature Operations
procedure ClearBuffers;
//var i: integer;
begin
  asm
    lea edi, BUFI
    lea esi, BUFO
    mov eax, 0

    mov [edi], eax
    mov [edi + 4], eax
    mov [edi + 8], al

    mov [esi], eax
    mov [esi + 4], eax
    mov [esi + 8], al
  end;
end;
function SetMP710PORTkk(ID: DWORD; N, REG, COM: Byte; CMD, PRG: Word): Boolean;
var
  i, hID: Integer;
begin
  Result := False;

  for i := Low(MP710) to High(MP710) do begin
    if MP710[i].ID = ID then begin
      hID := i;
      Break;
    end;
    Exit;
  end;

  ClearBuffers;
  i := 3;
  // Запись состояния порта
  asm
    lea edi, BUFO
    mov al, N
    mov ah, REG
    mov bl, COM

    mov cx, CMD
    mov dx, PRG

    mov byte ptr [edi + 1], CMD_PORT_SET
    mov [edi + 2], al
    mov [edi + 3], ah
    mov [edi + 4], bl
    xchg ch, cl
    mov [edi + 5], cx
    xchg dh, dl
    mov [edi + 7], dx
  end;
//  BUFO[1] := $63;
//  BUFO[2] := N;
//  BUFO[3] := REG;
//  BUFO[4] := COM;
//  BUFO[5] := CMD shr 8;
//  BUFO[6] := CMD and $FF;
//  BUFO[7] := PRG shr 8;
//  BUFO[8] := PRG and $FF;

  while (Not Result) and (i>0) do begin
    if MP710[hID].HID.SetFeature(BUFO[0], 9) then
      if MP710[hID].HID.GetFeature(BUFI[0], 9) then begin
        asm
          lea esi, BUFO
          lea esi, BUFI
          mov ecx, 9
          cld
          repe cmpsb
          jne @End
          mov Result, True
        @End:
          nop
        end;
//        Result := (BUFI[1] = $63) and
//                  (BUFI[2] = N) and
//                  (BUFI[3] = REG) and
//                  (BUFI[4] = COM) and
//                  (BUFI[5] = BUFO[5]) and
//                  (BUFI[6] = BUFO[6]) and
//                  (BUFI[7] = BUFO[7]) and
//                  (BUFI[8] = BUFO[8]);
        Break;
      end;
    Dec(i);
  end;
end;
function GetMP710PORTkk(ID: DWORD; N: Byte; var REG, COM: Byte; var CMD, PRG: Word): Boolean;
var
  i, hID: Integer;
begin
  Result := False;

  for i := Low(MP710) to High(MP710) do begin
    if MP710[i].ID = ID then begin
      hID := i;
      Break;
    end;
    Exit;
  end;

  ClearBuffers;
  i := 3;
  // Чтение состояния порта
  BUFO[1] := CMD_PORT_GET;
  BUFO[2] := N;

  while (Not Result) and (i>0) do begin
    if MP710[hID].HID.SetFeature(BUFO[0], 9) then
      if MP710[hID].HID.GetFeature(BUFI[0], 9) then begin
{        asm
          lea esi, BUFI
          mov al, CMD_PORT_GET
          mov ah, N

          cmp [esi], al
          jne @End
          cmp [esi + 1], ah
          jne @End

          mov al, [esi + 3]
          mov ah, [esi + 4]
          mov bx, [esi + 5]
          xchg bl, bh
          mov cx, [esi + 7]
          xchg cl, ch

          mov REG, al
          mov COM, ah
          mov CMD, bx
          mov PRG, cx
          mov Result, True
        @End:
          nop
        end;   }
        Result := (BUFI[1] = $36) and (BUFI[2] = N);
        REG := BUFI[3];
        COM := BUFI[4];
        CMD := (BUFI[5] shl 8) + BUFI[6];
        PRG := (BUFI[7] shl 8) + BUFI[8];
        Break;
      end;
    Dec(i);
  end;
end;

function GetMP710PORTkkEnabled(ID: DWORD; N: Byte; var Value: Boolean): Boolean;
begin

end;
function SetMP710PORTkkEnabled(ID: DWORD; N: Byte; Value: Boolean): Boolean;
begin

end;

// **** TJvHidMP710ControllersList
// Constructors / Destructors
constructor TJvHidMP710ControllersList.Create(AOwner: TComponent);
begin
  FDevicesList := TStringList.Create;
  inherited Create(AOwner);
  OnArrival := jvHidDeviceArrival;
  OnRemoval := jvHidDeviceRemoval;
end;
destructor TJvHidMP710ControllersList.Destroy;
var i: Integer;
begin
  FMP710ArrivalEvent := Nil;
  FMP710RemovalEvent := Nil;
  if FDevicesList.Count > 0 then
    for i := 0 to FDevicesList.Count-1 do
      (FDevicesList.Objects[i] as TJvHidDevice).Free;
  FDevicesList.Free;

  inherited Destroy;
end;
// Parent Events
procedure TJvHidMP710ControllersList.jvHidDeviceArrival(HidDev: TJvHidDevice);
var UID: Integer;
begin
  if (HidDev.VendorName = 'www.masterkit.ru') and
     (HidDev.ProductName = 'MP710') and
     GetUID(HidDev, UID) then begin
    FDevicesList.BeginUpdate;
    FDevicesList.AddObject(IntToStr(UID), HidDev);
    FDevicesList.EndUpdate;
    DoMP710Arrival(UID);
  end;
end;
procedure TJvHidMP710ControllersList.jvHidDeviceRemoval(HidDev: TJvHidDevice);
var idx: Integer;
begin
  if (HidDev.VendorName = 'www.masterkit.ru') and
     (HidDev.ProductName = 'MP710') then begin
    idx := FDevicesList.IndexOfObject(HidDev);
    if idx >0 then begin
      DoMP710Removal(UIDs[idx]);
      FDevicesList.BeginUpdate;
      (FDevicesList.Objects[idx] as TJvHidDevice).Free;
      FDevicesList.Delete(idx);
      FDevicesList.EndUpdate;
    end;
  end;
end;
// Events support
procedure TJvHidMP710ControllersList.DoMP710Arrival(UID: Integer);
begin
  if Assigned(FMP710ArrivalEvent) then begin
    FMP710ArrivalEvent(UID);
  end;
end;
procedure TJvHidMP710ControllersList.DoMP710Removal(UID: Integer);
begin
  if Assigned(FMP710RemovalEvent) then begin
    FMP710RemovalEvent(UID);
  end;
end;
// Property support
function TJvHidMP710ControllersList.GetCount: Integer;
begin
  Result := FDevicesList.Count;
end;
function TJvHidMP710ControllersList.GetUID(Index: Integer): Integer;
begin
  Result := StrToIntDef(FDevicesList.Strings[Index], -1);
end;
function TJvHidMP710ControllersList.GetMP710Device(Index: Integer): TJvHidDevice;
begin
  Result := FDevicesList.Objects[Index] as TJvHidDevice;
end;
function TJvHidMP710ControllersList.GetIndexOfUID(UID: Integer): Integer;
begin
  Result := FDevicesList.IndexOf(IntToStr(UID));
end;
function TJvHidMP710ControllersList.GetMP710DeviceByUID(UID: Integer): TJvHidDevice;
begin
  Result := FDevicesList.Objects[FDevicesList.IndexOf(IntToStr(UID))] as TJvHidDevice;
end;
function TJvHidMP710ControllersList.GetUIDsList;
begin
  Result := FDevicesList.CommaText;
end;
function TJvHidMP710ControllersList.GetMP710PORTkkEnabled(UID: Integer; N: Byte): Boolean;
var
  REG, COM: Byte;
  CMD, PRG: Word;
  HidDev: TJvHidDevice;
begin
  HidDev := GetMP710DeviceByUID(UID);
  GetPORTkk(HidDev, N, REG, COM, CMD, PRG);
  Result := REG > 64;
end;
procedure TJvHidMP710ControllersList.SetMP710PORTkkEnabled(UID: Integer; N: Byte; Value: Boolean);
var
  REG, COM: Byte;
  CMD, PRG: Word;
  HidDev: TJvHidDevice;
begin
  HidDev := GetMP710DeviceByUID(UID);
  REG := Ord(Value) shl 7;
  COM := 0;
  CMD := 0;
  PRG := 0;
  SetPORTkk(HidDev, N, REG, COM, CMD, PRG);
end;
// Utilites
procedure TJvHidMP710ControllersList.ClearBuffers;
var i: Integer;
begin
  for i := Low(BUFI) to High(BUFI) do begin
    BUFI[i] := 0;
    BUFO[i] := 0;
  end;
end;
function TJvHidMP710ControllersList.GetUID(HidDev: TJvHidDevice; var UID: Integer): Boolean;
var i: Integer;
begin
  Result := False;
  UID := -1;
  ClearBuffers;
  i := 3;
  // Чтение уникального идентификатора устройства MP710
  BUFO[1] := $1D;

  while (Not Result) and (i>0) do begin
    if HidDev.SetFeature(BUFO[0], 9) then
      if HidDev.GetFeature(BUFI[0], 9) then
        if BUFI[1] = $1D then begin
          Result := True;
          UID := BUFI[5] shl 24 + BUFI[6] shl 16 + BUFI[7] shl 8 + BUFI[8];
          Break;
        end;
    Dec(i);
  end;
end;
function TJvHidMP710ControllersList.SetPORTkk(HidDev: TJvHidDevice; N, REG, COM: Byte; CMD, PRG: Word): Boolean;
var i: Integer;
begin
  Result := False;
  ClearBuffers;
  i := 3;
  // Запись состояния порта
  BUFO[1] := $63;
  BUFO[2] := N;
  BUFO[3] := REG;
  BUFO[4] := COM;
  BUFO[5] := CMD shr 8;
  BUFO[6] := CMD and $FF;
  BUFO[7] := PRG shr 8;
  BUFO[8] := PRG and $FF;

  while (Not Result) and (i>0) do begin
    if HidDev.SetFeature(BUFO[0], 9) then
      if HidDev.GetFeature(BUFI[0], 9) then begin
        Result := (BUFI[1] = $63) and
                  (BUFI[2] = N) and
                  (BUFI[3] = REG) and
                  (BUFI[4] = COM) and
                  (BUFI[5] = BUFO[5]) and
                  (BUFI[6] = BUFO[6]) and
                  (BUFI[7] = BUFO[7]) and
                  (BUFI[8] = BUFO[8]);
        Break;
      end;
    Dec(i);
  end;
end;
function TJvHidMP710ControllersList.GetPORTkk(HidDev: TJvHidDevice; N: Byte; var REG, COM: Byte; var CMD, PRG: Word): Boolean;
var i: Integer;
begin
  Result := False;
  ClearBuffers;
  i := 3;
  // Чтение состояния порта
  BUFO[1] := $36;
  BUFO[2] := N;

  while (Not Result) and (i>0) do begin
    if HidDev.SetFeature(BUFO[0], 9) then
      if HidDev.GetFeature(BUFI[0], 9) then begin
        Result := (BUFI[1] = $36) and (BUFI[2] = N);
        REG := BUFI[3];
        COM := BUFI[4];
        CMD := (BUFI[5] shl 8) + BUFI[6];
        PRG := (BUFI[7] shl 8) + BUFI[8];
        Break;
      end;
    Dec(i);
  end;
end;

initialization
  HID := TJvHidDeviceController.Create(nil);
  HID.OnArrival := EvHandler.HIDArrival;
  HID.OnRemoval := EvHandler.HIDRemoval;

finalization
  for i := Low(MP710) to High(MP710) do
    if Assigned(MP710[i].HID) then
      MP710[i].HID.Free;

  if Assigned(MP724) then MP724.Free;
  if Assigned(HID) then HID.Free;
end.
