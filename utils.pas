  unit Utils;

interface

uses
  WinApi.Windows , System.Classes, System.SysUtils, JvHidControllerClass;

type
  TBUFF = array [0..9] of byte;

  TEventHandlers = class
    procedure HIDArrival(HidDev: TJvHidDevice);
    procedure HIDRemoval(HidDev: TJvHidDevice);
  end;

  THidDevice = record
    ID: DWORD;
    HID: TJvHidDevice;
  end;

  procedure ClearBuffers;

  function SetMP710PORTkk(ID: DWORD; N, REG, COM: Byte; CMD, PRG: Word): Boolean;
  function GetMP710PORTkk(ID: DWORD; N: Byte; var REG, COM: Byte; var CMD, PRG: Word): Boolean;
  function SetMP710PORTkkEnabled(ID: DWORD; N: Byte; Value: Boolean): Boolean;
  function GetMP710PORTkkEnabled(ID: DWORD; N: Byte; var Value: Boolean): Boolean;

  function GetDeviceIndex (ID: DWORD): Integer;

  procedure InitializeUtils;
  procedure FinalizeUtils;

var
  EvHandler: TEventHandlers;
  HID: TJvHidDeviceController;
  MP710: array of THidDevice;
  BUFI, BUFO: TBUFF;
  i: Integer;

const
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

      frmMain.SetItems(ID);
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

        frmMain.SetItemsOff(MP710[i].ID);

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
  FillChar(BUFI, 9, #0);
end;
function SetMP710PORTkk(ID: DWORD; N, REG, COM: Byte; CMD, PRG: Word): Boolean;
var
  i, hID: Integer;
begin
  Result := False;

  hID := GetDeviceIndex(ID);
  if hID < 0 then Exit;

  ClearBuffers;
  i := 3;
  // Запись состояния порта
  asm
    lea edi, BUFO
    mov al, N
    mov ah, REG
    mov bl, COM
    mov cx, CMD
    xchg ch, cl
    mov dx, PRG
    xchg dh, dl

    mov byte ptr [edi + 1], CMD_PORT_SET
    mov [edi + 2], al   // N
    mov [edi + 3], ah   // REG
    mov [edi + 4], bl   // COM
    mov [edi + 5], cx   // CMD
    mov [edi + 7], dx   // PRG
  end;

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

  if not Result then
    frmMain.sInfo('Set to M710[' + IntToStr(ID) + ']:' + IntToStr(N) + ' failed !!!');
end;
function GetMP710PORTkk(ID: DWORD; N: Byte; var REG, COM: Byte; var CMD, PRG: Word): Boolean;
var
  i, hID: Integer;
begin
  Result := False;

  hID := GetDeviceIndex(ID);
  if hID < 0 then Exit;

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

  if not Result then
    frmMain.sInfo('Get from M710[' + IntToStr(ID) + ']:' + IntToStr(N) + ' failed !!!');
end;
function SetMP710PORTkkEnabled(ID: DWORD; N: Byte; Value: Boolean): Boolean;
var
  REG, COM: Byte;
  CMD, PRG: Word;
begin
  //Result := False;

  REG := Ord(Value) shl 7;
  COM := 0;
  CMD := 0;
  PRG := 0;
  Result := SetMP710PORTkk(ID, N, REG, COM, CMD, PRG);
end;
function GetMP710PORTkkEnabled(ID: DWORD; N: Byte; var Value: Boolean): Boolean;
var
  REG, COM: Byte;
  CMD, PRG: Word;
begin
  //Result := False;

  Result := GetMP710PORTkk(ID, N, REG, COM, CMD, PRG);
  Value := REG > 64;
end;

// Utilites
function GetDeviceIndex (ID: DWORD): Integer;
begin
  Result := -1;

  for i := Low(MP710) to High(MP710) do
    if MP710[i].ID = ID then begin
      Result := i;
      Break;
    end;
  if Result < 0 then
    frmMain.sInfo('Device MP710[' + IntToStr(ID) + '] not found !!!');
end;

procedure InitializeUtils;
begin
  if not Assigned(HID) then begin
    HID := TJvHidDeviceController.Create(frmMain);
    HID.OnArrival := EvHandler.HIDArrival;
    HID.OnRemoval := EvHandler.HIDRemoval;
  end;
end;
procedure FinalizeUtils;
var
  i: Integer;
begin
  for i := Low(MP710) to High(MP710) do
    if Assigned(MP710[i].HID) then
      MP710[i].HID.Free;
  if Assigned(HID) then HID.Free;
end;
initialization
//  HID := TJvHidDeviceController.Create(nil);
//  HID.OnArrival := EvHandler.HIDArrival;
//  HID.OnRemoval := EvHandler.HIDRemoval;

finalization
//  for i := Low(MP710) to High(MP710) do
//    if Assigned(MP710[i].HID) then
//      MP710[i].HID.Free;
//  if Assigned(HID) then HID.Free;
end.
