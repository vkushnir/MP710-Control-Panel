unit Main;

interface

uses
  Utils,
  Winapi.Windows,
  System.Classes, System.SysUtils, System.IniFiles, System.TypInfo,
  Vcl.Forms, Vcl.Controls, Vcl.ComCtrls, Vcl.ExtCtrls,
  Vcl.Graphics, Vcl.Imaging.pngimage, Vcl.ImgList;

type
  TGroupMode = (gmFree, gmSame, gmSingleOn, gmSingleOff);
  TTableSize = record
    Rows: Integer;
    Columns: Integer;
  end;
  TSize = record
    Left: Integer;
    Top: Integer;
    Width: Integer;
    Height: Integer;
  end;
  TImageName = record
    Enabled: String;
    Disabled: String;
  end;
  TIcons = record
    Enabled: TPicture;
    Disabled: TPicture;
  end;
  TItemData = record
    AObject: TImage;
    AIcons: TIcons;
    Size: TSize;
    Title: String;
    ImageName: TImageName;
    GroupID: Integer;
    DeviceID: Integer;
    DevicePort: Byte;
    PrevDeviceID: Integer;
    PrevPort: Byte;
    PrevPortState: Boolean;
    PrevPortTime: Integer;
    Enabled: Boolean;
  end;

  TfrmMain = class(TForm)
    sbInfo: TStatusBar;
    tIcon: TTrayIcon;
    tIcons: TImageList;
    tsbInfo: TTimer;
    pDevices: TPanel;
    pControls: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure tsbInfoTimer(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ButtonClick(Sender: TObject);
  private
    FwndRect: TSize;
    ImgLoc: String;
    DSize: TTableSize;
    AItems: array of TItemData;
    AGroups: array of TGroupMode;
    cfg: TINIFile;
    sInfoBUFF: TStringList;
    WindowLocked: Boolean;
    procedure SetItem(Idx: Integer; Value: Boolean; SetHW: Boolean = False);
  public
    procedure sInfo(const msg: String); overload;
    procedure sInfo(const msg: String; const sec: Integer); overload;
    procedure SetItems (const ID: DWORD);
    procedure SetItemsOff (const ID: DWORD);
  end;

var
  frmMain: TfrmMain;

const
  sMain = 'GENERAL';
  sItem = 'ELEMENT#';
  sGrps = 'GROUPS';
  iGrp = 'GROUP#';
  iWidth = 'Columns';
  iHeight = 'Rows';
  iFullScreen = 'FullScreen';
  iStretch = 'StretchEnable';
  iStretchP = 'StretchProportional';
  iVSpan = 'VSpan';
  iHSpan = 'HSpan';
  iLeft = 'Column';
  iTop = 'Row';
  iGroup = 'GroupID';
  iDevID = 'DeviceID';
  iDevPort = 'DevicePort';
  iPDevID = 'PrevDeviceID';
  iPDevPort = 'PrevDevicePort';
  iPDevPortS = 'PrevDevicePortState';
  iPDevPortT = 'PrevDevicePortTime';

  iTitle = 'Title';
  iImgE = 'ImageEnabled';
  iImgD = 'ImageDisabled';
  iImgLoc = 'ImagesFolder';

implementation

{$R *.dfm}

procedure TfrmMain.FormClose(Sender: TObject; var Action: TCloseAction);
var
  i: Integer;
begin
  FinalizeUtils;

  for i := Low(AItems) to High(AItems) do begin
    if Assigned(AItems[i].AObject) then AItems[i].AObject.Free;
    if Assigned(AItems[i].AIcons.Enabled) then AItems[i].AIcons.Enabled.Free;
    if Assigned(AItems[i].AIcons.Disabled) then AItems[i].AIcons.Disabled.Free;
  end;
  cfg.Free;
  sInfoBUFF.Free;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
var
  FullScreen: Boolean;
  ItemList: TStringList;
  i, n: Integer;
  S, fn: String;
  BWidth, BHeight: Real;
begin
//  {$IFDEF DEBUG}
//    fn := IncludeTrailingPathDelimiter(ExpandFileName(ExtractFilePath(Application.ExeName) +
//      '\..\..')) + ChangeFileExt(ExtractFileName(Application.ExeName), '.ini');
//  {$ELSE}
    fn := IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName)) +
      ChangeFileExt(ExtractFileName(Application.ExeName), '.ini');
//  {$ENDIF}
  cfg := TINIFile.Create(fn);

  FullScreen := cfg.ReadBool(sMain, iFullSCreen, False);
  if FullScreen then begin
    FwndRect.Left := Left;
    FwndRect.Top := Top;
    FwndRect.Width:= Width;
    FwndRect.Height :=  Height;

    BorderStyle := bsNone;
    FormStyle   := fsStayOnTop;
    Left        := 0;
    Top         := 0;
    Width       := Screen.Width;
    Height      := Screen.Height;
  end;
  // Read Main settings
  DSize.Columns := cfg.ReadInteger(sMain, iWidth, 4);
  DSize.Rows := cfg.ReadInteger(sMain, iHeight, 4);
  {$IFDEF DEBUG}
    cfg.WriteDateTime('DEBUG', 'DATE', now);
    cfg.WriteBool(sMain, iFullSCreen, FullScreen);
    cfg.WriteInteger(sMain, iWidth, DSize.Columns);
    cfg.WriteInteger(sMain, iHeight, DSize.Rows);
//    ImgLoc := IncludeTrailingPathDelimiter(
//      cfg.ReadString(sMain, iImgLoc, IncludeTrailingPathDelimiter(
//      ExpandFileName(ExtractFilePath(Application.ExeName) +
//      '\..\..')) + 'Images'));
//  {$ELSE}
  {$ENDIF}
    ImgLoc := IncludeTrailingPathDelimiter(
      cfg.ReadString(sMain, iImgLoc, IncludeTrailingPathDelimiter(
      ExtractFilePath(Application.ExeName)) + 'Images'));
//  {$ENDIF}

  // Read INI File
  ItemList := TStringList.Create;
  try
    cfg.ReadSections(ItemList);
    if ItemList.Count <= 0 then Exit;
    i := 0;
    while i < ItemList.Count do begin
      if Pos(sItem, ItemList[i]) = 1 then
        Inc(i)
      else
        ItemList.Delete(i);
    end;
    if ItemList.Count <= 0 then Exit;
    SetLength(AItems, ItemList.Count+1);
    for i := 1 to ItemList.Count do begin
      S := ItemList[i-1];
      with AItems[i] do begin
        Size.Left := cfg.ReadInteger(S, iLeft, 0);
        Size.Top := cfg.ReadInteger(S, iTop, 0);
        Size.Width := cfg.ReadInteger(S, iHSpan, 1);
        Size.Height := cfg.ReadInteger(S, iVSpan, 1);
        Title := cfg.ReadString(S, iTitle, S);
        GroupID := cfg.ReadInteger(S, iGroup, 0);
        DeviceID := cfg.ReadInteger(S, iDevID, 0);
        DevicePort := cfg.ReadInteger(S, iDevPort, 0);
        PrevDeviceID := cfg.ReadInteger(S, iPDevID, 0);
        PrevPort := cfg.ReadInteger(S, iPDevPort, 0);
        PrevPortState := cfg.ReadBool(S, iPDevPortS, False);
        PrevPortTime := cfg.ReadInteger(S, iPDevPortT, 100);
        Enabled := False;
      end;
    end;
    ItemList.Clear;
    cfg.ReadSectionValues(sGrps, ItemList);
    if ItemList.Count <= 0 then begin
      SetLength(AGroups, 1);
    end else begin
      SetLength(AGroups, ItemList.Count + 1);
      for i := 0 to ItemList.Count-1 do begin
        if Pos(iGRP, ItemList[i]) = 1 then begin
          n := StrToIntDef(Copy(ItemList.Names[i], Length(iGRP)+1, MaxInt), -1);
          S := ItemList.ValueFromIndex[i];
          AGroups[n] := TGroupMode(System.TypInfo.GetEnumValue(TypeInfo(TGroupMode),S));
        end;
      end;
    end;
    AGroups[0] := gmFree;
  finally
    ItemList.Free;
  end;

  // Generate buttons
WindowLocked := LockWindowUpdate(frmMain.Handle);
try
  BWidth := pDevices.ClientWidth / DSize.Columns;
  BHeight := pDevices.ClientHeight / DSize.Rows;
  for i := 1 to High(AItems) do begin
    AItems[i].AObject := TImage.Create(frmMain);
    S := sItem + IntToStr(i);

    with AItems[i].AIcons do begin
      Enabled := TPicture.Create;
      Disabled := TPicture.Create;
      try
        fn := ImgLoc + cfg.ReadString(S, iImgE, 'noimage');
        Enabled.LoadFromFile(fn);
      except
        sInfo('Fail image to load: ' + fn);
      end;
      try
        fn := ImgLoc + cfg.ReadString(S, iImgD, ExtractFileName(ChangeFileExt(fn, '_d'+ExtractFileExt(fn))));
        Disabled.LoadFromFile(fn);
      except
        sInfo('Fail image to load: ' + fn);
      end;
    end;

    with AItems[i].AObject do begin
      Left := Round(AItems[i].Size.Left * BWidth);
      Top := Round(AItems[i].Size.Top * BHeight);
      Width := Round(AItems[i].Size.Width * BWidth);
      Height := Round(AItems[i].Size.Height * BHeight);
      Cursor := crHandPoint;
      Proportional := True;
      Transparent := True;
      Center := True;
      Stretch := cfg.ReadBool(S, iStretch, True);
      Proportional := cfg.ReadBool(S, iStretchP, True);
      Tag := i;
      Name := 'ELEMENT_' + IntToStr(i);
      Picture.Assign(AItems[i].AIcons.Disabled);
      OnClick := ButtonClick;
      Hint := AItems[i].Title;
      ShowHint := True;
      Parent := PDevices;
    end;
  end;
finally
  if WindowLocked then
    LockWindowUpdate(0);
end;

  InitializeUtils;
end;

procedure TfrmMain.ButtonClick(Sender: TObject);
var
  n, i, g, p: Integer;
  Val: Boolean;
  ID, PORT: Integer;
begin
WindowLocked := LockWindowUpdate(frmMain.Handle);
try
  n := (Sender as TControl).Tag;
  g := AItems[n].GroupID;

  ID := AItems[n].DeviceID;
  PORT := AItems[n].DevicePort;
  GetMP710PORTkkEnabled(ID, PORT, VAL);

  case AGroups[g] of
    gmFree, gmSame:
      Val := not Val;
    gmSingleOn:
      Val := True;
    gmSingleOff:
      Val := False;
  end;

  if (AItems[n].PrevDeviceID > 0) and (AItems[n].PrevPort > 0) then begin
    p := -1;
    for i := Low(AItems) to High(AItems) do
      if (AItems[i].DeviceID = AItems[n].PrevDeviceID) and
         (AItems[i].DevicePort = AItems[n].PrevPort) then begin
        p := i;
        Break;
      end;
    if p > 0 then begin
      SetItem(p, AItems[n].PrevPortState, True);
      AItems[p].AObject.Update;
      Sleep(AItems[n].PrevPortTime div 2);
      SetItem(p, not AItems[n].PrevPortState, True);
      AItems[p].AObject.Update;
      Sleep(AItems[n].PrevPortTime div 2);
    end else begin
      SetMP710PORTkkEnabled(AItems[n].PrevDeviceID, AItems[n].PrevPort, AItems[n].PrevPortState);
      Sleep(AItems[n].PrevPortTime div 2);
      SetMP710PORTkkEnabled(AItems[n].PrevDeviceID, AItems[n].PrevPort, not AItems[n].PrevPortState);
      Sleep(AItems[n].PrevPortTime div 2);
    end;
  end;

  SetItem(n, Val, True);

  for i := Low(AItems) to High(AItems) do begin
    if (i <> n) and (AItems[i].GroupID = g) then
      case AGroups[g] of
        //gmFree,
        gmSame:
          SetItem(i, Val, True);
        gmSingleOn, gmSingleOff:
          SetItem(i, not Val, True);
      end;
  end;
finally
  if WindowLocked then
    LockWindowUpdate(0);
end;
end;
procedure TfrmMain.SetItem(Idx: Integer; Value: Boolean; SetHW: Boolean = False);
var
  Val: Boolean;
begin
  Val := Value;
  if SetHW then begin
    SetMP710PORTkkEnabled(AItems[Idx].DeviceID, AItems[Idx].DevicePort, Val);
    GetMP710PORTkkEnabled(AItems[Idx].DeviceID, AItems[Idx].DevicePort, Val);
  end;

  if Val then begin
    if Assigned(AItems[Idx].AObject.Picture) and Assigned(AItems[Idx].AIcons.Enabled) then
      AItems[Idx].AObject.Picture.Assign(AItems[Idx].AIcons.Enabled);
  end else begin
    if Assigned(AItems[Idx].AObject.Picture) and Assigned(AItems[Idx].AIcons.Disabled) then
      AItems[Idx].AObject.Picture.Assign(AItems[Idx].AIcons.Disabled);
  end;
end;

procedure TfrmMain.SetItems(const ID: DWORD);
var
  i: Integer;
  Val, n: Boolean;
begin
WindowLocked := LockWindowUpdate(frmMain.Handle);
try
  for i := Low(AItems) to High(AItems) do
    if AItems[i].DeviceID = ID then begin
      n := GetMP710PORTkkEnabled(ID, AItems[i].DevicePort, Val);
      if n then
        SetItem(i, Val, False)
      else
        SetItem(i, False, False);
    end;
finally
  if WindowLocked then
    LockWindowUpdate(0);
end;
end;

procedure TfrmMain.SetItemsOff(const ID: DWORD);
var
  i: Integer;
begin
WindowLocked := LockWindowUpdate(frmMain.Handle);
try
  for i := Low(AItems) to High(AItems) do
    if AItems[i].DeviceID = ID then
      SetItem(i, False, False);
finally
  if WindowLocked then
    LockWindowUpdate(0);
end;
end;

// System Messages
procedure TfrmMain.sInfo(const msg: String);
begin
  if not Assigned(sInfoBUFF) then sInfoBUFF := TStringList.Create;
  if sInfoBuff.Count < 1 then begin
    sbInfo.SimpleText := trim(msg);
    tsbInfo.Interval := tsbInfo.Tag;
  end else
    if sInfoBUFF.Names[0] = '' then
      sbInfo.SimpleText := '[' + IntToStr(sInfoBUFF.Count+1) + '] ' + sInfoBUFF[0]
    else
      sbInfo.SimpleText := '[' + IntToStr(sInfoBUFF.Count+1) + '] ' + sInfoBUFF.Names[0];
  sInfoBUFF.Add(msg);
  if not tsbInfo.Enabled then tsbInfo.Enabled := True;
end;
procedure TfrmMain.sInfo(const msg: String; const sec: Integer);
begin
  if not Assigned(sInfoBUFF) then sInfoBUFF := TStringList.Create;
  if sInfoBuff.Count < 1 then begin
    sbInfo.SimpleText := trim(msg);
    tsbInfo.Interval := sec * 1000;
  end else
    if sInfoBUFF.Names[0] = '' then
      sbInfo.SimpleText := '[' + IntToStr(sInfoBUFF.Count+1) + '] ' + sInfoBUFF[0]
    else
      sbInfo.SimpleText := '[' + IntToStr(sInfoBUFF.Count+1) + '] ' + sInfoBUFF.Names[0];
  sInfoBUFF.Add(msg + '=' + IntToStr(sec * 1000));
  if not tsbInfo.Enabled then tsbInfo.Enabled := True;
end;
procedure TfrmMain.tsbInfoTimer(Sender: TObject);
var
  strCnt: String;
begin
  with (Sender as TTimer) do begin
    Enabled := False;
    Interval := Tag;
    if sInfoBUFF.Count > 1 then begin
      sInfoBUFF.Delete(0);
      strCnt := '[' + IntToStr(sInfoBUFF.Count) + '] ';
      if sInfoBUFF.Names[0] = '' then begin
        Interval := Tag;
        sbInfo.SimpleText := strCnt + sInfoBUFF[0];
      end else begin
        Interval := StrToIntDef(sInfoBUFF.ValueFromIndex[0], Tag);
        sbInfo.SimpleText := strCnt + sInfoBUFF.Names[0];
      end;
      Enabled := True;
    end else begin
      sbInfo.SimpleText := '';
      sInfoBUFF.Clear;
    end;
  end;
end;

end.
