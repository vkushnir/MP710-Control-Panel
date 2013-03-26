unit main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, System.IniFiles, System.TypInfo, Vcl.Graphics, Vcl.Controls,
  Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls,JvHidControllerClass, Vcl.ExtCtrls,
  Vcl.ImgList, Vcl.Menus, Vcl.StdCtrls, Utils, Vcl.Imaging.pngimage;

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
    Image1: TImage;
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
  PNG: TPNGImage;
  img: TPicture;
begin
  {$IFDEF DEBUG}
    fn := IncludeTrailingPathDelimiter(ExpandFileName(ExtractFilePath(Application.ExeName) +
      '\..\..')) + ChangeFileExt(ExtractFileName(Application.ExeName), '.ini');
  {$ELSE}
    fn := IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName)) +
      ChangeFileExt(ExtractFileName(Application.ExeName), '.ini');
  {$ENDIF}
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
  // Read main settings
  DSize.Columns := cfg.ReadInteger(sMain, iWidth, 4);
  DSize.Rows := cfg.ReadInteger(sMain, iHeight, 4);
  {$IFDEF DEBUG}
    cfg.WriteDateTime('DEBUG', 'DATE', now);
    cfg.WriteBool(sMain, iFullSCreen, FullScreen);
    cfg.WriteInteger(sMain, iWidth, DSize.Columns);
    cfg.WriteInteger(sMain, iHeight, DSize.Rows);
    ImgLoc := IncludeTrailingPathDelimiter(
      cfg.ReadString(sMain, iImgLoc, IncludeTrailingPathDelimiter(
      ExpandFileName(ExtractFilePath(Application.ExeName) +
      '\..\..')) + 'Images'));
  {$ELSE}
    ImgLoc := IncludeTrailingPathDelimiter(
      cfg.ReadString(sMain, iImgLoc, IncludeTrailingPathDelimiter(
      ExtractFilePath(Application.ExeName)) + 'Images'));
  {$ENDIF}

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
        ImageName.Enabled := cfg.ReadString(S, iImgE, 'noimage');
        ImageName.Disabled := cfg.ReadString(S, iImgD,
            ChangeFileExt(ImageName.Enabled, '_d'+ExtractFileExt(ImageName.Enabled)));
        GroupID := cfg.ReadInteger(S, iGroup, 0);
        DeviceID := cfg.ReadInteger(S, iDevID, -1);
        DevicePort := cfg.ReadInteger(S, iDevPort, -1);
        PrevDeviceID := cfg.ReadInteger(S, iPDevID, -1);
        PrevPort := cfg.ReadInteger(S, iPDevPort, -1);
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
  BWidth := pDevices.ClientWidth / DSize.Columns;
  BHeight := pDevices.ClientHeight / DSize.Rows;
  for i := 1 to High(AItems) do begin
    AItems[i].AObject := TImage.Create(frmMain);

    with AItems[i].AIcons do begin
      Enabled := TPicture.Create;
      Disabled := TPicture.Create;
      try
        Enabled.LoadFromFile(ImgLoc + AItems[i].ImageName.Enabled);
      finally
        sInfo('Fail image to load: ' + ImgLoc + AItems[i].ImageName.Enabled);
      end;
      try
        Disabled.LoadFromFile(ImgLoc + AItems[i].ImageName.Disabled);
      finally
        sInfo('Fail image to load: ' + ImgLoc + AItems[i].ImageName.Disabled);
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
      Stretch := True;
      Tag := i;
      Name := 'ELEMENT_' + IntToStr(i+1);
      if FileExists(ImgLoc + AItems[i].ImageName.Disabled) then
          Picture.LoadFromFile(ImgLoc + AItems[i].ImageName.Disabled);
      OnClick := ButtonClick;
      Parent := PDevices;
    end;
  end;
end;

procedure TfrmMain.ButtonClick(Sender: TObject);
var
  n, i, g: Integer;
  btn: TImage;
  chk: Boolean;
  Val: Boolean;
  ID, PORT: Integer;
begin
  btn := Sender as TImage;
  n := (Sender as TControl).Tag;
  g := AItems[n].GroupID;

  ID := AItems[n].DeviceID;
  PORT := AItems[n].DevicePort;
  GetMP710PORTkkEnabled(ID, PORT, VAL);

{  case AGroups[AItems[n].GroupID] of
    gmFree, gmSame: SetItem(Sender as TControl, not AItems[n].Enabled);
    gmSingleOn: SetItem(Sender as TControl, True);
    gmSingleOff: SetItem(Sender as TControl, False);
  end;
  for i := 0 to pDevices.ControlCount-1 do begin
    if (pDevices.Controls[i].Tag > 0) and (pDevices.Controls[i] is TImage) and
       (pDevices.Controls[i].Tag <> n) and (AItems[pDevices.Controls[i].Tag].GroupID = AItems[n].GroupID) then
      case AGroups[AItems[n].GroupID] of
        //gmFree,
        gmSame: SetItem(pDevices.Controls[i], AItems[n].Enabled);
        gmSingleOn: SetItem(pDevices.Controls[i], False);
        gmSingleOff: SetItem(pDevices.Controls[i], True);
      end;
  end;}
  case AGroups[g] of
    gmFree, gmSame:
      Val := not Val;
    gmSingleOn:
      Val := True;
    gmSingleOff:
      Val := False;
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
end;
procedure TfrmMain.SetItem(Idx: Integer; Value: Boolean; SetHW: Boolean = False);
var
  ID: DWORD;
  N: WORD;
  Val: Boolean;
begin
  if SetHW then begin
    SetMP710PORTkkEnabled(AItems[Idx].DeviceID, AItems[Idx].DevicePort, Val);
    GetMP710PORTkkEnabled(AItems[Idx].DeviceID, AItems[Idx].DevicePort, Val);
  end;

  if Value then
    if Assigned(AItems[Idx].AObject.Picture) and Assigned(AItems[Idx].AIcons.Enabled) then
      AItems[Idx].AObject.Picture.Assign(AItems[Idx].AIcons.Enabled)
  else
    if Assigned(AItems[Idx].AObject.Picture) and Assigned(AItems[Idx].AIcons.Disabled) then
      AItems[Idx].AObject.Picture.Assign(AItems[Idx].AIcons.Disabled);
end;

// System Messages
procedure TfrmMain.sInfo(const msg: String);
begin
  if not Assigned(sInfoBUFF) then sInfoBUFF := TStringList.Create;
  if sInfoBuff.Count < 1 then begin
    sbInfo.SimpleText := trim(msg);
    tsbInfo.Interval := tsbInfo.Tag;
  end else
    sbInfo.SimpleText := '[' + IntToStr(sInfoBUFF.Count+1) + '] ' + sbInfo.SimpleText;
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
    sbInfo.SimpleText := '[' + IntToStr(sInfoBUFF.Count+1) + '] ' + sbInfo.SimpleText;
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

procedure TfrmMain.SetItems(const ID: DWORD);
var
  i: Integer;
  Val, n: Boolean;
begin
  for i := Low(AItems) to High(AItems) do
    if AItems[i].DeviceID = ID then begin
      n := GetMP710PORTkkEnabled(ID, AItems[i].DevicePort, Val);
      if n then
        SetItem(i, Val, False)
      else
        SetItem(i, False, False);
    end;
end;

procedure TfrmMain.SetItemsOff(const ID: DWORD);
var
  i: Integer;
  Val, n: Boolean;
begin
  for i := Low(AItems) to High(AItems) do
    if AItems[i].DeviceID = ID then
      SetItem(i, False, False);
end;

end.
