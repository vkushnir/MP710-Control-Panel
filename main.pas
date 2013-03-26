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
  TItemData = record
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
    function GetUID(var UID: Integer): Boolean;
    procedure SetItem (Ctrl: TControl; Mode: Boolean);
  public
    CurrentDevice: TJvHidDevice;
    Devices: TJvHidMP710ControllersList;
    procedure sInfo(const msg: String); overload;
    procedure sInfo(const msg: String; const sec: Integer); overload;
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
begin
  cfg.Free;
  Devices.Free;
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
begin
  fn := IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName)) +
    ChangeFileExt(ExtractFileName(Application.ExeName), '.ini');
  if FileExists(fn) then
    cfg := TINIFile.Create(fn)
  else begin
    cfg := TINIFile.Create(fn);
    // Save Default values
    // General
    cfg.WriteString(sMain, 'Descr', 'Основные настройки');
    cfg.WriteString(sMain, iFullScreen, 'Запускать в полноэкранном режиме');
    cfg.WriteString(sMain, iImgLoc, 'Папка с изображениями кнопок');
    cfg.WriteString(sMain, iWidth, 'Количество столбцов');
    cfg.WriteString(sMain, iHeight, 'Количество строк');
    // Groups
    cfg.WriteString(sGrps, 'Descr', 'Группы элементов; n - номер группы');
    cfg.WriteString(sGrps, iGrp + 'n', 'gmFree - Независимое переключение; gmSame - Вся группа переключается одинаково; gmSingleOn - Остальные элементы группы всегда включены; gmSingleOff - Остальные элементы группы всегда отключены');
    // Element
    cfg.WriteString(sItem+'n', 'Descr', 'Группы элементов; n - номер элелмента');
    cfg.WriteString(sItem+'n', iTitle, 'Наименование элемента');
    cfg.WriteString(sItem+'n', iDevID, 'Идентификатор MP710');
    cfg.WriteString(sItem+'n', iDevPort, 'Порт MP710');
    cfg.WriteString(sItem+'n', iGroup, 'Идентификатор группы (по умолчанию 0)');
    cfg.WriteString(sItem+'n', iPDevID, 'Идентификатор MP710 для предварительного переключения');
    cfg.WriteString(sItem+'n', iPDevPort, 'Порт MP710 для предварительного переключения');
    cfg.WriteString(sItem+'n', iPDevPortS, 'Состоянение порта MP710 для предварительного переключения');
    cfg.WriteString(sItem+'n', iPDevPortT, 'Время предварительного переключения');
    cfg.WriteString(sItem+'n', iLeft, 'Номер столбца');
    cfg.WriteString(sItem+'n', iTop, 'Номер строки');
    cfg.WriteString(sItem+'n', iHSpan, 'Объединение ячеек по горизонтали');
    cfg.WriteString(sItem+'n', iVSpan, 'Объединение ячеек по вертикали');
    cfg.WriteString(sItem+'n', iImgE, 'Изображения состояния включено');
    cfg.WriteString(sItem+'n', iImgD, 'Изображение состояние отключено');
  end;


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
  {$IFDEF DEBUG}
  cfg.WriteDateTime('DEBUG', 'DATE', now);
  cfg.WriteBool(sMain, iFullSCreen, FullScreen);
  {$ENDIF}
  DSize.Columns := cfg.ReadInteger(sMain, iWidth, 4);
  DSize.Rows := cfg.ReadInteger(sMain, iHeight, 4);
  {$IFDEF DEBUG}
  cfg.WriteInteger(sMain, iWidth, DSize.Columns);
  cfg.WriteInteger(sMain, iHeight, DSize.Rows);
  {$ENDIF}
  ImgLoc := IncludeTrailingPathDelimiter(cfg.ReadString(sMain, iImgLoc, IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName)) + 'Images'));

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
    with TImage.Create(frmMain) do begin
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
  n, i: Integer;
  btn: TImage;
  chk: Boolean;
begin
  btn := Sender as TImage;
  n := (Sender as TControl).Tag;
  case AGroups[AItems[n].GroupID] of
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
  end;
      //Devices.MP710PORTkkEnabled[732, StrToIntDef((Controls[i] as TButton).Hint, -1)] := False;
end;
procedure TfrmMain.SetItem(Ctrl: TControl; Mode: Boolean);
begin
  if Mode then begin
    AItems[Ctrl.Tag].Enabled := True;
    if FileExists(ImgLoc + AItems[Ctrl.Tag].ImageName.Enabled) then
      (Ctrl as TImage).Picture.LoadFromFile(ImgLoc + AItems[Ctrl.Tag].ImageName.Enabled);
  end else begin
    AItems[Ctrl.Tag].Enabled := False;
    if FileExists(ImgLoc + AItems[Ctrl.Tag].ImageName.Disabled) then
      (Ctrl as TImage).Picture.LoadFromFile(ImgLoc + AItems[Ctrl.Tag].ImageName.Disabled);
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

function TfrmMain.GetUID(var UID: Integer): Boolean;
var
  BUFO, BUFI : array [0..9] of byte;
  i: Integer;
begin
  Result := False;
  UID := -1;

  BUFO[1] := $1D;
  i := 3;

  while (Not Result) and (i>0) do begin
    if CurrentDevice.SetFeature(BUFO[0], 9) then
      if CurrentDevice.GetFeature(BUFI[0], 9) then
        if BUFI[1] = $1D then begin
          Result := True;
          UID := BUFI[5] shl 24 + BUFI[6] shl 16 + BUFI[7] shl 8 + BUFI[8];
        end else begin
          Result := False;
          sInfo ('Ошибка чтения уникального номера устройства');
        end;
  end;
end;

end.
