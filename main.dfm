object frmMain: TfrmMain
  Left = 0
  Top = 0
  BorderStyle = bsSingle
  Caption = 'M710 cPanel'
  ClientHeight = 383
  ClientWidth = 561
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object sbInfo: TStatusBar
    Left = 0
    Top = 364
    Width = 561
    Height = 19
    Panels = <>
    SimplePanel = True
  end
  object pDevices: TPanel
    Left = 0
    Top = 0
    Width = 561
    Height = 284
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
  end
  object pControls: TPanel
    Left = 0
    Top = 284
    Width = 561
    Height = 80
    Align = alBottom
    BevelOuter = bvLowered
    Caption = 'pControls'
    TabOrder = 2
  end
  object tIcon: TTrayIcon
    Icons = tIcons
    Left = 48
    Top = 16
  end
  object tIcons: TImageList
    Left = 88
    Top = 16
  end
  object tsbInfo: TTimer
    Tag = 500
    Enabled = False
    OnTimer = tsbInfoTimer
    Left = 8
    Top = 16
  end
end
