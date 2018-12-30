object fMain: TfMain
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'fMain'
  ClientHeight = 453
  ClientWidth = 629
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object lvWatchActions: TListView
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 623
    Height = 400
    Margins.Bottom = 1
    Align = alClient
    Columns = <
      item
        Caption = 'Type'
        Width = 150
      end
      item
        Caption = 'FileName'
        Width = 450
      end>
    TabOrder = 0
    ViewStyle = vsReport
  end
  object pnBottom: TPanel
    AlignWithMargins = True
    Left = 3
    Top = 407
    Width = 623
    Height = 43
    Align = alBottom
    BevelOuter = bvNone
    BorderStyle = bsSingle
    Color = clWhite
    Ctl3D = False
    ParentBackground = False
    ParentCtl3D = False
    TabOrder = 1
    object sbSelectDirectory: TSpeedButton
      Left = 327
      Top = 10
      Width = 23
      Height = 21
      Caption = '...'
      OnClick = sbSelectDirectoryClick
    end
    object eDirectoryName: TEdit
      Left = 7
      Top = 11
      Width = 314
      Height = 19
      TabOrder = 0
      OnDblClick = eDirectoryNameDblClick
    end
    object cbWatchSubdirectories: TCheckBox
      Left = 364
      Top = 12
      Width = 122
      Height = 17
      Caption = 'Watch Subdirectories'
      TabOrder = 1
    end
    object btnStop: TButton
      Left = 557
      Top = 8
      Width = 60
      Height = 25
      Caption = 'Stop'
      Enabled = False
      TabOrder = 2
      OnClick = btnStopClick
    end
    object btnStart: TButton
      Left = 494
      Top = 8
      Width = 61
      Height = 25
      Caption = 'Start'
      TabOrder = 3
      OnClick = btnStartClick
    end
  end
end
