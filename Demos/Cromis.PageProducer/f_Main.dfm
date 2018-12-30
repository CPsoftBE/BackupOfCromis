object fMain: TfMain
  Left = 0
  Top = 0
  Caption = 'fMain'
  ClientHeight = 586
  ClientWidth = 821
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object spVertical: TSplitter
    Left = 409
    Top = 0
    Width = 5
    Height = 586
    ExplicitHeight = 546
  end
  object mmSource: TMemo
    Left = 0
    Top = 0
    Width = 409
    Height = 586
    Align = alLeft
    Ctl3D = True
    ParentCtl3D = False
    TabOrder = 0
  end
  object mmDestination: TMemo
    Left = 414
    Top = 0
    Width = 407
    Height = 586
    Align = alClient
    Ctl3D = True
    ParentCtl3D = False
    TabOrder = 1
  end
  object OpenDialog: TOpenDialog
    Left = 16
    Top = 8
  end
  object MainMenu1: TMainMenu
    Left = 48
    Top = 8
    object miFileGroup: TMenuItem
      Caption = 'File'
      object miOpenFile: TMenuItem
        Caption = 'Open'
        OnClick = miOpenFileClick
      end
    end
    object miActionGroup: TMenuItem
      Caption = 'Action'
      object miParseToStream: TMenuItem
        Caption = 'Parse to Stream'
        OnClick = miParseToStreamClick
      end
    end
  end
end
