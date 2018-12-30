object fMain: TfMain
  Left = 0
  Top = 0
  Caption = 'fMain'
  ClientHeight = 457
  ClientWidth = 736
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    736
    457)
  PixelsPerInch = 96
  TextHeight = 13
  object lbServerPort: TLabel
    Left = 8
    Top = 13
    Width = 55
    Height = 13
    Caption = 'Server Port'
  end
  object ListBox1: TListBox
    Left = 134
    Top = 8
    Width = 595
    Height = 441
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 13
    TabOrder = 0
  end
  object eServerPort: TEdit
    Left = 8
    Top = 32
    Width = 113
    Height = 21
    TabOrder = 1
    Text = '10610'
  end
  object btnStart: TButton
    Left = 24
    Top = 96
    Width = 75
    Height = 25
    Caption = 'Start'
    TabOrder = 2
    OnClick = btnStartClick
  end
end
