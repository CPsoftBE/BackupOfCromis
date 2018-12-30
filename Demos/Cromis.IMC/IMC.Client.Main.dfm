object fMain: TfMain
  Left = 0
  Top = 0
  Caption = 'fMain'
  ClientHeight = 434
  ClientWidth = 657
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object lbServerAddress: TLabel
    Left = 8
    Top = 16
    Width = 74
    Height = 13
    Caption = 'Server Address'
  end
  object ListBox1: TListBox
    Left = 168
    Top = 8
    Width = 473
    Height = 409
    ItemHeight = 13
    TabOrder = 0
  end
  object eServerAddress: TEdit
    Left = 8
    Top = 35
    Width = 154
    Height = 21
    TabOrder = 1
    Text = 'localhost:10610'
  end
  object btnSend: TButton
    Left = 48
    Top = 88
    Width = 75
    Height = 25
    Caption = 'Send'
    TabOrder = 2
    OnClick = btnSendClick
  end
  object btnSendFiltered: TButton
    Left = 16
    Top = 134
    Width = 135
    Height = 25
    Caption = 'Encrypted / Compressed'
    TabOrder = 3
    OnClick = btnSendFilteredClick
  end
end
