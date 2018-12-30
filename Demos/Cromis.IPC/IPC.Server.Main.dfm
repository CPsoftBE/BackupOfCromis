object fMain: TfMain
  Left = 0
  Top = 0
  Caption = 'IPC Server'
  ClientHeight = 462
  ClientWidth = 740
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
    740
    462)
  PixelsPerInch = 96
  TextHeight = 13
  object lbServerName: TLabel
    Left = 10
    Top = 16
    Width = 62
    Height = 13
    Caption = 'Server Name'
  end
  object eServerName: TEdit
    Left = 10
    Top = 35
    Width = 142
    Height = 21
    TabOrder = 0
    Text = 'TestServer'
  end
  object ListBox1: TListBox
    Left = 160
    Top = 10
    Width = 572
    Height = 446
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 13
    TabOrder = 1
  end
  object btnStart: TButton
    Left = 40
    Top = 72
    Width = 75
    Height = 25
    Caption = 'Start'
    TabOrder = 2
    OnClick = btnStartClick
  end
end
