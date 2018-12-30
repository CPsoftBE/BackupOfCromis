object fMain: TfMain
  Left = 0
  Top = 0
  Caption = 'SimpleStorage XML Builder '
  ClientHeight = 562
  ClientWidth = 784
  Color = clBtnFace
  Constraints.MinHeight = 600
  Constraints.MinWidth = 800
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  DesignSize = (
    784
    562)
  PixelsPerInch = 96
  TextHeight = 13
  object btnExample1: TButton
    Left = 22
    Top = 20
    Width = 75
    Height = 25
    Caption = 'Example 1'
    TabOrder = 0
    OnClick = btnExample1Click
  end
  object mmResultingXML: TMemo
    Left = 120
    Top = 8
    Width = 656
    Height = 547
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 1
  end
  object btnExample2: TButton
    Left = 22
    Top = 64
    Width = 75
    Height = 25
    Caption = 'Example 2'
    TabOrder = 2
    OnClick = btnExample2Click
  end
end
