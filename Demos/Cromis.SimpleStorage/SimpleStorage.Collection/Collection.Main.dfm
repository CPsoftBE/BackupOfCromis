object fMain: TfMain
  Left = 0
  Top = 0
  Caption = 'Collection Demo'
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
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object btnEnumerateDevice: TButton
    Left = 4
    Top = 8
    Width = 158
    Height = 25
    Caption = 'Enumerate Single Device'
    TabOrder = 0
    OnClick = btnEnumerateDeviceClick
  end
  object btnSelectRandomDocuments: TButton
    Left = 168
    Top = 8
    Width = 186
    Height = 25
    Caption = 'Select Random Documents'
    TabOrder = 1
    OnClick = btnSelectRandomDocumentsClick
  end
  object btnCalcualteSum: TButton
    Left = 360
    Top = 8
    Width = 201
    Height = 25
    Caption = 'Calculate Traffic SUM for all Devices'
    TabOrder = 2
    OnClick = btnCalcualteSumClick
  end
  object lvCollectionLog: TListView
    AlignWithMargins = True
    Left = 3
    Top = 39
    Width = 778
    Height = 520
    Align = alBottom
    Columns = <
      item
        Caption = 'Indentification'
        Width = 450
      end
      item
        Caption = 'InBound'
        Width = 150
      end
      item
        Caption = 'OutBound'
        Width = 150
      end>
    TabOrder = 3
    ViewStyle = vsReport
  end
end
