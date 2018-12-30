object fMain: TfMain
  Left = 0
  Top = 0
  Caption = 'SimpleStorage Demo'
  ClientHeight = 504
  ClientWidth = 879
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object spData: TSplitter
    Left = 305
    Top = 0
    Width = 5
    Height = 504
    Beveled = True
    ExplicitLeft = 336
    ExplicitTop = -8
    ExplicitHeight = 589
  end
  object mmSourceData: TMemo
    Left = 0
    Top = 0
    Width = 305
    Height = 504
    Align = alLeft
    TabOrder = 0
    WordWrap = False
  end
  object mmResultsData: TMemo
    Left = 310
    Top = 0
    Width = 314
    Height = 504
    Align = alClient
    TabOrder = 1
    WordWrap = False
  end
  object pnActions: TPanel
    Left = 624
    Top = 0
    Width = 255
    Height = 504
    Align = alRight
    TabOrder = 2
    DesignSize = (
      255
      504)
    object imgLogo: TImage
      Left = 6
      Top = 324
      Width = 243
      Height = 173
      Anchors = [akTop, akRight]
      Stretch = True
    end
    object rgDemoSelection: TRadioGroup
      Left = 6
      Top = 8
      Width = 243
      Height = 310
      Anchors = [akTop, akRight]
      Caption = 'Demo Selection'
      Items.Strings = (
        '1 - Simple Single Value Selection'
        '2 - Simple Values Enumeration'
        '3 - Simple Elements Enumeration'
        '4 - Add Some Elements'
        '5 - Assign one Storage to Other'
        '6 - Store and Read Binary Data'
        '7 - Enumeration with XPath selection'
        '8 - Store and Read Compressed Data'
        '9 - Store and Read Encrypted Data'
        '10 - Filter Chaining (Compress -> Encrypt)'
        '11 - Read and Write CData integer value'
        '12 - Assign a subtree to element'
        '13 - Update a element with subtree'
        '14 - Handling load parse error '
        '15 - Use SimpleStorage with legacy code')
      TabOrder = 0
      OnClick = rgDemoSelectionClick
    end
  end
  object XPManifest: TXPManifest
    Left = 648
    Top = 336
  end
end
