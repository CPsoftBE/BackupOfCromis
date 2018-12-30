object fMain: TfMain
  Left = 0
  Top = 0
  Caption = 'Dynamic array implementations comparison'
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
  object lbSpeedResults: TListBox
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 473
    Height = 484
    Align = alLeft
    ItemHeight = 13
    TabOrder = 0
  end
  object lbSliceData: TListBox
    AlignWithMargins = True
    Left = 482
    Top = 3
    Width = 299
    Height = 484
    Align = alClient
    ItemHeight = 13
    TabOrder = 1
  end
  object sbSpeedTest: TStatusBar
    AlignWithMargins = True
    Left = 3
    Top = 540
    Width = 778
    Height = 19
    Panels = <>
  end
  object pnCommands: TPanel
    AlignWithMargins = True
    Left = 3
    Top = 493
    Width = 778
    Height = 41
    Align = alBottom
    TabOrder = 3
    object lbSliceBufferMpl: TLabel
      Left = 629
      Top = 11
      Width = 71
      Height = 13
      Caption = 'SliceBufferMpl:'
    end
    object lbSliceSize: TLabel
      Left = 503
      Top = 11
      Width = 44
      Height = 13
      Caption = 'SliceSize:'
    end
    object btnTestLargeSize: TButton
      Left = 6
      Top = 6
      Width = 99
      Height = 25
      Caption = 'Test large size'
      TabOrder = 0
      OnClick = btnTestLargeSizeClick
    end
    object btnTestSmallSize: TButton
      Left = 111
      Top = 6
      Width = 99
      Height = 25
      Caption = 'Test small size'
      TabOrder = 1
      OnClick = btnTestSmallSizeClick
    end
    object cbDynamicArray: TRadioButton
      Left = 232
      Top = 10
      Width = 113
      Height = 17
      Caption = 'amDynamicArray'
      TabOrder = 2
    end
    object cbSlicedArray: TRadioButton
      Left = 343
      Top = 10
      Width = 113
      Height = 17
      Caption = 'amSlicedArray'
      Checked = True
      TabOrder = 3
      TabStop = True
    end
    object eSliceSize: TEdit
      Left = 554
      Top = 8
      Width = 65
      Height = 21
      TabOrder = 4
      Text = '10000'
    end
    object eSliceBufferMpl: TEdit
      Left = 704
      Top = 8
      Width = 65
      Height = 21
      TabOrder = 5
      Text = '1.1'
    end
  end
  object XPManifest: TXPManifest
    Left = 696
    Top = 40
  end
end
