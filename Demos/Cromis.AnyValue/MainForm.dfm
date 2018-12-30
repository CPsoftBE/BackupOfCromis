object fMain: TfMain
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'fMain'
  ClientHeight = 672
  ClientWidth = 994
  Color = clBtnFace
  Constraints.MinHeight = 700
  Constraints.MinWidth = 1000
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object pcAnyValue: TPageControl
    Left = 0
    Top = 0
    Width = 994
    Height = 672
    ActivePage = tsAnyArrayDemo
    Align = alClient
    TabOrder = 0
    object tsAnyValueDemo: TTabSheet
      Caption = 'tsAnyValueDemo'
      object lbTestAnyValue: TListBox
        Left = 288
        Top = 4
        Width = 693
        Height = 637
        ItemHeight = 13
        TabOrder = 0
      end
      object btnTestAnyValue: TButton
        Left = 72
        Top = 24
        Width = 105
        Height = 41
        Caption = 'Test AnyValue'
        TabOrder = 1
        OnClick = btnTestAnyValueClick
      end
    end
    object tsAnyArrayDemo: TTabSheet
      Caption = 'tsAnyArrayDemo'
      ImageIndex = 1
      object lbAnyValue: TListBox
        Left = 263
        Top = 3
        Width = 570
        Height = 475
        ItemHeight = 13
        TabOrder = 0
      end
      object gbAnyArray: TGroupBox
        Left = 6
        Top = 4
        Width = 251
        Height = 635
        Caption = 'Any Array'
        TabOrder = 1
        object btnCreateArray: TButton
          Left = 16
          Top = 24
          Width = 105
          Height = 25
          Caption = 'Create Array'
          TabOrder = 0
          OnClick = btnCreateArrayClick
        end
        object btnPushItems: TButton
          Left = 16
          Top = 54
          Width = 105
          Height = 25
          Caption = 'Push Items'
          Enabled = False
          TabOrder = 1
          OnClick = btnPushItemsClick
        end
        object btnPopItem: TButton
          Left = 16
          Top = 114
          Width = 105
          Height = 25
          Caption = 'Pop Item'
          Enabled = False
          TabOrder = 2
          OnClick = btnPopItemClick
        end
        object btnSliceArray: TButton
          Left = 16
          Top = 144
          Width = 105
          Height = 25
          Caption = 'Slice Array'
          Enabled = False
          TabOrder = 3
          OnClick = btnSliceArrayClick
        end
        object btnIndexOf: TButton
          Left = 16
          Top = 174
          Width = 105
          Height = 25
          Caption = 'IndexOf'
          Enabled = False
          TabOrder = 4
          OnClick = btnIndexOfClick
        end
        object btnContains: TButton
          Left = 16
          Top = 204
          Width = 105
          Height = 25
          Caption = 'Contains'
          Enabled = False
          TabOrder = 5
          OnClick = btnContainsClick
        end
        object eCreateArray: TEdit
          Left = 144
          Top = 26
          Width = 45
          Height = 21
          TabOrder = 6
          Text = '1,5,3,8'
        end
        object ePushItems: TEdit
          Left = 144
          Top = 57
          Width = 91
          Height = 21
          TabOrder = 7
          Text = '2,5,9'
        end
        object eSliceArray: TEdit
          Left = 144
          Top = 146
          Width = 91
          Height = 21
          TabOrder = 8
          Text = '2,6'
        end
        object eIndexOf: TEdit
          Left = 144
          Top = 176
          Width = 91
          Height = 21
          TabOrder = 9
          Text = '5'
        end
        object eContains: TEdit
          Left = 144
          Top = 206
          Width = 91
          Height = 21
          TabOrder = 10
          Text = '5'
        end
        object btnReverseItems: TButton
          Left = 16
          Top = 84
          Width = 105
          Height = 25
          Caption = 'Reverse Items'
          Enabled = False
          TabOrder = 11
          OnClick = btnReverseItemsClick
        end
        object btnSortASC: TButton
          Left = 16
          Top = 234
          Width = 105
          Height = 25
          Caption = 'Sort ASC'
          Enabled = False
          TabOrder = 12
          OnClick = btnSortASCClick
        end
        object btnSortDSC: TButton
          Left = 16
          Top = 264
          Width = 105
          Height = 25
          Caption = 'Sort DSC'
          Enabled = False
          TabOrder = 13
          OnClick = btnSortDSCClick
        end
        object btnClone: TButton
          Left = 16
          Top = 294
          Width = 105
          Height = 25
          Caption = 'Clone Array'
          Enabled = False
          TabOrder = 14
          OnClick = btnCloneClick
        end
        object btnDeleteItem: TButton
          Left = 16
          Top = 324
          Width = 105
          Height = 25
          Caption = 'Delete item'
          Enabled = False
          TabOrder = 15
          OnClick = btnDeleteItemClick
        end
        object eDeleteItem: TEdit
          Left = 144
          Top = 326
          Width = 91
          Height = 21
          TabOrder = 16
          Text = '5'
        end
        object btnClear: TButton
          Left = 16
          Top = 354
          Width = 105
          Height = 25
          Caption = 'Clear Array'
          Enabled = False
          TabOrder = 17
          OnClick = btnClearClick
        end
        object btnStreamOps: TButton
          Left = 16
          Top = 414
          Width = 105
          Height = 25
          Caption = 'Stream ops'
          Enabled = False
          TabOrder = 18
          OnClick = btnStreamOpsClick
        end
        object btnAddArrays: TButton
          Left = 16
          Top = 384
          Width = 105
          Height = 25
          Caption = 'Add Arrays'
          Enabled = False
          TabOrder = 19
          OnClick = btnAddArraysClick
        end
        object btnAddNamedValue: TButton
          Left = 16
          Top = 444
          Width = 105
          Height = 25
          Caption = 'Add Named Val.'
          Enabled = False
          TabOrder = 20
          OnClick = btnAddNamedValueClick
        end
        object eAddNamedValue: TEdit
          Left = 144
          Top = 446
          Width = 91
          Height = 21
          TabOrder = 21
          Text = 'Delphi,XE3'
        end
        object btnFindNamedValue: TButton
          Left = 16
          Top = 474
          Width = 105
          Height = 25
          Caption = 'Find Named Val.'
          Enabled = False
          TabOrder = 22
          OnClick = btnFindNamedValueClick
        end
        object eFindNamedValue: TEdit
          Left = 144
          Top = 476
          Width = 91
          Height = 21
          TabOrder = 23
          Text = 'Delphi'
        end
        object btnInsert: TButton
          Left = 16
          Top = 504
          Width = 105
          Height = 25
          Caption = 'Insert Items'
          Enabled = False
          TabOrder = 24
          OnClick = btnInsertClick
        end
        object eInsertItems: TEdit
          Left = 144
          Top = 506
          Width = 91
          Height = 21
          TabOrder = 25
          Text = '2,7,3'
        end
        object eSliceSize: TEdit
          Left = 193
          Top = 26
          Width = 42
          Height = 21
          TabOrder = 26
          Text = '10'
        end
        object btnUnshiftItems: TButton
          Left = 16
          Top = 534
          Width = 105
          Height = 25
          Caption = 'Unshift Items'
          Enabled = False
          TabOrder = 27
          OnClick = btnUnshiftItemsClick
        end
        object btnShiftItem: TButton
          Left = 16
          Top = 564
          Width = 105
          Height = 25
          Caption = 'Shift Item'
          Enabled = False
          TabOrder = 28
          OnClick = btnShiftItemClick
        end
        object eUnshiftItems: TEdit
          Left = 144
          Top = 536
          Width = 91
          Height = 21
          TabOrder = 29
          Text = '6,8,1'
        end
        object btnEnumerate: TButton
          Left = 16
          Top = 594
          Width = 105
          Height = 25
          Caption = 'Enumerate'
          Enabled = False
          TabOrder = 30
          OnClick = btnEnumerateClick
        end
      end
      object lbSliceInfo: TListBox
        Left = 836
        Top = 3
        Width = 147
        Height = 475
        ItemHeight = 13
        TabOrder = 2
      end
      object lbSliceData: TListBox
        Left = 263
        Top = 484
        Width = 720
        Height = 155
        ItemHeight = 13
        TabOrder = 3
      end
    end
  end
end
