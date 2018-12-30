object fMain: TfMain
  Left = 0
  Top = 0
  Caption = 'fMain'
  ClientHeight = 497
  ClientWidth = 728
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object mmXML: TMemo
    AlignWithMargins = True
    Left = 3
    Top = 236
    Width = 722
    Height = 258
    Align = alBottom
    TabOrder = 0
  end
  object DBGrid: TDBGrid
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 722
    Height = 227
    Align = alClient
    DataSource = DataSource
    TabOrder = 1
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'Tahoma'
    TitleFont.Style = []
  end
  object DataSource: TDataSource
    DataSet = ClientDS
    Left = 272
    Top = 48
  end
  object DataSetProvider: TDataSetProvider
    ResolveToDataSet = True
    Left = 184
    Top = 48
  end
  object ClientDS: TClientDataSet
    Aggregates = <>
    Params = <>
    ProviderName = 'DataSetProvider'
    Left = 112
    Top = 48
  end
  object MainMenu: TMainMenu
    Left = 48
    Top = 48
    object mgAction: TMenuItem
      Caption = 'Action'
      object miDataSetToSS: TMenuItem
        Caption = 'Execute -> DataSet to SS'
        OnClick = miDataSetToSSClick
      end
      object miSSToDataset: TMenuItem
        Caption = 'Execute -> SS to DataSet'
        OnClick = miSSToDatasetClick
      end
    end
  end
end
