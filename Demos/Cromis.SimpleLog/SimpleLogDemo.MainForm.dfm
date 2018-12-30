object fMain: TfMain
  Left = 0
  Top = 0
  Caption = 'fMain'
  ClientHeight = 302
  ClientWidth = 563
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
  object btnLogEvent: TButton
    Left = 176
    Top = 24
    Width = 195
    Height = 65
    Caption = 'Log Event'
    TabOrder = 0
    OnClick = btnLogEventClick
  end
  object btnHandledException: TButton
    Left = 176
    Top = 95
    Width = 195
    Height = 65
    Caption = 'Handled Exception'
    TabOrder = 1
    OnClick = btnHandledExceptionClick
  end
  object btnUnhadnledException: TButton
    Left = 176
    Top = 167
    Width = 195
    Height = 65
    Caption = 'Unhandled Exception'
    TabOrder = 2
    OnClick = btnUnhadnledExceptionClick
  end
  object stLogLocations: TStaticText
    Left = 85
    Top = 264
    Width = 379
    Height = 20
    Caption = 'your logs will be saved, where demo application is located'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 3
  end
end
