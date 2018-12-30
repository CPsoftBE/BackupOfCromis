object fMain: TfMain
  Left = 439
  Top = 280
  Caption = 'fMain'
  ClientHeight = 406
  ClientWidth = 621
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object btnStart: TButton
    Left = 16
    Top = 49
    Width = 121
    Height = 25
    Caption = 'Start Test'
    TabOrder = 0
    OnClick = btnStartClick
  end
  object ePoolSize: TEdit
    Left = 16
    Top = 24
    Width = 121
    Height = 21
    TabOrder = 1
    Text = '20'
  end
  object lbThreadResults: TListBox
    Left = 153
    Top = 24
    Width = 443
    Height = 289
    ItemHeight = 13
    TabOrder = 2
  end
  object pbPoolSize: TProgressBar
    Left = 0
    Top = 373
    Width = 621
    Height = 33
    Align = alBottom
    TabOrder = 3
  end
  object btnStop: TButton
    Left = 16
    Top = 78
    Width = 121
    Height = 25
    Caption = 'Stop Test'
    TabOrder = 4
    OnClick = btnStopClick
  end
  object tbThreadTimeout: TTrackBar
    Left = 35
    Top = 120
    Width = 33
    Height = 233
    Max = 1000
    Min = 1
    Orientation = trVertical
    Frequency = 100
    Position = 1000
    TabOrder = 5
    OnChange = tbThreadTimeoutChange
  end
  object tbCreationTimeout: TTrackBar
    Left = 90
    Top = 120
    Width = 33
    Height = 233
    Max = 100
    Min = 1
    Orientation = trVertical
    Frequency = 100
    Position = 50
    TabOrder = 6
    OnChange = tbCreationTimeoutChange
  end
  object stPoolSizeText: TStaticText
    Left = 153
    Top = 321
    Width = 46
    Height = 17
    Caption = 'Pool Size'
    TabOrder = 7
  end
  object stFreeThreadsText: TStaticText
    Left = 153
    Top = 344
    Width = 68
    Height = 17
    Caption = 'Free Threads'
    TabOrder = 8
  end
  object stPoolSizeValue: TStaticText
    Left = 240
    Top = 321
    Width = 10
    Height = 17
    Caption = '0'
    TabOrder = 9
  end
  object stFreeThreadsValue: TStaticText
    Left = 240
    Top = 344
    Width = 10
    Height = 17
    Caption = '0'
    TabOrder = 10
  end
  object stCreationTimeout: TStaticText
    Left = 82
    Top = 350
    Width = 38
    Height = 17
    Alignment = taCenter
    AutoSize = False
    Caption = '0'
    TabOrder = 11
  end
  object stThreadTimeout: TStaticText
    Left = 27
    Top = 350
    Width = 38
    Height = 17
    Alignment = taCenter
    AutoSize = False
    Caption = '0'
    TabOrder = 12
  end
  object StaticText1: TStaticText
    Left = 16
    Top = 107
    Width = 121
    Height = 17
    Alignment = taCenter
    AutoSize = False
    Caption = 'Times are in milliseconds'
    TabOrder = 13
  end
  object cbDynamicPoolSize: TCheckBox
    Left = 269
    Top = 321
    Width = 137
    Height = 17
    Caption = 'Dynamic pool size'
    TabOrder = 14
    OnClick = cbDynamicPoolSizeClick
  end
  object stThreadsFinishedLabel: TStaticText
    Left = 269
    Top = 344
    Width = 87
    Height = 17
    Caption = 'Threads finished:'
    TabOrder = 15
  end
  object stThreadsFinishedValue: TStaticText
    Left = 360
    Top = 344
    Width = 10
    Height = 17
    Caption = '0'
    TabOrder = 16
  end
  object XPManifest: TXPManifest
    Left = 456
    Top = 56
  end
  object tmPoolStatus: TTimer
    Enabled = False
    OnTimer = tmPoolStatusTimer
    Left = 520
    Top = 56
  end
end
