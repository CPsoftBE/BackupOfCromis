object fMain: TfMain
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'IAnyArray stress test'
  ClientHeight = 490
  ClientWidth = 747
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object lbRunningText: TLabel
    Left = 446
    Top = 41
    Width = 60
    Height = 13
    Caption = 'Running for:'
  end
  object lbRunningValue: TLabel
    Left = 523
    Top = 41
    Width = 64
    Height = 13
    Caption = 'Running Time'
  end
  object lbNumSlicesText: TLabel
    Left = 446
    Top = 62
    Width = 58
    Height = 13
    Caption = 'Num. Slices:'
  end
  object lbNumSlicesValue: TLabel
    Left = 523
    Top = 62
    Width = 65
    Height = 13
    Caption = 'Number slices'
  end
  object lbArrayActions: TListBox
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 433
    Height = 461
    Align = alLeft
    ItemHeight = 13
    TabOrder = 0
  end
  object btnStart: TButton
    Left = 442
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Start'
    TabOrder = 1
    OnClick = btnStartClick
  end
  object btnStop: TButton
    Left = 523
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Stop'
    TabOrder = 2
    OnClick = btnStopClick
  end
  object lbSliceData: TListBox
    Left = 442
    Top = 88
    Width = 301
    Height = 376
    ItemHeight = 13
    TabOrder = 3
  end
  object btnReplay: TButton
    Left = 664
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Replay'
    TabOrder = 4
    OnClick = btnReplayClick
  end
  object pbReplayProgress: TProgressBar
    AlignWithMargins = True
    Left = 3
    Top = 470
    Width = 741
    Height = 17
    Align = alBottom
    TabOrder = 5
  end
  object btnSimulate: TButton
    Left = 664
    Top = 36
    Width = 75
    Height = 25
    Caption = 'Simulate'
    TabOrder = 6
    OnClick = btnSimulateClick
  end
  object tmCurrentTime: TTimer
    Enabled = False
    OnTimer = tmCurrentTimeTimer
    Left = 680
    Top = 112
  end
end
