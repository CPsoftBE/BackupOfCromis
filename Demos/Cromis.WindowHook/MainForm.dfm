object fMain: TfMain
  Left = 0
  Top = 0
  Caption = 'Global Window Creation Hook'
  ClientHeight = 502
  ClientWidth = 784
  Color = clBtnFace
  Constraints.MinHeight = 500
  Constraints.MinWidth = 800
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = mmMainMenu
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object lvWindowMessages: TListView
    Left = 0
    Top = 0
    Width = 784
    Height = 483
    Align = alClient
    Columns = <
      item
        Caption = 'Action'
        Width = 80
      end
      item
        Caption = 'Title'
        Width = 150
      end
      item
        Caption = 'Class'
        Width = 100
      end
      item
        Caption = 'Handle'
        Width = 70
      end
      item
        Caption = 'Parent'
        Width = 70
      end
      item
        Caption = 'Thread'
        Width = 70
      end
      item
        Caption = 'ProcessID'
        Width = 70
      end
      item
        Caption = 'Process Name'
        Width = 150
      end
      item
        Caption = 'Menu'
        Width = 70
      end
      item
        Caption = 'Left'
        Width = 70
      end
      item
        Caption = 'Top'
        Width = 70
      end
      item
        Caption = 'Width'
        Width = 70
      end
      item
        Caption = 'Height'
        Width = 70
      end
      item
        Caption = 'Style'
        Width = 70
      end
      item
        Caption = 'StyleEx'
        Width = 70
      end>
    PopupMenu = pmActions
    TabOrder = 0
    ViewStyle = vsReport
  end
  object sbStatusBar: TStatusBar
    Left = 0
    Top = 483
    Width = 784
    Height = 19
    Panels = <>
    SimplePanel = True
    SimpleText = 'Global Window Hook is Idle'
  end
  object mmMainMenu: TMainMenu
    Left = 32
    Top = 40
    object miActions: TMenuItem
      Caption = 'Actions'
      object miStartListening: TMenuItem
        Caption = 'Start Listening'
        OnClick = miStartListeningClick
      end
      object miStopListening: TMenuItem
        Caption = 'Stop Listening'
        OnClick = miStopListeningClick
      end
      object miDiv: TMenuItem
        Caption = '-'
      end
      object miExit: TMenuItem
        Caption = 'Exit'
        OnClick = miExitClick
      end
    end
    object miActiveHooks: TMenuItem
      Caption = 'Active Hooks'
      object miHCBTDESTROYWND: TMenuItem
        Caption = 'HCBT_DESTROYWND'
        Checked = True
        OnClick = miHCBTDESTROYWNDClick
      end
      object miHCBTCREATEWND: TMenuItem
        Caption = 'HCBT_CREATEWND'
        Checked = True
        OnClick = miHCBTCREATEWNDClick
      end
      object miHCBTMOVESIZE: TMenuItem
        Caption = 'HCBT_MOVESIZE'
        Checked = True
        OnClick = miHCBTMOVESIZEClick
      end
      object miHCBTACTIVATE: TMenuItem
        Caption = 'HCBT_ACTIVATE'
        Checked = True
        OnClick = miHCBTACTIVATEClick
      end
    end
  end
  object pmActions: TPopupMenu
    Left = 104
    Top = 40
    object pmiStartListening: TMenuItem
      Caption = 'Start Listening'
      OnClick = miStartListeningClick
    end
    object pmiStopListening: TMenuItem
      Caption = 'Stop Listening'
      OnClick = miStopListeningClick
    end
    object pmiClearView: TMenuItem
      Caption = 'Clear View'
      OnClick = pmiClearViewClick
    end
  end
end
