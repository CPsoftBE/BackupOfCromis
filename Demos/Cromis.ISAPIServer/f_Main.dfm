object fMain: TfMain
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Sample ISAPI Server'
  ClientHeight = 327
  ClientWidth = 585
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
  object mmInstructions: TMemo
    Left = 56
    Top = 64
    Width = 481
    Height = 193
    Alignment = taCenter
    Ctl3D = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -19
    Font.Name = 'Tahoma'
    Font.Style = []
    Lines.Strings = (
      ''
      ''
      'Just open the browser and type in'
      ''
      'http://localhost:9999/ISAPIDemo.dll/info')
    ParentCtl3D = False
    ParentFont = False
    ReadOnly = True
    TabOrder = 0
  end
  object HTTPServer: TIdHTTPServer
    Bindings = <>
    OnCommandGet = HTTPServerCommandGet
    Left = 24
    Top = 32
  end
end
