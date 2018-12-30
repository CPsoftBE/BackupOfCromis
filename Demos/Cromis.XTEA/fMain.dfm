object Form1: TForm1
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Form1'
  ClientHeight = 706
  ClientWidth = 794
  Color = clBtnFace
  Constraints.MinHeight = 600
  Constraints.MinWidth = 800
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
  object mmOriginal: TMemo
    Left = 8
    Top = 11
    Width = 778
    Height = 263
    Lines.Strings = (
      
        '<answers AppVer="1.0.3600.24516" QID="733CD2CA" UserID="001" Loc' +
        'ationID="001" StationID="FD8878619756D90C866C37C6D4614265" Sampl' +
        'eID="7" XmlVer="1.0" StartDate="20091109T133810" EndDate="200911' +
        '09T133817">'
      '  <S1 et="1453">1</S1>'
      '  <q2 et="969">5</q2>'
      '  <S2 et="1765">1</S2>'
      '  <q4 et="1406">3-8</q4>'
      '  <q3 et="1937">5-6</q3>'
      '</answers>')
    TabOrder = 0
    WordWrap = False
  end
  object mmDecrypted: TMemo
    Left = 8
    Top = 406
    Width = 778
    Height = 248
    TabOrder = 1
    WordWrap = False
  end
  object btnEnrypt: TButton
    Left = 8
    Top = 660
    Width = 153
    Height = 38
    Caption = 'Encrypt'
    TabOrder = 2
    OnClick = btnEnryptClick
  end
  object btnDecrypt: TButton
    Left = 167
    Top = 660
    Width = 153
    Height = 38
    Caption = 'Decrypt'
    TabOrder = 3
    OnClick = btnDecryptClick
  end
  object mmEncrypted: TMemo
    Left = 8
    Top = 281
    Width = 778
    Height = 119
    TabOrder = 4
  end
  object XPManifest: TXPManifest
    Left = 720
    Top = 472
  end
end
