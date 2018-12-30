object fMain: TfMain
  Left = 0
  Top = 0
  Caption = 'Document Filters Examples'
  ClientHeight = 362
  ClientWidth = 584
  Color = clBtnFace
  Constraints.MinHeight = 400
  Constraints.MinWidth = 600
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object btnEncrypt: TButton
    Left = 24
    Top = 24
    Width = 193
    Height = 25
    Caption = 'Normal XML -> EnryptedXML'
    TabOrder = 0
    OnClick = btnEncryptClick
  end
  object btnDecrypt: TButton
    Left = 24
    Top = 55
    Width = 193
    Height = 25
    Caption = 'Enrypted XML -> Normal XML'
    TabOrder = 1
    OnClick = btnDecryptClick
  end
  object btnChainIn: TButton
    Left = 264
    Top = 24
    Width = 289
    Height = 25
    Caption = 'Normal XML -> Compressed XML + Enrypted XML'
    TabOrder = 2
    OnClick = btnChainInClick
  end
  object btnCompress: TButton
    Left = 24
    Top = 112
    Width = 193
    Height = 25
    Caption = 'Normal XML -> Compressed XML'
    TabOrder = 3
    OnClick = btnCompressClick
  end
  object btnDecompress: TButton
    Left = 24
    Top = 143
    Width = 193
    Height = 25
    Caption = 'Compressed XML -> Normal XML'
    TabOrder = 4
    OnClick = btnDecompressClick
  end
  object btnChainOut: TButton
    Left = 264
    Top = 55
    Width = 289
    Height = 25
    Caption = 'Compressed XML + Enrypted XML -> Normal XML'
    TabOrder = 5
    OnClick = btnChainOutClick
  end
end
