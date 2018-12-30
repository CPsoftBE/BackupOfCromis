object fMain: TfMain
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'Automated Mail Sender'
  ClientHeight = 567
  ClientWidth = 656
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
  object ProgressBar: TProgressBar
    AlignWithMargins = True
    Left = 3
    Top = 541
    Width = 650
    Height = 23
    Align = alBottom
    Step = 1
    TabOrder = 0
  end
  object gbMailSettings: TGroupBox
    Left = 3
    Top = 118
    Width = 333
    Height = 258
    Caption = 'Mail Settings'
    TabOrder = 1
    object lbHMTLContent: TLabel
      Left = 10
      Top = 22
      Width = 89
      Height = 13
      Caption = 'HTML Mail Content'
    end
    object lbPlainTextContent: TLabel
      Left = 10
      Top = 69
      Width = 110
      Height = 13
      Caption = 'Plain Text Mail Content'
    end
    object lbMailSubject: TLabel
      Left = 10
      Top = 114
      Width = 57
      Height = 13
      Caption = 'Mail Subject'
    end
    object lbFromAdrress: TLabel
      Left = 10
      Top = 160
      Width = 66
      Height = 13
      Caption = 'From Address'
    end
    object lbTargetMailList: TLabel
      Left = 10
      Top = 206
      Width = 90
      Height = 13
      Caption = 'List of Target Mails'
    end
    object eHMTLContent: TAdvFileNameEdit
      Left = 9
      Top = 40
      Width = 312
      Height = 21
      Flat = False
      LabelFont.Charset = DEFAULT_CHARSET
      LabelFont.Color = clWindowText
      LabelFont.Height = -11
      LabelFont.Name = 'Tahoma'
      LabelFont.Style = []
      Lookup.Separator = ';'
      Color = clWindow
      Enabled = True
      ReadOnly = False
      TabOrder = 0
      Visible = True
      Version = '1.3.2.8'
      ButtonStyle = bsButton
      ButtonWidth = 18
      Etched = False
      Glyph.Data = {
        CE000000424DCE0000000000000076000000280000000C0000000B0000000100
        0400000000005800000000000000000000001000000000000000000000000000
        8000008000000080800080000000800080008080000080808000C0C0C0000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00D00000000DDD
        00000077777770DD00000F077777770D00000FF07777777000000FFF00000000
        00000FFFFFFF0DDD00000FFF00000DDD0000D000DDDDD0000000DDDDDDDDDD00
        0000DDDDD0DDD0D00000DDDDDD000DDD0000}
      FilterIndex = 0
      DialogOptions = []
      DialogKind = fdOpen
    end
    object ePlainTextContent: TAdvFileNameEdit
      Left = 9
      Top = 85
      Width = 312
      Height = 21
      Flat = False
      LabelFont.Charset = DEFAULT_CHARSET
      LabelFont.Color = clWindowText
      LabelFont.Height = -11
      LabelFont.Name = 'Tahoma'
      LabelFont.Style = []
      Lookup.Separator = ';'
      Color = clWindow
      Enabled = True
      ReadOnly = False
      TabOrder = 1
      Visible = True
      Version = '1.3.2.8'
      ButtonStyle = bsButton
      ButtonWidth = 18
      Etched = False
      Glyph.Data = {
        CE000000424DCE0000000000000076000000280000000C0000000B0000000100
        0400000000005800000000000000000000001000000000000000000000000000
        8000008000000080800080000000800080008080000080808000C0C0C0000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00D00000000DDD
        00000077777770DD00000F077777770D00000FF07777777000000FFF00000000
        00000FFFFFFF0DDD00000FFF00000DDD0000D000DDDDD0000000DDDDDDDDDD00
        0000DDDDD0DDD0D00000DDDDDD000DDD0000}
      FilterIndex = 0
      DialogOptions = []
      DialogKind = fdOpen
    end
    object eMailSubject: TEdit
      Left = 9
      Top = 130
      Width = 312
      Height = 21
      TabOrder = 2
    end
    object eFromAdrress: TEdit
      Left = 9
      Top = 176
      Width = 312
      Height = 21
      TabOrder = 3
    end
    object eTargetMailList: TAdvFileNameEdit
      Left = 9
      Top = 223
      Width = 312
      Height = 21
      Flat = False
      LabelFont.Charset = DEFAULT_CHARSET
      LabelFont.Color = clWindowText
      LabelFont.Height = -11
      LabelFont.Name = 'Tahoma'
      LabelFont.Style = []
      Lookup.Separator = ';'
      Color = clWindow
      Enabled = True
      ReadOnly = False
      TabOrder = 4
      Visible = True
      Version = '1.3.2.8'
      ButtonStyle = bsButton
      ButtonWidth = 18
      Etched = False
      Glyph.Data = {
        CE000000424DCE0000000000000076000000280000000C0000000B0000000100
        0400000000005800000000000000000000001000000000000000000000000000
        8000008000000080800080000000800080008080000080808000C0C0C0000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00D00000000DDD
        00000077777770DD00000F077777770D00000FF07777777000000FFF00000000
        00000FFFFFFF0DDD00000FFF00000DDD0000D000DDDDD0000000DDDDDDDDDD00
        0000DDDDD0DDD0D00000DDDDDD000DDD0000}
      FilterIndex = 0
      DialogOptions = []
      DialogKind = fdOpen
    end
  end
  object mmSentList: TListBox
    AlignWithMargins = True
    Left = 344
    Top = 14
    Width = 308
    Height = 361
    Margins.Top = 5
    Ctl3D = True
    ItemHeight = 13
    ParentCtl3D = False
    TabOrder = 2
  end
  object gbMailStatus: TGroupBox
    Left = 3
    Top = 8
    Width = 333
    Height = 105
    Caption = 'Mail Sending Status'
    TabOrder = 3
    object lbCurrentSendingMail: TLabel
      Left = 9
      Top = 23
      Width = 110
      Height = 13
      Caption = 'Currently sending mail:'
    end
    object lbSentMailCount: TLabel
      Left = 128
      Top = 64
      Width = 193
      Height = 13
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Sent 0 of 0 mails'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object sbSendMail: TSpeedButton
      Left = 9
      Top = 65
      Width = 35
      Height = 35
      Hint = 'Start Sending Mails'
      Flat = True
      Glyph.Data = {
        C6040000424DC604000000000000C60000002800000020000000200000000100
        08000000000000040000120B0000120B0000240000002400000000000000FFFF
        FF00FF00FF00C2EABE009ED69E00AEE2AE009AC69A00B6E6B600AEDAAE0092B6
        9200B6DAB600A6C6A6008AA28A0096AA9600D6F2D600A2B6A20082928200CEE6
        CE006E7A6E007E8A7E00B6C6B6005A625A00C2D2C200D6E6D600E6F6E600D2DE
        D200E2EEE200565A560082868200AEE2B200BEE6C200CEEED200666666004E4E
        4E00444444003E3E3E0002020202020202020202020202020202020202020202
        0202020202020202020202020202020202020202020202020202020202020202
        0202020202020202020202020202020202020202020202020202020202020202
        0202020202020202020202222202020202020202020202020202020202020202
        0202020202020202020202221222020202020202020202020202020202020202
        020202020202020202020222150F220202020202020202020202020202020202
        02020202020202020202020222190D2202020202020202020202020202020202
        020202020202020202020202221C170D22020202020222220202020202020202
        02020202020202020202020202221A1710220202022214212202020202020202
        02020202020202020202020202220F1717132202220F17161B22020202020202
        02020222220202020202020202221B18171F1222131717171115220202020202
        020222232202020202020202020222160E111113111111111111202202020202
        0202230D2202020202020202020222130E111111111111111111111222020202
        022213110F220202020202020202022217111111111111111111111113220202
        222011110A22020202020202020202220D0E1111111111111111111111102202
        220A1111112202020202020202020202220E111116171103111E03111E110C22
        0B1E110311220202020202020202020222141117121017070707070707070708
        07070707070C2202020202020202020222130E0F1B220F110707070707070707
        0707070707092202020202020202020202221420220222161107070707070707
        0707070707082202020202020202020202022222020202221107070707070707
        0707070707072202020202020202020202020202020202221C171D0505050505
        050505050505102202020202020202020202020202020202220D110808080808
        0808080808080D22020202020202020202020202020202020222090808080808
        0808080808080622020202020202020202020202020202022210080808080808
        0808080808080822020202020202020202020202020202221C04080808080808
        0808080808080810220202020202020202020202020202220604040404040404
        040404040404040C220202020202020202020202020222090404040404040404
        040404040404040922020202020202020202020202220D171717171717171717
        1717171717171711220202020202020202020202020222222222222222222222
        2222222222222222220202020202020202020202020202020202020202020202
        0202020202020202020202020202020202020202020202020202020202020202
        0202020202020202020202020202020202020202020202020202020202020202
        02020202020202020202}
      ParentShowHint = False
      ShowHint = True
      OnClick = sbSendMailClick
    end
    object sbStopSending: TSpeedButton
      Left = 52
      Top = 65
      Width = 35
      Height = 35
      Hint = 'Stop Sending Mails'
      Flat = True
      Glyph.Data = {
        7E040000424D7E040000000000007E0000002800000020000000200000000100
        08000000000000040000120B0000120B0000120000001200000000000000FFFF
        FF00827EC600726E9600FF00FF006E7296007E82C600868ACA006262C2007E7E
        C6008E8ED2009E9ED6009696CA00AAAADE0072729200C6C6EA00BABAD6004444
        4400040404040404040404040404040404040404040404040404040404040404
        0404040404040404040404111111111111111111111111111104040404040404
        04040404040404040404110E0C0C0C0C0C0C0C0C0C0C0C0C0E11040404040404
        040404040404040404110E0C0C0C0C0C0C0C0C0C0C0C0C0C0C0E110404040404
        0404040404040404110E0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0E1104040404
        04040404040404110E0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0E11040404
        04040404040411050C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0E110404
        0404040404110E070C070C070C070C070C070C070C070C070C070C070C0E1104
        040404041103070C070C070C070C070C070C070C070C070C070C070C07070E11
        040404110307070707070707070707070707070707070707070707070707070E
        1104041107070707070707070707070707070707070707070707070707070707
        1104041107070707070707070707070707070707070707070707070707070707
        1104041107070707070707070707070707070707070707070707070707070707
        1104041107090706070607090706070207090709070907090709070907020709
        1104041109070907090709070907090709070907090709070907090709070907
        1104041109090909090909090909090909090909090909090909090909090909
        1104041109090909090909090909090909090909090909090909090909090909
        1104041109090909090909090909090909090909090909090909090909090909
        1104041107090909090909090909090909090909090909090909090909090909
        110404110A09090909090909090909090909090909090909090909090909090A
        110404110D08090809080908090809080908090809080908090809080908090B
        110404110D09080908090809080908090809080908090809080908090809080D
        1104041110080808080808080808080808080808080808080808080808080810
        1104040411100808080808080808080808080808080808080808080808081011
        0404040404111008080808080808080808080808080808080808080808101104
        0404040404041110080808080808080808080808080808080808080810110404
        0404040404040411100808080808080808080808080808080808081011040404
        0404040404040404111008080808080808080808080808080808101104040404
        0404040404040404041110080808080808080808080808080810110404040404
        0404040404040404040411100F0F0F0F0F0F0F0F0F0F0F0F1011040404040404
        0404040404040404040404111111111111111111111111111104040404040404
        0404040404040404040404040404040404040404040404040404040404040404
        0404}
      ParentShowHint = False
      ShowHint = True
      OnClick = sbStopSendingClick
    end
    object lbSendingMail: TStaticText
      Left = 9
      Top = 41
      Width = 312
      Height = 19
      AutoSize = False
      BorderStyle = sbsSunken
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 0
    end
    object lbSendingStatus: TStaticText
      Left = 128
      Top = 84
      Width = 193
      Height = 18
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'STAUS: IDLE'
      Font.Charset = ANSI_CHARSET
      Font.Color = clBlack
      Font.Height = -16
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 1
    end
  end
  object gbServerSettings: TGroupBox
    Left = 3
    Top = 382
    Width = 333
    Height = 153
    Caption = 'Mail Server Settings'
    TabOrder = 4
    object lbUserPassword: TLabel
      Left = 10
      Top = 108
      Width = 71
      Height = 13
      Caption = 'User Password'
    end
    object lbUserName: TLabel
      Left = 10
      Top = 63
      Width = 52
      Height = 13
      Caption = 'User Name'
    end
    object lbServerName: TLabel
      Left = 10
      Top = 21
      Width = 62
      Height = 13
      Caption = 'Server Name'
    end
    object eUserName: TEdit
      Left = 10
      Top = 80
      Width = 311
      Height = 21
      TabOrder = 0
    end
    object eUserPassword: TEdit
      Left = 9
      Top = 124
      Width = 312
      Height = 21
      PasswordChar = '*'
      TabOrder = 1
    end
    object eServerName: TEdit
      Left = 10
      Top = 36
      Width = 311
      Height = 21
      TabOrder = 2
    end
  end
  object gbOtherSettings: TGroupBox
    Left = 344
    Top = 382
    Width = 308
    Height = 153
    Caption = 'Other Mail Settings'
    TabOrder = 5
    object lbMailImages: TLabel
      Left = 10
      Top = 108
      Width = 67
      Height = 13
      Caption = 'List of Images'
    end
    object lbMailAttachments: TLabel
      Left = 10
      Top = 63
      Width = 93
      Height = 13
      Caption = 'List of Attachments'
    end
    object lbContentEncoding: TLabel
      Left = 10
      Top = 21
      Width = 85
      Height = 13
      Caption = 'Content Encoding'
    end
    object eContentEncoding: TEdit
      Left = 10
      Top = 36
      Width = 284
      Height = 21
      TabOrder = 0
    end
    object eMailAttachments: TAdvFileNameEdit
      Left = 10
      Top = 85
      Width = 284
      Height = 21
      Flat = False
      LabelFont.Charset = DEFAULT_CHARSET
      LabelFont.Color = clWindowText
      LabelFont.Height = -11
      LabelFont.Name = 'Tahoma'
      LabelFont.Style = []
      Lookup.Separator = ';'
      Color = clWindow
      Enabled = True
      ReadOnly = False
      TabOrder = 1
      Visible = True
      Version = '1.3.2.8'
      ButtonStyle = bsButton
      ButtonWidth = 18
      Etched = False
      Glyph.Data = {
        CE000000424DCE0000000000000076000000280000000C0000000B0000000100
        0400000000005800000000000000000000001000000000000000000000000000
        8000008000000080800080000000800080008080000080808000C0C0C0000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00D00000000DDD
        00000077777770DD00000F077777770D00000FF07777777000000FFF00000000
        00000FFFFFFF0DDD00000FFF00000DDD0000D000DDDDD0000000DDDDDDDDDD00
        0000DDDDD0DDD0D00000DDDDDD000DDD0000}
      FilterIndex = 0
      DialogOptions = []
      DialogKind = fdOpen
    end
    object eMailImages: TAdvFileNameEdit
      Left = 10
      Top = 124
      Width = 284
      Height = 21
      Flat = False
      LabelFont.Charset = DEFAULT_CHARSET
      LabelFont.Color = clWindowText
      LabelFont.Height = -11
      LabelFont.Name = 'Tahoma'
      LabelFont.Style = []
      Lookup.Separator = ';'
      Color = clWindow
      Enabled = True
      ReadOnly = False
      TabOrder = 2
      Visible = True
      Version = '1.3.2.8'
      ButtonStyle = bsButton
      ButtonWidth = 18
      Etched = False
      Glyph.Data = {
        CE000000424DCE0000000000000076000000280000000C0000000B0000000100
        0400000000005800000000000000000000001000000000000000000000000000
        8000008000000080800080000000800080008080000080808000C0C0C0000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00D00000000DDD
        00000077777770DD00000F077777770D00000FF07777777000000FFF00000000
        00000FFFFFFF0DDD00000FFF00000DDD0000D000DDDDD0000000DDDDDDDDDD00
        0000DDDDD0DDD0D00000DDDDDD000DDD0000}
      FilterIndex = 0
      DialogOptions = []
      DialogKind = fdOpen
    end
  end
  object XPManifest: TXPManifest
    Left = 431
    Top = 26
  end
  object RoboMailer: TRoboMailer
    OnBeforeMailSend = RoboMailerBeforeMailSend
    OnMailSuccess = RoboMailerMailSuccess
    OnMailFailure = RoboMailerMailFailure
    ConvertToCharSet = True
    ContentType = smtpHtml
    DataDelimiter = '%'
    OnMailsDone = RoboMailerMailsDone
    HTMLCharSet = 'utf-8'
    AuthType = smtpAuthNone
    CodePage = 28591
    CharSet = 'iso-8859-1'
    Blocking = False
    Port = 'smtp'
    Left = 367
    Top = 25
  end
end
