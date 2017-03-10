object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 417
  ClientWidth = 653
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 32
    Width = 111
    Height = 13
    Caption = 'Battery Percentage'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label2: TLabel
    Left = 8
    Top = 51
    Width = 122
    Height = 13
    Caption = 'Console Display State'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label3: TLabel
    Left = 8
    Top = 70
    Width = 119
    Height = 13
    Caption = 'Global User Presence'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label4: TLabel
    Left = 256
    Top = 13
    Width = 82
    Height = 13
    Caption = 'Monitor Power'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label5: TLabel
    Left = 256
    Top = 32
    Width = 116
    Height = 13
    Caption = 'Power Saving Status'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label6: TLabel
    Left = 256
    Top = 51
    Width = 121
    Height = 13
    Caption = 'Session Display State'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label7: TLabel
    Left = 256
    Top = 70
    Width = 127
    Height = 13
    Caption = 'Session User Presence'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object lblAwayModeLabel: TLabel
    Left = 256
    Top = 89
    Width = 65
    Height = 13
    Caption = 'Away Mode'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object lblBatteryPerc: TLabel
    Left = 144
    Top = 32
    Width = 67
    Height = 13
    Caption = '[Undefined]'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object lblConsoleDisplay: TLabel
    Left = 144
    Top = 51
    Width = 67
    Height = 13
    Caption = '[Undefined]'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object lblGlobalUser: TLabel
    Left = 144
    Top = 70
    Width = 67
    Height = 13
    Caption = '[Undefined]'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object lblMonitorPower: TLabel
    Left = 400
    Top = 13
    Width = 67
    Height = 13
    Caption = '[Undefined]'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object lblPowerSaver: TLabel
    Left = 400
    Top = 32
    Width = 67
    Height = 13
    Caption = '[Undefined]'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object lblSessionDisplay: TLabel
    Left = 400
    Top = 51
    Width = 67
    Height = 13
    Caption = '[Undefined]'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object lblSessionUser: TLabel
    Left = 400
    Top = 70
    Width = 67
    Height = 13
    Caption = '[Undefined]'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object lblAwayMode: TLabel
    Left = 400
    Top = 89
    Width = 67
    Height = 13
    Caption = '[Undefined]'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label9: TLabel
    Left = 8
    Top = 13
    Width = 77
    Height = 13
    Caption = 'Power Source'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object lblPowerSource: TLabel
    Left = 144
    Top = 13
    Width = 67
    Height = 13
    Caption = '[Undefined]'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label10: TLabel
    Left = 496
    Top = 13
    Width = 75
    Height = 13
    Caption = 'Hibernate At:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label11: TLabel
    Left = 8
    Top = 89
    Width = 102
    Height = 13
    Caption = 'Power Personality'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object lblPersonality: TLabel
    Left = 144
    Top = 89
    Width = 67
    Height = 13
    Caption = '[Undefined]'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Log: TMemo
    Left = 0
    Top = 120
    Width = 653
    Height = 297
    Align = alBottom
    Anchors = [akLeft, akTop, akRight, akBottom]
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 0
    WordWrap = False
  end
  object BitBtn1: TBitBtn
    Left = 496
    Top = 46
    Width = 139
    Height = 25
    Caption = 'Hibernate Now'
    TabOrder = 1
    OnClick = BitBtn1Click
  end
  object txtPerc: TSpinEdit
    Left = 585
    Top = 8
    Width = 50
    Height = 22
    MaxValue = 99
    MinValue = 1
    TabOrder = 2
    Value = 20
  end
end
