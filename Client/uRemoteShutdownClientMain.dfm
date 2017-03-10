object frmCliMain: TfrmCliMain
  Left = 0
  Top = 0
  Caption = 'Remote Shutdown Client'
  ClientHeight = 552
  ClientWidth = 457
  Color = 16378098
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 0
    Top = 339
    Width = 457
    Height = 5
    Cursor = crVSplit
    Align = alBottom
    Beveled = True
    ExplicitTop = 365
    ExplicitWidth = 450
  end
  object lstClients: TListView
    Left = 0
    Top = 0
    Width = 457
    Height = 297
    Align = alTop
    Anchors = [akLeft, akTop, akRight, akBottom]
    Checkboxes = True
    Columns = <
      item
        Caption = 'Display Name'
        Width = 220
      end
      item
        Caption = 'IP Address'
        Width = 120
      end>
    HideSelection = False
    ReadOnly = True
    RowSelect = True
    TabOrder = 0
    ViewStyle = vsReport
    ExplicitWidth = 450
  end
  object Panel1: TPanel
    Left = 0
    Top = 344
    Width = 457
    Height = 208
    Align = alBottom
    TabOrder = 1
    ExplicitWidth = 450
    DesignSize = (
      457
      208)
    object Label1: TLabel
      Left = 333
      Top = 11
      Width = 44
      Height = 13
      Caption = 'Time-Out'
    end
    object cmdRefresh: TBitBtn
      Left = 8
      Top = 8
      Width = 97
      Height = 25
      Caption = '&Refresh'
      Glyph.Data = {
        DE010000424DDE01000000000000760000002800000024000000120000000100
        0400000000006801000000000000000000001000000000000000000000000000
        80000080000000808000800000008000800080800000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333444444
        33333333333F8888883F33330000324334222222443333388F3833333388F333
        000032244222222222433338F8833FFFFF338F3300003222222AAAAA22243338
        F333F88888F338F30000322222A33333A2224338F33F8333338F338F00003222
        223333333A224338F33833333338F38F00003222222333333A444338FFFF8F33
        3338888300003AAAAAAA33333333333888888833333333330000333333333333
        333333333333333333FFFFFF000033333333333344444433FFFF333333888888
        00003A444333333A22222438888F333338F3333800003A2243333333A2222438
        F38F333333833338000033A224333334422224338338FFFFF8833338000033A2
        22444442222224338F3388888333FF380000333A2222222222AA243338FF3333
        33FF88F800003333AA222222AA33A3333388FFFFFF8833830000333333AAAAAA
        3333333333338888883333330000333333333333333333333333333333333333
        0000}
      NumGlyphs = 2
      TabOrder = 0
      OnClick = cmdRefreshClick
    end
    object cmdSend: TBitBtn
      Left = 327
      Top = 176
      Width = 121
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = '&Send Command'
      Glyph.Data = {
        DE010000424DDE01000000000000760000002800000024000000120000000100
        0400000000006801000000000000000000001000000000000000000000000000
        80000080000000808000800000008000800080800000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
        3333333333333333333333330000333333333333333333333333F33333333333
        00003333344333333333333333388F3333333333000033334224333333333333
        338338F3333333330000333422224333333333333833338F3333333300003342
        222224333333333383333338F3333333000034222A22224333333338F338F333
        8F33333300003222A3A2224333333338F3838F338F33333300003A2A333A2224
        33333338F83338F338F33333000033A33333A222433333338333338F338F3333
        0000333333333A222433333333333338F338F33300003333333333A222433333
        333333338F338F33000033333333333A222433333333333338F338F300003333
        33333333A222433333333333338F338F00003333333333333A22433333333333
        3338F38F000033333333333333A223333333333333338F830000333333333333
        333A333333333333333338330000333333333333333333333333333333333333
        0000}
      NumGlyphs = 2
      TabOrder = 1
      OnClick = cmdSendClick
      ExplicitLeft = 320
      ExplicitTop = 56
    end
    object cboCommand: TComboBox
      Left = 160
      Top = 8
      Width = 97
      Height = 21
      Style = csDropDownList
      ItemIndex = 2
      TabOrder = 2
      Text = 'Hibernate'
      Items.Strings = (
        'Shutdown'
        'Restart'
        'Hibernate')
    end
    object chkForce: TCheckBox
      Left = 160
      Top = 35
      Width = 177
      Height = 17
      Caption = 'Force applications to close'
      TabOrder = 3
    end
    object txtComment: TMemo
      Left = 8
      Top = 80
      Width = 440
      Height = 90
      Anchors = [akLeft, akTop, akRight, akBottom]
      Lines.Strings = (
        
          'Initiated by your system administrator using the JD Remote Shutd' +
          'own system.')
      ScrollBars = ssVertical
      TabOrder = 4
      ExplicitWidth = 433
    end
    object chkHybrid: TCheckBox
      Left = 160
      Top = 57
      Width = 177
      Height = 17
      Caption = 'Hybrid Shutdown'
      TabOrder = 5
    end
    object txtTimeout: TSpinEdit
      Left = 384
      Top = 8
      Width = 57
      Height = 22
      MaxValue = 86400
      MinValue = 0
      TabOrder = 6
      Value = 0
    end
  end
  object Web: TIdHTTP
    AllowCookies = True
    ProxyParams.BasicAuthentication = False
    ProxyParams.ProxyPort = 0
    Request.ContentLength = -1
    Request.ContentRangeEnd = -1
    Request.ContentRangeStart = -1
    Request.ContentRangeInstanceLength = -1
    Request.Accept = 'text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8'
    Request.BasicAuthentication = False
    Request.UserAgent = 'Mozilla/3.0 (compatible; Indy Library)'
    Request.Ranges.Units = 'bytes'
    Request.Ranges = <>
    HTTPOptions = [hoForceEncodeParams]
    Left = 72
    Top = 208
  end
end
