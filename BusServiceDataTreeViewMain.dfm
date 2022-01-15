object BusServiceDataTreeViewMainForm: TBusServiceDataTreeViewMainForm
  Left = 0
  Top = 0
  Caption = 'BusServiceDataTreeViewMainForm'
  ClientHeight = 525
  ClientWidth = 934
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object treeView: TTreeView
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 928
    Height = 424
    Align = alClient
    Indent = 19
    TabOrder = 0
    OnExpanding = treeViewExpanding
    ExplicitLeft = -2
  end
  object logMemo: TMemo
    AlignWithMargins = True
    Left = 3
    Top = 433
    Width = 928
    Height = 89
    Align = alBottom
    Color = clBtnFace
    Ctl3D = False
    ParentCtl3D = False
    ScrollBars = ssVertical
    TabOrder = 1
    Visible = False
  end
  object actions: TActionManager
    Left = 600
    Top = 104
    StyleName = 'Platform Default'
    object openInputFile: TAction
      Caption = 'Open'
      OnExecute = openInputFileExecute
    end
  end
  object inputFileOpenDialog: TFileOpenDialog
    FavoriteLinks = <>
    FileTypes = <
      item
        DisplayName = 'CSV files (*.csv)'
        FileMask = '*.csv'
      end
      item
        DisplayName = 'All files (*.*)'
        FileMask = '*.*'
      end>
    Options = []
    Left = 840
    Top = 72
  end
  object MainMenu: TMainMenu
    Left = 456
    Top = 264
    object mainFileMenu: TMenuItem
      Caption = 'File'
      object fileOpenMenu: TMenuItem
        Action = openInputFile
      end
    end
  end
end
