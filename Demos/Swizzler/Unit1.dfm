object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 300
  ClientWidth = 635
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object TabSet1: TTabSet
    Left = 0
    Top = 279
    Width = 635
    Height = 21
    Align = alBottom
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    StartMargin = 100
    Tabs.Strings = (
      'One'
      'Two'
      'Three')
    TabIndex = 0
  end
  object Button1: TButton
    Left = 268
    Top = 164
    Width = 75
    Height = 25
    Caption = 'Button1'
    TabOrder = 1
    OnClick = Button1Click
  end
  object Memo1: TMemo
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 629
    Height = 138
    Align = alTop
    Lines.Strings = (
      'Demo of method "swizzling"'
      ''
      
        'The SetStartMargin method of TTabSet is "swizzled" (effectively ' +
        'overridden) to prevent the StartMargin property from being '
      'altered. This technique is used in the Codex Delphi add-in: '
      ''
      '  https://www.delphiworlds.com/codex'
      ''
      
        'So that the editor tabs in the IDE stay on the left-hand side of' +
        ' the status bar, as they were in older versions of Delphi')
    ReadOnly = True
    TabOrder = 2
  end
end
