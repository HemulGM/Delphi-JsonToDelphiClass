unit uSaveUnitForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Layouts, FMX.Memo, FMX.Memo.Types, FMX.Controls.Presentation,
  FMX.ScrollBox, ChatGPT.Code, FMX.Memo.Style, FMX.TextLayout;

type
  TSaveUnitForm = class(TForm)
    Memo1: TMemo;
    Panel1: TPanel;
    btnClose: TButton;
    btnSave: TButton;
    sd: TSaveDialog;
    Label1: TLabel;
    procedure btnCloseClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure Memo1KeyUp(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
    procedure FormKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
    procedure FormCreate(Sender: TObject);
  private
    FCodeSyntax: TCodeSyntax;
    FStyledMemo: TStyledMemo;
    procedure UpdateLayout(Sender: TObject; Layout: TTextLayout; const Index: Integer);
  public
    { Public declarations }
  end;

var
  SaveUnitForm: TSaveUnitForm;

implementation

{$R *.fmx}

uses
  uMainForm, FMX.BehaviorManager;

procedure TSaveUnitForm.btnCloseClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TSaveUnitForm.btnSaveClick(Sender: TObject);
begin
  if sd.Execute then
  begin

  end;
end;

procedure TSaveUnitForm.FormCreate(Sender: TObject);
begin
  Memo1.ScrollAnimation := TBehaviorBoolean.True;
  Memo1.DisableDisappear := True;
  Memo1.ApplyStyleLookup;
  FStyledMemo := (Memo1.Presentation as TStyledMemo);
  FStyledMemo.OnUpdateLayoutParams := UpdateLayout;
  FCodeSyntax := TCodeSyntax.FindSyntax('pascal', Memo1.TextSettings.Font, Memo1.FontColor);
end;

procedure TSaveUnitForm.FormKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
begin
  if Key = 27 then
    ModalResult := mrCancel;
end;

procedure TSaveUnitForm.Memo1KeyUp(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
begin
  if Key = 27 then
    ModalResult := mrCancel;
end;

procedure TSaveUnitForm.UpdateLayout(Sender: TObject; Layout: TTextLayout; const Index: Integer);
begin
  if not Assigned(Layout) then
    Exit;
  Layout.BeginUpdate;
  try
    Layout.ClearAttributes;
    Layout.Padding.Top := 1;
    Layout.Padding.Bottom := 1;
    if Assigned(FCodeSyntax) then
      for var Attr in FCodeSyntax.GetAttributesForLine(Memo1.Lines[Index], Index) do
        Layout.AddAttribute(Attr.Range, Attr.Attribute);
  finally
    Layout.EndUpdate;
  end;
end;

end.

