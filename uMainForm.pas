unit uMainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Layouts, FMX.Memo, System.Json, Rest.Json, FMX.TreeView, TypInfo, RTTI,
  regularexpressions, generics.collections, Pkg.Json.Mapper, System.NetEncoding,
  FMX.Menus, FMX.Controls.Presentation, FMX.Edit, FMX.ConstrainedForm,
  REST.Client, uUpdate, System.Threading, uGitHub, FMX.Objects, uUpdateForm,
  SyncObjs, FMX.ScrollBox, FMX.Memo.Types, FMX.Effects, FMX.Filter.Effects,
  FMX.Memo.Style, ChatGPT.Code, FMX.TextLayout, FMX.Ani;

const
  JsonValidatorUrl = 'https://jsonlint.com';

type
  TMainForm = class(TConstrainedForm)
    Memo1: TMemo;
    tv: TTreeView;
    MainPopupMenu: TPopupMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    Panel1: TLayout;
    Panel2: TLayout;
    Splitter1: TSplitter;
    Panel3: TPanel;
    btnVisualize: TButton;
    btnOnlineJsonValidator: TButton;
    btnExit: TButton;
    Label3: TLabel;
    Label4: TLabel;
    MemoPopupMenu: TPopupMenu;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    btnGenerateUnit: TButton;
    StyleBookMD3: TStyleBook;
    Label5: TLabel;
    Edit2: TEdit;
    procedure btnVisualizeClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure PreviewUnitClick(Sender: TObject);
    procedure btnExitClick(Sender: TObject);
    procedure MainPopupMenuPopup(Sender: TObject);
    procedure tvDblClick(Sender: TObject);
    procedure MenuItem3Click(Sender: TObject);
    procedure MenuItem5Click(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
    procedure tvKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
    procedure MenuItem8Click(Sender: TObject);
    procedure btnOnlineJsonValidatorClick(Sender: TObject);
    procedure Memo1Change(Sender: TObject);
    procedure Memo1ChangeTracking(Sender: TObject);
    procedure MenuItem4Click(Sender: TObject);
  private
    procedure DisableMenuItems;
    procedure VisualizeClass;
    procedure PrepareMenu;
    procedure UpdateLayout(Sender: TObject; Layout: TTextLayout; const Index: Integer);
  public
    jm: TPkgJsonMapper;
    FCodeSyntax: TCodeSyntax;
    FStyledMemo: TStyledMemo;
    FCheckVersionResponse: TObject;
    FChanged: boolean;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.fmx}

uses
  uSaveUnitForm,
  {$IFDEF MSWINDOWS}
  Winapi.ShellAPI, Winapi.Windows,
  {$ENDIF MSWINDOWS}
  {$IFDEF POSIX}
  Posix.Stdlib,
  {$ENDIF POSIX} FMX.DialogService, FMX.BehaviorManager;

procedure TMainForm.btnOnlineJsonValidatorClick(Sender: TObject);
begin
  MenuItem8Click(nil);
end;

procedure TMainForm.btnVisualizeClick(Sender: TObject);
begin
  if FChanged then
    TDialogService.MessageDialog('You made changes to the structure. Do you want to load original class?', TMsgDlgType.mtWarning, [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo], TMsgDlgBtn.mbNo, 0,
      procedure(const AResult: TModalResult)
      begin
        if AResult = mrYes then
          VisualizeClass;
      end)
  else
    VisualizeClass;
end;

procedure TMainForm.DisableMenuItems;
var
  k: integer;
begin
  for k := 0 to MainPopupMenu.ItemsCount - 1 do
  begin
    MainPopupMenu.Items[k].Enabled := false;
  end;
end;

procedure TMainForm.PreviewUnitClick(Sender: TObject);
begin
  if tv.Count = 0 then
    btnVisualizeClick(self);

  jm.DestinationUnitName := edit2.Text;
  SaveUnitForm.StyleBook := StyleBook;
  SaveUnitForm.sd.FileName := jm.DestinationUnitName + '.pas';

  SaveUnitForm.Memo1.DeleteSelection;
  SaveUnitForm.Memo1.Text := jm.GenerateUnit;
  SaveUnitForm.Caption := 'Preview Delphi Unit - ' + SaveUnitForm.sd.FileName;

  SaveUnitForm.ShowModal;
end;

procedure TMainForm.btnExitClick(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  TAnimation.AniFrameRate := 300;
  tv.AniCalculations.Animation := True;
  Memo1.ScrollAnimation := TBehaviorBoolean.True;
  Memo1.DisableDisappear := True;
  Memo1.ApplyStyleLookup;
  FStyledMemo := (Memo1.Presentation as TStyledMemo);
  FStyledMemo.OnUpdateLayoutParams := UpdateLayout;
  FCodeSyntax := TCodeSyntax.FindSyntax('json', Memo1.TextSettings.Font, Memo1.FontColor);
  self.Constraints.MinWidth := 1024;
  self.Constraints.MinHeight := 560;

  Caption := 'JsonToDelphiClass - ' + FloatToStr(ProgramVersion, PointDsFormatSettings) + ' | By HemulGM';

  jm := TPkgJsonMapper.Create(tv);
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(jm);
  FreeAndNil(FCheckVersionResponse);
end;

procedure TMainForm.FormKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
begin
  if Key = 27 then
    Close;
end;

procedure TMainForm.Memo1Change(Sender: TObject);
begin
  FCodeSyntax.DropCache;
  FStyledMemo.UpdateVisibleLayoutParams;
  FStyledMemo.Repaint;
end;

procedure TMainForm.Memo1ChangeTracking(Sender: TObject);
begin
  FCodeSyntax.DropCache;
  FStyledMemo.UpdateVisibleLayoutParams;
  FStyledMemo.Repaint;
end;

procedure TMainForm.MenuItem3Click(Sender: TObject);
var
  LString: string;
  LField: TStubField;
begin
  LField := (Sender as TFmxObject).TagObject as TStubField;
  TDialogService.InputQuery('Rename Property ' + LField.Name, ['Enter new Property name'], [LField.Name],
    procedure(const AResult: TModalResult; const AValues: array of string)
    begin
      LString := AValues[0];
      if (LString <> '') and (LString.ToLower <> LField.Name.ToLower) then
      begin
        FChanged := true;
        LField.Name := LString;
        jm.Visualize(tv, '');
      end;
    end);
end;

procedure TMainForm.MenuItem4Click(Sender: TObject);
var
  LTsl: TStringList;
  LJsonValue: TJSONValue;
begin
  LTsl := TStringList.Create;
  try
    LJsonValue := TJSONObject.ParseJSONValue(memo1.Text);
    try
      if LJsonValue <> nil then
        PrettyPrintJSON(LJsonValue, LTsl);
    finally
      LJsonValue.Free;
    end;
    memo1.Text := LTsl.Text;
  finally
    LTsl.Free;
  end;
end;

procedure TMainForm.MenuItem5Click(Sender: TObject);
var
  LString: string;
  LClass: TStubClass;
begin
  LClass := (Sender as TFmxObject).TagObject as TStubClass;
  TDialogService.InputQuery('Rename Class ' + LClass.Name, ['Enter new Class name'], [LClass.PureClassName],
    procedure(const AResult: TModalResult; const AValues: array of string)
    begin
      LString := AValues[0];
      if (LString <> '') and (LString.ToLower <> LClass.PureClassName.ToLower) then
      begin
        FChanged := true;
        LClass.Name := LString;
        jm.Visualize(tv, '');
      end;
    end);
end;

procedure TMainForm.MenuItem8Click(Sender: TObject);
begin
  {$IFDEF MSWINDOWS}
  ShellExecute(0, 'OPEN', PChar(JsonValidatorUrl), '', '', SW_SHOWNORMAL);
  {$ENDIF MSWINDOWS}
  {$IFDEF POSIX}
  _system(PAnsiChar('open ' + AnsiString(JsonValidatorUrl)));
  {$ENDIF POSIX}
end;

procedure TMainForm.MainPopupMenuPopup(Sender: TObject);
var
  LItem: TTreeViewItem;
  LPoint: TPointF;
begin
  DisableMenuItems;
  MainPopupMenu.Items[0].Text := '---';
  LPoint := tv.AbsoluteToLocal(ScreenToClient(MainPopupMenu.PopupPoint));
  LItem := tv.ItemByPoint(LPoint.X, LPoint.Y);
  if LItem <> nil then
    LItem.Select;

  PrepareMenu;
end;

procedure TMainForm.PrepareMenu;
var
  LField: TStubField;
begin
  if tv.Selected <> nil then
  begin
    MainPopupMenu.Items[0].Text := tv.Selected.Text;

    if tv.Selected <> tv.Items[0] then
    begin
      LField := tv.Selected.TagObject as TStubField;

      MainPopupMenu.Items[2].Enabled := true;
      MainPopupMenu.Items[2].TagObject := LField;

      if (LField is TStubContainerField) and ((LField as TStubContainerField).ContainedType = TJsonType.jtObject) then
      begin
        MainPopupMenu.Items[3].Enabled := true;
        MainPopupMenu.Items[3].TagObject := (LField as TStubContainerField).FieldClass;
      end;
    end
    else
    begin
      MainPopupMenu.Items[3].Enabled := true;
      MainPopupMenu.Items[3].TagObject := tv.Selected.TagObject;
    end;
  end;
end;

procedure TMainForm.tvDblClick(Sender: TObject);
begin
  if tv.Selected <> nil then
    tv.Selected.IsExpanded := not tv.Selected.IsExpanded;
end;

procedure TMainForm.tvKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
begin
  if ((KeyChar = #0) and (Key = 113)) and (tv.Selected <> nil) then
  begin
    PrepareMenu;

    if tv.Selected = tv.Items[0] then
      MenuItem5Click(MenuItem5)
    else
      MenuItem3Click(MenuItem3);
  end;
end;

procedure TMainForm.UpdateLayout(Sender: TObject; Layout: TTextLayout; const Index: Integer);
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

procedure TMainForm.VisualizeClass;
begin
  FChanged := false;

  jm.Parse(memo1.Text, 'Root');
  jm.Visualize(tv, '');

  //  Workarround for QC129540
  Panel1.Width := Panel1.Width + 1;
  Panel1.Width := Panel1.Width - 1;
end;

end.

