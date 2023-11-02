unit uMainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Layouts, FMX.Memo, System.Json, Rest.Json, FMX.TreeView, TypInfo, RTTI,
  regularexpressions, generics.collections, Pkg.Json.Mapper, System.NetEncoding,
  FMX.Menus, FMX.Controls.Presentation, FMX.Edit, FMX.ConstrainedForm,
  REST.Client, System.Threading, FMX.Objects, SyncObjs, FMX.ScrollBox,
  FMX.Memo.Types, FMX.Effects, FMX.Filter.Effects, FMX.Memo.Style, ChatGPT.Code,
  FMX.TextLayout, FMX.Ani, HGM.MaterialDesignStyle, System.ImageList,
  FMX.ImgList, FMX.SVGIconImageList, FMX.TabControl;

const
  JsonValidatorUrl = 'https://jsonlint.com';

type
  TMainForm = class(TConstrainedForm)
    MemoJSON: TMemo;
    TreeViewVis: TTreeView;
    MainPopupMenu: TPopupMenu;
    MenuItemFieldCaption: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItemRenameProp: TMenuItem;
    Panel1: TLayout;
    Panel2: TLayout;
    Splitter1: TSplitter;
    PanelControls: TPanel;
    ButtonParse: TButton;
    btnOnlineJsonValidator: TButton;
    Label3: TLabel;
    MemoPopupMenu: TPopupMenu;
    MenuItemFormatJson: TMenuItem;
    MenuItemChangeClassName: TMenuItem;
    MenuItemChangeType: TMenuItem;
    MenuItemValidateJSON: TMenuItem;
    StyleBookMD3: TStyleBook;
    Label5: TLabel;
    EditUnitName: TEdit;
    Layout1: TLayout;
    CheckBoxMerge: TCheckBox;
    Layout2: TLayout;
    Layout3: TLayout;
    ButtonPaste: TButton;
    ImageList: TSVGIconImageList;
    ButtonParseTree: TButton;
    TreeViewItem1: TTreeViewItem;
    TreeViewItem2: TTreeViewItem;
    TreeViewItem3: TTreeViewItem;
    TreeViewItem4: TTreeViewItem;
    TreeViewItem5: TTreeViewItem;
    EditRootName: TEdit;
    Label2: TLabel;
    CheckBoxSort: TCheckBox;
    TabControlMain: TTabControl;
    TabItemJSON: TTabItem;
    TabItemUnit: TTabItem;
    Label4: TLabel;
    Label1: TLabel;
    Layout4: TLayout;
    Layout5: TLayout;
    ButtonCopy: TButton;
    MemoCode: TMemo;
    SaveDialogUnit: TSaveDialog;
    ButtonSaveUnit: TButton;
    AniIndicatorWork: TAniIndicator;
    ButtonCopyJSON: TButton;
    ButtonPasteAndParse: TButton;
    PathHintParse: TPath;
    Label6: TLabel;
    MenuItemJSONUndo: TMenuItem;
    MenuItem1: TMenuItem;
    MenuItemJSONCut: TMenuItem;
    MenuItemJSONCopy: TMenuItem;
    MenuItemJSONPaste: TMenuItem;
    MenuItemJSONDelete: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItemJSONSelectAll: TMenuItem;
    MenuItem4: TMenuItem;
    procedure ButtonParseClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure MainPopupMenuPopup(Sender: TObject);
    procedure TreeViewVisDblClick(Sender: TObject);
    procedure MenuItemRenamePropClick(Sender: TObject);
    procedure MenuItemChangeClassNameClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
    procedure TreeViewVisKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
    procedure MenuItemValidateJSONClick(Sender: TObject);
    procedure btnOnlineJsonValidatorClick(Sender: TObject);
    procedure MemoJSONChange(Sender: TObject);
    procedure MemoJSONChangeTracking(Sender: TObject);
    procedure MenuItemFormatJsonClick(Sender: TObject);
    procedure ButtonPasteClick(Sender: TObject);
    procedure ButtonCopyClick(Sender: TObject);
    procedure ButtonSaveUnitClick(Sender: TObject);
    procedure MemoJSONPaint(Sender: TObject; Canvas: TCanvas; const ARect: TRectF);
    procedure MemoCodePaint(Sender: TObject; Canvas: TCanvas; const ARect: TRectF);
    procedure MemoCodeChangeTracking(Sender: TObject);
    procedure MemoCodeChange(Sender: TObject);
    procedure ButtonCopyJSONClick(Sender: TObject);
    procedure ButtonPasteAndParseClick(Sender: TObject);
    procedure MenuItemJSONUndoClick(Sender: TObject);
    procedure MenuItemJSONSelectAllClick(Sender: TObject);
    procedure MenuItemJSONPasteClick(Sender: TObject);
    procedure MenuItemJSONDeleteClick(Sender: TObject);
    procedure MenuItemJSONCutClick(Sender: TObject);
    procedure MenuItemJSONCopyClick(Sender: TObject);
  private
    FMDStyle3: TMaterialDesignStyle3;
    FCodeSyntax: TCodeSyntax;
    FStyledMemo: TStyledMemo;
    FCheckVersionResponse: TObject;
    FChanged: boolean;
    FCodeSyntaxUnit: TCodeSyntax;
    FStyledMemoUnit: TStyledMemo;
    FMemoMarginsLeft, FMemoMarginsLeftUnit: Integer;
    procedure DisableMenuItems;
    procedure VisualizeClass;
    procedure PrepareMenu;
    procedure UpdateLayoutUnit(Sender: TObject; Layout: TTextLayout; const Index: Integer);
    procedure UpdateLayout(Sender: TObject; Layout: TTextLayout; const Index: Integer);
    procedure SetStyle;
    procedure FOnCaretMove(Sender: TObject);
    procedure DoWork;
    procedure EndWork;
  public
    JsonMapper: TPkgJsonMapper;
  end;

const
  ProgramVersion = 1.1;

var
  MainForm: TMainForm;

implementation

{$R *.fmx}

uses
  System.IOUtils, System.Math, HGM.ObjectHolder,
  {$IFDEF MSWINDOWS}
  Winapi.ShellAPI, Winapi.Windows,
  {$ENDIF MSWINDOWS}
  {$IFDEF POSIX}
  Posix.Stdlib,
  {$ENDIF POSIX} FMX.DialogService, FMX.BehaviorManager;

procedure TMainForm.btnOnlineJsonValidatorClick(Sender: TObject);
begin
  MenuItemValidateJSONClick(nil);
end;

procedure TMainForm.SetStyle;
begin
  var Style := TMaterialDesignStyle3.DefaultDarkPallete;
  Style.ColorPrimary := $FF245eab;
  Style.ColorOnPrimary := $FFffffff;
  Style.ColorSecondaryContainer := $FFcee5ff;
  Style.ColorOnSecondaryContainer := $FF001d32;
  Style.ColorSurface := $FFECF5FD;
  Style.ColorSurfaceContainer := $FFECF5FD;

  Style.ColorOutline := $FF74777f;
  Style.ColorPrimaryContainer := $FFd6e3ff;
  Style.ColorOnSurface := $FF001b3d;
  Style.ColorOnSurfaceVariant := $FF44474e;
  Style.ColorOnPrimaryContainer := $FF001b3e;
  Style.ColorOutlineVariant := $FFe0e2ec;
  Style.ColorError := $FFba1a1a;
  Style.ColorOnError := $FFffffff;

  Style.ColorPrimary008 := GetColorWithAlpha(Style.ColorPrimary, 8);
  Style.ColorPrimary012 := GetColorWithAlpha(Style.ColorPrimary, 12);
  Style.ColorOnSurface008 := GetColorWithAlpha(Style.ColorOnSurface, 12);
  Style.ColorOnSurfaceVariant008 := GetColorWithAlpha(Style.ColorOnSurfaceVariant, 8);
  Style.ColorOnSurfaceVariant012 := GetColorWithAlpha(Style.ColorOnSurfaceVariant, 12);
  Style.ColorOnSecondaryContainer008 := GetColorWithAlpha(Style.ColorOnSecondaryContainer, 8);
  Style.ColorSecondaryContainer000 := GetColorWithAlpha(Style.ColorSecondaryContainer, 0);

  Style.ColorSurfaceContainerLow := ColorLighter(Style.ColorSurfaceContainer, 5);
  Style.ColorSurfaceContainerHigh := ColorDarker(Style.ColorSurfaceContainer, 5);
  Style.ColorSurfaceContainerHighest := ColorDarker(Style.ColorSurfaceContainer, 10);

  FMDStyle3.ApplyStyle(Style);
end;

procedure TMainForm.ButtonParseClick(Sender: TObject);
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

procedure TMainForm.ButtonCopyClick(Sender: TObject);
begin
  MemoCode.CopyToClipboard;
end;

procedure TMainForm.ButtonCopyJSONClick(Sender: TObject);
begin
  MemoJSON.CopyToClipboard;
end;

procedure TMainForm.ButtonPasteAndParseClick(Sender: TObject);
begin
  MemoJSON.Lines.Clear;
  MemoJSON.PasteFromClipboard;
  MemoJSON.SelStart := 0;
  MenuItemFormatJsonClick(nil);
  ButtonParseClick(nil);
end;

procedure TMainForm.ButtonPasteClick(Sender: TObject);
begin
  MemoJSON.Lines.Clear;
  MemoJSON.PasteFromClipboard;
  MemoJSON.SelStart := 0;
  MenuItemFormatJsonClick(nil);
end;

procedure TMainForm.ButtonSaveUnitClick(Sender: TObject);
begin
  SaveDialogUnit.FileName := JsonMapper.DestinationUnitName + '.pas';
  if SaveDialogUnit.Execute then
  begin
    TFile.Create(SaveDialogUnit.FileName).Free;
    TFile.WriteAllText(SaveDialogUnit.FileName, MemoCode.Text, TEncoding.UTF8);
  end;
end;

procedure TMainForm.DisableMenuItems;
begin
  for var i := 0 to MainPopupMenu.ItemsCount - 1 do
    MainPopupMenu.Items[i].Enabled := False;
end;

procedure TMainForm.DoWork;
begin
  PanelControls.Enabled := False;
  AniIndicatorWork.Visible := True;
  AniIndicatorWork.Enabled := True;
end;

procedure TMainForm.EndWork;
begin
  PanelControls.Enabled := True;
  AniIndicatorWork.Visible := False;
  AniIndicatorWork.Enabled := False;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  // Style
  FMDStyle3 := TMaterialDesignStyle3.Create(StyleBookMD3);
  SetStyle;
  TAnimation.AniFrameRate := 300;
  TreeViewVis.AniCalculations.Animation := True;
  MemoJSON.ScrollAnimation := TBehaviorBoolean.True;
  MemoJSON.DisableDisappear := True;
  (MemoJSON.Presentation as TStyledMemo).OnCaretMove := FOnCaretMove;
  MemoJSON.ApplyStyleLookup;
  FStyledMemo := (MemoJSON.Presentation as TStyledMemo);
  FStyledMemo.OnUpdateLayoutParams := UpdateLayout;
  FCodeSyntax := TCodeSyntax.FindSyntax('json', MemoJSON.TextSettings.Font, MemoJSON.FontColor);
  MemoJSONChangeTracking(nil);

  MemoCode.ScrollAnimation := TBehaviorBoolean.True;
  MemoCode.DisableDisappear := True;
  (MemoCode.Presentation as TStyledMemo).OnCaretMove := FOnCaretMove;
  MemoCode.ApplyStyleLookup;
  FStyledMemoUnit := (MemoCode.Presentation as TStyledMemo);
  FStyledMemoUnit.OnUpdateLayoutParams := UpdateLayoutUnit;
  FCodeSyntaxUnit := TCodeSyntax.FindSyntax('pascal', MemoCode.TextSettings.Font, MemoCode.FontColor);
  MemoCodeChangeTracking(nil);

  Caption := 'JsonToDelphiClass v' + FloatToStr(ProgramVersion, PointDsFormatSettings) + ' | By Petar Georgiev feat HemulGM';

  JsonMapper := TPkgJsonMapper.Create(TreeViewVis);

  TabControlMain.ActiveTab := TabItemJSON;
  Width := 1400;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(JsonMapper);
  FreeAndNil(FCheckVersionResponse);
end;

procedure TMainForm.FormKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
begin
  if Key = 27 then
    Close;
end;

procedure TMainForm.MemoCodeChange(Sender: TObject);
begin
  FCodeSyntaxUnit.DropCache;
  var Memo :=(MemoCode.Presentation as TStyledMemo);
  Memo.UpdateVisibleLayoutParams;
end;

procedure TMainForm.MemoCodeChangeTracking(Sender: TObject);
begin
  var Memo :=(MemoCode.Presentation as TStyledMemo);
  MemoCode.Canvas.Font.Assign(MemoCode.TextSettings.Font);
  var W := Ceil(MemoCode.Canvas.TextWidth(MemoCode.Lines.Count.ToString) + 20);
  MemoCode.StylesData['content_client.Padding.Left'] := W;
  FMemoMarginsLeftUnit := W;
  Memo.UpdateVisibleLayoutParams;
  Memo.RealignContent;
end;

procedure TMainForm.MemoCodePaint(Sender: TObject; Canvas: TCanvas; const ARect: TRectF);
begin
    // Line number
  var BRect := MemoCode.BoundsRect;
  var Memo :=(MemoCode.Presentation as TStyledMemo);
  for var i := 0 to Memo.LineObjects.Count - 1 do
    if Memo.LineObjects.Items[i].SizeValid then
    begin
      var Rect := Memo.LineObjects.Items[i].Rect;
      Rect.Left := 0;
      Rect.Width := FMemoMarginsLeftUnit - 10;
      if Rect.Top < BRect.Height then
        Rect.Bottom := Min(Rect.Bottom, MemoCode.ContentLayout.Height);
      Rect.Offset(2, 2);
      Rect.NormalizeRect;
      if (Rect.Top < 0) and (Rect.Bottom < 0) then
        Continue;
      if (Rect.Top > BRect.Height) and (Rect.Bottom > BRect.Height) then
        Continue;
      Canvas.Fill.Color := TAlphaColorRec.White;
      Canvas.Font.Assign(MemoCode.TextSettings.Font);
      var HDelta: Single :=(100 / Memo.LineObjects.Items[i].Rect.Height * Rect.Height) / 100;
      if i = Memo.CaretPosition.Line then
      begin
        Canvas.FillRect(Rect, 0.1);
        HDelta := 300;
      end;
      Canvas.FillText(Rect, (i + 1).ToString, False, 0.3 * HDelta, [], TTextAlign.Trailing, TTextAlign.Leading);
    end;
end;

procedure TMainForm.MemoJSONChange(Sender: TObject);
begin
  FCodeSyntax.DropCache;
  FStyledMemo.UpdateVisibleLayoutParams;
  FStyledMemo.Repaint;
end;

procedure TMainForm.MemoJSONChangeTracking(Sender: TObject);
begin
  var Memo :=(MemoJSON.Presentation as TStyledMemo);
  MemoJSON.Canvas.Font.Assign(MemoJSON.TextSettings.Font);
  var W := Ceil(MemoJSON.Canvas.TextWidth(MemoJSON.Lines.Count.ToString) + 20);
  MemoJSON.StylesData['content_client.Padding.Left'] := W;
  FMemoMarginsLeft := W;
  Memo.UpdateVisibleLayoutParams;
  Memo.RealignContent;

  FCodeSyntax.DropCache;
  FStyledMemo.UpdateVisibleLayoutParams;
  FStyledMemo.Repaint;
end;

procedure TMainForm.FOnCaretMove(Sender: TObject);
begin
  //
end;

procedure TMainForm.MemoJSONPaint(Sender: TObject; Canvas: TCanvas; const ARect: TRectF);
begin
    // Line number
  var BRect := MemoJSON.BoundsRect;
  var Memo :=(MemoJSON.Presentation as TStyledMemo);
  for var i := 0 to Memo.LineObjects.Count - 1 do
    if Memo.LineObjects.Items[i].SizeValid then
    begin
      var Rect := Memo.LineObjects.Items[i].Rect;
      Rect.Left := 0;
      Rect.Width := FMemoMarginsLeft - 10;
      if Rect.Top < BRect.Height then
        Rect.Bottom := Min(Rect.Bottom, MemoJSON.ContentLayout.Height);
      Rect.Offset(2, 2);
      Rect.NormalizeRect;
      if (Rect.Top < 0) and (Rect.Bottom < 0) then
        Continue;
      if (Rect.Top > BRect.Height) and (Rect.Bottom > BRect.Height) then
        Continue;
      Canvas.Fill.Color := TAlphaColorRec.Black;
      Canvas.Font.Assign(MemoJSON.TextSettings.Font);
      var HDelta: Single :=(100 / Memo.LineObjects.Items[i].Rect.Height * Rect.Height) / 100;
      if i = Memo.CaretPosition.Line then
      begin
        Canvas.FillRect(Rect, 0.1);
        HDelta := 300;
      end;
      Canvas.FillText(Rect, (i + 1).ToString, False, 0.3 * HDelta, [], TTextAlign.Trailing, TTextAlign.Leading);
    end;
end;

procedure TMainForm.MenuItemRenamePropClick(Sender: TObject);
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
        FChanged := True;
        LField.Name := LString;
        JsonMapper.Visualize(TreeViewVis, '');
      end;
    end);
end;

procedure TMainForm.MenuItemFormatJsonClick(Sender: TObject);
var
  LTsl: TStringList;
  LJsonValue: TJSONValue;
begin
  LTsl := TStringList.Create;
  try
    LJsonValue := TJSONObject.ParseJSONValue(MemoJSON.Text);
    try
      if LJsonValue <> nil then
        PrettyPrintJSON(LJsonValue, LTsl);
    finally
      LJsonValue.Free;
    end;
    MemoJSON.Text := LTsl.Text;
  finally
    LTsl.Free;
  end;
end;

procedure TMainForm.MenuItemJSONCopyClick(Sender: TObject);
begin
  MemoJSON.CopyToClipboard;
end;

procedure TMainForm.MenuItemJSONCutClick(Sender: TObject);
begin
  MemoJSON.CutToClipboard;
end;

procedure TMainForm.MenuItemJSONDeleteClick(Sender: TObject);
begin
  MemoJSON.DeleteSelection;
end;

procedure TMainForm.MenuItemJSONPasteClick(Sender: TObject);
begin
  MemoJSON.PasteFromClipboard;
end;

procedure TMainForm.MenuItemJSONSelectAllClick(Sender: TObject);
begin
  MemoJSON.SelectAll;
end;

procedure TMainForm.MenuItemJSONUndoClick(Sender: TObject);
begin
  MemoJSON.UnDo;
end;

procedure TMainForm.MenuItemChangeClassNameClick(Sender: TObject);
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
        FChanged := True;
        LClass.Name := LString;
        JsonMapper.Visualize(TreeViewVis, '');
      end;
    end);
end;

procedure TMainForm.MenuItemValidateJSONClick(Sender: TObject);
begin
  {$IFDEF MSWINDOWS}
  ShellExecute(0, 'open', PChar(JsonValidatorUrl), '', '', SW_SHOWNORMAL);
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
  MenuItemFieldCaption.Text := '---';
  LPoint := TreeViewVis.AbsoluteToLocal(ScreenToClient(MainPopupMenu.PopupPoint));
  LItem := TreeViewVis.ItemByPoint(LPoint.X, LPoint.Y);
  if LItem <> nil then
    LItem.Select;

  PrepareMenu;
end;

procedure TMainForm.PrepareMenu;
var
  LField: TStubField;
begin
  if TreeViewVis.Selected <> nil then
  begin
    MenuItemFieldCaption.Text := TreeViewVis.Selected.Text;

    if TreeViewVis.Selected <> TreeViewVis.Items[0] then
    begin
      LField := TreeViewVis.Selected.TagObject as TStubField;

      MenuItemRenameProp.Enabled := true;
      MenuItemRenameProp.TagObject := LField;

      if (LField is TStubContainerField) and ((LField as TStubContainerField).ContainedType = TJsonType.jtObject) then
      begin
        MenuItemChangeClassName.Enabled := true;
        MenuItemChangeClassName.TagObject := (LField as TStubContainerField).FieldClass;
      end;
    end
    else
    begin
      MenuItemChangeClassName.Enabled := true;
      MenuItemChangeClassName.TagObject := TreeViewVis.Selected.TagObject;
    end;
  end;
end;

procedure TMainForm.TreeViewVisDblClick(Sender: TObject);
begin
  if TreeViewVis.Selected <> nil then
  begin
    if TreeViewVis.Selected.Enabled then
      TreeViewVis.Selected.IsExpanded := not TreeViewVis.Selected.IsExpanded;
  end;
end;

procedure TMainForm.TreeViewVisKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
begin
  if ((KeyChar = #0) and (Key = 113)) and (TreeViewVis.Selected <> nil) then
  begin
    PrepareMenu;
    if TreeViewVis.Selected = TreeViewVis.Items[0] then
      MenuItemChangeClassNameClick(MenuItemChangeClassName)
    else
      MenuItemRenamePropClick(MenuItemRenameProp);
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
      for var Attr in FCodeSyntax.GetAttributesForLine(MemoJSON.Lines[Index], Index) do
        Layout.AddAttribute(Attr.Range, Attr.Attribute);
  finally
    Layout.EndUpdate;
  end;
end;

procedure TMainForm.UpdateLayoutUnit(Sender: TObject; Layout: TTextLayout; const Index: Integer);
begin
  if not Assigned(Layout) then
    Exit;
  Layout.BeginUpdate;
  try
    Layout.ClearAttributes;
    Layout.Padding.Top := 1;
    Layout.Padding.Bottom := 1;
    if Assigned(FCodeSyntaxUnit) then
      for var Attr in FCodeSyntaxUnit.GetAttributesForLine(MemoCode.Lines[Index], Index) do
        Layout.AddAttribute(Attr.Range, Attr.Attribute);
  finally
    Layout.EndUpdate;
  end;
end;

procedure TMainForm.VisualizeClass;
begin
  FChanged := False;
  TreeViewVis.Enabled := True;
  ButtonParseTree.Visible := False;
  PathHintParse.Visible := False;
  DoWork;
  TaskRun(Self,
    procedure(Holder: IComponentHolder)
    begin
      var Error: string;
      try
        JsonMapper.Parse(MemoJSON.Text, CheckBoxMerge.IsChecked, CheckBoxSort.IsChecked, EditRootName.Text);
      except
        on E: Exception do
          Error := E.Message;
      end;
      TThread.Queue(nil,
        procedure
        begin
          if not Holder.IsLive then
            Exit;

          EndWork;

          if not Error.IsEmpty then
            raise Exception.Create(Error);

          JsonMapper.Visualize(TreeViewVis, '');
          JsonMapper.DestinationUnitName := EditUnitName.Text;
          MemoCode.Text := JsonMapper.GenerateUnit;
        end);
    end);
end;

end.

