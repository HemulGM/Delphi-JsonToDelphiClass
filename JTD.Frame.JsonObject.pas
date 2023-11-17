unit JTD.Frame.JsonObject;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Memo.Types, FMX.Objects, FMX.ScrollBox, FMX.Memo, FMX.Edit,
  FMX.Controls.Presentation, FMX.TreeView, FMX.Layouts, FMX.TabControl,
  System.JSON, FMX.TextLayout, Json.Mapper, ChatGPT.Code, FMX.Memo.Style,
  FMX.Menus;

type
  TJSONError = record
    Path: string;
    Offset: Integer;
    Line: Integer;
    Position: Integer;
    Available: Boolean;
  end;

  TFrameJsonObject = class(TFrame)
    LayoutObjectStruct: TLayout;
    TabControlView: TTabControl;
    TabItemJOClass: TTabItem;
    TreeViewJOClass: TTreeView;
    TreeViewItem1: TTreeViewItem;
    TreeViewItem2: TTreeViewItem;
    TreeViewItem3: TTreeViewItem;
    TreeViewItem4: TTreeViewItem;
    TreeViewItem5: TTreeViewItem;
    TabItemJOClasses: TTabItem;
    TreeViewClasses: TTreeView;
    TreeViewItem6: TTreeViewItem;
    TreeViewItem7: TTreeViewItem;
    TreeViewItem8: TTreeViewItem;
    TreeViewItem9: TTreeViewItem;
    TreeViewItem10: TTreeViewItem;
    ButtonJOParseTree: TButton;
    PanelControls: TPanel;
    ButtonJOParse: TButton;
    ButtonJOOnlineJsonValidator: TButton;
    Layout1: TLayout;
    EditUnitName: TEdit;
    Label5: TLabel;
    CheckBoxMerge: TCheckBox;
    EditRootName: TEdit;
    Label2: TLabel;
    CheckBoxSort: TCheckBox;
    CheckBoxNumericDuplicate: TCheckBox;
    CheckBoxForwardDeclarate: TCheckBox;
    Label7: TLabel;
    EditBaseClassName: TEdit;
    EditBaseClassUnit: TEdit;
    AniIndicatorWork: TAniIndicator;
    ButtonJOPasteAndParse: TButton;
    ButtonTest: TButton;
    Splitter1: TSplitter;
    TabControlJOMain: TTabControl;
    TabItemJOSource: TTabItem;
    MemoJOSource: TMemo;
    Label4: TLabel;
    Layout2: TLayout;
    Layout3: TLayout;
    ButtonPaste: TButton;
    ButtonCopyJSON: TButton;
    TabItemJOUnit: TTabItem;
    MemoJOUnit: TMemo;
    Label1: TLabel;
    Layout4: TLayout;
    LayoutUnitActions: TLayout;
    ButtonCopy: TButton;
    ButtonSaveUnit: TButton;
    PathHintParse: TPath;
    Label6: TLabel;
    MemoPopupMenu: TPopupMenu;
    MenuItemJSONUndo: TMenuItem;
    MenuItem1: TMenuItem;
    MenuItemJSONCut: TMenuItem;
    MenuItemJSONCopy: TMenuItem;
    MenuItemJSONPaste: TMenuItem;
    MenuItemJSONDelete: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItemJSONSelectAll: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItemJSONFormat: TMenuItem;
    MenuItemValidateJSON: TMenuItem;
    MainPopupMenu: TPopupMenu;
    MenuItemFieldCaption: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItemRenameProp: TMenuItem;
    MenuItemChangeClassName: TMenuItem;
    MenuItemChangeType: TMenuItem;
    SaveDialogUnit: TSaveDialog;
    Layout5: TLayout;
    LabelMemoUnitPos: TLabel;
    LabelMemoUnitSyn: TLabel;
    LabelMemoUnitCP: TLabel;
    Layout6: TLayout;
    LabelMemoSourcePos: TLabel;
    LabelMemoSourceSyn: TLabel;
    LabelMemoSourceCP: TLabel;
    procedure MemoJOSourceChange(Sender: TObject);
    procedure MemoJOSourceChangeTracking(Sender: TObject);
    procedure MemoJOSourcePaint(Sender: TObject; Canvas: TCanvas; const ARect: TRectF);
    procedure MemoJOUnitChange(Sender: TObject);
    procedure MemoJOUnitChangeTracking(Sender: TObject);
    procedure MemoJOUnitPaint(Sender: TObject; Canvas: TCanvas; const ARect: TRectF);
    procedure MemoPopupMenuPopup(Sender: TObject);
    procedure MenuItemChangeClassNameClick(Sender: TObject);
    procedure MenuItemChangeTypeClick(Sender: TObject);
    procedure MenuItemJSONCopyClick(Sender: TObject);
    procedure MenuItemJSONCutClick(Sender: TObject);
    procedure MenuItemJSONDeleteClick(Sender: TObject);
    procedure MenuItemJSONFormatClick(Sender: TObject);
    procedure MenuItemJSONPasteClick(Sender: TObject);
    procedure MenuItemJSONSelectAllClick(Sender: TObject);
    procedure MenuItemJSONUndoClick(Sender: TObject);
    procedure MenuItemRenamePropClick(Sender: TObject);
    procedure MenuItemValidateJSONClick(Sender: TObject);
    procedure TreeViewClassesDblClick(Sender: TObject);
    procedure TreeViewClassesMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure TreeViewJOClassDblClick(Sender: TObject);
    procedure TreeViewJOClassMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure UpdateLayout(Sender: TObject; Layout: TTextLayout; const Index: Integer);
    procedure UpdateLayoutUnit(Sender: TObject; Layout: TTextLayout; const Index: Integer);
    procedure ButtonJOPasteAndParseClick(Sender: TObject);
    procedure ButtonPasteClick(Sender: TObject);
    procedure ButtonSaveUnitClick(Sender: TObject);
    procedure ButtonCopyClick(Sender: TObject);
    procedure ButtonCopyJSONClick(Sender: TObject);
    procedure ButtonJOParseClick(Sender: TObject);
    procedure ButtonJOOnlineJsonValidatorClick(Sender: TObject);
  private
    FCodeSyntax: TCodeSyntax;
    FStyledMemo: TStyledMemo;
    FChanged: boolean;
    FCodeSyntaxUnit: TCodeSyntax;
    FStyledMemoUnit: TStyledMemo;
    FMemoMarginsLeft, FMemoMarginsLeftUnit: Integer;
    procedure JumpTo(Obj: TObject);
    procedure ShowJsonError(E: TJSONError);
    procedure UpdateUnit(SavePos: Boolean = False);
    procedure VisualizeClass;
    procedure FOnCaretMove(Sender: TObject);
    procedure PrepareMenu(TreeView: TTreeView);
    procedure UpdateAll;
    procedure DoWork;
    procedure EndWork;
  public
    JsonMapper: TJsonMapper;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

uses
  System.IOUtils, System.Math, HGM.ObjectHolder, FMX.Text, JTD.Main,
  {$IFDEF MSWINDOWS}
  Winapi.ShellAPI, Winapi.Windows,
  {$ENDIF MSWINDOWS}
  {$IFDEF DEBUG}
  RootUnit, Rest.Json,
  {$ENDIF}
  {$IFDEF POSIX}
  Posix.Stdlib,
  {$ENDIF POSIX} FMX.DialogService, FMX.BehaviorManager;

{$R *.fmx}

procedure TFrameJsonObject.ButtonJOOnlineJsonValidatorClick(Sender: TObject);
begin
  MenuItemValidateJSONClick(nil);
end;

procedure TFrameJsonObject.ButtonJOParseClick(Sender: TObject);
begin
  if FChanged then
    TDialogService.MessageDialog('You made changes to the structure. Do you want to load original class?', TMsgDlgType.mtWarning, [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo], TMsgDlgBtn.mbNo, 0,
      procedure(const AResult: TModalResult)
      begin
        if AResult <> mrYes then
          Exit;
        VisualizeClass;
      end)
  else
    VisualizeClass;
end;

procedure TFrameJsonObject.ButtonCopyClick(Sender: TObject);
begin
  MemoJOUnit.CopyToClipboard;
end;

procedure TFrameJsonObject.ButtonCopyJSONClick(Sender: TObject);
begin
  MemoJOSource.CopyToClipboard;
end;

destructor TFrameJsonObject.Destroy;
begin
  FCodeSyntax.Free;
  FCodeSyntaxUnit.Free;
  JsonMapper.Free;
  inherited;
end;

procedure TFrameJsonObject.DoWork;
begin
  //
end;

procedure TFrameJsonObject.EndWork;
begin
  //
end;

procedure TFrameJsonObject.ButtonJOPasteAndParseClick(Sender: TObject);
begin
  MemoJOSource.Lines.Clear;
  MemoJOSource.PasteFromClipboard;
  MemoJOSource.SelStart := 0;
  MenuItemJSONFormatClick(nil);
  ButtonJOParseClick(nil);
end;

procedure TFrameJsonObject.ButtonPasteClick(Sender: TObject);
begin
  MemoJOSource.Lines.Clear;
  MemoJOSource.PasteFromClipboard;
  MemoJOSource.SelStart := 0;
  MenuItemJSONFormatClick(nil);
end;

procedure TFrameJsonObject.ButtonSaveUnitClick(Sender: TObject);
begin
  SaveDialogUnit.FileName := JsonMapper.DestinationUnitName + '.pas';
  if SaveDialogUnit.Execute then
  begin
    TFile.Create(SaveDialogUnit.FileName).Free;
    TFile.WriteAllText(SaveDialogUnit.FileName, MemoJOUnit.Text, TEncoding.UTF8);
  end;
end;

procedure TFrameJsonObject.MemoJOUnitChange(Sender: TObject);
begin
  FCodeSyntaxUnit.DropCache;
  var Memo := MemoJOUnit.Presentation as TStyledMemo;
  Memo.UpdateVisibleLayoutParams;
  LayoutUnitActions.Enabled := MemoJOUnit.Text.Length > 0;
end;

procedure TFrameJsonObject.MemoJOUnitChangeTracking(Sender: TObject);
begin
  if Parent = nil then
    Exit;
  var Memo := MemoJOUnit.Presentation as TStyledMemo;
  MemoJOUnit.Canvas.Font.Assign(MemoJOUnit.TextSettings.Font);
  var W := Ceil(MemoJOUnit.Canvas.TextWidth(MemoJOUnit.Lines.Count.ToString) + 20);
  MemoJOUnit.StylesData['content_client.Padding.Left'] := W;
  FMemoMarginsLeftUnit := W;
  Memo.UpdateVisibleLayoutParams;
  Memo.RealignContent;
end;

procedure TFrameJsonObject.MemoJOUnitPaint(Sender: TObject; Canvas: TCanvas; const ARect: TRectF);
begin
  // Line number
  var BRect := MemoJOUnit.BoundsRect;
  var Memo := MemoJOUnit.Presentation as TStyledMemo;
  for var i := 0 to Memo.LineObjects.Count - 1 do
    if Memo.LineObjects.Items[i].SizeValid then
    begin
      var Rect := Memo.LineObjects.Items[i].Rect;
      Rect.Left := 0;
      Rect.Width := FMemoMarginsLeftUnit - 10;
      if Rect.Top < BRect.Height then
        Rect.Bottom := Min(Rect.Bottom, MemoJOUnit.ContentLayout.Height);
      Rect.Offset(2, 2);
      Rect.NormalizeRect;
      if (Rect.Top < 0) and (Rect.Bottom < 0) then
        Continue;
      if (Rect.Top > BRect.Height) and (Rect.Bottom > BRect.Height) then
        Continue;
      Canvas.Fill.Color := TAlphaColorRec.White;
      Canvas.Font.Assign(MemoJOUnit.TextSettings.Font);
      var HDelta: Single :=(100 / Memo.LineObjects.Items[i].Rect.Height * Rect.Height) / 100;
      if i = Memo.CaretPosition.Line then
      begin
        Canvas.FillRect(Rect, 0.1);
        HDelta := 300;
      end;
      Canvas.FillText(Rect, (i + 1).ToString, False, 0.3 * HDelta, [], TTextAlign.Trailing, TTextAlign.Leading);
    end;
end;

procedure TFrameJsonObject.MemoJOSourceChange(Sender: TObject);
begin
  FCodeSyntax.DropCache;
  FStyledMemo.UpdateVisibleLayoutParams;
  FStyledMemo.Repaint;
end;

procedure TFrameJsonObject.MemoJOSourceChangeTracking(Sender: TObject);
begin
  if Parent = nil then
    Exit;
  var Memo := MemoJOSource.Presentation as TStyledMemo;
  MemoJOSource.Canvas.Font.Assign(MemoJOSource.TextSettings.Font);
  var W := Ceil(MemoJOSource.Canvas.TextWidth(MemoJOSource.Lines.Count.ToString) + 20);
  MemoJOSource.StylesData['content_client.Padding.Left'] := W;
  FMemoMarginsLeft := W;
  Memo.UpdateVisibleLayoutParams;
  Memo.RealignContent;

  FCodeSyntax.DropCache;
  FStyledMemo.UpdateVisibleLayoutParams;
  FStyledMemo.Repaint;
end;

constructor TFrameJsonObject.Create(AOwner: TComponent);
begin
  inherited;
  JsonMapper := TJsonMapper.Create;

  TreeViewJOClass.AniCalculations.Animation := True;
  TreeViewClasses.AniCalculations.Animation := True;

  TThread.ForceQueue(nil,
    procedure
    begin
      MemoJOSource.ScrollAnimation := TBehaviorBoolean.True;
      MemoJOSource.DisableDisappear := True;
      (MemoJOSource.Presentation as TStyledMemo).OnCaretMove := FOnCaretMove;
      MemoJOSource.ApplyStyleLookup;
      FStyledMemo := MemoJOSource.Presentation as TStyledMemo;
      FStyledMemo.OnUpdateLayoutParams := UpdateLayout;
      FCodeSyntax := TCodeSyntax.FindSyntax('json', MemoJOSource.TextSettings.Font, MemoJOSource.FontColor);
      MemoJOSourceChangeTracking(nil);

      MemoJOUnit.ScrollAnimation := TBehaviorBoolean.True;
      MemoJOUnit.DisableDisappear := True;
      (MemoJOUnit.Presentation as TStyledMemo).OnCaretMove := FOnCaretMove;
      MemoJOUnit.ApplyStyleLookup;
      FStyledMemoUnit := MemoJOUnit.Presentation as TStyledMemo;
      FStyledMemoUnit.OnUpdateLayoutParams := UpdateLayoutUnit;
      FCodeSyntaxUnit := TCodeSyntax.FindSyntax('pascal', MemoJOUnit.TextSettings.Font, MemoJOUnit.FontColor);
      MemoJOUnitChangeTracking(nil);

      {$IFDEF DEBUG}
      ButtonTest.Visible := True;
      {$ELSE}
      ButtonTest.Visible := False;
      {$ENDIF}


      TabControlJOMain.ActiveTab := TabItemJOSource;
      TabControlView.ActiveTab := TabItemJOClass;
    end);
end;

procedure TFrameJsonObject.FOnCaretMove(Sender: TObject);
begin
  var Memo := MemoJOUnit.Presentation as TStyledMemo;
  LabelMemoUnitPos.Text := Format('%d: %d', [Memo.CaretPosition.Line + 1, Memo.CaretPosition.Pos + 1]);
  Memo := MemoJOSource.Presentation as TStyledMemo;
  LabelMemoSourcePos.Text := Format('%d: %d', [Memo.CaretPosition.Line + 1, Memo.CaretPosition.Pos + 1]);
end;

procedure TFrameJsonObject.MemoJOSourcePaint(Sender: TObject; Canvas: TCanvas; const ARect: TRectF);
begin
    // Line number
  var BRect := MemoJOSource.BoundsRect;
  var Memo := MemoJOSource.Presentation as TStyledMemo;
  for var i := 0 to Memo.LineObjects.Count - 1 do
    if Memo.LineObjects.Items[i].SizeValid then
    begin
      var Rect := Memo.LineObjects.Items[i].Rect;
      Rect.Left := 0;
      Rect.Width := FMemoMarginsLeft - 10;
      if Rect.Top < BRect.Height then
        Rect.Bottom := Min(Rect.Bottom, MemoJOSource.ContentLayout.Height);
      Rect.Offset(2, 2);
      Rect.NormalizeRect;
      if (Rect.Top < 0) and (Rect.Bottom < 0) then
        Continue;
      if (Rect.Top > BRect.Height) and (Rect.Bottom > BRect.Height) then
        Continue;
      Canvas.Fill.Color := TAlphaColorRec.Black;
      Canvas.Font.Assign(MemoJOSource.TextSettings.Font);
      var HDelta: Single :=(100 / Memo.LineObjects.Items[i].Rect.Height * Rect.Height) / 100;
      if i = Memo.CaretPosition.Line then
      begin
        Canvas.FillRect(Rect, 0.1);
        HDelta := 300;
      end;
      Canvas.FillText(Rect, (i + 1).ToString, False, 0.3 * HDelta, [], TTextAlign.Trailing, TTextAlign.Leading);
    end;
end;

procedure TFrameJsonObject.MemoPopupMenuPopup(Sender: TObject);
begin
  var Memo := MemoJOSource.Presentation as TStyledMemo;
  MenuItemJSONUndo.Enabled := Memo.CanUndo;
  MenuItemJSONCut.Enabled := Memo.CanCut;
  MenuItemJSONCopy.Enabled := Memo.CanCopy;
  MenuItemJSONPaste.Enabled := Memo.CanPaste;
  MenuItemJSONDelete.Enabled := Memo.CanDelete;
  MenuItemJSONSelectAll.Enabled := Memo.CanSelectAll;
  MenuItemJSONFormat.Enabled := MemoJOSource.Text.Length > 0;
end;

procedure TFrameJsonObject.MenuItemRenamePropClick(Sender: TObject);
begin
  var LItem :=(Sender as TFmxObject).TagObject as TJProperty;
  TDialogService.InputQuery('Rename Property ' + LItem.Name, ['Enter new Property name'], [LItem.Name],
    procedure(const AResult: TModalResult; const AValues: array of string)
    begin
      if AResult <> mrOk then
        Exit;
      var LString := AValues[0];
      if (LString <> '') and (LString.ToLower <> LItem.Name.ToLower) then
      begin
        FChanged := True;
        LItem.ChangeName(LString);
        UpdateAll;
      end;
    end);
end;

procedure TFrameJsonObject.MenuItemJSONFormatClick(Sender: TObject);
begin
  try
    var LJsonValue := TJSONObject.ParseJSONValue(MemoJOSource.Text, False, True);
    if Assigned(LJsonValue) then
    try
      MemoJOSource.Text := LJsonValue.Format;
    finally
      LJsonValue.Free;
    end;
  except
    on E: EJSONParseException do
    begin
      var ErrorInfo: TJSONError;
      ErrorInfo.Path := E.Path;
      ErrorInfo.Offset := E.Offset;
      ErrorInfo.Line := E.Line;
      ErrorInfo.Position := E.Position;
      ErrorInfo.Available := True;
      ShowJsonError(ErrorInfo);
      raise;
    end;
    on E: Exception do
      raise;
  end;
end;

procedure TFrameJsonObject.MenuItemJSONCopyClick(Sender: TObject);
begin
  MemoJOSource.CopyToClipboard;
end;

procedure TFrameJsonObject.MenuItemJSONCutClick(Sender: TObject);
begin
  MemoJOSource.CutToClipboard;
end;

procedure TFrameJsonObject.MenuItemJSONDeleteClick(Sender: TObject);
begin
  MemoJOSource.DeleteSelection;
end;

procedure TFrameJsonObject.MenuItemJSONPasteClick(Sender: TObject);
begin
  MemoJOSource.PasteFromClipboard;
end;

procedure TFrameJsonObject.MenuItemJSONSelectAllClick(Sender: TObject);
begin
  MemoJOSource.SelectAll;
end;

procedure TFrameJsonObject.MenuItemJSONUndoClick(Sender: TObject);
begin
  MemoJOSource.UnDo;
end;

procedure TFrameJsonObject.MenuItemChangeClassNameClick(Sender: TObject);
begin
  var LItem :=(Sender as TFmxObject).TagObject as TJClass;
  TDialogService.InputQuery('Rename Class ' + LItem.Name, ['Enter new Class name'], [LItem.Name],
    procedure(const AResult: TModalResult; const AValues: array of string)
    begin
      if AResult <> mrOk then
        Exit;
      var LString := AValues[0];
      if (LString <> '') and (LString.ToLower <> LItem.Name.ToLower) then
      begin
        FChanged := True;
        LItem.NameDirect := LString;
        UpdateAll;
      end;
    end);
end;

procedure TFrameJsonObject.UpdateAll;
begin
  JsonMapper.Visualize(TreeViewJOClass, '');
  JsonMapper.VisualizeClasses(TreeViewClasses, '');
  UpdateUnit(True);
end;

procedure TFrameJsonObject.MenuItemChangeTypeClick(Sender: TObject);
begin
  var LItem :=(Sender as TFmxObject).TagObject as TJProperty;
  var SType := '';
  if LItem is TJPropertyArray then
    SType := TJPropertyArray(LItem).GetContainedTypeAsString
  else
    SType := LItem.GetTypeAsString;
  TDialogService.InputQuery('Change type ' + LItem.Name, ['Enter new Type name'], [SType],
    procedure(const AResult: TModalResult; const AValues: array of string)
    begin
      if AResult <> mrOk then
        Exit;
      FChanged := True;
      LItem.CustomType := AValues[0];
      UpdateAll;
    end);
end;

procedure TFrameJsonObject.MenuItemValidateJSONClick(Sender: TObject);
begin
  {$IFDEF MSWINDOWS}
  ShellExecute(0, 'open', PChar(JsonValidatorUrl), '', '', SW_SHOWNORMAL);
  {$ENDIF MSWINDOWS}
  {$IFDEF POSIX}
  _system(PAnsiChar('open ' + AnsiString(JsonValidatorUrl)));
  {$ENDIF POSIX}
end;

procedure TFrameJsonObject.PrepareMenu(TreeView: TTreeView);
begin
  for var i := 0 to MainPopupMenu.ItemsCount - 1 do
    MainPopupMenu.Items[i].Enabled := False;
  MenuItemFieldCaption.Text := '---';
  if (TreeView.Selected <> nil) and (TreeView.Selected.TagObject <> nil) then
  begin
    MenuItemFieldCaption.Text := TreeView.Selected.Text;
    if TreeView.Selected.TagObject is TJProperty then
    begin
      MenuItemRenameProp.Enabled := True;
      MenuItemRenameProp.TagObject := TreeView.Selected.TagObject;

      var LField := TJProperty(TreeView.Selected.TagObject);

      if (LField is TJPropertyArray) then
        if (TJPropertyArray(LField).ContainedType = TJsonType.jtObject) then
        begin
          MenuItemChangeClassName.Enabled := True;
          MenuItemChangeClassName.TagObject := TJPropertyArray(LField).FieldClass;
        end
        else
        begin
          MenuItemChangeType.Enabled := True;
          MenuItemChangeType.TagObject := LField;
        end;

      if LField is TJPropertyObject then
      begin
        MenuItemChangeClassName.Enabled := True;
        MenuItemChangeClassName.TagObject := TJPropertyObject(LField).FieldClass;
      end;

      if LField is TJPropertyPrimitive then
      begin
        MenuItemChangeType.Enabled := True;
        MenuItemChangeType.TagObject := LField;
      end;
    end
    else if TreeView.Selected.TagObject is TJClass then
    begin
      MenuItemChangeClassName.Enabled := True;
      MenuItemChangeClassName.TagObject := TreeView.Selected.TagObject;
    end;
  end;
end;

procedure TFrameJsonObject.TreeViewClassesDblClick(Sender: TObject);
begin
  if TreeViewClasses.Selected <> nil then
    if TreeViewClasses.Selected.Enabled then
    begin
      //TreeViewClasses.Selected.IsExpanded := not TreeViewClasses.Selected.IsExpanded;
      JumpTo(TreeViewClasses.Selected.TagObject);
    end;
end;

procedure TFrameJsonObject.TreeViewClassesMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  if Button = TMouseButton.mbRight then
  begin
    var Item := TreeViewClasses.ItemByPoint(X, Y);
    if Assigned(Item) then
    begin
      Item.IsSelected := True;
      PrepareMenu(TreeViewClasses);
      MainPopupMenu.Popup(Screen.MousePos.X, Screen.MousePos.Y);
    end;
  end;
end;

procedure TFrameJsonObject.JumpTo(Obj: TObject);
begin
  if Obj is TJProperty then
  begin
    TabControlJOMain.ActiveTab := TabItemJOUnit;
    MemoJOUnit.Model.CaretPosition := TCaretPosition.Create(TJProperty(Obj).LineNumber, 0);
    var Pt := MemoJOUnit.Caret.Pos;
    Pt.Offset(0, -MemoJOUnit.Height / 2);
    MemoJOUnit.ViewportPosition := Pt;
    MemoJOUnit.Repaint;
  end
  else if Obj is TJClass then
  begin
    TabControlJOMain.ActiveTab := TabItemJOUnit;
    MemoJOUnit.Model.CaretPosition := TCaretPosition.Create(TJClass(Obj).LineNumber, 0);
    var Pt := MemoJOUnit.Caret.Pos;
    Pt.Offset(0, -MemoJOUnit.Height / 2);
    MemoJOUnit.ViewportPosition := Pt;
    MemoJOUnit.Repaint;
  end;
end;

procedure TFrameJsonObject.TreeViewJOClassDblClick(Sender: TObject);
begin
  if TreeViewJOClass.Selected <> nil then
    if TreeViewJOClass.Selected.Enabled then
    begin
      //TreeViewJOClass.Selected.IsExpanded := not TreeViewJOClass.Selected.IsExpanded;
      JumpTo(TreeViewJOClass.Selected.TagObject);
    end;
end;

procedure TFrameJsonObject.TreeViewJOClassMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  if Button = TMouseButton.mbRight then
  begin
    var Item := TreeViewJOClass.ItemByPoint(X, Y);
    if Assigned(Item) then
    begin
      Item.IsSelected := True;
      PrepareMenu(TreeViewJOClass);
      MainPopupMenu.Popup(Screen.MousePos.X, Screen.MousePos.Y);
    end;
  end;
end;

procedure TFrameJsonObject.UpdateLayout(Sender: TObject; Layout: TTextLayout; const Index: Integer);
begin
  if not Assigned(Layout) then
    Exit;
  Layout.BeginUpdate;
  try
    Layout.ClearAttributes;
    Layout.Padding.Top := 1;
    Layout.Padding.Bottom := 1;
    if Assigned(FCodeSyntax) then
      for var Attr in FCodeSyntax.GetAttributesForLine(MemoJOSource.Lines[Index], Index) do
        Layout.AddAttribute(Attr.Range, Attr.Attribute);
  finally
    Layout.EndUpdate;
  end;
end;

procedure TFrameJsonObject.UpdateLayoutUnit(Sender: TObject; Layout: TTextLayout; const Index: Integer);
begin
  if not Assigned(Layout) then
    Exit;
  Layout.BeginUpdate;
  try
    Layout.ClearAttributes;
    Layout.Padding.Top := 1;
    Layout.Padding.Bottom := 1;
    if Assigned(FCodeSyntaxUnit) then
      for var Attr in FCodeSyntaxUnit.GetAttributesForLine(MemoJOUnit.Lines[Index], Index) do
        Layout.AddAttribute(Attr.Range, Attr.Attribute);
  finally
    Layout.EndUpdate;
  end;
end;

procedure TFrameJsonObject.UpdateUnit(SavePos: Boolean);
begin
  var Pos := MemoJOUnit.VScrollBar.Value;
  MemoJOUnit.Text := JsonMapper.GenerateUnit;
  if SavePos then
    MemoJOUnit.VScrollBar.Value := Pos
  else
    MemoJOUnit.VScrollBar.Value := 0;
end;

procedure TFrameJsonObject.ShowJsonError(E: TJSONError);
begin
  MemoJOSource.Model.CaretPosition := TCaretPosition.Create(E.Line - 1, E.Position - 1);
  TabControlJOMain.ActiveTab := TabItemJOSource;
  MemoJOSource.SetFocus;
end;

procedure TFrameJsonObject.VisualizeClass;
begin
  FChanged := False;
  TreeViewJOClass.Enabled := True;
  TreeViewClasses.Enabled := True;
  ButtonJOParseTree.Visible := False;
  PathHintParse.Visible := False;

  var JSON := MemoJOSource.Text;
  var RootName := EditRootName.Text;

  JsonMapper.MergeSameClasses := CheckBoxMerge.IsChecked;
  JsonMapper.SortFields := CheckBoxSort.IsChecked;
  JsonMapper.DestinationUnitName := EditUnitName.Text;
  JsonMapper.BaseClassName := EditBaseClassName.Text;
  JsonMapper.BaseClassUnit := EditBaseClassUnit.Text;
  JsonMapper.NumericDuplicate := CheckBoxNumericDuplicate.IsChecked;
  JsonMapper.ForwardDeclarate := CheckBoxForwardDeclarate.IsChecked;

  DoWork;
  TaskRun(Self,
    procedure(Holder: IComponentHolder)
    begin
      var Error: string;
      var ErrorInfo: TJSONError;
      ErrorInfo.Available := False;
      try
        JsonMapper.Parse(JSON, RootName);
      except
        on E: EJSONParseException do
        begin
          ErrorInfo.Path := E.Path;
          ErrorInfo.Offset := E.Offset;
          ErrorInfo.Line := E.Line;
          ErrorInfo.Position := E.Position;
          ErrorInfo.Available := True;
          Error := E.Message;
        end;
        on E: Exception do
          Error := E.Message;
      end;
      Queue(
        procedure
        begin
          if not Holder.IsLive then
            Exit;
          try
            if not Error.IsEmpty then
            begin
              if ErrorInfo.Available then
              begin
                ShowJsonError(ErrorInfo);
              end;
              raise Exception.Create(Error);
            end;

            JsonMapper.Visualize(TreeViewJOClass, '');
            JsonMapper.VisualizeClasses(TreeViewClasses, '');
            UpdateUnit;
          finally
            EndWork;
          end;
        end);
    end);
end;

end.

