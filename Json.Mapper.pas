unit Json.Mapper;

interface

uses
  System.SysUtils, System.Classes, System.JSON, Rest.Json,
  System.RegularExpressions, System.Generics.Collections,
  System.Generics.Defaults, FMX.TreeView, Rest.Json.Interceptors;

type
  EJsonMapper = class(Exception);

  TJsonType = (jtUnknown, jtObject, jtArray, jtString, jtBool, jtNumber, jtDate, jtDateTime, jtNull);

  TJClass = class;

  TJsonMapper = class;

  TJProperty = class abstract
  private
    FName: string;
    FPropertyName: string;
    FFieldName: string;
    FFieldType: TJsonType;
    FParentClass: TJClass;
    FOriginalFieldName: string;
    FLineNumber: Integer;
    FCustomType: string;
    procedure SetName(const Value: string);
    procedure SetLineNumber(const Value: Integer);
    procedure SetCustomType(const Value: string);
  public
    constructor Create(AParentClass: TJClass; AItemName: string; AFieldType: TJsonType);
    destructor Destroy; override;
    property Name: string read FName write SetName;
    property FieldName: string read FFieldName write FFieldName;
    property OriginalFieldName: string read FOriginalFieldName write FOriginalFieldName;
    property PropertyName: string read FPropertyName write FPropertyName;
    property FieldType: TJsonType read FFieldType write FFieldType;
    property LineNumber: Integer read FLineNumber write SetLineNumber;
    property CustomType: string read FCustomType write SetCustomType;
    procedure ChangeName(const Value: string);
    function GetTypeAsString: string; virtual; abstract;
  end;

  TJPropertyPrimitive = class(TJProperty)
  public
    function GetTypeAsString: string; override;
  end;

  TJPropertyObject = class(TJProperty)
  private
    FFieldClass: TJClass;
  public
    property FieldClass: TJClass read FFieldClass write FFieldClass;
    constructor Create(AParentClass: TJClass; AItemName: string; AItemClass: TJClass);
    function GetTypeAsString: string; override;
  end;

  TJPropertyArray = class(TJProperty)
  private
    FLevel: Integer;
    FFieldClass: TJClass;
    FContainedType: TJsonType;
  public
    property FieldClass: TJClass read FFieldClass write FFieldClass;
    property ContainedType: TJsonType read FContainedType write FContainedType;
    constructor Create(AClass: TJClass; AItemName: string; ALevel: Integer; AItemSubType: TJsonType; AItemClass: TJClass);
    function GetTypeAsString: string; override;
    function GetContainedTypeAsString: string;
  end;

  TJClass = class
  private
    FItems: TObjectList<TJProperty>;
    FComplexItems, FArrayItems: TList<TJProperty>;
    FName: string;
    FComparison: TComparison<TJProperty>;
    FComparer: IComparer<TJProperty>;
    FParentClass: TJClass;
    FMapper: TJsonMapper;
    FOriginalName: string;
    FMerged: Boolean;
    FLineNumber: Integer;
    function GetMergedName: string;
    procedure SetLineNumber(const Value: Integer);
    procedure SetMerged(const Value: Boolean);
    procedure SetName(const Value: string);
    procedure SortFields;
    procedure DeleteItem(Item: TJProperty);
  public
    constructor Create(AParentClass: TJClass; AClassName: string; AMapper: TJsonMapper);
    destructor Destroy; override;
    property ArrayItems: TList<TJProperty> read FArrayItems write FArrayItems;
    property ComplexItems: TList<TJProperty> read FComplexItems write FComplexItems;
    function ContainsField(const OriginalFieldName: string; out JProp: TJProperty; RemoveIfNull: Boolean): Boolean;
    function GetDeclarationPart(const Offset: Integer): TArray<string>;
    function GetForwardDeclaration: string;
    function GetImplementationPart: TArray<string>;
    property Items: TObjectList<TJProperty> read FItems write FItems;
    property LineNumber: Integer read FLineNumber write SetLineNumber;
    property Merged: Boolean read FMerged write SetMerged;
    property MergedName: string read GetMergedName;
    property Name: string read FName write SetName;
    property NameDirect: string read FName write FName;
    property OriginalName: string read FOriginalName write FOriginalName;
  end;

  TMergeType = record
    Old: TJClass;
    New: TJClass;
  end;

  TJsonMapper = class
  private
    FClasses: TObjectList<TJClass>;
    FRootClass: TJClass;
    FUnitName: string;
    FMergedTypes: TList<TMergeType>;
    FNumericDuplicate: Boolean;
    FMergeSameClasses: Boolean;
    FSortFields: Boolean;
    FForwardDeclarate: Boolean;
    FBaseClassName: string;
    FBaseClassUnit: string;
    procedure SetUnitName(const Value: string);
    procedure MergeClasses(LClass: TJClass);
    procedure MergeClass(SourceClass, TargetClass: TJClass);
    procedure SetNumericDuplicate(const Value: Boolean);
    procedure SetMergeSameClasses(const Value: Boolean);
    procedure SetSortFields(const Value: Boolean);
    procedure PopulateFields(ItemClass: TJClass; JsonArray: TJSONArray); overload;
    procedure PopulateFields(ItemClass: TJClass; JsonObject: TJSONObject); overload;
    procedure SetForwardDeclarate(const Value: Boolean);
    procedure SetBaseClassName(const Value: string);
    procedure SetBaseClassUnit(const Value: string);
  protected
    function GetJsonType(AJsonValue: TJsonValue): TJsonType;
    function GetFirstArrayItem(AJsonValue: TJsonValue): TJsonValue;
    procedure ProcessJsonObject(JsonObject: TJSONObject; AParentClass: TJClass);
    procedure ProcessJsonArray(JsonArray: TJSONArray; AItemName: string; var ItemType: TJsonType; var ItemClass: TJClass; var Level: Integer);
    procedure ClearClasses;
    procedure InternalVisualize(ATreeViewItem: TTreeViewItem; AClass: TJClass; AItemStyleLookup: string);
    function SuggestClassName(AClass: TJClass; ASuggestedClassName: string): string;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Debug(ALines: TStrings);
    /// <summary>
    /// Parses a JSON string and creates internal stub class structure
    /// </summary>
    procedure Parse(AJsonString: string; ARootClassName: string = 'Root');
    /// <summary>
    /// Generates resultant unit
    /// </summary>
    function GenerateUnit: string;
    /// <summary>
    /// Visualizes stub class structure in a treeview
    /// </summary>
    procedure Visualize(ATreeView: TTreeView; AItemStyleLookup: string);
    /// <summary>
    /// Visualizes all stub classes structure in a treeview
    /// </summary>
    procedure VisualizeClasses(ATreeView: TTreeView; AItemStyleLookup: string);
    property DestinationUnitName: string read FUnitName write SetUnitName;
    property BaseClassName: string read FBaseClassName write SetBaseClassName;
    property BaseClassUnit: string read FBaseClassUnit write SetBaseClassUnit;
    property MergeSameClasses: Boolean read FMergeSameClasses write SetMergeSameClasses;
    property NumericDuplicate: Boolean read FNumericDuplicate write SetNumericDuplicate;
    property SortFields: Boolean read FSortFields write SetSortFields;
    property ForwardDeclarate: Boolean read FForwardDeclarate write SetForwardDeclarate;
  end;

implementation

uses
  FMX.Ani, System.StrUtils;

var
  ReservedWords: TList<string>;

const
  AllowedChars =['0'..'9', 'a'..'z', 'A'..'Z'];
  REGEX_DATE = '^([0-9]{4})(-?)(1[0-2]|0[1-9])\2(3[01]|0[1-9]|[12][0-9])$';
  // ISO 8601
  REGEX_DATETIME = '^([0-9]{4})-?(1[0-2]|0[1-9])-?(3[01]|0[1-9]|[12][0-9])(T| )(2[0-3]|[01][0-9]):?([0-5][0-9]):?([0-5][0-9])(Z?|(-|\+)(2[0-3]|[01][0-9]):?([0-5][0-9]))$';

{ TJsonMapper }

procedure TJsonMapper.Parse(AJsonString: string; ARootClassName: string);
begin
  ClearClasses;

  var JSON := TJSONObject.ParseJSONValue(AJsonString, False, True);
  if Assigned(JSON) then
  try
    FRootClass := TJClass.Create(nil, ARootClassName, Self);
    case GetJsonType(JSON) of
      jtObject:
        begin
          ProcessJsonObject(TJSONObject(JSON), FRootClass);
        end;
      jtArray:
        begin
          var LJsonType := jtNull;
          var LClass: TJClass := nil;
          var Level: Integer := 1;
          ProcessJsonArray(TJSONArray(JSON), 'item', LJsonType, LClass, Level);
          TJPropertyArray.Create(FRootClass, 'items', Level, LJsonType, LClass);
        end;
    end;
    if FMergeSameClasses then
      for var i := FClasses.Count - 1 downto 0 do
        MergeClasses(FClasses[i]);
  finally
    JSON.Free;
  end
  else
    raise EJsonMapper.Create('Unable to parse the JSON String!');
end;

procedure TJsonMapper.PopulateFields(ItemClass: TJClass; JsonArray: TJSONArray);
begin
  for var JsonObject in JsonArray do
    if JsonObject is TJSONObject then
      PopulateFields(ItemClass, TJSONObject(JsonObject));

  ItemClass.SortFields;
end;

procedure TJsonMapper.PopulateFields(ItemClass: TJClass; JsonObject: TJSONObject);
begin
  var JProp: TJProperty;
  for var JsonPair in JsonObject do
    if not ItemClass.ContainsField(JsonPair.JsonString.Value, JProp, True) then
    begin
      var PairValue := JsonPair.JsonValue;
      var PairType := GetJsonType(PairValue);

      case PairType of
        jtObject:
          begin
            var LClass := TJClass.Create(ItemClass, JsonPair.JsonString.Value, Self);
            TJPropertyObject.Create(ItemClass, JsonPair.JsonString.Value, LClass);
            ProcessJsonObject(TJSONObject(PairValue), LClass);
          end;
        jtArray:
          begin
            var Level: Integer := 1;
            var ArrayItemType := jtNull;
            var ArrayItemClass: TJClass := nil;
            ProcessJsonArray(TJSONArray(JsonPair.JsonValue), JsonPair.JsonString.Value, ArrayItemType, ArrayItemClass, Level);
            TJPropertyArray.Create(ItemClass, JsonPair.JsonString.Value, Level, ArrayItemType, ArrayItemClass);
          end;
        jtNumber, jtString, jtDate, jtDateTime, jtBool, jtNull:
          TJPropertyPrimitive.Create(ItemClass, JsonPair.JsonString.Value, PairType);
      end;
    end
    else if (JsonPair.JsonValue is TJSONObject) and (JProp is TJPropertyObject) then
      PopulateFields(TJPropertyObject(JProp).FieldClass, TJSONObject(JsonPair.JsonValue));
end;

procedure TJsonMapper.ProcessJsonArray(JsonArray: TJSONArray; AItemName: string; var ItemType: TJsonType; var ItemClass: TJClass; var Level: Integer);
begin
  ItemType := jtNull;
  var FirstArrayItem := GetFirstArrayItem(JsonArray);
  if FirstArrayItem <> nil then // empty array
  begin
    ItemType := GetJsonType(FirstArrayItem);
    case ItemType of
      jtObject:
        begin
          ItemClass := TJClass.Create(FRootClass, AItemName, Self);
          ProcessJsonObject(TJSONObject(FirstArrayItem), ItemClass);
          PopulateFields(ItemClass, JsonArray);
        end;
      jtArray:
        begin
          Inc(Level);
          ProcessJsonArray(TJSONArray(FirstArrayItem), AItemName, ItemType, ItemClass, Level);
        end;
    end;
  end;
end;

procedure TJsonMapper.ProcessJsonObject(JsonObject: TJSONObject; AParentClass: TJClass);
begin
  for var JsonPair in JsonObject do
  begin
    var PairValue := JsonPair.JsonValue;
    var PairType := GetJsonType(PairValue);

    case PairType of
      jtObject:
        begin
          var LClass := TJClass.Create(AParentClass, JsonPair.JsonString.Value, Self);
          TJPropertyObject.Create(AParentClass, JsonPair.JsonString.Value, LClass);
          ProcessJsonObject(TJSONObject(PairValue), LClass);
        end;
      jtArray:
        begin
          var Level: Integer := 1;
          var ArrayItemType := jtNull;
          var ArrayItemClass: TJClass := nil;
          ProcessJsonArray(TJSONArray(JsonPair.JsonValue), JsonPair.JsonString.Value, ArrayItemType, ArrayItemClass, Level);
          TJPropertyArray.Create(AParentClass, JsonPair.JsonString.Value, Level, ArrayItemType, ArrayItemClass);
        end;
      jtNumber, jtString, jtDate, jtDateTime, jtBool, jtNull:
        TJPropertyPrimitive.Create(AParentClass, JsonPair.JsonString.Value, PairType);
    end;
  end;

  AParentClass.SortFields;
end;

function TJsonMapper.SuggestClassName(AClass: TJClass; ASuggestedClassName: string): string;
begin
  Result := ASuggestedClassName;
  var LMax := 0;
  for var LClass in FClasses do
    if LClass.OriginalName.ToLower = ASuggestedClassName.ToLower then
      if FNumericDuplicate then
        Inc(LMax)
      else
        Exit(AClass.FParentClass.Name + ASuggestedClassName.Remove(0, 1));

  if LMax > 0 then
    Result := Format('%s_%0.3d', [ASuggestedClassName, LMax]);
end;

function TJsonMapper.GetJsonType(AJsonValue: TJsonValue): TJsonType;
begin
  if AJsonValue is TJSONObject then
    Result := jtObject
  else if AJsonValue is TJSONArray then
    Result := jtArray
  else if AJsonValue is TJSONNumber then
    Result := jtNumber
  else if AJsonValue is TJSONBool then
    Result := jtBool
  else if AJsonValue is TJSONNull then
    Result := jtNull
  else if AJsonValue is TJSONTrue then
    Result := jtBool
  else if AJsonValue is TJSONFalse then
    Result := jtBool
  else if AJsonValue is TJSONString then
  begin
    var LJsonString := AJsonValue as TJSONString;
    var DateValue: TDateTime;
    if TRegEx.IsMatch(LJsonString.Value, REGEX_DATETIME) then
      Result := jtDateTime
    else if TRegEx.IsMatch(LJsonString.Value, REGEX_DATE) then
      Result := jtDate
    else if TryStrToDate(LJsonString.Value, DateValue) then
      Result := jtDate
    else if TryStrToDateTime(LJsonString.Value, DateValue) then
      Result := jtDateTime
    else
      Result := jtString;
  end
  else
    Result := jtUnknown;
end;

function TJsonMapper.GetFirstArrayItem(AJsonValue: TJsonValue): TJsonValue;
begin
  Result := nil;
  for var LJsonValue in AJsonValue as TJsonArray do
    Exit(LJsonValue);
end;

procedure TJsonMapper.MergeClasses(LClass: TJClass);
begin
  if LClass.Merged then
    Exit;
  for var TargetClass in FClasses do
    if TargetClass <> LClass then
      MergeClass(TargetClass, LClass);

  for var TargetClass in FClasses do
    if TargetClass <> LClass then
    begin
      if TargetClass.Merged then
        Continue;
      if TargetClass.OriginalName = LClass.OriginalName then
        Exit;
    end;

  for var i := 0 to FMergedTypes.Count - 1 do
    if FMergedTypes[i].New.Name = LClass.FName then
    begin
      var M := FMergedTypes[i];
      M.New := LClass;
      FMergedTypes[i] := M;
    end;

  LClass.FName := LClass.OriginalName;
end;

procedure TJsonMapper.MergeClass(SourceClass, TargetClass: TJClass);
begin
  if SourceClass.Items.Count <> TargetClass.Items.Count then
    Exit;

  if SourceClass.ArrayItems.Count <> TargetClass.ArrayItems.Count then
    Exit;

  if SourceClass.ComplexItems.Count <> TargetClass.ComplexItems.Count then
    Exit;

  for var i := SourceClass.Items.Count - 1 downto 0 do
  begin
    var Found := False;
    var SourceField := SourceClass.Items[i];
    for var Field in TargetClass.Items do
      if SourceField.OriginalFieldName = Field.OriginalFieldName then
      begin
        Found := True;
        Break;
      end;
    if not Found then
      Exit;
  end;

  for var i := SourceClass.ArrayItems.Count - 1 downto 0 do
  begin
    var Found := False;
    var SourceField := SourceClass.ArrayItems[i];
    for var Field in TargetClass.ArrayItems do
      if SourceField.OriginalFieldName = Field.OriginalFieldName then
      begin
        Found := True;
        Break;
      end;
    if not Found then
      Exit;
  end;

  for var i := SourceClass.ComplexItems.Count - 1 downto 0 do
  begin
    var Found := False;
    var SourceField := SourceClass.ComplexItems[i];
    for var Field in TargetClass.ComplexItems do
      if SourceField.OriginalFieldName = Field.OriginalFieldName then
      begin
        Found := True;
        Break;
      end;
    if not Found then
      Exit;
  end;

  SourceClass.Merged := True;
  var M: TMergeType;
  M.Old := SourceClass;
  M.New := TargetClass;
  FMergedTypes.Add(M);
end;

function TJsonMapper.GenerateUnit: string;
begin
  var LList := TStringList.Create;
  try
    LList.Add('unit ' + FUnitName + ';');
    LList.Add('');
    LList.Add('interface');
    LList.Add('');
    LList.Add('uses');
    LList.Add('  Rest.Json, Rest.Json.Types' + IfThen(BaseClassUnit.IsEmpty, '', ', ' + BaseClassUnit) + ';');
    LList.Add('');
    LList.Add('type');

    if FForwardDeclarate then
      for var i := FClasses.Count - 1 downto 0 do
      begin
        var LClass := FClasses[i];
        if LClass.Merged then
          Continue;
        LList.Add(LClass.GetForwardDeclaration);
        LList.Add('');
      end;

    for var i := FClasses.Count - 1 downto 0 do
    begin
      var LClass := FClasses[i];
      if LClass.Merged then
        Continue;
      var L := LList.Count;
      LList.AddStrings(LClass.GetDeclarationPart(LList.Count));
      LClass.LineNumber := L;
      if i <> 0 then
        LList.Add('');
    end;

    LList.Add('');
    LList.Add('implementation');

    for var i := FClasses.Count - 1 downto 0 do
    begin
      var LClass := FClasses[i];
      if LClass.Merged then
        Continue;
      LList.AddStrings(LClass.GetImplementationPart);
    end;

    LList.Add('');
    LList.Add('end.');

    Result := LList.Text;
  finally
    LList.Free;
  end;
end;

procedure TJsonMapper.Visualize(ATreeView: TTreeView; AItemStyleLookup: string);
begin
  ATreeView.Opacity := 0;
  ATreeView.BeginUpdate;
  try
    ATreeView.Clear;
    if FRootClass <> nil then
    begin
      var LItem := TTreeViewItem.Create(ATreeView);
      LItem.StyleLookup := AItemStyleLookup;
      LItem.TagObject := FRootClass;
      LItem.WordWrap := False;
      LItem.Text := FRootClass.MergedName;
      LItem.StylesData['details'] := '';
      ATreeView.AddObject(LItem);
      InternalVisualize(LItem, FRootClass, AItemStyleLookup);
    end;
  finally
    if ATreeView.GlobalCount > 0 then
      ATreeView.ItemByGlobalIndex(0).Expand;
    TAnimator.AnimateFloatDelay(ATreeView, 'Opacity', 1, 0.2, 0.1);
    ATreeView.EndUpdate;
  end;
end;

procedure TJsonMapper.VisualizeClasses(ATreeView: TTreeView; AItemStyleLookup: string);
begin
  ATreeView.Opacity := 0;
  ATreeView.BeginUpdate;
  try
    ATreeView.Clear;
    for var i := FClasses.Count - 1 downto 0 do
    begin
      var LClass := FClasses[i];
      if LClass.Merged then
        Continue;
      var LItem := TTreeViewItem.Create(ATreeView);
      LItem.StyleLookup := AItemStyleLookup;
      LItem.TagObject := LClass;
      LItem.WordWrap := False;
      LItem.Text := LClass.MergedName;
      LItem.StylesData['details'] := '';
      ATreeView.AddObject(LItem);
      InternalVisualize(LItem, LClass, AItemStyleLookup);
    end;
  finally
    TAnimator.AnimateFloatDelay(ATreeView, 'Opacity', 1, 0.2, 0.1);
    ATreeView.EndUpdate;
  end;
end;

procedure TJsonMapper.ClearClasses;
begin
  FClasses.Clear;
  FMergedTypes.Clear;
end;

constructor TJsonMapper.Create;
begin
  inherited Create;
  FNumericDuplicate := True;
  FUnitName := '';
  FSortFields := False;
  FMergedTypes := TList<TMergeType>.Create;
  FClasses := TObjectList<TJClass>.Create;
end;

procedure TJsonMapper.Debug(ALines: TStrings);
begin
  ALines.Clear;
  for var LClass in FClasses do
  begin
    ALines.Add('-------');
    ALines.Add(LClass.Name);
    for var LField in LClass.Items do
      ALines.Add(Format('%-15s | %s', [LField.FieldName, LField.GetTypeAsString]));
  end;
end;

destructor TJsonMapper.Destroy;
begin
  ClearClasses;
  FClasses.Free;
  FMergedTypes.Free;
  inherited;
end;

procedure TJsonMapper.SetBaseClassName(const Value: string);
begin
  FBaseClassName := Value;
end;

procedure TJsonMapper.SetBaseClassUnit(const Value: string);
begin
  FBaseClassUnit := Value;
end;

procedure TJsonMapper.SetForwardDeclarate(const Value: Boolean);
begin
  FForwardDeclarate := Value;
end;

procedure TJsonMapper.SetMergeSameClasses(const Value: Boolean);
begin
  FMergeSameClasses := Value;
end;

procedure TJsonMapper.SetNumericDuplicate(const Value: Boolean);
begin
  FNumericDuplicate := Value;
end;

procedure TJsonMapper.SetSortFields(const Value: Boolean);
begin
  FSortFields := Value;
end;

procedure TJsonMapper.SetUnitName(const Value: string);
begin
  FUnitName := Value;
end;

procedure TJsonMapper.InternalVisualize(ATreeViewItem: TTreeViewItem; AClass: TJClass; AItemStyleLookup: string);
begin
  for var LField in AClass.FItems do
  begin
    var LItem := TTreeViewItem.Create(ATreeViewItem);
    LItem.StyleLookup := AItemStyleLookup;
    LItem.TagObject := LField;
    LItem.WordWrap := False;

    case LField.FieldType of
      jtObject:
        begin
          LItem.Text := LField.Name;
          LItem.StylesData['details'] := ': ' + LField.GetTypeAsString;
          InternalVisualize(LItem, (LField as TJPropertyObject).FieldClass, AItemStyleLookup);
        end;
      jtArray:
        begin
          LItem.Text := LField.Name;
          LItem.StylesData['details'] := ': ' + LField.GetTypeAsString;
          if (LField as TJPropertyArray).ContainedType = jtObject then
          begin
            InternalVisualize(LItem, (LField as TJPropertyArray).FieldClass, AItemStyleLookup);
          end;
        end;
    else
      LItem.Text := LField.Name;
      LItem.StylesData['details'] := ': ' + LField.GetTypeAsString;
    end;

    ATreeViewItem.AddObject(LItem);
  end;
end;

{ TJClass }

function TJClass.ContainsField(const OriginalFieldName: string; out JProp: TJProperty; RemoveIfNull: Boolean): Boolean;
begin
  JProp := nil;
  for var Item in FItems do
    if Item.OriginalFieldName = OriginalFieldName then
    begin
      if RemoveIfNull then
        if (Item.FieldType = jtNull) or ((Item is TJPropertyArray) and (TJPropertyArray(Item).ContainedType = jtNull)) then
        begin
          DeleteItem(Item);
          Exit(False);
        end;
      JProp := Item;
      Exit(True);
    end;
  Result := False;
end;

procedure TJClass.DeleteItem(Item: TJProperty);
begin
  FComplexItems.Remove(Item);
  FArrayItems.Remove(Item);
  FItems.Remove(Item);
end;

constructor TJClass.Create(AParentClass: TJClass; AClassName: string; AMapper: TJsonMapper);
begin
  inherited Create;
  FMapper := AMapper;
  FMerged := False;
  FParentClass := AParentClass;

  Name := AClassName;

  FItems := TObjectList<TJProperty>.Create;
  FComplexItems := TList<TJProperty>.Create;
  FArrayItems := TList<TJProperty>.Create;
  FMapper.FClasses.Add(Self);

  FComparison :=
    function(const Left, Right: TJProperty): Integer
    begin
      if Left.FName > Right.FName then
        Result := 1
      else if Left.FName < Right.FName then
        Result := -1
      else
        Result := 0;
    end;

  FComparer := TComparer<TJProperty>.Construct(FComparison);
end;

destructor TJClass.Destroy;
begin
  FItems.Free;
  FComplexItems.Free;
  FArrayItems.Free;
  inherited;
end;

function TJClass.GetImplementationPart: TArray<string>;
begin
  if (FComplexItems.Count <= 0) and (FArrayItems.Count <= 0) then
    Exit;

  var LLines := TStringList.Create;
  try
    var LClassName := Format('%s', [FName]);
    LLines.Add('');
    LLines.Add(Format('{ %s }', [LClassName]));
    LLines.Add('');

    LLines.Add(Format('destructor %s.Destroy;', [LClassName]));
    LLines.Add('begin');

    for var Item in FArrayItems do
    begin
      LLines.Add(Format(' for var Item in %s do', [Item.FieldName]));
      LLines.Add('   Item.Free;');
    end;

    for var Item in FComplexItems do
      LLines.Add(Format('  %s.Free;', [Item.FieldName]));

    LLines.Add('  inherited;');
    LLines.Add('end;');

    Result := LLines.ToStringArray;
  finally
    LLines.Free;
  end;
end;

function TJClass.GetMergedName: string;
begin
  if Merged then
    for var M in FMapper.FMergedTypes do
      if M.Old.Name = Name then
        Exit(M.New.Name);
  Result := Name;
end;

procedure TJClass.SetLineNumber(const Value: Integer);
begin
  FLineNumber := Value;
end;

procedure TJClass.SetMerged(const Value: Boolean);
begin
  FMerged := Value;
end;

procedure TJClass.SetName(const Value: string);
begin
  var LName := '';
  for var i := 1 to Value.Length do
    if CharInSet(Value[i], AllowedChars) then
    begin
      if (LName.IsEmpty) or ((i > 1) and (not CharInSet(Value[i - 1], AllowedChars))) then
        LName := LName + UpperCase(Value[i])
      else
        LName := LName + Value[i];
    end;
  LName := 'T' + LName;
  FOriginalName := LName;
  FName := FMapper.SuggestClassName(Self, LName);
end;

procedure TJClass.SortFields;
begin
  if FMapper.FSortFields then
    FItems.Sort(FComparer);
end;

function TJClass.GetDeclarationPart(const Offset: Integer): TArray<string>;
begin
  var LLines := TStringList.Create;
  try
    LLines.Add('  ' + FName + ' = class' + IfThen(FMapper.BaseClassName.IsEmpty, '', '(' + FMapper.BaseClassName + ')'));
    if FItems.Count > 0 then
    begin
      LLines.Add('  private');

      for var LItem in FItems do
      begin
        LLines.Add(Format('    [JsonNameAttribute(''%s'')]', [LItem.OriginalFieldName]));
        LLines.Add(Format('    %s: %s;', [LItem.FieldName, LItem.GetTypeAsString]));
      end;

      LLines.Add('  public');

      for var LItem in FItems do
      begin
        var L := LLines.Add(Format('    property %s: %s read %s write %s;',
          [LItem.PropertyName, LItem.GetTypeAsString, LItem.FieldName, LItem.FieldName]));
        LItem.LineNumber := Offset + L;
      end;

      if (FComplexItems.Count > 0) or (FArrayItems.Count > 0) then
        LLines.Add('    destructor Destroy; override;');
    end;
    LLines.Add('  end;');

    Result := LLines.ToStringArray;
  finally
    LLines.Free;
  end;
end;

function TJClass.GetForwardDeclaration: string;
begin
  Result := '  ' + FName + ' = class;';
end;

{ TJProperty }

constructor TJProperty.Create(AParentClass: TJClass; AItemName: string; AFieldType: TJsonType);
begin
  inherited Create;
  FParentClass := AParentClass;
  FFieldType := AFieldType;
  Name := AItemName;
  if FParentClass <> nil then
    FParentClass.FItems.Add(Self);
end;

destructor TJProperty.Destroy;
begin
  inherited;
end;

procedure TJProperty.SetCustomType(const Value: string);
begin
  FCustomType := Value;
end;

procedure TJProperty.SetLineNumber(const Value: Integer);
begin
  FLineNumber := Value;
end;

procedure TJProperty.ChangeName(const Value: string);
begin
  FName := '';

  for var i := 1 to Value.Length do
    if CharInSet(Value[i], AllowedChars) then
    begin
      if (FName.IsEmpty) or ((i > 1) and (not CharInSet(Value[i - 1], AllowedChars))) then
        FName := FName + UpperCase(Value[i])
      else
        FName := FName + Value[i];
    end;

  FFieldName := 'F' + string(Copy(FName, 1, 1)).ToUpper + Copy(FName, 2);

  var FExt: Extended;
  if TryStrToFloat(FName, FExt) then
    FName := 'Field' + FName;

  if ReservedWords.Contains(FName.ToLower) then
    FPropertyName := '&' + FName
  else
    FPropertyName := FName;
end;

procedure TJProperty.SetName(const Value: string);
begin
  FOriginalFieldName := Value;
  ChangeName(Value);
end;

{ TJPropertyPrimitive }

function TJPropertyPrimitive.GetTypeAsString: string;
begin
  if not FCustomType.IsEmpty then
    Exit(FCustomType);
  case FFieldType of
    jtString:
      Result := 'string';
    jtBool:
      Result := 'Boolean';
    jtNumber:
      Result := 'Extended';
    jtDate:
      Result := 'TDate';
    jtDateTime:
      Result := 'TDateTime';
    jtNull:
      Result := 'Unknown';
  else
    Result := 'Unknown';
  end;
end;

{ TJPropertyArray }

constructor TJPropertyArray.Create(AClass: TJClass; AItemName: string; ALevel: Integer; AItemSubType: TJsonType; AItemClass: TJClass);
begin
  inherited Create(AClass, AItemName, jtArray);
  FContainedType := AItemSubType;
  FLevel := ALevel;
  FFieldClass := AItemClass;
  if FContainedType = TJsonType.jtObject then
    AClass.FArrayItems.Add(Self);
end;

function TJPropertyArray.GetContainedTypeAsString: string;
begin
  if not FCustomType.IsEmpty then
    Exit(FCustomType);
  if FContainedType = jtObject then
    Result := FFieldClass.MergedName
  else
    case FContainedType of
      jtString:
        Result := 'string';
      jtBool:
        Result := 'Boolean';
      jtNumber:
        Result := 'Extended';
      jtDate:
        Result := 'TDate';
      jtNull:
        Result := 'Unknown';
      jtDateTime:
        Result := 'TDateTime';
    else
      Result := 'Unknown';
    end;
end;

function TJPropertyArray.GetTypeAsString: string;
begin
  if FLevel > 1 then
  begin
    var S := '';
    for var i := 1 to FLevel do
      S := S + 'TArray<';
    S := S + '%s';
    for var i := 1 to FLevel do
      S := S + '>';
    Result := Format(S, [GetContainedTypeAsString])
  end
  else
    Result := Format('TArray<%s>', [GetContainedTypeAsString]);
end;

{ TJPropertyObject }

constructor TJPropertyObject.Create(AParentClass: TJClass; AItemName: string; AItemClass: TJClass);
begin
  inherited Create(AParentClass, AItemName, jtObject);
  FFieldClass := AItemClass;
  AParentClass.FComplexItems.Add(Self);
end;

function TJPropertyObject.GetTypeAsString: string;
begin
  Result := FFieldClass.MergedName;
end;

initialization
  ReservedWords := TList<string>.Create;
  ReservedWords.Add('constructor');
  ReservedWords.Add('destructor');
  ReservedWords.Add('string');
  ReservedWords.Add('integer');
  ReservedWords.Add('word');
  ReservedWords.Add('byte');
  ReservedWords.Add('char');
  ReservedWords.Add('type');
  ReservedWords.Add('with');
  ReservedWords.Add('while');
  ReservedWords.Add('until');
  ReservedWords.Add('to');
  ReservedWords.Add('then');
  ReservedWords.Add('set');
  ReservedWords.Add('of');
  ReservedWords.Add('or');
  ReservedWords.Add('packed');
  ReservedWords.Add('for');
  ReservedWords.Add('var');
  ReservedWords.Add('program');
  ReservedWords.Add('repeat');
  ReservedWords.Add('begin');
  ReservedWords.Add('end');
  ReservedWords.Add('function');
  ReservedWords.Add('procedure');
  ReservedWords.Add('class');
  ReservedWords.Add('record');
  ReservedWords.Add('string');
  ReservedWords.Add('initialization');
  ReservedWords.Add('finalization');
  ReservedWords.Add('interface');
  ReservedWords.Add('implementation');
  ReservedWords.Add('inline');
  ReservedWords.Add('threadvar');
  ReservedWords.Add('unit');
  ReservedWords.Add('and');
  ReservedWords.Add('array');
  ReservedWords.Add('case');
  ReservedWords.Add('const');
  ReservedWords.Add('div');
  ReservedWords.Add('do');
  ReservedWords.Add('downto');
  ReservedWords.Add('else');
  ReservedWords.Add('file');
  ReservedWords.Add('goto');
  ReservedWords.Add('if');
  ReservedWords.Add('in');
  ReservedWords.Add('label');
  ReservedWords.Add('mod');
  ReservedWords.Add('nil');
  ReservedWords.Add('not');
  ReservedWords.Add('finally');
  ReservedWords.Add('except');
  ReservedWords.Add('try');
  ReservedWords.Add('public');
  ReservedWords.Add('private');
  ReservedWords.Add('protected');
  ReservedWords.Add('strict');
  ReservedWords.Add('deprecated');
  ReservedWords.Add('forward');
  ReservedWords.Add('object');

finalization
  FreeAndNil(ReservedWords);

end.

