unit Pkg.Json.Mapper;

interface

uses
  FMX.TreeView, System.JSON, Rest.Json, RTTI, RegularExpressions, TypInfo,
  SysUtils, classes, Generics.Collections, Generics.Defaults;

type
  EJsonMapper = class(Exception);

  TJsonType = (jtUnknown, jtObject, jtArray, jtString, jtTrue, jtFalse, jtNumber, jtDate, jtDateTime, jtBytes);

  TStubClass = class;

  TPkgJsonMapper = class;

  TStubField = class
  private
    FName: string;
    FPropertyName: string;
    FFieldName: string;
    FFieldType: TJsonType;
    FParentClass: TStubClass;
    FOriginalFieldName: string;
    procedure SetName(const Value: string);
  public
    constructor Create(AParentClass: TStubClass; AItemName: string; AFieldType: TJsonType);
    destructor Destroy; override;
    property Name: string read FName write SetName;
    property FieldName: string read FFieldName write FFieldName;
    property OriginalFieldName: string read FOriginalFieldName write FOriginalFieldName;
    property PropertyName: string read FPropertyName write FPropertyName;
    property FieldType: TJsonType read FFieldType write FFieldType;
    function GetTypeAsString: string; overload; virtual;
    class function GetTypeAsString(AType: TJsonType): string; overload;
  end;

  TStubContainerField = class(TStubField)
  private
    FFieldClass: TStubClass;
    FContainedType: TJsonType;
  public
    property ContainedType: TJsonType read FContainedType write FContainedType;
    property FieldClass: TStubClass read FFieldClass write FFieldClass;
  end;

  TStubObjectField = class(TStubContainerField)
  private
  public
    constructor Create(AParentClass: TStubClass; AItemName: string; AItemClass: TStubClass);
    function GetTypeAsString: string; override;
  end;

  TStubArrayField = class(TStubContainerField)
  private
  public
    constructor Create(AClass: TStubClass; AItemName: string; AItemSubType: TJsonType; AItemClass: TStubClass);
    function GetTypeAsString: string; override;
  end;

  TStubClass = class
  private
    FItems, FComplexItems, FArrayItems: TList<TStubField>;
    FName: string;
    FComparison: TComparison<TStubField>;
    FComparer: IComparer<TStubField>;
    FParentClass: TStubClass;
    FMapper: TPkgJsonMapper;
    FPureClassName: string;
    FArrayProperty: string;
    FOriginalName: string;
    FMerged: Boolean;
    procedure SortFields;
    procedure SetName(const Value: string);
    procedure SetPureClassName(const Value: string);
    procedure SetMerged(const Value: Boolean);
    function GetMergedName: string;
  public
    constructor Create(AParentClass: TStubClass; AClassName: string; AMapper: TPkgJsonMapper; AArrayProperty: string = '');
    destructor Destroy; override;
    property Name: string read FName write SetName;
    property OriginalName: string read FOriginalName write FOriginalName;
    property Items: TList<TStubField> read FItems write FItems;
    property ArrayItems: TList<TStubField> read FArrayItems write FArrayItems;
    property ComplexItems: TList<TStubField> read FComplexItems write FComplexItems;
    function GetDeclarationPart: string;
    function GetImplementationPart: string;
    property PureClassName: string read FPureClassName write SetPureClassName;
    property ArrayProperty: string read FArrayProperty write FArrayProperty;
    property Merged: Boolean read FMerged write SetMerged;
    property MergedName: string read GetMergedName;
  end;

  TMergeType = record
    OldName: string;
    NewName: string;
  end;

  TPkgJsonMapper = class
  private
    FTreeView: TTreeView;
    FClasses: TList<TStubClass>;
    FRootClass: TStubClass;
    FUnitName: string;
    FMergedTypes: TList<TMergeType>;
    FSort: Boolean;
    procedure SetUnitName(const Value: string);
    procedure MergeClasses(LClass: TStubClass);
    procedure MergeClass(SourceClass, TargetClass: TStubClass);
    procedure SetMergedTypes(const Value: TList<TMergeType>);
  protected
    function GetJsonType(AJsonValue: TJsonValue): TJsonType;
    function GetFirstArrayItem(AJsonValue: TJsonValue): TJsonValue;
    procedure ProcessJsonObject(AJsonValue: TJsonValue; AParentClass: TStubClass);
    procedure ClearClasses;
    procedure InternalFormatTreeViewFields(AItem: TTreeViewItem);
    procedure FormatFields(ATreeView: TTreeView);
    procedure InternalVisualize(ATreeViewItem: TTreeViewItem; AClass: TStubClass; AItemStyleLookup: string);
    function SuggestClassName(ASuggestedClassName: string): string;
  public
    constructor Create(ATreeView: TTreeView);
    destructor Destroy; override;
      //  Parses a JSON string and creates internal stub class structure
    procedure Parse(AJsonString: string; AMerge: Boolean; ASort: Boolean; ARootClassName: string = 'Root');
      //  Generates resultant unit
    function GenerateUnit: string;
    procedure Debug(ALines: TStrings);
      //  Visualizes stub class structure in a treeview
    procedure Visualize(ATreeView: TTreeView; AItemStyleLookup: string);
    property DestinationUnitName: string read FUnitName write SetUnitName;
    property MergedTypes: TList<TMergeType> read FMergedTypes write SetMergedTypes;
  end;

procedure PrettyPrintJSON(JSONValue: TJSONValue; OutputStrings: TStrings; indent: integer = 0);

var
  PointDsFormatSettings: TFormatSettings;

implementation

var
  ReservedWords: TList<string>;

const
  INDENT_SIZE = 2;
  AllowedChars =['0'..'9', 'a'..'z', 'A'..'Z'];

//  http://stackoverflow.com/a/12198174
procedure PrettyPrintPair(JSONValue: TJSONPair; OutputStrings: TStrings; last: boolean; indent: integer);
const
  TEMPLATE = '%s:%s';
var
  line: string;
  newList: TStringList;
begin
  newList := TStringList.Create;
  try
    PrettyPrintJSON(JSONValue.JsonValue, newList, indent);
    line := format(TEMPLATE, [JSONValue.JsonString.ToString, Trim(newList.Text)]);
  finally
    newList.Free;
  end;

  line := StringOfChar(' ', indent + INDENT_SIZE) + line;
  if not last then
    line := line + ',';
  OutputStrings.add(line);
end;

procedure PrettyPrintArray(JSONValue: TJSONArray; OutputStrings: TStrings; last: boolean; indent: integer);
var
  i: integer;
begin
  OutputStrings.add(StringOfChar(' ', indent + INDENT_SIZE) + '[');
  for i := 0 to JSONValue.Count - 1 do
  begin
    PrettyPrintJSON(JSONValue.Items[i], OutputStrings, indent);
    if i < JSONValue.Count - 1 then
      OutputStrings[OutputStrings.Count - 1] := OutputStrings[OutputStrings.Count - 1] + ',';
  end;
  OutputStrings.add(StringOfChar(' ', indent + INDENT_SIZE - 2) + ']');
end;

procedure PrettyPrintJSON(JSONValue: TJSONValue; OutputStrings: TStrings; indent: integer = 0);
var
  i: integer;
  LIdent: integer;
begin
  LIdent := indent + INDENT_SIZE;
  i := 0;

  if JSONValue is TJSONObject then
  begin
    OutputStrings.add(StringOfChar(' ', LIdent) + '{');
    for i := 0 to TJSONObject(JSONValue).Count - 1 do
      PrettyPrintPair(TJSONObject(JSONValue).Pairs[i], OutputStrings, i = TJSONObject(JSONValue).Count - 1, LIdent);
    OutputStrings.add(StringOfChar(' ', LIdent) + '}');
  end
  else if JSONValue is TJSONArray then
    PrettyPrintArray(TJSONArray(JSONValue), OutputStrings, i = TJSONObject(JSONValue).Count - 1, LIdent)
  else
    OutputStrings.add(StringOfChar(' ', LIdent) + JSONValue.ToString);
end;


{ TPkgJsonMapper }

procedure TPkgJsonMapper.ProcessJsonObject(AJsonValue: TJsonValue; AParentClass: TStubClass);
var
  LJsonObj: TJSONObject;
  LJsonPair: TJSONPair;
  LJsonVal, LJsonVal2: TJSONValue;
  LJsonType, LJsonType2: TJsonType;
  LClass: TStubClass;
begin
  LJsonObj := AJsonValue as TJSONObject;

  for LJsonPair in LJsonObj do
  begin
    LJsonVal := LJsonPair.JsonValue;
    LJsonType := GetJsonType(LJsonVal);

    case LJsonType of
      jtObject:
        begin
          LClass := TStubClass.Create(AParentClass, LJsonPair.JsonString.Value, self);
          TStubObjectField.Create(AParentClass, LJsonPair.JsonString.Value, LClass);
          ProcessJsonObject(LJsonVal, LClass);
        end;
      jtArray:
        begin
          LClass := nil;
          LJsonType2 := jtUnknown;

          LJsonVal2 := GetFirstArrayItem(LJsonVal);
          if LJsonVal2 <> nil then
          begin
            LJsonType2 := GetJsonType(LJsonVal2);
            case LJsonType2 of
              jtObject:
                begin
                  LClass := TStubClass.Create(AParentClass, LJsonPair.JsonString.Value, self);
                  ProcessJsonObject(LJsonVal2, LClass);
                end;
              jtArray:
                begin
                  LJsonType := jtUnknown;
                  LClass := nil;

                  var LJsonValue2 := GetFirstArrayItem(LJsonVal2);
                  if LJsonValue2 <> nil then
                  begin
                    LJsonType := GetJsonType(LJsonValue2);
                    LClass := TStubClass.Create(FRootClass, 'Item', self);
                  end;

                  FRootClass.ArrayProperty := 'Items';
                  TStubArrayField.Create(FRootClass, 'Items', LJsonType, LClass);
                  ProcessJsonObject(LJsonValue2, LClass);
                //raise EJsonMapper.Create('Nested Arrays are not supported!');
                end;
            end;
          end;

          TStubArrayField.Create(AParentClass, LJsonPair.JsonString.Value, LJsonType2, LClass);
        end;
      jtNumber, jtString, jtDate, jtDateTime, jtTrue, jtFalse:
        TStubField.Create(AParentClass, LJsonPair.JsonString.Value, LJsonType);
    end;
  end;

  AParentClass.SortFields;
end;

procedure TPkgJsonMapper.MergeClasses(LClass: TStubClass);
begin
  for var TargetClass in FClasses do
  begin
    if TargetClass = LClass then
      Continue;
    //if TargetClass.OriginalName = LClass.OriginalName then
    MergeClass(TargetClass, LClass);
  end;

  for var TargetClass in FClasses do
  begin
    if TargetClass = LClass then
      Continue;
    if TargetClass.Merged then
      Continue;
    if TargetClass.OriginalName = LClass.OriginalName then
      Exit;
  end;

  for var i := 0 to FMergedTypes.Count - 1 do
  begin
    if FMergedTypes[i].NewName = LClass.FName then
    begin
      var M := FMergedTypes[i];
      M.NewName := LClass.OriginalName;
      FMergedTypes[i] := M;
    end;
  end;

  LClass.FName := LClass.OriginalName;
end;

procedure TPkgJsonMapper.MergeClass(SourceClass, TargetClass: TStubClass);
begin
  var FoundDif: Boolean := False;

  if SourceClass.Items.Count <> TargetClass.Items.Count then
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
      FoundDif := True;
  end;

  if FoundDif then
    Exit;

  if SourceClass.ArrayItems.Count <> TargetClass.ArrayItems.Count then
    Exit;

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
      FoundDif := True;
  end;

  if FoundDif then
    Exit;

  if SourceClass.ComplexItems.Count <> TargetClass.ComplexItems.Count then
    Exit;

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
      FoundDif := True;
  end;

  if FoundDif then
    Exit;

  SourceClass.Merged := True;
  var M: TMergeType;
  M.OldName := SourceClass.Name;
  M.NewName := TargetClass.Name;
  FMergedTypes.Add(M);
end;

function TPkgJsonMapper.GenerateUnit: string;
var
  LClass: TStubClass;
  k: integer;
  LList: TStringList;
begin
  LList := TStringList.Create;
  try
    LList.Add('unit ' + FUnitName + ';');
    LList.Add('');
    LList.Add('interface');
    LList.Add('');
    LList.Add('uses');
    LList.Add('  Rest.Json, Rest.Json.Types;');
    LList.Add('');
    LList.Add('type');

    for k := FClasses.Count - 1 downto 0 do
    begin
      LClass := FClasses[k];

      if LClass.Merged then
        Continue;

      LList.Add(LClass.GetDeclarationPart.TrimRight);
      if k <> 0 then
        LList.Add('');
    end;

    LList.Add('');
    LList.Add('implementation');

    for k := FClasses.Count - 1 downto 0 do
    begin
      LClass := FClasses[k];
      if LClass.Merged then
        Continue;
      var LItem := LClass.GetImplementationPart.TrimRight;
      if not LItem.IsEmpty then
        LList.Add(LItem);
    end;

    LList.Add('');
    LList.Add('end.');

    Result := LList.Text;
  finally
    LList.Free;
  end;
end;

procedure TPkgJsonMapper.Visualize(ATreeView: TTreeView; AItemStyleLookup: string);
var
  LItem: TTreeViewItem;
begin
  ATreeView.BeginUpdate;
  try
    ATreeView.Clear;
    if FRootClass <> nil then
    begin
      LItem := TTreeViewItem.Create(ATreeView);
      LItem.Text := FRootClass.MergedName;
      LItem.TagObject := FRootClass;
      LItem.WordWrap := False;
      ATreeView.AddObject(LItem);
      InternalVisualize(LItem, FRootClass, AItemStyleLookup);
      FormatFields(ATreeView);
    end;
  finally
    if ATreeView.GlobalCount > 0 then
      ATreeView.ItemByGlobalIndex(0).Expand;
    ATreeView.EndUpdate;
  end;
end;

function TPkgJsonMapper.GetFirstArrayItem(AJsonValue: TJsonValue): TJsonValue;
begin
  Result := nil;
  for var LJsonValue in AJsonValue as TJsonArray do
  begin
    Result := LJsonValue;
    break;
  end;
end;

procedure TPkgJsonMapper.ClearClasses;
begin
  for var LClass in FClasses do
    LClass.Free;
  FClasses.Clear;
  FMergedTypes.Clear;
end;

constructor TPkgJsonMapper.Create(ATreeView: TTreeView);
begin
  inherited Create;
  FTreeView := ATreeView;
  FMergedTypes := TList<TMergeType>.Create;
  FClasses := TList<TStubClass>.Create;
end;

procedure TPkgJsonMapper.Debug(ALines: TStrings);
var
  LClass: TStubClass;
  LField: TStubField;
begin
  ALines.Clear;

  for LClass in FClasses do
  begin
    ALines.Add('-------');
    ALines.Add(LClass.Name);
    for LField in LClass.FItems do
    begin
      ALines.Add(format('%-15s | %s', [LField.FieldName, LField.GetTypeAsString]));
    end;
  end;
end;

destructor TPkgJsonMapper.Destroy;
begin
  ClearClasses;
  FreeAndNil(FClasses);
  FMergedTypes.Free;
  inherited;
end;

procedure TPkgJsonMapper.FormatFields(ATreeView: TTreeView);
begin
  if ATreeView.Count = 1 then
  begin
    InternalFormatTreeViewFields(ATreeView.Items[0]);
  end;
end;

procedure TPkgJsonMapper.SetMergedTypes(const Value: TList<TMergeType>);
begin
  FMergedTypes := Value;
end;

procedure TPkgJsonMapper.SetUnitName(const Value: string);
begin
  FUnitName := Value;
end;

function TPkgJsonMapper.SuggestClassName(ASuggestedClassName: string): string;
var
  LMax: integer;
begin
  Result := ASuggestedClassName;
  LMax := 0;
  for var LClass in FClasses do
  begin
    if LClass.OriginalName.ToLower = ASuggestedClassName.ToLower then
      Inc(LMax);
  end;

  if LMax > 0 then
    Result := Format('%s_%0.3d', [ASuggestedClassName, LMax]);
end;

function TPkgJsonMapper.GetJsonType(AJsonValue: TJsonValue): TJsonType;
var
  LJsonString: TJSONString;
begin
  if AJsonValue is TJSONObject then
    result := jtObject
  else if AJsonValue is TJSONArray then
    result := jtArray
  else if (AJsonValue is TJSONNumber) then
    result := jtNumber
  else if AJsonValue is TJSONTrue then
    result := jtTrue
  else if AJsonValue is TJSONFalse then
    result := jtFalse
  else if AJsonValue is TJSONString then
  begin
    LJsonString := (AJsonValue as TJSONString);
    if TRegEx.IsMatch(LJsonString.Value, '^([0-9]{4})-?(1[0-2]|0[1-9])-?(3[01]|0[1-9]|[12][0-9])(T| )(2[0-3]|[01][0-9]):?([0-5][0-9]):?([0-5][0-9])$') then
      result := jtDateTime
    else if TRegEx.IsMatch(LJsonString.Value, '^([0-9]{4})(-?)(1[0-2]|0[1-9])\2(3[01]|0[1-9]|[12][0-9])$') then
      result := jtDate
    else
      result := jtString
  end
  else
    result := jtUnknown;
end;

procedure TPkgJsonMapper.InternalFormatTreeViewFields(AItem: TTreeViewItem);
var
  LItem: TTreeViewItem;
  k: Integer;
  LSize, LPos: integer;
begin
  Exit;
  LSize := 0;

  //  Find max len
  for k := 0 to AItem.Count - 1 do
  begin
    LItem := AItem.Items[k];
    LPos := Pos(':', LItem.Text);
    if (LPos > 0) and (LPos > LSize) then
      LSize := LPos;
  end;

  for k := 0 to AItem.Count - 1 do
  begin
    LItem := AItem.Items[k];
    LPos := LSize - Pos(':', LItem.Text);
    if (LPos > 0) then
      LItem.Text := LItem.Text.Replace(':', StringOfChar(' ', LPos) + ':');

    InternalFormatTreeViewFields(LItem);
  end;
end;

procedure TPkgJsonMapper.InternalVisualize(ATreeViewItem: TTreeViewItem; AClass: TStubClass; AItemStyleLookup: string);
var
  LField: TStubField;
  LItem: TTreeViewItem;
begin
  for LField in AClass.FItems do
  begin
    LItem := TTreeViewItem.Create(ATreeViewItem);
    LItem.StyleLookup := AItemStyleLookup;
    LItem.TagObject := LField;
    LItem.WordWrap := false;

    case LField.FieldType of
      jtObject:
        begin
          LItem.Text := LField.Name;
          LItem.StylesData['details'] := ': ' + LField.GetTypeAsString;
          InternalVisualize(LItem, (LField as TStubObjectField).FieldClass, AItemStyleLookup);
        end;
      jtArray:
        begin
          LItem.Text := LField.Name;
          LItem.StylesData['details'] := ': ' + LField.GetTypeAsString;
          if (LField as TStubArrayField).ContainedType = jtObject then
          begin
            InternalVisualize(LItem, (LField as TStubArrayField).FieldClass, AItemStyleLookup);
          end;
        end;
    else
      begin
        LItem.Text := LField.Name;
        LItem.StylesData['details'] := ': ' + LField.GetTypeAsString;
      end;
    end;

    ATreeViewItem.AddObject(LItem);
  end;
end;

procedure TPkgJsonMapper.Parse(AJsonString: string; AMerge: Boolean; ASort: Boolean; ARootClassName: string);
var
  LJsonValue, LJsonValue2: TJSONValue;
  LJsonType: TJsonType;
  LClass: TStubClass;
begin
  ClearClasses;
  FSort := ASort;

  LJsonValue := TJSONObject.ParseJSONValue(AJsonString);
  if LJsonValue <> nil then
  begin
    try
      FRootClass := TStubClass.Create(nil, ARootClassName, Self);

      case GetJsonType(LJsonValue) of
        jtObject:
          begin
            ProcessJsonObject(LJsonValue, FRootClass);
          end;
        jtArray:
          begin
            LJsonType := jtUnknown;
            LClass := nil;

            LJsonValue2 := GetFirstArrayItem(LJsonValue);
            if LJsonValue2 <> nil then
            begin
              LJsonType := GetJsonType(LJsonValue2);
              LClass := TStubClass.Create(FRootClass, 'Item', Self);
            end;

            FRootClass.ArrayProperty := 'Items';
            TStubArrayField.Create(FRootClass, 'Items', LJsonType, LClass);
            ProcessJsonObject(LJsonValue2, LClass);
          end;
      end;
      if AMerge then
        for var i := FClasses.Count - 1 downto 0 do
          MergeClasses(FClasses[i]);
    finally
      LJsonValue.Free;
    end;
  end
  else
    raise EJsonMapper.Create('Unable to parse the JSON String!');
end;

{ TVirtualClass }

constructor TStubClass.Create(AParentClass: TStubClass; AClassName: string; AMapper: TPkgJsonMapper; AArrayProperty: string);
begin
  inherited Create;
  FMapper := AMapper;
  FMerged := False;
  Name := AClassName;

  FItems := TList<TStubField>.Create;
  FComplexItems := TList<TStubField>.Create;
  FArrayItems := TList<TStubField>.Create;
  FMapper.FClasses.Add(self);
  FArrayProperty := AArrayProperty;

  FParentClass := AParentClass;

  FComparison :=
    function(const Left, Right: TStubField): Integer
    begin
      if Left.FName > Right.FName then
        result := 1
      else if Left.FName < Right.FName then
        result := -1
      else
        result := 0;
    end;

  FComparer := TComparer<TStubField>.Construct(FComparison);
end;

destructor TStubClass.Destroy;
var
  LItem: TStubField;
begin
  //  ToArray is needed because stub field remove themselves from FItems
  for LItem in FItems.ToArray do
    LItem.Free;

  FreeAndNil(FComplexItems);
  FreeAndNil(FItems);
  FreeAndNil(FArrayItems);
  inherited;
end;

function TStubClass.GetImplementationPart: string;
var
  LLines: TStringList;
  LString: string;
  LClassName: string;
  LItem: TStubField;
begin
  LLines := TStringList.Create;
  try
    if (FComplexItems.Count > 0) or (FArrayItems.Count > 0) then
    begin
      LClassName := format('%s', [FName]);
      LLines.Add('');
      LLines.Add(format('{ %s }', [LClassName]));
      LLines.Add('');

      LLines.Add(format('destructor %s.Destroy;', [LClassName]));
              {
      if FArrayItems.Count > 0 then
      begin
        LLines.Add('var');
        for LItem in FArrayItems do
        begin
          LString := format('  L%sItem: %s;', [LItem.FName, (LItem as TStubContainerField).FFieldClass.Name]);
          LLines.Add(LString);
        end;
      end;       }

      LLines.Add('begin');

      if FArrayItems.Count > 0 then
      begin
        for LItem in FArrayItems do
        begin
          LLines.Add(format(' for var L%sItem in %s do', [LItem.FName, LItem.FieldName]));
          LLines.Add(format('   L%sItem.Free;', [LItem.FName]));
        end;
      end;

      for LItem in FComplexItems do
      begin
        LString := Format('  %s.Free;', [LItem.FieldName]);
        LLines.Add(LString);
      end;

      LLines.Add('  inherited;');
      LLines.Add('end;')
    end;

    Result := LLines.Text;
  finally
    LLines.Free;
  end;
end;

function TStubClass.GetMergedName: string;
begin
  if Merged then
    for var M in FMapper.MergedTypes do
      if M.OldName = Name then
        Exit(M.NewName);
  Result := Name;
end;

procedure TStubClass.SetMerged(const Value: Boolean);
begin
  FMerged := Value;
end;

procedure TStubClass.SetName(const Value: string);
var
  LName: string;
begin
  LName := '';
  for var i := 1 to Value.Length do
    if CharInSet(Value[i], AllowedChars) then
    begin
      if (LName.IsEmpty) or ((i > 1) and (not CharInSet(Value[i - 1], AllowedChars))) then
        LName := LName + UpperCase(Value[i])
      else
        LName := LName + Value[i];
    end;
  FPureClassName := LName;
  LName := 'T' + FPureClassName;
  FOriginalName := LName;
  FName := FMapper.SuggestClassName(LName);
end;

procedure TStubClass.SetPureClassName(const Value: string);
begin
  FPureClassName := Value;
end;

procedure TStubClass.SortFields;
begin
  if FMapper.FSort then
    FItems.Sort(FComparer);
end;

function TStubClass.GetDeclarationPart: string;
var
  LLines: TStringList;
  LItem: TStubField;
begin
  LLines := TStringList.Create;
  try
    LLines.Add('  ' + FName + ' = class');
    LLines.Add('  private');

    for LItem in FItems do
    begin
      if (LItem.FieldType = jtUnknown) or ((LItem is TStubContainerField) and ((LItem as TStubContainerField).ContainedType = jtUnknown)) then
        //raise EJsonMapper.CreateFmt('The property [%s] has unknown type!', [LItem.PropertyName]);
        Continue;
      LLines.Add(Format('    [JsonNameAttribute(''%s'')]', [LItem.OriginalFieldName]));
      LLines.Add(Format('    %s: %s;', [LItem.FieldName, LItem.GetTypeAsString]));
    end;

    LLines.Add('  public');

    for LItem in FItems do
    begin
      if (LItem.FieldType = jtUnknown) or ((LItem is TStubContainerField) and ((LItem as TStubContainerField).ContainedType = jtUnknown)) then
        //raise EJsonMapper.CreateFmt('The property [%s] has unknown type!', [LItem.PropertyName]);
        Continue;

      LLines.Add(Format('    property %s: %s read %s write %s;', [LItem.PropertyName, LItem.GetTypeAsString, LItem.FieldName, LItem.FieldName]));
    end;

    if (FComplexItems.Count > 0) or (FArrayItems.Count > 0) then
      LLines.Add('    destructor Destroy; override;');

    LLines.Add('  end;');

    Result := LLines.Text;
  finally
    LLines.Free;
  end;
end;

{ TVirtualClassItemBase }

constructor TStubField.Create(AParentClass: TStubClass; AItemName: string; AFieldType: TJsonType);
begin
  inherited Create;

  //if AItemName.Contains('-') then
  //  raise EJsonMapper.CreateFmt('%s: Hyphens are not allowed!', [AItemName]);

  FParentClass := AParentClass;
  FFieldType := AFieldType;
  Name := AItemName;

  if FParentClass <> nil then
    FParentClass.FItems.Add(self);
end;

destructor TStubField.Destroy;
begin
  if FParentClass <> nil then
    FParentClass.FItems.Remove(self);
  inherited;
end;

class function TStubField.GetTypeAsString(AType: TJsonType): string;
begin
  case AType of
    jtUnknown:
      result := 'Unknown';
    jtString:
      result := 'string';
    jtTrue, jtFalse:
      result := 'Boolean';
    jtNumber:
      result := 'Extended';
    jtDate:
      result := 'TDate';
    jtDateTime:
      result := 'TDateTime';
    jtBytes:
      result := 'Byte';
  end;
end;

procedure TStubField.SetName(const Value: string);
begin
  if (FParentClass.FArrayProperty <> '') and (FParentClass.FArrayProperty = FName) then
    FParentClass.FArrayProperty := Value;

  FOriginalFieldName := Value;
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

function TStubField.GetTypeAsString: string;
begin
  Result := GetTypeAsString(FFieldType);
end;

{ TArrayItem }

constructor TStubArrayField.Create(AClass: TStubClass; AItemName: string; AItemSubType: TJsonType; AItemClass: TStubClass);
begin
  inherited Create(AClass, AItemName, jtArray);
  FContainedType := AItemSubType;
  FFieldClass := AItemClass;
  if FContainedType = TJsonType.jtObject then
    AClass.FArrayItems.Add(self);
end;

function TStubArrayField.GetTypeAsString: string;
var
  LSubType: string;
begin
  case FContainedType of
    jtObject:
      //LSubType := FFieldClass.Name;
      LSubType := FFieldClass.MergedName;
    jtArray:
      LSubType := format('TArray<%s>', [FFieldClass.Name]);
      //LSubType := format('TArray<%s>', [FFieldClass.OriginalName]);
      //raise EJsonMapper.Create('Nested arrays are not supported!');
  else
    LSubType := GetTypeAsString(FContainedType);
  end;
  Result := format('TArray<%s>', [LSubType]);
end;

{ TStubObjectField }

constructor TStubObjectField.Create(AParentClass: TStubClass; AItemName: string; AItemClass: TStubClass);
begin
  inherited Create(AParentClass, AItemName, jtObject);
  FFieldClass := AItemClass;
  AParentClass.FComplexItems.Add(self);
  FContainedType := jtObject;
end;

function TStubObjectField.GetTypeAsString: string;
begin
  //Result := FFieldClass.Name;
  Result := FFieldClass.MergedName;
end;

initialization
  PointDsFormatSettings := TFormatSettings.Create();
  PointDsFormatSettings.DecimalSeparator := '.';

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

finalization
  FreeAndNil(ReservedWords);

end.

