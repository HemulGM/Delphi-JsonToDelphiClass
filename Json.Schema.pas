unit Json.Schema;

interface

uses
  System.SysUtils, System.Generics.Collections, System.JSON, FMX.TreeView;

{$SCOPEDENUMS ON}

type
  TJSONType = (jString, jNumber, jInteger, jArray, jObject, jBoolean, jNull);

  TDelphiType = (dUnknown, dString, dExtended, dInteger, dInt64, dArray, dObject, dBoolean, dDate, dTime, dDateTime);

  EJsonSchema = class(Exception);

  TJSONSchemaMeta = class
  private
    FSchema: string;
    FTitle: string;
    FDescription: string;
    FId: string;
  public
    property Schema: string read FSchema write FSchema;
    property Id: string read FId write FId;
    property Title: string read FTitle write FTitle;
    property Description: string read FDescription write FDescription;
  end;

  TDClass = class;

  TDClassProperty = class;

  TDClasses = TObjectList<TDClass>;

  TDClassPropertyies = TObjectList<TDClassProperty>;

  TDClassPropertyType = class
  private
    FJsonType: TJSONType;
    FMeta: TJSONSchemaMeta;
    procedure SetJsonType(const Value: TJSONType);
  protected
    function GetName: string; virtual; abstract;
  public
    property JsonType: TJSONType read FJsonType write SetJsonType;
    property Name: string read GetName;
    property Meta: TJSONSchemaMeta read FMeta;
    constructor Create;
    destructor Destroy; override;
  end;

  TDClassPropertyTypeClass = class of TDClassPropertyType;

  TDTypeString = class(TDClassPropertyType)
  protected
    function GetName: string; override;
  end;

  TDTypeInteger = class(TDClassPropertyType)
  protected
    function GetName: string; override;
  end;

  TDTypeInt64 = class(TDClassPropertyType)
  protected
    function GetName: string; override;
  end;

  TDTypeExtended = class(TDClassPropertyType)
  protected
    function GetName: string; override;
  end;

  TDTypeBoolean = class(TDClassPropertyType)
  protected
    function GetName: string; override;
  end;

  TDTypeDate = class(TDClassPropertyType)
  protected
    function GetName: string; override;
  end;

  TDTypeTime = class(TDClassPropertyType)
  protected
    function GetName: string; override;
  end;

  TDTypeDateTime = class(TDClassPropertyType)
  protected
    function GetName: string; override;
  end;

  TDTypeObject = class(TDClassPropertyType)
  private
    FTypeClass: TDClass;
    procedure SetTypeClass(const Value: TDClass);
  protected
    function GetName: string; override;
  public
    property TypeClass: TDClass read FTypeClass write SetTypeClass;
  end;

  TDTypeArray = class(TDClassPropertyType)
  private
    FItemType: TDClassPropertyType;
    procedure SetItemType(const Value: TDClassPropertyType);
  protected
    function GetName: string; override;
  public
    property TypeItem: TDClassPropertyType read FItemType write SetItemType;
    destructor Destroy; override;
  end;

  TDClassProperty = class
  private
    FName: string;
    FPropertyType: TDClassPropertyType;
    procedure SetName(const Value: string);
    procedure SetPropertyType(const Value: TDClassPropertyType);
  public
    property Name: string read FName write SetName;
    property PropertyType: TDClassPropertyType read FPropertyType write SetPropertyType;
    destructor Destroy; override;
  end;

  TDClass = class
  private
    FName: string;
    FProperties: TDClassPropertyies;
    FMeta: TJSONSchemaMeta;
    procedure SetName(const Value: string);
  public
    property Name: string read FName write SetName;
    property Properties: TDClassPropertyies read FProperties;
    property Meta: TJSONSchemaMeta read FMeta;
    constructor Create(const Name: string); reintroduce;
    destructor Destroy; override;
  end;

  TFormatTypes = TDictionary<string, TDClassPropertyTypeClass>;

  TJSONSchema = class
  private
    FRootClass: TDClass;
    FClasses: TDClasses;
    FFormatTypes: TFormatTypes;
    FUnitName: string;
    FForwardDeclarate: Boolean;
    FSortFields: Boolean;
    FNumericDuplicate: Boolean;
    FBaseClassUnit: string;
    FBaseClassName: string;
    function GetDelphiType(const FormatValue: string): TDClassPropertyTypeClass;
    procedure FillFormatTypes;
    procedure ProcessSchema(const DClass: TDClass; const JObject, JRoot: TJSONObject);
    procedure ProcessProperties(const DClass: TDClass; const JObject, JRoot: TJSONObject);
    function CreateProp(const Name: string; const JProp, JRoot: TJSONObject): TDClassPropertyType;
    function CreatePropString(const JProp, JRoot: TJSONObject): TDClassPropertyType;
    function CreatePropInteger(const JProp, JRoot: TJSONObject): TDClassPropertyType;
    function CreatePropNumber(const JProp, JRoot: TJSONObject): TDClassPropertyType;
    function CreatePropBoolean(const JProp, JRoot: TJSONObject): TDClassPropertyType;
    function CreatePropObject(const Name: string; const JProp, JRoot: TJSONObject): TDClassPropertyType;
    function CreatePropArray(const Name: string; const JProp, JRoot: TJSONObject): TDClassPropertyType;
    function CreatePropFromRef(const Ref: string; const JRoot: TJSONObject): TDClassPropertyType;
    procedure SetBaseClassName(const Value: string);
    procedure SetBaseClassUnit(const Value: string);
    procedure SetForwardDeclarate(const Value: Boolean);
    procedure SetNumericDuplicate(const Value: Boolean);
    procedure SetSortFields(const Value: Boolean);
    procedure SetUnitName(const Value: string);
    procedure InternalVisualize(ATreeViewItem: TTreeViewItem; AClass: TDClass; AItemStyleLookup: string);
    procedure ProcessMeta(const Meta: TJSONSchemaMeta; const JObject, JRoot: TJSONObject);
  public
    procedure Clear;
    property Classes: TDClasses read FClasses;
    property FormatTypes: TFormatTypes read FFormatTypes;
    constructor Create;
    destructor Destroy; override;
    /// <summary>
    /// Parses a JSON string and creates internal stub class structure
    /// </summary>
    procedure Parse(const JsonString: string; const RootClassName: string = '');
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
    property NumericDuplicate: Boolean read FNumericDuplicate write SetNumericDuplicate;
    property SortFields: Boolean read FSortFields write SetSortFields;
    property ForwardDeclarate: Boolean read FForwardDeclarate write SetForwardDeclarate;
  end;

implementation

uses
  FMX.Ani;

{ TJSONSchema }

procedure TJSONSchema.Clear;
begin
  FRootClass := nil;
  Classes.Clear;
end;

constructor TJSONSchema.Create;
begin
  inherited;
  FClasses := TDClasses.Create;
  FFormatTypes := TFormatTypes.Create;
  FillFormatTypes;
end;

destructor TJSONSchema.Destroy;
begin
  FClasses.Free;
  FFormatTypes.Free;
  inherited;
end;

procedure TJSONSchema.FillFormatTypes;
begin
  FFormatTypes.Add('int32', TDTypeInteger);
  FFormatTypes.Add('int64', TDTypeInt64);
  FFormatTypes.Add('date-time', TDTypeDateTime);
end;

procedure TJSONSchema.Parse(const JsonString, RootClassName: string);
begin
  Clear;
  var JSON := TJSONObject.ParseJSONValue(JsonString, False, True);
  if Assigned(JSON) and (JSON is TJSONObject) then
  try
    FRootClass := TDClass.Create(RootClassName);
    ProcessSchema(FRootClass, TJSONObject(JSON), TJSONObject(JSON));
  finally
    JSON.Free;
  end
  else
    raise EJsonSchema.Create('Unable to parse the JSON String!');
end;

procedure TJSONSchema.ProcessSchema(const DClass: TDClass; const JObject, JRoot: TJSONObject);
begin
  FClasses.Add(DClass);
  ProcessMeta(DClass.Meta, JObject, JRoot);
  ProcessProperties(DClass, JObject, JRoot);
end;

procedure TJSONSchema.SetBaseClassName(const Value: string);
begin
  FBaseClassName := Value;
end;

procedure TJSONSchema.SetBaseClassUnit(const Value: string);
begin
  FBaseClassUnit := Value;
end;

procedure TJSONSchema.SetForwardDeclarate(const Value: Boolean);
begin
  FForwardDeclarate := Value;
end;

procedure TJSONSchema.SetNumericDuplicate(const Value: Boolean);
begin
  FNumericDuplicate := Value;
end;

procedure TJSONSchema.SetSortFields(const Value: Boolean);
begin
  FSortFields := Value;
end;

procedure TJSONSchema.SetUnitName(const Value: string);
begin
  FUnitName := Value;
end;

procedure TJSONSchema.Visualize(ATreeView: TTreeView; AItemStyleLookup: string);
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
      LItem.Text := FRootClass.Name;
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

procedure TJSONSchema.InternalVisualize(ATreeViewItem: TTreeViewItem; AClass: TDClass; AItemStyleLookup: string);
begin
  for var LField in AClass.Properties do
  begin
    var LItem := TTreeViewItem.Create(ATreeViewItem);
    LItem.StyleLookup := AItemStyleLookup;
    LItem.TagObject := LField;
    LItem.WordWrap := False;

    if LField.PropertyType is TDTypeObject then
    begin
      LItem.Text := LField.Name;
      LItem.StylesData['details'] := ': ' + LField.PropertyType.Name;
      InternalVisualize(LItem, TDTypeObject(LField.PropertyType).TypeClass, AItemStyleLookup);
    end
    else if LField.PropertyType is TDTypeArray then
    begin
      LItem.Text := LField.Name;
      LItem.StylesData['details'] := ': ' + LField.PropertyType.Name;
      if TDTypeArray(LField.PropertyType).TypeItem is TDTypeObject then
      begin
        InternalVisualize(LItem, TDTypeObject(TDTypeArray(LField.PropertyType).TypeItem).TypeClass, AItemStyleLookup);
      end;
    end
    else
    begin
      LItem.Text := LField.Name;
      LItem.StylesData['details'] := ': ' + LField.PropertyType.Name;
    end;

    ATreeViewItem.AddObject(LItem);
  end;
end;

procedure TJSONSchema.VisualizeClasses(ATreeView: TTreeView; AItemStyleLookup: string);
begin

end;

procedure TJSONSchema.ProcessProperties(const DClass: TDClass; const JObject, JRoot: TJSONObject);
begin
  var JProps := JObject.GetValue<TJSONObject>('properties', nil);
  if not Assigned(JProps) then
    Exit;
  for var JProp in JProps do
    if JProp.JsonValue is TJSONObject then
    begin
      var PropType := CreateProp(JProp.JsonString.Value, TJSONObject(JProp.JsonValue), JRoot);
      if Assigned(PropType) then
      begin
        var Prop := TDClassProperty.Create;
        Prop.Name := JProp.JsonString.Value;
        Prop.PropertyType := PropType;
        DClass.Properties.Add(Prop);
      end;
    end;
end;

procedure TJSONSchema.ProcessMeta(const Meta: TJSONSchemaMeta; const JObject, JRoot: TJSONObject);
begin
  Meta.Description := JObject.GetValue<string>('description', '');
  Meta.Title := JObject.GetValue<string>('title', '');
  Meta.Id := JObject.GetValue<string>('$id', '');
  Meta.Schema := JObject.GetValue<string>('$schema', '');
end;

function TJSONSchema.CreatePropFromRef(const Ref: string; const JRoot: TJSONObject): TDClassPropertyType;
begin
  var Path := '';
  { TODO -oHemulGM -c : Сделать загрузку $ref по ссылке на внешний источник 21.11.2023 3:56:15 }
  //if Ref.StartsWith('#') then
  Path := Ref.Trim(['#', '/']).Replace('/', '.');
  var JProp := JRoot.GetValue<TJSONObject>(Path, nil);
  var Name := '';
  var PathItems := Path.Split(['.']);
  if Length(PathItems) > 0 then
    Name := PathItems[High(PathItems)];
  Result := CreateProp(Name, JProp, JRoot);
end;

function TJSONSchema.CreateProp(const Name: string; const JProp, JRoot: TJSONObject): TDClassPropertyType;
begin
  Result := nil;
  if not Assigned(JProp) then
    Exit;
  var Ref := JProp.GetValue<string>('$ref', '').ToLower;
  if not Ref.IsEmpty then
    Exit(CreatePropFromRef(Ref, JRoot));
  var JType := JProp.GetValue<string>('type', '').ToLower;
  if JType.IsEmpty then
    Exit;
  if JType = 'string' then
    Result := CreatePropString(JProp, JRoot)
  else if JType = 'integer' then
    Result := CreatePropInteger(JProp, JRoot)
  else if JType = 'number' then
    Result := CreatePropNumber(JProp, JRoot)
  else if JType = 'boolean' then
    Result := CreatePropBoolean(JProp, JRoot)
  else if JType = 'object' then
    Result := CreatePropObject(Name, JProp, JRoot)
  else if JType = 'array' then
    Result := CreatePropArray(Name, JProp, JRoot);
  ProcessMeta(Result.Meta, JProp, JRoot);
end;

function TJSONSchema.GenerateUnit: string;
begin

end;

function TJSONSchema.GetDelphiType(const FormatValue: string): TDClassPropertyTypeClass;
begin
  if FormatValue.IsEmpty then
    Exit(nil);
  if not FFormatTypes.TryGetValue(FormatValue, Result) then
    Result := nil;
end;

function TJSONSchema.CreatePropString(const JProp, JRoot: TJSONObject): TDClassPropertyType;
begin
  var JFormat := JProp.GetValue<string>('format', '').ToLower;
  var DType := GetDelphiType(JFormat);
  if DType = nil then
    Result := TDTypeString.Create
  else
    Result := DType.Create;
  Result.JsonType := TJSONType.jString;
end;

function TJSONSchema.CreatePropInteger(const JProp, JRoot: TJSONObject): TDClassPropertyType;
begin
  var JFormat := JProp.GetValue<string>('format', '').ToLower;
  var DType := GetDelphiType(JFormat);
  if DType = nil then
    Result := TDTypeInteger.Create
  else
    Result := DType.Create;
  Result.JsonType := TJSONType.jInteger;
end;

function TJSONSchema.CreatePropNumber(const JProp, JRoot: TJSONObject): TDClassPropertyType;
begin
  var JFormat := JProp.GetValue<string>('format', '').ToLower;
  var DType := GetDelphiType(JFormat);
  if DType = nil then
    Result := TDTypeExtended.Create
  else
    Result := DType.Create;
  Result.JsonType := TJSONType.jNumber;
end;

function TJSONSchema.CreatePropBoolean(const JProp, JRoot: TJSONObject): TDClassPropertyType;
begin
  var JFormat := JProp.GetValue<string>('format', '').ToLower;
  var DType := GetDelphiType(JFormat);
  if DType = nil then
    Result := TDTypeBoolean.Create
  else
    Result := DType.Create;
  Result.JsonType := TJSONType.jBoolean;
end;

function TJSONSchema.CreatePropObject(const Name: string; const JProp, JRoot: TJSONObject): TDClassPropertyType;
begin
  Result := TDTypeObject.Create;
  Result.JsonType := TJSONType.jObject;
  TDTypeObject(Result).TypeClass := TDClass.Create(Name);
  ProcessSchema(TDTypeObject(Result).TypeClass, JProp, JRoot);
end;

function TJSONSchema.CreatePropArray(const Name: string; const JProp, JRoot: TJSONObject): TDClassPropertyType;
begin
  Result := TDTypeArray.Create;
  Result.JsonType := TJSONType.jArray;
  var Items: TJSONObject := nil;
  if JProp.TryGetValue<TJSONObject>('items', Items) then
  begin
    TDTypeArray(Result).TypeItem := CreateProp(Name, Items, JRoot);
  end;
end;

{ TDClass }

constructor TDClass.Create(const Name: string);
begin
  inherited Create;
  FMeta := TJSONSchemaMeta.Create;
  FName := Name;
  FProperties := TDClassPropertyies.Create;
end;

destructor TDClass.Destroy;
begin
  FProperties.Free;
  FMeta.Free;
  inherited;
end;

procedure TDClass.SetName(const Value: string);
begin
  FName := Value;
end;

{ TDClassProperty }

destructor TDClassProperty.Destroy;
begin
  FPropertyType.Free;
  inherited;
end;

procedure TDClassProperty.SetName(const Value: string);
begin
  FName := Value;
end;

procedure TDClassProperty.SetPropertyType(const Value: TDClassPropertyType);
begin
  FPropertyType := Value;
end;

{ TDClassPropertyType }

constructor TDClassPropertyType.Create;
begin
  inherited;
  FMeta := TJSONSchemaMeta.Create;
end;

destructor TDClassPropertyType.Destroy;
begin
  FMeta.Free;
  inherited;
end;

procedure TDClassPropertyType.SetJsonType(const Value: TJSONType);
begin
  FJsonType := Value;
end;

{ TDTypeObject }

function TDTypeObject.GetName: string;
begin
  Result := TypeClass.Name;
end;

procedure TDTypeObject.SetTypeClass(const Value: TDClass);
begin
  FTypeClass := Value;
end;

{ TDTypeArray }

destructor TDTypeArray.Destroy;
begin
  FItemType.Free;
  inherited;
end;

function TDTypeArray.GetName: string;
begin
  if Assigned(TypeItem) then
    Result := 'TArray<' + TypeItem.Name + '>'
  else
    Result := 'TArray<Unknown>';
end;

procedure TDTypeArray.SetItemType(const Value: TDClassPropertyType);
begin
  FItemType := Value;
end;

{ TDTypeString }

function TDTypeString.GetName: string;
begin
  Result := 'string';
end;

{ TDTypeDateTime }

function TDTypeDateTime.GetName: string;
begin
  Result := 'TDateTime';
end;

{ TDTypeTime }

function TDTypeTime.GetName: string;
begin
  Result := 'TTime';
end;

{ TDTypeDate }

function TDTypeDate.GetName: string;
begin
  Result := 'TDate';
end;

{ TDTypeBoolean }

function TDTypeBoolean.GetName: string;
begin
  Result := 'Boolean';
end;

{ TDTypeExtended }

function TDTypeExtended.GetName: string;
begin
  Result := 'Extended';
end;

{ TDTypeInt64 }

function TDTypeInt64.GetName: string;
begin
  Result := 'Int64';
end;

{ TDTypeInteger }

function TDTypeInteger.GetName: string;
begin
  Result := 'Integer';
end;

end.

