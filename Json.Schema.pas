unit Json.Schema;

interface

uses
  System.SysUtils, System.Generics.Collections, System.JSON, FMX.TreeView,
  System.Classes;

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
    FMinimum: Extended;
    FHaveMinimum: Boolean;
    FExclusiveMinimum: Boolean;
    FMinItems: Integer;
    FHaveMinItems: Boolean;
    FUniqueItems: Boolean;
    FHaveUniqueItems: Boolean;
    FRequired: TArray<string>;
    FComment: string;
    FEnum: TArray<string>;
    FConstValue: string;
    FHaveMaximum: Boolean;
    FMaximum: Extended;
    FExclusiveMaximum: Boolean;
  public
    property Schema: string read FSchema write FSchema;
    property Id: string read FId write FId;
    property Title: string read FTitle write FTitle;
    property Description: string read FDescription write FDescription;
    property Comment: string read FComment write FComment;
    property Minimum: Extended read FMinimum write FMinimum;
    property ExclusiveMinimum: Boolean read FExclusiveMinimum write FExclusiveMinimum;
    property HaveMinimum: Boolean read FHaveMinimum write FHaveMinimum;
    property Maximum: Extended read FMaximum write FMaximum;
    property ExclusiveMaximum: Boolean read FExclusiveMaximum write FExclusiveMaximum;
    property HaveMaximum: Boolean read FHaveMaximum write FHaveMaximum;
    property HaveMinItems: Boolean read FHaveMinItems write FHaveMinItems;
    property MinItems: Integer read FMinItems write FMinItems;
    property UniqueItems: Boolean read FUniqueItems write FUniqueItems;
    property HaveUniqueItems: Boolean read FHaveUniqueItems write FHaveUniqueItems;
    property Required: TArray<string> read FRequired write FRequired;
    property Enum: TArray<string> read FEnum write FEnum;
    property ConstValue: string read FConstValue write FConstValue;
    function Render(const Prefix: string): TArray<string>;
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
    FLineNumber: Integer;
    procedure SetName(const Value: string);
    procedure SetPropertyType(const Value: TDClassPropertyType);
    function GetDelphiPropertyName: string;
    function GetDelphiFieldName: string;
    procedure SetLineNumber(const Value: Integer);
  public
    property Name: string read FName write SetName;
    property PropertyType: TDClassPropertyType read FPropertyType write SetPropertyType;
    property DelphiPropertyName: string read GetDelphiPropertyName;
    property DelphiFieldName: string read GetDelphiFieldName;
    property LineNumber: Integer read FLineNumber write SetLineNumber;
    destructor Destroy; override;
  end;

  TJSONSchema = class;

  TDClass = class
  private
    FName: string;
    FProperties: TDClassPropertyies;
    FMeta: TJSONSchemaMeta;
    [Weak]
    FMapper: TJSONSchema;
    FLineNumber: Integer;
    FComplexItemCount: Integer;
    FArrayItemCount: Integer;
    FRef: string;
    procedure FOnPropertiesNotify(Sender: TObject; const Item: TDClassProperty; Action: TCollectionNotification);
    procedure SetName(const Value: string);
    function GetForwardDeclaration: string;
    function GetDelphiClassName: string;
    function GetDeclarationPart(const Offset: Integer): TArray<string>;
    procedure SetLineNumber(const Value: Integer);
    function GetImplementationPart: TArray<string>;
    procedure SetRef(const Value: string);
  public
    property Name: string read FName write SetName;
    property DelphiClassName: string read GetDelphiClassName;
    property Properties: TDClassPropertyies read FProperties;
    property Meta: TJSONSchemaMeta read FMeta;
    property LineNumber: Integer read FLineNumber write SetLineNumber;
    property Ref: string read FRef write SetRef;
    constructor Create(const Name, Ref: string; Mapper: TJSONSchema); reintroduce;
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
    FBaseClassUnit: string;
    FBaseClassName: string;
    FSkipErrors: Boolean;
    function GetDelphiType(const FormatValue: string): TDClassPropertyTypeClass;
    procedure FillFormatTypes;
    procedure ProcessSchema(const DClass: TDClass; const JObject, JRoot: TJSONObject);
    procedure ProcessProperties(const DClass: TDClass; const JObject, JRoot: TJSONObject);
    function CreateProp(const Name: string; const JProp, JRoot: TJSONObject; const ByRef: string): TDClassPropertyType;
    function CreatePropString(const JProp, JRoot: TJSONObject): TDClassPropertyType;
    function CreatePropInteger(const JProp, JRoot: TJSONObject): TDClassPropertyType;
    function CreatePropNumber(const JProp, JRoot: TJSONObject): TDClassPropertyType;
    function CreatePropBoolean(const JProp, JRoot: TJSONObject): TDClassPropertyType;
    function CreatePropObject(const Name: string; const JProp, JRoot: TJSONObject; const Ref: string): TDClassPropertyType;
    function CreatePropArray(const Name: string; const JProp, JRoot: TJSONObject): TDClassPropertyType;
    function CreatePropFromRef(const Ref: string; const JRefProp, JRoot: TJSONObject): TDClassPropertyType;
    procedure SetBaseClassName(const Value: string);
    procedure SetBaseClassUnit(const Value: string);
    procedure SetForwardDeclarate(const Value: Boolean);
    procedure SetUnitName(const Value: string);
    procedure InternalVisualize(ATreeViewItem: TTreeViewItem; AClass: TDClass; AItemStyleLookup: string);
    procedure ProcessMeta(const Meta: TJSONSchemaMeta; const JObject, JRoot: TJSONObject);
    procedure SetSkipErrors(const Value: Boolean);
    function FindClassByRef(const Ref: string; out DClass: TDClass): Boolean;
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
    property ForwardDeclarate: Boolean read FForwardDeclarate write SetForwardDeclarate;
    property SkipErrors: Boolean read FSkipErrors write SetSkipErrors;
  end;

const
  AllowedChars =['0'..'9', 'a'..'z', 'A'..'Z'];

var
  ReservedWords: TList<string>;

implementation

uses
  FMX.Ani, System.StrUtils, JTD.Utils;

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
    FRootClass := TDClass.Create(RootClassName, '#', Self);
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

procedure TJSONSchema.SetSkipErrors(const Value: Boolean);
begin
  FSkipErrors := Value;
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
      LItem.Text := FRootClass.DelphiClassName;
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
      LItem.Text := LField.GetDelphiPropertyName;
      LItem.StylesData['details'] := ': ' + LField.PropertyType.Name;
      InternalVisualize(LItem, TDTypeObject(LField.PropertyType).TypeClass, AItemStyleLookup);
    end
    else if LField.PropertyType is TDTypeArray then
    begin
      LItem.Text := LField.GetDelphiPropertyName;
      LItem.StylesData['details'] := ': ' + LField.PropertyType.Name;
      if TDTypeArray(LField.PropertyType).TypeItem is TDTypeObject then
        InternalVisualize(LItem, TDTypeObject(TDTypeArray(LField.PropertyType).TypeItem).TypeClass, AItemStyleLookup);
    end
    else
    begin
      LItem.Text := LField.GetDelphiPropertyName;
      LItem.StylesData['details'] := ': ' + LField.PropertyType.Name;
    end;

    ATreeViewItem.AddObject(LItem);
  end;
end;

procedure TJSONSchema.VisualizeClasses(ATreeView: TTreeView; AItemStyleLookup: string);
begin
  ATreeView.Opacity := 0;
  ATreeView.BeginUpdate;
  try
    ATreeView.Clear;
    for var i := FClasses.Count - 1 downto 0 do
    begin
      var LClass := FClasses[i];
      var LItem := TTreeViewItem.Create(ATreeView);
      LItem.StyleLookup := AItemStyleLookup;
      LItem.TagObject := LClass;
      LItem.WordWrap := False;
      LItem.Text := LClass.DelphiClassName;
      LItem.StylesData['details'] := '';
      ATreeView.AddObject(LItem);
      InternalVisualize(LItem, LClass, AItemStyleLookup);
    end;
  finally
    TAnimator.AnimateFloatDelay(ATreeView, 'Opacity', 1, 0.2, 0.1);
    ATreeView.EndUpdate;
  end;
end;

procedure TJSONSchema.ProcessProperties(const DClass: TDClass; const JObject, JRoot: TJSONObject);
begin
  var JProps := JObject.GetValue<TJSONObject>('properties', nil);
  if not Assigned(JProps) then
    Exit;
  for var JProp in JProps do
    if JProp.JsonValue is TJSONObject then
    begin
      var PropType := CreateProp(JProp.JsonString.Value, TJSONObject(JProp.JsonValue), JRoot, '');
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
  Meta.Comment := JObject.GetValue<string>('comment', '');
  Meta.ConstValue := JObject.GetValue<string>('const', '');

  var Ext: Extended;
  var Int: Integer;
  var Bool: Boolean;
  var JArr: TJSONArray;

  {
  "type": "string",
  "minLength": 3,
  "maxLength": 10,
  "pattern": "^test\\/[a-z-]+$",
  "contentEncoding": "base64",
  "contentMediaType": "application/json"
  }

  {
  "type": "number",
  "minimum": 1.5,
  "exclusiveMinimum": true,
  "maximum": 12.3,
  "exclusiveMaximum": true,
  "multipleOf": 0.5
  }

  {
  "type": "object"
  properties
  required
  dependencies
  minProperties
  maxProperties
  propertyNames
  patternProperties
  additionalProperties
  }

  {
  "type": "array"
  minItems
  maxItems
  uniqueItems
  contains
  items
  additionalItems
  }

  if JObject.TryGetValue<Extended>('minimum', Ext) then
  begin
    Meta.HaveMinimum := True;
    Meta.Minimum := Ext;
    Meta.ExclusiveMinimum := JObject.GetValue<Boolean>('exclusiveMinimum', False);
  end;

  if JObject.TryGetValue<Extended>('maximum', Ext) then
  begin
    Meta.HaveMaximum := True;
    Meta.Maximum := Ext;
    Meta.ExclusiveMaximum := JObject.GetValue<Boolean>('exclusiveMaximum', False);
  end;

  if JObject.TryGetValue<Integer>('minItems', Int) then
  begin
    Meta.HaveMinItems := True;
    Meta.MinItems := Int;
  end;

  if JObject.TryGetValue<Boolean>('uniqueItems', Bool) then
  begin
    Meta.HaveUniqueItems := True;
    Meta.UniqueItems := Bool;
  end;

  if JObject.TryGetValue<TJSONArray>('required', JArr) then
    for var JItem in JArr do
      if JItem is TJSONString then
        Meta.Required := Meta.Required + [TJSONString(JItem).Value];

  if JObject.TryGetValue<TJSONArray>('enum', JArr) then
    for var JItem in JArr do
      if JItem is TJSONString then
        Meta.Enum := Meta.Enum + [TJSONString(JItem).Value];
end;

function TJSONSchema.CreatePropFromRef(const Ref: string; const JRefProp, JRoot: TJSONObject): TDClassPropertyType;
begin
  Result := nil;

  if not Ref.StartsWith('#') then
  try
    var ExternalJS := DownloadText(Ref);
    var JValue := TJSONObject.ParseJSONValue(ExternalJS);
    if JValue is TJSONObject then
    try
      var JProp := TJSONObject(JValue);
      var Name := '';
      var PathItems := Ref.Split(['/']);
      if Length(PathItems) > 0 then
        Name := PathItems[High(PathItems)];
      Result := CreateProp(Name, JProp, JRoot, Ref);
    finally
      JValue.Free;
    end;
  except
    if SkipErrors then
      Exit
    else
      raise;
  end
  else
  begin
    var Path := Ref.Trim(['#', '/']).Replace('/', '.');
    var JProp := JRoot.GetValue<TJSONObject>(Path, nil);
    var Name := '';
    var PathItems := Path.Split(['.']);
    if Length(PathItems) > 0 then
      Name := PathItems[High(PathItems)];
    Result := CreateProp(Name, JProp, JRoot, Ref);
  end;

  // override meta
  if Assigned(Result) then
    ProcessMeta(Result.Meta, JRefProp, JRoot);
end;

function TJSONSchema.CreateProp(const Name: string; const JProp, JRoot: TJSONObject; const ByRef: string): TDClassPropertyType;
begin
  Result := nil;
  if not Assigned(JProp) then
    Exit;

  // $ref
  var Ref := JProp.GetValue<string>('$ref', '');
  if not Ref.IsEmpty then
    Exit(CreatePropFromRef(Ref, JProp, JRoot));

  // type
  var JType := '';
  try
    JType := JProp.GetValue<string>('type', '').ToLower;
  except
    JType := '';
  end;

  // type array
  if JType.IsEmpty then
  try
    var JTypeArr := JProp.GetValue<TJSONArray>('type', nil);
    if Assigned(JTypeArr) then
    begin
      if JTypeArr.Count > 0 then
        if JTypeArr[0] is TJSONString then
          JType := TJSONString(JTypeArr[0]).Value.ToLower;
    end;
  except
    JType := '';
  end;

  // anyOf
  if JType.IsEmpty then
  try
    var JTypeArr := JProp.GetValue<TJSONArray>('anyOf', nil);
    if Assigned(JTypeArr) then
    begin
      if JTypeArr.Count > 0 then
        if JTypeArr[0] is TJSONObject then
        begin
          // inner type proc
          Exit(CreateProp(Name, TJSONObject(JTypeArr[0]), JRoot, ''));
        end;
    end;
  except
    JType := '';
  end;

  // no type
  if JType.IsEmpty then
    Exit;

  // create typed prop
  if JType = 'string' then
    Result := CreatePropString(JProp, JRoot)
  else if JType = 'integer' then
    Result := CreatePropInteger(JProp, JRoot)
  else if JType = 'number' then
    Result := CreatePropNumber(JProp, JRoot)
  else if JType = 'boolean' then
    Result := CreatePropBoolean(JProp, JRoot)
  else if JType = 'object' then
    Result := CreatePropObject(Name, JProp, JRoot, ByRef)
  else if JType = 'array' then
    Result := CreatePropArray(Name, JProp, JRoot);

  // type/prop meta
  if Assigned(Result) then
    ProcessMeta(Result.Meta, JProp, JRoot);
end;

function TJSONSchema.GenerateUnit: string;
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
        LList.Add(LClass.GetForwardDeclaration);
        LList.Add('');
      end;

    for var i := FClasses.Count - 1 downto 0 do
    begin
      var LClass := FClasses[i];
      LList.AddStrings(LClass.Meta.Render('  '));
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
      LList.AddStrings(LClass.GetImplementationPart);
    end;

    LList.Add('');
    LList.Add('end.');

    Result := LList.Text;
  finally
    LList.Free;
  end;
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

function TJSONSchema.FindClassByRef(const Ref: string; out DClass: TDClass): Boolean;
begin
  for var Item in Classes do
    if Item.Ref = Ref then
    begin
      DClass := Item;
      Exit(True);
    end;
  Result := False;
end;

function TJSONSchema.CreatePropObject(const Name: string; const JProp, JRoot: TJSONObject; const Ref: string): TDClassPropertyType;
begin
  Result := TDTypeObject.Create;
  try
    Result.JsonType := TJSONType.jObject;
    var DClass: TDClass := nil;
    if FindClassByRef(Ref, DClass) then
      TDTypeObject(Result).TypeClass := DClass
    else
    begin
      TDTypeObject(Result).TypeClass := TDClass.Create(Name, Ref, Self);
      ProcessSchema(TDTypeObject(Result).TypeClass, JProp, JRoot);
    end;
  except
    Result.Free;
    raise;
  end;
end;

function TJSONSchema.CreatePropArray(const Name: string; const JProp, JRoot: TJSONObject): TDClassPropertyType;
begin
  Result := TDTypeArray.Create;
  try
    Result.JsonType := TJSONType.jArray;
    var Items: TJSONObject := nil;
    if JProp.TryGetValue<TJSONObject>('items', Items) then
      TDTypeArray(Result).TypeItem := CreateProp(Name, Items, JRoot, '');
  except
    Result.Free;
    raise;
  end;
end;

{ TDClass }

constructor TDClass.Create(const Name, Ref: string; Mapper: TJSONSchema);
begin
  inherited Create;
  FMeta := TJSONSchemaMeta.Create;
  FMapper := Mapper;
  FName := Name;
  FRef := Ref;
  FProperties := TDClassPropertyies.Create;
  FProperties.OnNotify := FOnPropertiesNotify;
  FComplexItemCount := 0;
  FArrayItemCount := 0;
end;

destructor TDClass.Destroy;
begin
  FProperties.Free;
  FMeta.Free;
  inherited;
end;

procedure TDClass.FOnPropertiesNotify(Sender: TObject; const Item: TDClassProperty; Action: TCollectionNotification);
begin
  if Action = TCollectionNotification.cnAdded then
  begin
    if Item.PropertyType is TDTypeObject then
      Inc(FComplexItemCount);
    if Item.PropertyType is TDTypeArray then
      Inc(FArrayItemCount);
  end;
end;

procedure TDClass.SetLineNumber(const Value: Integer);
begin
  FLineNumber := Value;
end;

procedure TDClass.SetName(const Value: string);
begin
  FName := Value;
end;

procedure TDClass.SetRef(const Value: string);
begin
  FRef := Value;
end;

function TDClass.GetDelphiClassName: string;
begin
  Result := '';
  for var i := 1 to Name.Length do
    if CharInSet(Name[i], AllowedChars) then
    begin
      if (Result.IsEmpty) or ((i > 1) and (not CharInSet(Name[i - 1], AllowedChars))) then
        Result := Result + UpperCase(Name[i])
      else
        Result := Result + Name[i];
    end;
  Result := 'T' + Result;
end;

function TDClass.GetForwardDeclaration: string;
begin
  Result := '  ' + GetDelphiClassName + ' = class;';
end;

function TDClass.GetDeclarationPart(const Offset: Integer): TArray<string>;
begin
  var LLines := TStringList.Create;
  try
    LLines.Add('  ' + DelphiClassName + ' = class' + IfThen(FMapper.BaseClassName.IsEmpty, '', '(' + FMapper.BaseClassName + ')'));
    if Properties.Count > 0 then
    begin
      LLines.Add('  private');

      for var LItem in Properties do
      begin
        LLines.Add(Format('    [JsonNameAttribute(''%s'')]', [LItem.Name]));
        LLines.Add(Format('    %s: %s;', [LItem.DelphiFieldName, LItem.PropertyType.Name]));
      end;

      LLines.Add('  public');

      for var LItem in Properties do
      begin
        LLines.AddStrings(LItem.PropertyType.Meta.Render('    '));
        var L := LLines.Add(Format('    property %s: %s read %s write %s;',
          [LItem.DelphiPropertyName, LItem.PropertyType.Name, LItem.DelphiFieldName, LItem.DelphiFieldName]));
        LItem.LineNumber := Offset + L;
      end;

      if (FComplexItemCount > 0) or (FArrayItemCount > 0) then
        LLines.Add('    destructor Destroy; override;');
    end;
    LLines.Add('  end;');

    Result := LLines.ToStringArray;
  finally
    LLines.Free;
  end;
end;

function TDClass.GetImplementationPart: TArray<string>;
begin
  if (FComplexItemCount <= 0) and (FArrayItemCount <= 0) then
    Exit;

  var LLines := TStringList.Create;
  try
    var LClassName := Format('%s', [FName]);
    LLines.Add('');
    LLines.Add(Format('{ %s }', [LClassName]));
    LLines.Add('');

    LLines.Add(Format('destructor %s.Destroy;', [LClassName]));
    LLines.Add('begin');

    for var Item in Properties do
      if Item.PropertyType is TDTypeArray then
      begin
        LLines.Add(Format(' for var Item in %s do', [Item.DelphiFieldName]));
        LLines.Add('   Item.Free;');
      end;

    for var Item in Properties do
      if Item.PropertyType is TDTypeObject then
        LLines.Add(Format('  %s.Free;', [Item.DelphiFieldName]));

    LLines.Add('  inherited;');
    LLines.Add('end;');

    Result := LLines.ToStringArray;
  finally
    LLines.Free;
  end;
end;

{ TDClassProperty }

destructor TDClassProperty.Destroy;
begin
  FPropertyType.Free;
  inherited;
end;

function TDClassProperty.GetDelphiFieldName: string;
begin
  var LName := '';

  for var i := 1 to Name.Length do
    if CharInSet(Name[i], AllowedChars) then
    begin
      if (LName.IsEmpty) or ((i > 1) and (not CharInSet(Name[i - 1], AllowedChars))) then
        LName := LName + UpperCase(Name[i])
      else
        LName := LName + Name[i];
    end;

  Result := 'F' + string(Copy(LName, 1, 1)).ToUpper + Copy(LName, 2);
end;

function TDClassProperty.GetDelphiPropertyName: string;
begin
  var LName := '';

  for var i := 1 to Name.Length do
    if CharInSet(Name[i], AllowedChars) then
    begin
      if (LName.IsEmpty) or ((i > 1) and (not CharInSet(Name[i - 1], AllowedChars))) then
        LName := LName + UpperCase(Name[i])
      else
        LName := LName + Name[i];
    end;

  var FFieldName := 'F' + string(Copy(LName, 1, 1)).ToUpper + Copy(LName, 2);

  var FExt: Extended;
  if TryStrToFloat(LName, FExt) then
    LName := 'Field' + LName;

  if ReservedWords.Contains(LName.ToLower) then
    Result := '&' + LName
  else
    Result := LName;
end;

procedure TDClassProperty.SetLineNumber(const Value: Integer);
begin
  FLineNumber := Value;
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
  if Assigned(TypeClass) then
    Result := TypeClass.DelphiClassName
  else
    Result := 'Unknown';
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

{ TJSONSchemaMeta }

function TJSONSchemaMeta.Render(const Prefix: string): TArray<string>;
begin
  var LLines := TStringList.Create;
  try
    if not Title.IsEmpty then
      LLines.Add(Format(Prefix + '/// %s', [Title]));
    if not Description.IsEmpty then
      LLines.Add(Format(Prefix + '/// %s', [Description]));
    if not Comment.IsEmpty then
      LLines.Add(Format(Prefix + '/// Comment: %s', [Comment]));
    if Length(Required) > 0 then
      LLines.Add(Format(Prefix + '/// Required: %s', [string.Join(', ', Required)]));
    if Length(Enum) > 0 then
      LLines.Add(Format(Prefix + '/// Enum: %s', [string.Join(', ', Enum)]));
    if not ConstValue.IsEmpty then
      LLines.Add(Format(Prefix + '/// Const: %s', [ConstValue]));

    if Self.HaveMinItems then
      LLines.Add(Format(Prefix + '/// MinItems: %s', [MinItems.ToString]));

    if Self.HaveMinimum then
    begin
      LLines.Add(Format(Prefix + '/// Minimum: %s', [Minimum.ToString]));
      if Self.ExclusiveMinimum then
        LLines.Add(Format(Prefix + '/// Exclusive Minimum: %s', [Minimum.ToString]));
    end;

    if Self.HaveMaximum then
    begin
      LLines.Add(Format(Prefix + '/// Maximum: %s', [Maximum.ToString]));
      if Self.ExclusiveMaximum then
        LLines.Add(Format(Prefix + '/// Exclusive Maximum: %s', [Maximum.ToString]));
    end;

    if Self.HaveUniqueItems then
      LLines.Add(Format(Prefix + '/// UniqueItems: %s', [UniqueItems.ToString]));

    if not Id.IsEmpty then
      LLines.Add(Format(Prefix + '/// Id: %s', [Id]));
    if not Schema.IsEmpty then
      LLines.Add(Format(Prefix + '/// Schema: %s', [Schema]));

    if LLines.Count > 0 then
    begin
      LLines.Insert(0, Prefix + '/// <summary>');
      LLines.Add(Prefix + '/// </summary>');
    end;
    Result := LLines.ToStringArray;
  finally
    LLines.Free;
  end;
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

