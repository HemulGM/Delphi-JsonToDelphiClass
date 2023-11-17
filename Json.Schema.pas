unit Json.Schema;

interface

uses
  System.SysUtils, System.Generics.Collections, System.JSON;

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
    procedure SetJsonType(const Value: TJSONType);
  public
    property JsonType: TJSONType read FJsonType write SetJsonType;
  end;

  TDClassPropertyTypeClass = class of TDClassPropertyType;

  TDTypeString = class(TDClassPropertyType);

  TDTypeInteger = class(TDClassPropertyType);

  TDTypeInt64 = class(TDClassPropertyType);

  TDTypeExtended = class(TDClassPropertyType);

  TDTypeBoolean = class(TDClassPropertyType);

  TDTypeDate = class(TDClassPropertyType);

  TDTypeTime = class(TDClassPropertyType);

  TDTypeDateTime = class(TDClassPropertyType);

  TDTypeObject = class(TDClassPropertyType)
  private
    FTypeClass: TDClass;
    procedure SetTypeClass(const Value: TDClass);
  public
    property TypeClass: TDClass read FTypeClass write SetTypeClass;
  end;

  TDTypeArray = class(TDClassPropertyType)
  private
    FItemType: TDClassPropertyType;
    procedure SetItemType(const Value: TDClassPropertyType);
  public
    property TypeItem: TDClassPropertyType read FItemType write SetItemType;
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
  end;

  TDClass = class
  private
    FName: string;
    FProperties: TDClassPropertyies;
    procedure SetName(const Value: string);
  public
    property Name: string read FName write SetName;
    property Properties: TDClassPropertyies read FProperties;
    constructor Create(const Name: string); reintroduce;
    destructor Destroy; override;
  end;

  TFormatTypes = TDictionary<string, TDClassPropertyTypeClass>;

  TJSONSchema = class
  private
    FRootClass: TDClass;
    FClasses: TDClasses;
    FFormatTypes: TFormatTypes;
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
  public
    procedure Parse(const JsonString: string; const RootClassName: string = '');
    procedure Clear;
    property Classes: TDClasses read FClasses;
    property FormatTypes: TFormatTypes read FFormatTypes;
    constructor Create;
    destructor Destroy; override;
  end;

implementation

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
  //ProcessMeta(DClass, JObject, JRoot);
  ProcessProperties(DClass, JObject, JRoot);
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

function TJSONSchema.CreatePropFromRef(const Ref: string; const JRoot: TJSONObject): TDClassPropertyType;
begin
  { TODO -oHemulGM -c : Сделать чтение ссылки на тип 17.11.2023 16:33:15 }
  Result := nil;
end;

function TJSONSchema.CreateProp(const Name: string; const JProp, JRoot: TJSONObject): TDClassPropertyType;
begin
  Result := nil;
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
  TDTypeArray(Result).TypeItem := CreateProp(Name, JProp, JRoot);
end;

{ TDClass }

constructor TDClass.Create(const Name: string);
begin
  inherited Create;
  FName := Name;
  FProperties := TDClassPropertyies.Create;
end;

destructor TDClass.Destroy;
begin
  FProperties.Free;
  inherited;
end;

procedure TDClass.SetName(const Value: string);
begin
  FName := Value;
end;

{ TDClassProperty }

procedure TDClassProperty.SetName(const Value: string);
begin
  FName := Value;
end;

procedure TDClassProperty.SetPropertyType(const Value: TDClassPropertyType);
begin
  FPropertyType := Value;
end;

{ TDClassPropertyType }

procedure TDClassPropertyType.SetJsonType(const Value: TJSONType);
begin
  FJsonType := Value;
end;

{ TDTypeObject }

procedure TDTypeObject.SetTypeClass(const Value: TDClass);
begin
  FTypeClass := Value;
end;

{ TDTypeArray }

procedure TDTypeArray.SetItemType(const Value: TDClassPropertyType);
begin
  FItemType := Value;
end;

end.

