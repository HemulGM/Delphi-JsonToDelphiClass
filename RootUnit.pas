unit RootUnit;

interface

uses
  Rest.Json, Rest.Json.Types;

type
  TExternalDocs = class
  private
    [JsonNameAttribute('description')]
    FDescription: String;
    [JsonNameAttribute('url')]
    FUrl: String;
  public
    property Description: String read FDescription write FDescription;
    property Url: String read FUrl write FUrl;
  end;

  TXml_008 = class
  private
    [JsonNameAttribute('name')]
    FName: String;
  public
    property Name: String read FName write FName;
  end;

  TUserStatus = class
  private
    [JsonNameAttribute('description')]
    FDescription: String;
    [JsonNameAttribute('format')]
    FFormat: String;
    [JsonNameAttribute('type')]
    FType: String;
  public
    property Description: String read FDescription write FDescription;
    property Format: String read FFormat write FFormat;
    property &Type: String read FType write FType;
  end;

  TPhone = class
  private
    [JsonNameAttribute('type')]
    FType: String;
  public
    property &Type: String read FType write FType;
  end;

  TId = class
  private
    [JsonNameAttribute('format')]
    FFormat: String;
    [JsonNameAttribute('type')]
    FType: String;
  public
    property Format: String read FFormat write FFormat;
    property &Type: String read FType write FType;
  end;

  TProperties_005 = class
  private
    [JsonNameAttribute('email')]
    FEmail: TPhone;
    [JsonNameAttribute('firstName')]
    FFirstName: TPhone;
    [JsonNameAttribute('id')]
    FId: TId;
    [JsonNameAttribute('lastName')]
    FLastName: TPhone;
    [JsonNameAttribute('password')]
    FPassword: TPhone;
    [JsonNameAttribute('phone')]
    FPhone: TPhone;
    [JsonNameAttribute('userStatus')]
    FUserStatus: TUserStatus;
    [JsonNameAttribute('username')]
    FUsername: TPhone;
  public
    property Email: TPhone read FEmail write FEmail;
    property FirstName: TPhone read FFirstName write FFirstName;
    property Id: TId read FId write FId;
    property LastName: TPhone read FLastName write FLastName;
    property Password: TPhone read FPassword write FPassword;
    property Phone: TPhone read FPhone write FPhone;
    property UserStatus: TUserStatus read FUserStatus write FUserStatus;
    property Username: TPhone read FUsername write FUsername;
    destructor Destroy; override;
  end;

  TUser_001 = class
  private
    [JsonNameAttribute('properties')]
    FProperties: TProperties_005;
    [JsonNameAttribute('type')]
    FType: String;
    [JsonNameAttribute('xml')]
    FXml: TXml_008;
  public
    property Properties: TProperties_005 read FProperties write FProperties;
    property &Type: String read FType write FType;
    property Xml: TXml_008 read FXml write FXml;
    destructor Destroy; override;
  end;

  TStatus = class
  private
    [JsonNameAttribute('description')]
    FDescription: String;
    [JsonNameAttribute('enum')]
    FEnum: TArray<String>;
    [JsonNameAttribute('type')]
    FType: String;
  public
    property Description: String read FDescription write FDescription;
    property Enum: TArray<String> read FEnum write FEnum;
    property &Type: String read FType write FType;
  end;

  TProperties_004 = class
  private
    [JsonNameAttribute('complete')]
    FComplete: TPhone;
    [JsonNameAttribute('id')]
    FId: TId;
    [JsonNameAttribute('petId')]
    FPetId: TId;
    [JsonNameAttribute('quantity')]
    FQuantity: TId;
    [JsonNameAttribute('shipDate')]
    FShipDate: TId;
    [JsonNameAttribute('status')]
    FStatus: TStatus;
  public
    property Complete: TPhone read FComplete write FComplete;
    property Id: TId read FId write FId;
    property PetId: TId read FPetId write FPetId;
    property Quantity: TId read FQuantity write FQuantity;
    property ShipDate: TId read FShipDate write FShipDate;
    property Status: TStatus read FStatus write FStatus;
    destructor Destroy; override;
  end;

  TProperties_003 = class
  private
    [JsonNameAttribute('id')]
    FId: TId;
    [JsonNameAttribute('name')]
    FName: TPhone;
  public
    property Id: TId read FId write FId;
    property Name: TPhone read FName write FName;
    destructor Destroy; override;
  end;

  TItems_007 = class
  private
    [JsonNameAttribute('$ref')]
    FRef: String;
    [JsonNameAttribute('xml')]
    FXml: TXml_008;
  public
    property Ref: String read FRef write FRef;
    property Xml: TXml_008 read FXml write FXml;
    destructor Destroy; override;
  end;

  TXml_003 = class
  private
    [JsonNameAttribute('wrapped')]
    FWrapped: Boolean;
  public
    property Wrapped: Boolean read FWrapped write FWrapped;
  end;

  TTags_001 = class
  private
    [JsonNameAttribute('items')]
    FItems: TItems_007;
    [JsonNameAttribute('type')]
    FType: String;
    [JsonNameAttribute('xml')]
    FXml: TXml_003;
  public
    property Items: TItems_007 read FItems write FItems;
    property &Type: String read FType write FType;
    property Xml: TXml_003 read FXml write FXml;
    destructor Destroy; override;
  end;

  TItems_006 = class
  private
    [JsonNameAttribute('type')]
    FType: String;
    [JsonNameAttribute('xml')]
    FXml: TXml_008;
  public
    property &Type: String read FType write FType;
    property Xml: TXml_008 read FXml write FXml;
    destructor Destroy; override;
  end;

  TName = class
  private
    [JsonNameAttribute('example')]
    FExample: String;
    [JsonNameAttribute('type')]
    FType: String;
  public
    property Example: String read FExample write FExample;
    property &Type: String read FType write FType;
  end;

  TCategory = class
  private
    [JsonNameAttribute('$ref')]
    FRef: String;
  public
    property Ref: String read FRef write FRef;
  end;

  TProperties_002 = class
  private
    [JsonNameAttribute('category')]
    FCategory: TCategory;
    [JsonNameAttribute('id')]
    FId: TId;
    [JsonNameAttribute('name')]
    FName: TName;
    [JsonNameAttribute('photoUrls')]
    FPhotoUrls: TTags_001;
    [JsonNameAttribute('status')]
    FStatus: TStatus;
    [JsonNameAttribute('tags')]
    FTags: TTags_001;
  public
    property Category: TCategory read FCategory write FCategory;
    property Id: TId read FId write FId;
    property Name: TName read FName write FName;
    property PhotoUrls: TTags_001 read FPhotoUrls write FPhotoUrls;
    property Status: TStatus read FStatus write FStatus;
    property Tags: TTags_001 read FTags write FTags;
    destructor Destroy; override;
  end;

  TPet_001 = class
  private
    [JsonNameAttribute('properties')]
    FProperties: TProperties_002;
    [JsonNameAttribute('required')]
    FRequired: TArray<String>;
    [JsonNameAttribute('type')]
    FType: String;
    [JsonNameAttribute('xml')]
    FXml: TXml_008;
  public
    property Properties: TProperties_002 read FProperties write FProperties;
    property Required: TArray<String> read FRequired write FRequired;
    property &Type: String read FType write FType;
    property Xml: TXml_008 read FXml write FXml;
    destructor Destroy; override;
  end;

  TProperties = class
  private
    [JsonNameAttribute('code')]
    FCode: TId;
    [JsonNameAttribute('message')]
    FMessage: TPhone;
    [JsonNameAttribute('type')]
    FType: TPhone;
  public
    property Code: TId read FCode write FCode;
    property Message: TPhone read FMessage write FMessage;
    property &Type: TPhone read FType write FType;
    destructor Destroy; override;
  end;

  TApiResponse = class
  private
    [JsonNameAttribute('properties')]
    FProperties: TProperties;
    [JsonNameAttribute('type')]
    FType: String;
  public
    property Properties: TProperties read FProperties write FProperties;
    property &Type: String read FType write FType;
    destructor Destroy; override;
  end;

  TDefinitions = class
  private
    [JsonNameAttribute('ApiResponse')]
    FApiResponse: TApiResponse;
    [JsonNameAttribute('Category')]
    FCategory: TUser_001;
    [JsonNameAttribute('Order')]
    FOrder: TUser_001;
    [JsonNameAttribute('Pet')]
    FPet: TPet_001;
    [JsonNameAttribute('Tag')]
    FTag: TUser_001;
    [JsonNameAttribute('User')]
    FUser: TUser_001;
  public
    property ApiResponse: TApiResponse read FApiResponse write FApiResponse;
    property Category: TUser_001 read FCategory write FCategory;
    property Order: TUser_001 read FOrder write FOrder;
    property Pet: TPet_001 read FPet write FPet;
    property Tag: TUser_001 read FTag write FTag;
    property User: TUser_001 read FUser write FUser;
    destructor Destroy; override;
  end;

  TScopes = class
  private
    [JsonNameAttribute('read:pets')]
    FReadPets: String;
    [JsonNameAttribute('write:pets')]
    FWritePets: String;
  public
    property ReadPets: String read FReadPets write FReadPets;
    property WritePets: String read FWritePets write FWritePets;
  end;

  TPetstoreAuth = class
  private
    [JsonNameAttribute('authorizationUrl')]
    FAuthorizationUrl: String;
    [JsonNameAttribute('flow')]
    FFlow: String;
    [JsonNameAttribute('scopes')]
    FScopes: TScopes;
    [JsonNameAttribute('type')]
    FType: String;
  public
    property AuthorizationUrl: String read FAuthorizationUrl write FAuthorizationUrl;
    property Flow: String read FFlow write FFlow;
    property Scopes: TScopes read FScopes write FScopes;
    property &Type: String read FType write FType;
    destructor Destroy; override;
  end;

  TApiKey = class
  private
    [JsonNameAttribute('in')]
    FIn: String;
    [JsonNameAttribute('name')]
    FName: String;
    [JsonNameAttribute('type')]
    FType: String;
  public
    property &In: String read FIn write FIn;
    property Name: String read FName write FName;
    property &Type: String read FType write FType;
  end;

  TSecurityDefinitions = class
  private
    [JsonNameAttribute('api_key')]
    FApiKey: TApiKey;
    [JsonNameAttribute('petstore_auth')]
    FPetstoreAuth: TPetstoreAuth;
  public
    property ApiKey: TApiKey read FApiKey write FApiKey;
    property PetstoreAuth: TPetstoreAuth read FPetstoreAuth write FPetstoreAuth;
    destructor Destroy; override;
  end;

  TDefault = class
  private
    [JsonNameAttribute('description')]
    FDescription: String;
  public
    property Description: String read FDescription write FDescription;
  end;

  TResponses_019 = class
  private
    [JsonNameAttribute('default')]
    FDefault: TDefault;
  public
    property Default: TDefault read FDefault write FDefault;
    destructor Destroy; override;
  end;

  TParameters_017 = class
  private
    [JsonNameAttribute('description')]
    FDescription: String;
    [JsonNameAttribute('in')]
    FIn: String;
    [JsonNameAttribute('name')]
    FName: String;
    [JsonNameAttribute('required')]
    FRequired: Boolean;
    [JsonNameAttribute('schema')]
    FSchema: TCategory;
  public
    property Description: String read FDescription write FDescription;
    property &In: String read FIn write FIn;
    property Name: String read FName write FName;
    property Required: Boolean read FRequired write FRequired;
    property Schema: TCategory read FSchema write FSchema;
    destructor Destroy; override;
  end;

  TPost_006 = class
  private
    [JsonNameAttribute('consumes')]
    FConsumes: TArray<String>;
    [JsonNameAttribute('description')]
    FDescription: String;
    [JsonNameAttribute('operationId')]
    FOperationId: String;
    [JsonNameAttribute('parameters')]
    FParameters: TArray<TParameters_017>;
    [JsonNameAttribute('produces')]
    FProduces: TArray<String>;
    [JsonNameAttribute('responses')]
    FResponses: TResponses_019;
    [JsonNameAttribute('summary')]
    FSummary: String;
    [JsonNameAttribute('tags')]
    FTags: TArray<String>;
  public
    property Consumes: TArray<String> read FConsumes write FConsumes;
    property Description: String read FDescription write FDescription;
    property OperationId: String read FOperationId write FOperationId;
    property Parameters: TArray<TParameters_017> read FParameters write FParameters;
    property Produces: TArray<String> read FProduces write FProduces;
    property Responses: TResponses_019 read FResponses write FResponses;
    property Summary: String read FSummary write FSummary;
    property Tags: TArray<String> read FTags write FTags;
    destructor Destroy; override;
  end;

  TUser = class
  private
    [JsonNameAttribute('post')]
    FPost: TPost_006;
  public
    property Post: TPost_006 read FPost write FPost;
    destructor Destroy; override;
  end;

  TGet_007 = class
  private
    [JsonNameAttribute('description')]
    FDescription: String;
    [JsonNameAttribute('operationId')]
    FOperationId: String;
    [JsonNameAttribute('produces')]
    FProduces: TArray<String>;
    [JsonNameAttribute('responses')]
    FResponses: TResponses_019;
    [JsonNameAttribute('summary')]
    FSummary: String;
    [JsonNameAttribute('tags')]
    FTags: TArray<String>;
  public
    property Description: String read FDescription write FDescription;
    property OperationId: String read FOperationId write FOperationId;
    property Produces: TArray<String> read FProduces write FProduces;
    property Responses: TResponses_019 read FResponses write FResponses;
    property Summary: String read FSummary write FSummary;
    property Tags: TArray<String> read FTags write FTags;
    destructor Destroy; override;
  end;

  TUserLogout = class
  private
    [JsonNameAttribute('get')]
    FGet: TGet_007;
  public
    property Get: TGet_007 read FGet write FGet;
    destructor Destroy; override;
  end;

  THeaders = class
  private
    [JsonNameAttribute('X-Expires-After')]
    FXExpiresAfter: TUserStatus;
    [JsonNameAttribute('X-Rate-Limit')]
    FXRateLimit: TUserStatus;
  public
    property XExpiresAfter: TUserStatus read FXExpiresAfter write FXExpiresAfter;
    property XRateLimit: TUserStatus read FXRateLimit write FXRateLimit;
    destructor Destroy; override;
  end;

  T200_008 = class
  private
    [JsonNameAttribute('description')]
    FDescription: String;
    [JsonNameAttribute('headers')]
    FHeaders: THeaders;
    [JsonNameAttribute('schema')]
    FSchema: TPhone;
  public
    property Description: String read FDescription write FDescription;
    property Headers: THeaders read FHeaders write FHeaders;
    property Schema: TPhone read FSchema write FSchema;
    destructor Destroy; override;
  end;

  TResponses_017 = class
  private
    [JsonNameAttribute('200')]
    F200: T200_008;
    [JsonNameAttribute('400')]
    F400: TDefault;
  public
    property Field200: T200_008 read F200 write F200;
    property Field400: TDefault read F400 write F400;
    destructor Destroy; override;
  end;

  TParameters_016 = class
  private
    [JsonNameAttribute('description')]
    FDescription: String;
    [JsonNameAttribute('in')]
    FIn: String;
    [JsonNameAttribute('name')]
    FName: String;
    [JsonNameAttribute('required')]
    FRequired: Boolean;
    [JsonNameAttribute('type')]
    FType: String;
  public
    property Description: String read FDescription write FDescription;
    property &In: String read FIn write FIn;
    property Name: String read FName write FName;
    property Required: Boolean read FRequired write FRequired;
    property &Type: String read FType write FType;
  end;

  TGet_006 = class
  private
    [JsonNameAttribute('description')]
    FDescription: String;
    [JsonNameAttribute('operationId')]
    FOperationId: String;
    [JsonNameAttribute('parameters')]
    FParameters: TArray<TParameters_016>;
    [JsonNameAttribute('produces')]
    FProduces: TArray<String>;
    [JsonNameAttribute('responses')]
    FResponses: TResponses_017;
    [JsonNameAttribute('summary')]
    FSummary: String;
    [JsonNameAttribute('tags')]
    FTags: TArray<String>;
  public
    property Description: String read FDescription write FDescription;
    property OperationId: String read FOperationId write FOperationId;
    property Parameters: TArray<TParameters_016> read FParameters write FParameters;
    property Produces: TArray<String> read FProduces write FProduces;
    property Responses: TResponses_017 read FResponses write FResponses;
    property Summary: String read FSummary write FSummary;
    property Tags: TArray<String> read FTags write FTags;
    destructor Destroy; override;
  end;

  TResponses_016 = class
  private
    [JsonNameAttribute('400')]
    F400: TDefault;
    [JsonNameAttribute('404')]
    F404: TDefault;
  public
    property Field400: TDefault read F400 write F400;
    property Field404: TDefault read F404 write F404;
    destructor Destroy; override;
  end;

  T200_007 = class
  private
    [JsonNameAttribute('description')]
    FDescription: String;
    [JsonNameAttribute('schema')]
    FSchema: TCategory;
  public
    property Description: String read FDescription write FDescription;
    property Schema: TCategory read FSchema write FSchema;
    destructor Destroy; override;
  end;

  TResponses_014 = class
  private
    [JsonNameAttribute('200')]
    F200: T200_007;
    [JsonNameAttribute('400')]
    F400: TDefault;
    [JsonNameAttribute('404')]
    F404: TDefault;
  public
    property Field200: T200_007 read F200 write F200;
    property Field400: TDefault read F400 write F400;
    property Field404: TDefault read F404 write F404;
    destructor Destroy; override;
  end;

  TUserUsername = class
  private
    [JsonNameAttribute('delete')]
    FDelete: TGet_006;
    [JsonNameAttribute('get')]
    FGet: TGet_006;
    [JsonNameAttribute('put')]
    FPut: TPost_006;
  public
    property Delete: TGet_006 read FDelete write FDelete;
    property Get: TGet_006 read FGet write FGet;
    property Put: TPost_006 read FPut write FPut;
    destructor Destroy; override;
  end;

  TSchema_011 = class
  private
    [JsonNameAttribute('items')]
    FItems: TCategory;
    [JsonNameAttribute('type')]
    FType: String;
  public
    property Items: TCategory read FItems write FItems;
    property &Type: String read FType write FType;
    destructor Destroy; override;
  end;

  TSecurity_008 = class
  private
  public
  end;

  TSchema_009 = class
  private
    [JsonNameAttribute('additionalProperties')]
    FAdditionalProperties: TId;
    [JsonNameAttribute('type')]
    FType: String;
  public
    property AdditionalProperties: TId read FAdditionalProperties write FAdditionalProperties;
    property &Type: String read FType write FType;
    destructor Destroy; override;
  end;

  TResponses_011 = class
  private
    [JsonNameAttribute('200')]
    F200: T200_007;
  public
    property Field200: T200_007 read F200 write F200;
    destructor Destroy; override;
  end;

  TGet_004 = class
  private
    [JsonNameAttribute('description')]
    FDescription: String;
    [JsonNameAttribute('operationId')]
    FOperationId: String;
    [JsonNameAttribute('produces')]
    FProduces: TArray<String>;
    [JsonNameAttribute('responses')]
    FResponses: TResponses_011;
    [JsonNameAttribute('security')]
    FSecurity: TArray<TSecurity_008>;
    [JsonNameAttribute('summary')]
    FSummary: String;
    [JsonNameAttribute('tags')]
    FTags: TArray<String>;
  public
    property Description: String read FDescription write FDescription;
    property OperationId: String read FOperationId write FOperationId;
    property Produces: TArray<String> read FProduces write FProduces;
    property Responses: TResponses_011 read FResponses write FResponses;
    property Security: TArray<TSecurity_008> read FSecurity write FSecurity;
    property Summary: String read FSummary write FSummary;
    property Tags: TArray<String> read FTags write FTags;
    destructor Destroy; override;
  end;

  TParameters_010 = class
  private
    [JsonNameAttribute('description')]
    FDescription: String;
    [JsonNameAttribute('format')]
    FFormat: String;
    [JsonNameAttribute('in')]
    FIn: String;
    [JsonNameAttribute('minimum')]
    FMinimum: Extended;
    [JsonNameAttribute('name')]
    FName: String;
    [JsonNameAttribute('required')]
    FRequired: Boolean;
    [JsonNameAttribute('type')]
    FType: String;
  public
    property Description: String read FDescription write FDescription;
    property Format: String read FFormat write FFormat;
    property &In: String read FIn write FIn;
    property Minimum: Extended read FMinimum write FMinimum;
    property Name: String read FName write FName;
    property Required: Boolean read FRequired write FRequired;
    property &Type: String read FType write FType;
  end;

  TParameters_009 = class
  private
    [JsonNameAttribute('description')]
    FDescription: String;
    [JsonNameAttribute('format')]
    FFormat: String;
    [JsonNameAttribute('in')]
    FIn: String;
    [JsonNameAttribute('maximum')]
    FMaximum: Extended;
    [JsonNameAttribute('minimum')]
    FMinimum: Extended;
    [JsonNameAttribute('name')]
    FName: String;
    [JsonNameAttribute('required')]
    FRequired: Boolean;
    [JsonNameAttribute('type')]
    FType: String;
  public
    property Description: String read FDescription write FDescription;
    property Format: String read FFormat write FFormat;
    property &In: String read FIn write FIn;
    property Maximum: Extended read FMaximum write FMaximum;
    property Minimum: Extended read FMinimum write FMinimum;
    property Name: String read FName write FName;
    property Required: Boolean read FRequired write FRequired;
    property &Type: String read FType write FType;
  end;

  TStoreOrderOrderId = class
  private
    [JsonNameAttribute('delete')]
    FDelete: TGet_006;
    [JsonNameAttribute('get')]
    FGet: TGet_006;
  public
    property Delete: TGet_006 read FDelete write FDelete;
    property Get: TGet_006 read FGet write FGet;
    destructor Destroy; override;
  end;

  TSecurity_007 = class
  private
    [JsonNameAttribute('petstore_auth')]
    FPetstoreAuth: TArray<String>;
  public
    property PetstoreAuth: TArray<String> read FPetstoreAuth write FPetstoreAuth;
  end;

  TParameters_007 = class
  private
    [JsonNameAttribute('in')]
    FIn: String;
    [JsonNameAttribute('name')]
    FName: String;
    [JsonNameAttribute('required')]
    FRequired: Boolean;
    [JsonNameAttribute('type')]
    FType: String;
  public
    property &In: String read FIn write FIn;
    property Name: String read FName write FName;
    property Required: Boolean read FRequired write FRequired;
    property &Type: String read FType write FType;
  end;

  TDelete = class
  private
    [JsonNameAttribute('description')]
    FDescription: String;
    [JsonNameAttribute('operationId')]
    FOperationId: String;
    [JsonNameAttribute('parameters')]
    FParameters: TArray<TParameters_007>;
    [JsonNameAttribute('produces')]
    FProduces: TArray<String>;
    [JsonNameAttribute('responses')]
    FResponses: TResponses_016;
    [JsonNameAttribute('security')]
    FSecurity: TArray<TSecurity_007>;
    [JsonNameAttribute('summary')]
    FSummary: String;
    [JsonNameAttribute('tags')]
    FTags: TArray<String>;
  public
    property Description: String read FDescription write FDescription;
    property OperationId: String read FOperationId write FOperationId;
    property Parameters: TArray<TParameters_007> read FParameters write FParameters;
    property Produces: TArray<String> read FProduces write FProduces;
    property Responses: TResponses_016 read FResponses write FResponses;
    property Security: TArray<TSecurity_007> read FSecurity write FSecurity;
    property Summary: String read FSummary write FSummary;
    property Tags: TArray<String> read FTags write FTags;
    destructor Destroy; override;
  end;

  TResponses_006 = class
  private
    [JsonNameAttribute('405')]
    F405: TDefault;
  public
    property Field405: TDefault read F405 write F405;
    destructor Destroy; override;
  end;

  TParameters_006 = class
  private
    [JsonNameAttribute('description')]
    FDescription: String;
    [JsonNameAttribute('format')]
    FFormat: String;
    [JsonNameAttribute('in')]
    FIn: String;
    [JsonNameAttribute('name')]
    FName: String;
    [JsonNameAttribute('required')]
    FRequired: Boolean;
    [JsonNameAttribute('type')]
    FType: String;
  public
    property Description: String read FDescription write FDescription;
    property Format: String read FFormat write FFormat;
    property &In: String read FIn write FIn;
    property Name: String read FName write FName;
    property Required: Boolean read FRequired write FRequired;
    property &Type: String read FType write FType;
  end;

  TPost_002 = class
  private
    [JsonNameAttribute('consumes')]
    FConsumes: TArray<String>;
    [JsonNameAttribute('description')]
    FDescription: String;
    [JsonNameAttribute('operationId')]
    FOperationId: String;
    [JsonNameAttribute('parameters')]
    FParameters: TArray<TParameters_006>;
    [JsonNameAttribute('produces')]
    FProduces: TArray<String>;
    [JsonNameAttribute('responses')]
    FResponses: TResponses_006;
    [JsonNameAttribute('security')]
    FSecurity: TArray<TSecurity_007>;
    [JsonNameAttribute('summary')]
    FSummary: String;
    [JsonNameAttribute('tags')]
    FTags: TArray<String>;
  public
    property Consumes: TArray<String> read FConsumes write FConsumes;
    property Description: String read FDescription write FDescription;
    property OperationId: String read FOperationId write FOperationId;
    property Parameters: TArray<TParameters_006> read FParameters write FParameters;
    property Produces: TArray<String> read FProduces write FProduces;
    property Responses: TResponses_006 read FResponses write FResponses;
    property Security: TArray<TSecurity_007> read FSecurity write FSecurity;
    property Summary: String read FSummary write FSummary;
    property Tags: TArray<String> read FTags write FTags;
    destructor Destroy; override;
  end;

  TPetPetId = class
  private
    [JsonNameAttribute('delete')]
    FDelete: TDelete;
    [JsonNameAttribute('get')]
    FGet: TDelete;
    [JsonNameAttribute('post')]
    FPost: TPost_002;
  public
    property Delete: TDelete read FDelete write FDelete;
    property Get: TDelete read FGet write FGet;
    property Post: TPost_002 read FPost write FPost;
    destructor Destroy; override;
  end;

  TParameters_004 = class
  private
    [JsonNameAttribute('collectionFormat')]
    FCollectionFormat: String;
    [JsonNameAttribute('description')]
    FDescription: String;
    [JsonNameAttribute('in')]
    FIn: String;
    [JsonNameAttribute('items')]
    FItems: TPhone;
    [JsonNameAttribute('name')]
    FName: String;
    [JsonNameAttribute('required')]
    FRequired: Boolean;
    [JsonNameAttribute('type')]
    FType: String;
  public
    property CollectionFormat: String read FCollectionFormat write FCollectionFormat;
    property Description: String read FDescription write FDescription;
    property &In: String read FIn write FIn;
    property Items: TPhone read FItems write FItems;
    property Name: String read FName write FName;
    property Required: Boolean read FRequired write FRequired;
    property &Type: String read FType write FType;
    destructor Destroy; override;
  end;

  TGet_001 = class
  private
    [JsonNameAttribute('deprecated')]
    FDeprecated: Boolean;
    [JsonNameAttribute('description')]
    FDescription: String;
    [JsonNameAttribute('operationId')]
    FOperationId: String;
    [JsonNameAttribute('parameters')]
    FParameters: TArray<TParameters_004>;
    [JsonNameAttribute('produces')]
    FProduces: TArray<String>;
    [JsonNameAttribute('responses')]
    FResponses: TResponses_017;
    [JsonNameAttribute('security')]
    FSecurity: TArray<TSecurity_007>;
    [JsonNameAttribute('summary')]
    FSummary: String;
    [JsonNameAttribute('tags')]
    FTags: TArray<String>;
  public
    property Deprecated: Boolean read FDeprecated write FDeprecated;
    property Description: String read FDescription write FDescription;
    property OperationId: String read FOperationId write FOperationId;
    property Parameters: TArray<TParameters_004> read FParameters write FParameters;
    property Produces: TArray<String> read FProduces write FProduces;
    property Responses: TResponses_017 read FResponses write FResponses;
    property Security: TArray<TSecurity_007> read FSecurity write FSecurity;
    property Summary: String read FSummary write FSummary;
    property Tags: TArray<String> read FTags write FTags;
    destructor Destroy; override;
  end;

  TItems = class
  private
    [JsonNameAttribute('default')]
    FDefault: String;
    [JsonNameAttribute('enum')]
    FEnum: TArray<String>;
    [JsonNameAttribute('type')]
    FType: String;
  public
    property Default: String read FDefault write FDefault;
    property Enum: TArray<String> read FEnum write FEnum;
    property &Type: String read FType write FType;
  end;

  TResponses_002 = class
  private
    [JsonNameAttribute('400')]
    F400: TDefault;
    [JsonNameAttribute('404')]
    F404: TDefault;
    [JsonNameAttribute('405')]
    F405: TDefault;
  public
    property Field400: TDefault read F400 write F400;
    property Field404: TDefault read F404 write F404;
    property Field405: TDefault read F405 write F405;
    destructor Destroy; override;
  end;

  TPet = class
  private
    [JsonNameAttribute('post')]
    FPost: TPost_002;
    [JsonNameAttribute('put')]
    FPut: TPost_002;
  public
    property Post: TPost_002 read FPost write FPost;
    property Put: TPost_002 read FPut write FPut;
    destructor Destroy; override;
  end;

  TPaths = class
  private
    [JsonNameAttribute('/pet')]
    FPet: TPet;
    [JsonNameAttribute('/pet/findByStatus')]
    FPetFindByStatus: TUserLogout;
    [JsonNameAttribute('/pet/findByTags')]
    FPetFindByTags: TUserLogout;
    [JsonNameAttribute('/pet/{petId}')]
    FPetPetId: TPetPetId;
    [JsonNameAttribute('/pet/{petId}/uploadImage')]
    FPetPetIdUploadImage: TUser;
    [JsonNameAttribute('/store/inventory')]
    FStoreInventory: TUserLogout;
    [JsonNameAttribute('/store/order')]
    FStoreOrder: TUser;
    [JsonNameAttribute('/store/order/{orderId}')]
    FStoreOrderOrderId: TStoreOrderOrderId;
    [JsonNameAttribute('/user')]
    FUser: TUser;
    [JsonNameAttribute('/user/createWithArray')]
    FUserCreateWithArray: TUser;
    [JsonNameAttribute('/user/createWithList')]
    FUserCreateWithList: TUser;
    [JsonNameAttribute('/user/login')]
    FUserLogin: TUserLogout;
    [JsonNameAttribute('/user/logout')]
    FUserLogout: TUserLogout;
    [JsonNameAttribute('/user/{username}')]
    FUserUsername: TUserUsername;
  public
    property Pet: TPet read FPet write FPet;
    property PetFindByStatus: TUserLogout read FPetFindByStatus write FPetFindByStatus;
    property PetFindByTags: TUserLogout read FPetFindByTags write FPetFindByTags;
    property PetPetId: TPetPetId read FPetPetId write FPetPetId;
    property PetPetIdUploadImage: TUser read FPetPetIdUploadImage write FPetPetIdUploadImage;
    property StoreInventory: TUserLogout read FStoreInventory write FStoreInventory;
    property StoreOrder: TUser read FStoreOrder write FStoreOrder;
    property StoreOrderOrderId: TStoreOrderOrderId read FStoreOrderOrderId write FStoreOrderOrderId;
    property User: TUser read FUser write FUser;
    property UserCreateWithArray: TUser read FUserCreateWithArray write FUserCreateWithArray;
    property UserCreateWithList: TUser read FUserCreateWithList write FUserCreateWithList;
    property UserLogin: TUserLogout read FUserLogin write FUserLogin;
    property UserLogout: TUserLogout read FUserLogout write FUserLogout;
    property UserUsername: TUserUsername read FUserUsername write FUserUsername;
    destructor Destroy; override;
  end;

  TTags = class
  private
    [JsonNameAttribute('description')]
    FDescription: String;
    [JsonNameAttribute('externalDocs')]
    FExternalDocs: TExternalDocs;
    [JsonNameAttribute('name')]
    FName: String;
  public
    property Description: String read FDescription write FDescription;
    property ExternalDocs: TExternalDocs read FExternalDocs write FExternalDocs;
    property Name: String read FName write FName;
    destructor Destroy; override;
  end;

  TLicense = class
  private
    [JsonNameAttribute('name')]
    FName: String;
    [JsonNameAttribute('url')]
    FUrl: String;
  public
    property Name: String read FName write FName;
    property Url: String read FUrl write FUrl;
  end;

  TContact = class
  private
    [JsonNameAttribute('email')]
    FEmail: String;
  public
    property Email: String read FEmail write FEmail;
  end;

  TInfo = class
  private
    [JsonNameAttribute('contact')]
    FContact: TContact;
    [JsonNameAttribute('description')]
    FDescription: String;
    [JsonNameAttribute('license')]
    FLicense: TLicense;
    [JsonNameAttribute('termsOfService')]
    FTermsOfService: String;
    [JsonNameAttribute('title')]
    FTitle: String;
    [JsonNameAttribute('version')]
    FVersion: String;
  public
    property Contact: TContact read FContact write FContact;
    property Description: String read FDescription write FDescription;
    property License: TLicense read FLicense write FLicense;
    property TermsOfService: String read FTermsOfService write FTermsOfService;
    property Title: String read FTitle write FTitle;
    property Version: String read FVersion write FVersion;
    destructor Destroy; override;
  end;

  TRoot = class
  private
    [JsonNameAttribute('basePath')]
    FBasePath: String;
    [JsonNameAttribute('definitions')]
    FDefinitions: TDefinitions;
    [JsonNameAttribute('externalDocs')]
    FExternalDocs: TExternalDocs;
    [JsonNameAttribute('host')]
    FHost: String;
    [JsonNameAttribute('info')]
    FInfo: TInfo;
    [JsonNameAttribute('paths')]
    FPaths: TPaths;
    [JsonNameAttribute('schemes')]
    FSchemes: TArray<String>;
    [JsonNameAttribute('securityDefinitions')]
    FSecurityDefinitions: TSecurityDefinitions;
    [JsonNameAttribute('swagger')]
    FSwagger: String;
    [JsonNameAttribute('tags')]
    FTags: TArray<TTags>;
  public
    property BasePath: String read FBasePath write FBasePath;
    property Definitions: TDefinitions read FDefinitions write FDefinitions;
    property ExternalDocs: TExternalDocs read FExternalDocs write FExternalDocs;
    property Host: String read FHost write FHost;
    property Info: TInfo read FInfo write FInfo;
    property Paths: TPaths read FPaths write FPaths;
    property Schemes: TArray<String> read FSchemes write FSchemes;
    property SecurityDefinitions: TSecurityDefinitions read FSecurityDefinitions write FSecurityDefinitions;
    property Swagger: String read FSwagger write FSwagger;
    property Tags: TArray<TTags> read FTags write FTags;
    destructor Destroy; override;
  end;

implementation

{ TProperties_005 }

destructor TProperties_005.Destroy;
begin
  FId.Free;
  FUsername.Free;
  FFirstName.Free;
  FLastName.Free;
  FEmail.Free;
  FPassword.Free;
  FPhone.Free;
  FUserStatus.Free;
  inherited;
end;

{ TUser_001 }

destructor TUser_001.Destroy;
begin
  FProperties.Free;
  FXml.Free;
  inherited;
end;

{ TProperties_004 }

destructor TProperties_004.Destroy;
begin
  FId.Free;
  FPetId.Free;
  FQuantity.Free;
  FShipDate.Free;
  FStatus.Free;
  FComplete.Free;
  inherited;
end;

{ TProperties_003 }

destructor TProperties_003.Destroy;
begin
  FId.Free;
  FName.Free;
  inherited;
end;

{ TItems_007 }

destructor TItems_007.Destroy;
begin
  FXml.Free;
  inherited;
end;

{ TTags_001 }

destructor TTags_001.Destroy;
begin
  FXml.Free;
  FItems.Free;
  inherited;
end;

{ TItems_006 }

destructor TItems_006.Destroy;
begin
  FXml.Free;
  inherited;
end;

{ TProperties_002 }

destructor TProperties_002.Destroy;
begin
  FId.Free;
  FCategory.Free;
  FName.Free;
  FPhotoUrls.Free;
  FTags.Free;
  FStatus.Free;
  inherited;
end;

{ TPet_001 }

destructor TPet_001.Destroy;
begin
  FProperties.Free;
  FXml.Free;
  inherited;
end;

{ TProperties }

destructor TProperties.Destroy;
begin
  FCode.Free;
  FType.Free;
  FMessage.Free;
  inherited;
end;

{ TApiResponse }

destructor TApiResponse.Destroy;
begin
  FProperties.Free;
  inherited;
end;

{ TDefinitions }

destructor TDefinitions.Destroy;
begin
  FApiResponse.Free;
  FCategory.Free;
  FPet.Free;
  FTag.Free;
  FOrder.Free;
  FUser.Free;
  inherited;
end;

{ TPetstoreAuth }

destructor TPetstoreAuth.Destroy;
begin
  FScopes.Free;
  inherited;
end;

{ TSecurityDefinitions }

destructor TSecurityDefinitions.Destroy;
begin
  FApiKey.Free;
  FPetstoreAuth.Free;
  inherited;
end;

{ TResponses_019 }

destructor TResponses_019.Destroy;
begin
  FDefault.Free;
  inherited;
end;

{ TParameters_017 }

destructor TParameters_017.Destroy;
begin
  FSchema.Free;
  inherited;
end;

{ TPost_006 }

destructor TPost_006.Destroy;
begin
 for var LParametersItem in FParameters do
   LParametersItem.Free;
  FResponses.Free;
  inherited;
end;

{ TUser }

destructor TUser.Destroy;
begin
  FPost.Free;
  inherited;
end;

{ TGet_007 }

destructor TGet_007.Destroy;
begin
  FResponses.Free;
  inherited;
end;

{ TUserLogout }

destructor TUserLogout.Destroy;
begin
  FGet.Free;
  inherited;
end;

{ THeaders }

destructor THeaders.Destroy;
begin
  FXExpiresAfter.Free;
  FXRateLimit.Free;
  inherited;
end;

{ T200_008 }

destructor T200_008.Destroy;
begin
  FHeaders.Free;
  FSchema.Free;
  inherited;
end;

{ TResponses_017 }

destructor TResponses_017.Destroy;
begin
  F200.Free;
  F400.Free;
  inherited;
end;

{ TGet_006 }

destructor TGet_006.Destroy;
begin
 for var LParametersItem in FParameters do
   LParametersItem.Free;
  FResponses.Free;
  inherited;
end;

{ TResponses_016 }

destructor TResponses_016.Destroy;
begin
  F400.Free;
  F404.Free;
  inherited;
end;

{ T200_007 }

destructor T200_007.Destroy;
begin
  FSchema.Free;
  inherited;
end;

{ TResponses_014 }

destructor TResponses_014.Destroy;
begin
  F200.Free;
  F400.Free;
  F404.Free;
  inherited;
end;

{ TUserUsername }

destructor TUserUsername.Destroy;
begin
  FGet.Free;
  FPut.Free;
  FDelete.Free;
  inherited;
end;

{ TSchema_011 }

destructor TSchema_011.Destroy;
begin
  FItems.Free;
  inherited;
end;

{ TSchema_009 }

destructor TSchema_009.Destroy;
begin
  FAdditionalProperties.Free;
  inherited;
end;

{ TResponses_011 }

destructor TResponses_011.Destroy;
begin
  F200.Free;
  inherited;
end;

{ TGet_004 }

destructor TGet_004.Destroy;
begin
 for var LSecurityItem in FSecurity do
   LSecurityItem.Free;
  FResponses.Free;
  inherited;
end;

{ TStoreOrderOrderId }

destructor TStoreOrderOrderId.Destroy;
begin
  FGet.Free;
  FDelete.Free;
  inherited;
end;

{ TDelete }

destructor TDelete.Destroy;
begin
 for var LParametersItem in FParameters do
   LParametersItem.Free;
 for var LSecurityItem in FSecurity do
   LSecurityItem.Free;
  FResponses.Free;
  inherited;
end;

{ TResponses_006 }

destructor TResponses_006.Destroy;
begin
  F405.Free;
  inherited;
end;

{ TPost_002 }

destructor TPost_002.Destroy;
begin
 for var LParametersItem in FParameters do
   LParametersItem.Free;
 for var LSecurityItem in FSecurity do
   LSecurityItem.Free;
  FResponses.Free;
  inherited;
end;

{ TPetPetId }

destructor TPetPetId.Destroy;
begin
  FGet.Free;
  FPost.Free;
  FDelete.Free;
  inherited;
end;

{ TParameters_004 }

destructor TParameters_004.Destroy;
begin
  FItems.Free;
  inherited;
end;

{ TGet_001 }

destructor TGet_001.Destroy;
begin
 for var LParametersItem in FParameters do
   LParametersItem.Free;
 for var LSecurityItem in FSecurity do
   LSecurityItem.Free;
  FResponses.Free;
  inherited;
end;

{ TResponses_002 }

destructor TResponses_002.Destroy;
begin
  F400.Free;
  F404.Free;
  F405.Free;
  inherited;
end;

{ TPet }

destructor TPet.Destroy;
begin
  FPost.Free;
  FPut.Free;
  inherited;
end;

{ TPaths }

destructor TPaths.Destroy;
begin
  FPetPetIdUploadImage.Free;
  FPet.Free;
  FPetFindByStatus.Free;
  FPetFindByTags.Free;
  FPetPetId.Free;
  FStoreOrder.Free;
  FStoreOrderOrderId.Free;
  FStoreInventory.Free;
  FUserCreateWithArray.Free;
  FUserCreateWithList.Free;
  FUserUsername.Free;
  FUserLogin.Free;
  FUserLogout.Free;
  FUser.Free;
  inherited;
end;

{ TTags }

destructor TTags.Destroy;
begin
  FExternalDocs.Free;
  inherited;
end;

{ TInfo }

destructor TInfo.Destroy;
begin
  FContact.Free;
  FLicense.Free;
  inherited;
end;

{ TRoot }

destructor TRoot.Destroy;
begin
 for var LTagsItem in FTags do
   LTagsItem.Free;
  FInfo.Free;
  FPaths.Free;
  FSecurityDefinitions.Free;
  FDefinitions.Free;
  FExternalDocs.Free;
  inherited;
end;

end.

