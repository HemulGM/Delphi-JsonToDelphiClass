unit RootUnit;

interface

uses
  Rest.Json, Rest.Json.Types;

type
  TIndexValue = class
  private
    [JsonNameAttribute('Fi')]
    FFi: string;
    [JsonNameAttribute('Sv')]
    FSv: string;
  public
    property Fi: string read FFi write FFi;
    property Sv: string read FSv write FSv;
  end;

  TPrecautionaryStatementValues = class
  private
    [JsonNameAttribute('Index')]
    FIndex: Extended;
    [JsonNameAttribute('IndexValue')]
    FIndexValue: TIndexValue;
    [JsonNameAttribute('StatementCodeValue')]
    FStatementCodeValue: string;
  public
    property Index: Extended read FIndex write FIndex;
    property IndexValue: TIndexValue read FIndexValue write FIndexValue;
    property StatementCodeValue: string read FStatementCodeValue write FStatementCodeValue;
    destructor Destroy; override;
  end;

  TClassifications = class
  private
    [JsonNameAttribute('HazardCategoryClass')]
    FHazardCategoryClass: string;
    [JsonNameAttribute('HazardStatement')]
    FHazardStatement: string;
  public
    property HazardCategoryClass: string read FHazardCategoryClass write FHazardCategoryClass;
    property HazardStatement: string read FHazardStatement write FHazardStatement;
  end;

  TDocuments = class
  private
    [JsonNameAttribute('FileName')]
    FFileName: string;
    [JsonNameAttribute('KemiDigiURL')]
    FKemiDigiURL: string;
  public
    property FileName: string read FFileName write FFileName;
    property KemiDigiURL: string read FKemiDigiURL write FKemiDigiURL;
  end;

  TSprayingProtectionDistances = class
  private
    [JsonNameAttribute('Usage')]
    FUsage: string;
    [JsonNameAttribute('WindReductionNozzle50')]
    FWindReductionNozzle50: Extended;
    [JsonNameAttribute('WindReductionNozzle75')]
    FWindReductionNozzle75: Extended;
    [JsonNameAttribute('WindReductionNozzle90')]
    FWindReductionNozzle90: Extended;
  public
    property Usage: string read FUsage write FUsage;
    property WindReductionNozzle50: Extended read FWindReductionNozzle50 write FWindReductionNozzle50;
    property WindReductionNozzle75: Extended read FWindReductionNozzle75 write FWindReductionNozzle75;
    property WindReductionNozzle90: Extended read FWindReductionNozzle90 write FWindReductionNozzle90;
  end;

  TPermits = class
  private
    [JsonNameAttribute('ClearancePeriod')]
    FClearancePeriod: string;
    [JsonNameAttribute('Documents')]
    FDocuments: TArray<TDocuments>;
    [JsonNameAttribute('FrequentUseLimitation')]
    FFrequentUseLimitation: string;
    [JsonNameAttribute('HolderCompany')]
    FHolderCompany: string;
    [JsonNameAttribute('OtherUseLimitation')]
    FOtherUseLimitation: string;
    [JsonNameAttribute('PackageAndProductDisposing')]
    FPackageAndProductDisposing: string;
    [JsonNameAttribute('PollinatorUseLimitation')]
    FPollinatorUseLimitation: string;
    [JsonNameAttribute('ProtectionInstruction')]
    FProtectionInstruction: string;
    [JsonNameAttribute('ResistenceControl')]
    FResistenceControl: string;
    [JsonNameAttribute('SprayingProtectionDistances')]
    FSprayingProtectionDistances: TArray<TSprayingProtectionDistances>;
    [JsonNameAttribute('StoragingAndLastingness')]
    FStoragingAndLastingness: string;
    [JsonNameAttribute('Type')]
    FType: string;
    [JsonNameAttribute('UseDescription')]
    FUseDescription: string;
    [JsonNameAttribute('ValidTo')]
    FValidTo: TDateTime;
    [JsonNameAttribute('WatersUseLimitation')]
    FWatersUseLimitation: string;
  public
    property ClearancePeriod: string read FClearancePeriod write FClearancePeriod;
    property Documents: TArray<TDocuments> read FDocuments write FDocuments;
    property FrequentUseLimitation: string read FFrequentUseLimitation write FFrequentUseLimitation;
    property HolderCompany: string read FHolderCompany write FHolderCompany;
    property OtherUseLimitation: string read FOtherUseLimitation write FOtherUseLimitation;
    property PackageAndProductDisposing: string read FPackageAndProductDisposing write FPackageAndProductDisposing;
    property PollinatorUseLimitation: string read FPollinatorUseLimitation write FPollinatorUseLimitation;
    property ProtectionInstruction: string read FProtectionInstruction write FProtectionInstruction;
    property ResistenceControl: string read FResistenceControl write FResistenceControl;
    property SprayingProtectionDistances: TArray<TSprayingProtectionDistances> read FSprayingProtectionDistances write FSprayingProtectionDistances;
    property StoragingAndLastingness: string read FStoragingAndLastingness write FStoragingAndLastingness;
    property &Type: string read FType write FType;
    property UseDescription: string read FUseDescription write FUseDescription;
    property ValidTo: TDateTime read FValidTo write FValidTo;
    property WatersUseLimitation: string read FWatersUseLimitation write FWatersUseLimitation;
    destructor Destroy; override;
  end;

  TSubstanceContents = class
  private
    [JsonNameAttribute('Concentration')]
    FConcentration: Extended;
    [JsonNameAttribute('ConcentrationUnit')]
    FConcentrationUnit: string;
  public
    property Concentration: Extended read FConcentration write FConcentration;
    property ConcentrationUnit: string read FConcentrationUnit write FConcentrationUnit;
  end;

  TSubstances = class
  private
    [JsonNameAttribute('CAS')]
    FCAS: string;
    [JsonNameAttribute('KemiDigiURL')]
    FKemiDigiURL: string;
    [JsonNameAttribute('Name')]
    FName: string;
    [JsonNameAttribute('SubstanceContents')]
    FSubstanceContents: TArray<TSubstanceContents>;
  public
    property CAS: string read FCAS write FCAS;
    property KemiDigiURL: string read FKemiDigiURL write FKemiDigiURL;
    property Name: string read FName write FName;
    property SubstanceContents: TArray<TSubstanceContents> read FSubstanceContents write FSubstanceContents;
    destructor Destroy; override;
  end;

  TItem = class
  private
    [JsonNameAttribute('Classifications')]
    FClassifications: TArray<TClassifications>;
    [JsonNameAttribute('Consumer')]
    FConsumer: Boolean;
    [JsonNameAttribute('HazardPictograms')]
    FHazardPictograms: TArray<string>;
    [JsonNameAttribute('HazardStatementValues')]
    FHazardStatementValues: TArray<TPrecautionaryStatementValues>;
    [JsonNameAttribute('HazardStatements')]
    FHazardStatements: TArray<string>;
    [JsonNameAttribute('KemiDigiURL')]
    FKemiDigiURL: string;
    [JsonNameAttribute('Modified')]
    FModified: string;
    [JsonNameAttribute('Name')]
    FName: string;
    [JsonNameAttribute('Permits')]
    FPermits: TArray<TPermits>;
    [JsonNameAttribute('PrecautionaryStatementValues')]
    FPrecautionaryStatementValues: TArray<TPrecautionaryStatementValues>;
    [JsonNameAttribute('PrecautionaryStatements')]
    FPrecautionaryStatements: TArray<string>;
    [JsonNameAttribute('ProductGroups')]
    FProductGroups: TArray<string>;
    [JsonNameAttribute('ProductType')]
    FProductType: string;
    [JsonNameAttribute('Professional')]
    FProfessional: Boolean;
    [JsonNameAttribute('RegisterNumber')]
    FRegisterNumber: Extended;
    [JsonNameAttribute('Substances')]
    FSubstances: TArray<TSubstances>;
  public
    property Classifications: TArray<TClassifications> read FClassifications write FClassifications;
    property Consumer: Boolean read FConsumer write FConsumer;
    property HazardPictograms: TArray<string> read FHazardPictograms write FHazardPictograms;
    property HazardStatementValues: TArray<TPrecautionaryStatementValues> read FHazardStatementValues write FHazardStatementValues;
    property HazardStatements: TArray<string> read FHazardStatements write FHazardStatements;
    property KemiDigiURL: string read FKemiDigiURL write FKemiDigiURL;
    property Modified: string read FModified write FModified;
    property Name: string read FName write FName;
    property Permits: TArray<TPermits> read FPermits write FPermits;
    property PrecautionaryStatementValues: TArray<TPrecautionaryStatementValues> read FPrecautionaryStatementValues write FPrecautionaryStatementValues;
    property PrecautionaryStatements: TArray<string> read FPrecautionaryStatements write FPrecautionaryStatements;
    property ProductGroups: TArray<string> read FProductGroups write FProductGroups;
    property ProductType: string read FProductType write FProductType;
    property Professional: Boolean read FProfessional write FProfessional;
    property RegisterNumber: Extended read FRegisterNumber write FRegisterNumber;
    property Substances: TArray<TSubstances> read FSubstances write FSubstances;
    destructor Destroy; override;
  end;

  TRoot = class
  private
    [JsonNameAttribute('Items')]
    FItems: TArray<TItem>;
  public
    property Items: TArray<TItem> read FItems write FItems;
    destructor Destroy; override;
  end;

implementation

{ TPrecautionaryStatementValues }

destructor TPrecautionaryStatementValues.Destroy;
begin
  FIndexValue.Free;
  inherited;
end;

{ TPermits }

destructor TPermits.Destroy;
begin
 for var LSprayingProtectionDistancesItem in FSprayingProtectionDistances do
   LSprayingProtectionDistancesItem.Free;
 for var LDocumentsItem in FDocuments do
   LDocumentsItem.Free;
  inherited;
end;

{ TSubstances }

destructor TSubstances.Destroy;
begin
 for var LSubstanceContentsItem in FSubstanceContents do
   LSubstanceContentsItem.Free;
  inherited;
end;

{ TItem }

destructor TItem.Destroy;
begin
 for var LSubstancesItem in FSubstances do
   LSubstancesItem.Free;
 for var LPermitsItem in FPermits do
   LPermitsItem.Free;
 for var LClassificationsItem in FClassifications do
   LClassificationsItem.Free;
 for var LHazardStatementValuesItem in FHazardStatementValues do
   LHazardStatementValuesItem.Free;
 for var LPrecautionaryStatementValuesItem in FPrecautionaryStatementValues do
   LPrecautionaryStatementValuesItem.Free;
  inherited;
end;

{ TRoot }

destructor TRoot.Destroy;
begin
 for var LItemsItem in FItems do
   LItemsItem.Free;
  inherited;
end;

end.

