unit RootUnit;

interface

uses
  Rest.Json, Rest.Json.Types;

type
  TRoot = class
  private
    [JsonNameAttribute('items')]
    FItems: TArray<TDateTime>;
  public
    property Items: TArray<TDateTime> read FItems write FItems;
  end;

implementation

end.

