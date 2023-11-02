unit ChatGPT.Code.JSON;

interface

uses
  System.SysUtils, ChatGPT.Code, System.Generics.Collections, FMX.TextLayout,
  FMX.Graphics, System.UITypes;

type
  TCodeSyntaxJson = class(TCodeSyntax)
  private
    FStringKey, FNumKey, FFieldKey, FBoolKey: TKeyWord;
  public
    constructor Create(DefaultFont: TFont; DefaultColor: TAlphaColor); override;
    destructor Destroy; override;
    function GetAttributesForLine(const Line: string; const Index: Integer): TArray<TTextAttributedRangeData>; override;
  end;

implementation

uses
  System.Math;

{ TCodeSyntaxJson }

constructor TCodeSyntaxJson.Create(DefaultFont: TFont; DefaultColor: TAlphaColor);
begin
  inherited;
  FStringKey := TKeyWord.Create;
  FStringKey.Color := $FFBA8200;
  FStringKey.Font.Assign(FDefaultFont);

  FFieldKey := TKeyWord.Create;
  FFieldKey.Color := $FF0055E8;
  FFieldKey.Font.Assign(FDefaultFont);

  FNumKey := TKeyWord.Create;
  FNumKey.Color := $FF7F3E84;
  FNumKey.Font.Assign(FDefaultFont);

  FBoolKey := TKeyWord.Create;
  FBoolKey.Color := $FF007D5A;
  FBoolKey.Font.Assign(FDefaultFont);
end;

destructor TCodeSyntaxJson.Destroy;
begin
  FStringKey.Free;
  FNumKey.Free;
  FFieldKey.Free;
  FBoolKey.Free;
  inherited;
end;

function TCodeSyntaxJson.GetAttributesForLine(const Line: string; const Index: Integer): TArray<TTextAttributedRangeData>;
const
  Seps =[' ', ';', ')', '(', '[', ']', ':', '<', '>', ',', '+', '-', '=', '*', '/', '\'];
begin
  if FCached.TryGetValue(Index, Result) then
    Exit;
  try
    var Buf: string := '';
    var IsString: Boolean := False;
    var IsValue: Boolean := False;
    for var C := 0 to Min(Line.Length, 700) do
    begin
      if Line.IsEmpty then
        Continue;
      if IsString then
      begin
        if Line.Chars[C] = '"' then
        begin
          IsString := False;
          if not Buf.IsEmpty then
          begin
            if IsValue then
              Result := Result + [
                TTextAttributedRangeData.Create(
                TTextRange.Create(C - Buf.Length - 1, Buf.Length + 2),
                TTextAttribute.Create(FStringKey.Font, FStringKey.Color)
                )]
            else
              Result := Result + [
                TTextAttributedRangeData.Create(
                TTextRange.Create(C - Buf.Length - 1, Buf.Length + 2),
                TTextAttribute.Create(FFieldKey.Font, FFieldKey.Color)
                )];

            IsValue := False;
            Buf := '';
          end;
          Continue;
        end;
        Buf := Buf + Line.Chars[C];
        Continue;
      end;
      if Line.Chars[C] = '"' then
      begin
        IsString := True;
        Buf := '';
        Continue;
      end;

      if (C = Line.Length) or CharInSet(Line.Chars[C], Seps) then
      begin
        if Line.Chars[C] = ':' then
          IsValue := True;
        if not Buf.IsEmpty then
        begin
          var FL: Extended;
          if TryStrToFloat(Buf.Replace('.', FormatSettings.DecimalSeparator), FL) then
          begin
            Result := Result + [TTextAttributedRangeData.Create(
              TTextRange.Create(C - Buf.Length, Buf.Length),
              TTextAttribute.Create(FNumKey.Font, FNumKey.Color)
              )];
          end
          else if (Buf.ToLower = 'true') or (Buf.ToLower = 'false') then
          begin
            Result := Result + [TTextAttributedRangeData.Create(
              TTextRange.Create(C - Buf.Length, Buf.Length),
              TTextAttribute.Create(FBoolKey.Font, FBoolKey.Color)
              )];
          end
          else if Buf.StartsWith('\') then
          begin
            Result := Result + [TTextAttributedRangeData.Create(
              TTextRange.Create(C - Buf.Length, Buf.Length),
              TTextAttribute.Create(FFieldKey.Font, FFieldKey.Color)
              )];
          end;

          Buf := '';
        end;
      end
      else
        Buf := Buf + Line.Chars[C];
    end;
  finally
    FCached.AddOrSetValue(Index, Result);
  end;
end;

initialization
  TCodeSyntax.RegisterSyntax(['json'], TCodeSyntaxJson);

end.

