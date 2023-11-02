unit HGM.MaterialDesignStyle;

interface

uses
  System.UITypes, System.Classes, FMX.Controls, FMX.Types, FMX.Graphics,
  System.SysUtils;

type
  TMaterialDesignStylePallete3 = record
    ColorPrimary: TAlphaColor;
    ColorPrimary008: TAlphaColor;
    ColorPrimary012: TAlphaColor;
    ColorPrimaryContainer: TAlphaColor;
    ColorOutline: TAlphaColor;
    ColorSurface: TAlphaColor;
    ColorOnSurface: TAlphaColor;
    ColorOnSurface008: TAlphaColor;
    ColorOnSurface012: TAlphaColor;
    ColorOnSurface060: TAlphaColor;
    ColorOnSurfaceVariant: TAlphaColor;
    ColorOnSurfaceVariant008: TAlphaColor;
    ColorOnSurfaceVariant012: TAlphaColor;
    ColorOnPrimary: TAlphaColor;
    ColorOnPrimaryContainer: TAlphaColor;
    ColorCommonText: TAlphaColor;
    ColorOutlineVariant: TAlphaColor;
    ColorSurfaceContainer: TAlphaColor;
    ColorSurfaceContainerHigh: TAlphaColor;
    ColorSurfaceContainerHighest: TAlphaColor;
    ColorSecondaryContainer: TAlphaColor;
    ColorSecondaryContainer000: TAlphaColor;
    ColorOnSecondaryContainer: TAlphaColor;
    ColorOnSecondaryContainer008: TAlphaColor;
    ColorSurfaceContainerLow: TAlphaColor;
    ColorError: TAlphaColor;
    ColorOnError: TAlphaColor;
  end;

  TMaterialDesignStyle3 = class
  private
    FStyleBook: TStyleBook;
    FOriginalStyle: TMemoryStream;
    function FindColor(OldStyle, NewStyle: TMaterialDesignStylePallete3; const TargetColor: TAlphaColor; out FoundColor: TAlphaColor): Boolean;
  public
    class function DefaultDarkPallete: TMaterialDesignStylePallete3;
    class function DefaultLightPallete: TMaterialDesignStylePallete3;
    procedure ApplyStyle(StylePallete: TMaterialDesignStylePallete3);
    constructor Create(Style: TStyleBook); reintroduce;
    destructor Destroy; override;
  end;

function GetColorWithAlpha(Color: TAlphaColor; const AlphaPrecent: Single): TAlphaColor;

function ColorDarker(Color: TAlphaColor; Percent: Byte): TAlphaColor;

function ColorLighter(Color: TAlphaColor; Percent: Byte): TAlphaColor;

implementation

uses
  FMX.Styles, System.Math, FMX.Objects, FMX.Ani, FMX.StdCtrls,
  FMX.Styles.Objects, FMX.Styles.Switch, FMX.Filter.Effects, System.UIConsts;

function MathRound(AValue: Extended): Int64; inline;
begin
  if AValue >= 0 then
    Result := Trunc(AValue + 0.5)
  else
    Result := Trunc(AValue - 0.5);
end;

function MulDiv(nNumber, nNumerator, nDenominator: Integer): Integer;
begin
  if nDenominator = 0 then
    Result := -1
  else
    Result := MathRound(Int64(nNumber) * Int64(nNumerator) / nDenominator);
end;

function ColorDarker(Color: TAlphaColor; Percent: Byte): TAlphaColor;
begin
  var ColorRec := TAlphaColorRec.Create(Color);
  ColorRec.R := ColorRec.R - MulDiv(ColorRec.R, Percent, 100);
  ColorRec.G := ColorRec.G - MulDiv(ColorRec.G, Percent, 100);
  ColorRec.B := ColorRec.B - MulDiv(ColorRec.B, Percent, 100);
  Result := ColorRec.Color;
end;

function ColorLighter(Color: TAlphaColor; Percent: Byte): TAlphaColor;
begin
  var ColorRec := TAlphaColorRec.Create(Color);
  ColorRec.R := ColorRec.R + MulDiv(255 - ColorRec.R, Percent, 100);
  ColorRec.G := ColorRec.G + MulDiv(255 - ColorRec.G, Percent, 100);
  ColorRec.B := ColorRec.B + MulDiv(255 - ColorRec.B, Percent, 100);
  Result := ColorRec.Color;
end;

function GetColorWithAlpha(Color: TAlphaColor; const AlphaPrecent: Single): TAlphaColor;
begin
  var Rec := TAlphaColorRec.Create(Color);
  Rec.A := Round(255 * (AlphaPrecent / 100));
  Result := Rec.Color;
end;

{ TMaterialDesignStyle3 }

procedure TMaterialDesignStyle3.ApplyStyle(StylePallete: TMaterialDesignStylePallete3);

  function ReadBrushColor(Obj: TFmxObject; const StyleName: string): TAlphaColor;
  begin
    Result := TAlphaColors.Null;
    var Brush := Obj.FindStyleResource(StyleName);
    if Assigned(Brush) and (Brush is TBrushObject) then
      Result := TBrushObject(Brush).Brush.Color;
  end;

  procedure WriteBrushColor(Obj: TFmxObject; const StyleName: string; const Color: TAlphaColor);
  begin
    var Brush := Obj.FindStyleResource(StyleName);
    if Assigned(Brush) and (Brush is TBrushObject) then
      TBrushObject(Brush).Brush.Color := Color;
  end;

  procedure ForAll(Obj: TFmxObject; Proc: TProc<TFmxObject>);
  begin
    if Obj.StyleName = '#service_info' then
      Exit;
    Proc(Obj);
    if Assigned(Obj.Children) then
      for var Child in Obj.Children do
        ForAll(Child, Proc);
  end;

begin
  FStyleBook.Styles.Clear;
  var FStyle := FStyleBook.Styles.Add;
  FOriginalStyle.Position := 0;
  FStyle.LoadFromStream(FOriginalStyle);
  var Style := FStyle.Style;
  if Assigned(Style) then
  begin
    var ServiceStyle := Style.FindStyleResource('#service_info');
    if not Assigned(ServiceStyle) then
      Exit;
    var OriginalPallete: TMaterialDesignStylePallete3; //.Create;
    try
      OriginalPallete.ColorPrimary := ReadBrushColor(ServiceStyle, 'color_primary');
      OriginalPallete.ColorPrimary008 := ReadBrushColor(ServiceStyle, 'color_primary008');
      OriginalPallete.ColorPrimary012 := ReadBrushColor(ServiceStyle, 'color_primary012');
      OriginalPallete.ColorPrimaryContainer := ReadBrushColor(ServiceStyle, 'color_primary_container');
      OriginalPallete.ColorOutline := ReadBrushColor(ServiceStyle, 'color_outline');
      OriginalPallete.ColorSurface := ReadBrushColor(ServiceStyle, 'color_surface');
      OriginalPallete.ColorOnSurface := ReadBrushColor(ServiceStyle, 'color_on_surface');
      OriginalPallete.ColorOnPrimary := ReadBrushColor(ServiceStyle, 'color_on_primary');
      OriginalPallete.ColorCommonText := ReadBrushColor(ServiceStyle, 'color_common_text');
      OriginalPallete.ColorOutlineVariant := ReadBrushColor(ServiceStyle, 'color_outline_variant');
      OriginalPallete.ColorSurfaceContainer := ReadBrushColor(ServiceStyle, 'color_surface_container');
      OriginalPallete.ColorSurfaceContainerHigh := ReadBrushColor(ServiceStyle, 'color_surface_container_high');
      OriginalPallete.ColorSurfaceContainerHighest := ReadBrushColor(ServiceStyle, 'color_surface_container_highest');
      OriginalPallete.ColorOnPrimaryContainer := ReadBrushColor(ServiceStyle, 'color_on_primary_container');
      OriginalPallete.ColorOnSurface008 := ReadBrushColor(ServiceStyle, 'color_on_surface008');
      OriginalPallete.ColorOnSurface012 := ReadBrushColor(ServiceStyle, 'color_on_surface012');
      OriginalPallete.ColorOnSurface060 := ReadBrushColor(ServiceStyle, 'color_on_surface060');
      OriginalPallete.ColorOnSurfaceVariant := ReadBrushColor(ServiceStyle, 'color_on_surface_variant');
      OriginalPallete.ColorOnSurfaceVariant008 := ReadBrushColor(ServiceStyle, 'color_on_surface_variant008');
      OriginalPallete.ColorOnSurfaceVariant012 := ReadBrushColor(ServiceStyle, 'color_on_surface_variant012');
      OriginalPallete.ColorSecondaryContainer := ReadBrushColor(ServiceStyle, 'color_secondary_container');
      OriginalPallete.ColorSecondaryContainer000 := ReadBrushColor(ServiceStyle, 'color_secondary_container000');
      OriginalPallete.ColorOnSecondaryContainer := ReadBrushColor(ServiceStyle, 'color_on_secondary_container');
      OriginalPallete.ColorOnSecondaryContainer008 := ReadBrushColor(ServiceStyle, 'color_on_secondary_container008');
      OriginalPallete.ColorSurfaceContainerLow := ReadBrushColor(ServiceStyle, 'color_surface_container_low');
      OriginalPallete.ColorError := ReadBrushColor(ServiceStyle, 'color_error');
      OriginalPallete.ColorOnError := ReadBrushColor(ServiceStyle, 'color_on_error');
      ForAll(Style,
        procedure(Item: TFmxObject)
        begin
          Item.TagString := '';
        end);
      ForAll(Style,
        procedure(Item: TFmxObject)
        begin
          if not Item.TagString.IsEmpty then
            Exit;
          var Color: TAlphaColor;
          if Item is TShape then
          begin
            var Control := TRectangle(Item);
            if FindColor(OriginalPallete, StylePallete, Control.Fill.Color, Color) then
              Control.Fill.Color := Color;
            if FindColor(OriginalPallete, StylePallete, Control.Stroke.Color, Color) then
              Control.Stroke.Color := Color;
            Item.TagString := '0';
          end
          else if Item is TColorObject then
          begin
            var Control := TColorObject(Item);
            if FindColor(OriginalPallete, StylePallete, Control.Color, Color) then
              Control.Color := Color;
            Item.TagString := '0';
          end
          else if Item is TBrushObject then
          begin
            var Control := TBrushObject(Item);
            if FindColor(OriginalPallete, StylePallete, Control.Brush.Color, Color) then
              Control.Brush.Color := Color;
            Item.TagString := '0';
          end
          else if Item is TColorAnimation then
          begin
            var Control := TColorAnimation(Item);
            if FindColor(OriginalPallete, StylePallete, Control.StartValue, Color) then
              Control.StartValue := Color;
            if FindColor(OriginalPallete, StylePallete, Control.StopValue, Color) then
              Control.StopValue := Color;
            Item.TagString := '0';
          end
          else if Item is TLabel then
          begin
            var Control := TLabel(Item);
            if FindColor(OriginalPallete, StylePallete, Control.TextSettings.FontColor, Color) then
              Control.TextSettings.FontColor := Color;
            Item.TagString := '0';
          end
          else if Item is TText then
          begin
            if Item is TTabStyleTextObject then
            begin
              var Control := TTabStyleTextObject(Item);
              if FindColor(OriginalPallete, StylePallete, Control.HotColor, Color) then
                Control.HotColor := Color;
              if FindColor(OriginalPallete, StylePallete, Control.ActiveColor, Color) then
                Control.ActiveColor := Color;
              if FindColor(OriginalPallete, StylePallete, Control.Color, Color) then
                Control.Color := Color;
              Item.TagString := '0';
            end
            else if Item is TActiveStyleTextObject then
            begin
              var Control := TActiveStyleTextObject(Item);
              if FindColor(OriginalPallete, StylePallete, Control.ActiveColor, Color) then
                Control.ActiveColor := Color;
              if FindColor(OriginalPallete, StylePallete, Control.Color, Color) then
                Control.Color := Color;
              Item.TagString := '0';
            end
            else if Item is TButtonStyleTextObject then
            begin
              var Control := TButtonStyleTextObject(Item);
              if FindColor(OriginalPallete, StylePallete, Control.HotColor, Color) then
                Control.HotColor := Color;
              if FindColor(OriginalPallete, StylePallete, Control.FocusedColor, Color) then
                Control.FocusedColor := Color;
              if FindColor(OriginalPallete, StylePallete, Control.NormalColor, Color) then
                Control.NormalColor := Color;
              if FindColor(OriginalPallete, StylePallete, Control.PressedColor, Color) then
                Control.PressedColor := Color;
              Item.TagString := '0';
            end
            else
            begin
              var Control := TText(Item);
              if FindColor(OriginalPallete, StylePallete, Control.TextSettings.FontColor, Color) then
                Control.TextSettings.FontColor := Color;
              Item.TagString := '0';
            end;
          end
          else if Item is TSwitchObject then
          begin
            var Control := TSwitchObject(Item);
            if FindColor(OriginalPallete, StylePallete, Control.Fill.Color, Color) then
              Control.Fill.Color := Color;
            if FindColor(OriginalPallete, StylePallete, Control.FillOn.Color, Color) then
              Control.FillOn.Color := Color;
            if FindColor(OriginalPallete, StylePallete, Control.FillOn.Color, Color) then
              Control.FillOn.Color := Color;
            if FindColor(OriginalPallete, StylePallete, Control.Stroke.Color, Color) then
              Control.Stroke.Color := Color;
            if FindColor(OriginalPallete, StylePallete, Control.Thumb.Color, Color) then
              Control.Thumb.Color := Color;
            Item.TagString := '0';
          end
          else if Item is TFillRGBEffect then
          begin
            var Control := TFillRGBEffect(Item);
            if FindColor(OriginalPallete, StylePallete, Control.Color, Color) then
              Control.Color := Color;
            Item.TagString := '0';
          end;
        end);
      ForAll(Style,
        procedure(Item: TFmxObject)
        begin
          Item.TagString := '';
        end);
      WriteBrushColor(ServiceStyle, 'color_primary', StylePallete.ColorPrimary);
      WriteBrushColor(ServiceStyle, 'color_primary008', StylePallete.ColorPrimary008);
      WriteBrushColor(ServiceStyle, 'color_primary012', StylePallete.ColorPrimary012);
      WriteBrushColor(ServiceStyle, 'color_outline', StylePallete.ColorOutline);
      WriteBrushColor(ServiceStyle, 'color_primary_container', StylePallete.ColorPrimaryContainer);
      WriteBrushColor(ServiceStyle, 'color_surface', StylePallete.ColorSurface);
      WriteBrushColor(ServiceStyle, 'color_on_surface', StylePallete.ColorOnSurface);
      WriteBrushColor(ServiceStyle, 'color_on_primary', StylePallete.ColorOnPrimary);
      WriteBrushColor(ServiceStyle, 'color_common_text', StylePallete.ColorCommonText);
      WriteBrushColor(ServiceStyle, 'color_outline_variant', StylePallete.ColorOutlineVariant);
      WriteBrushColor(ServiceStyle, 'color_surface_container', StylePallete.ColorSurfaceContainer);
      WriteBrushColor(ServiceStyle, 'color_surface_container_high', StylePallete.ColorSurfaceContainerHigh);
      WriteBrushColor(ServiceStyle, 'color_surface_container_highest', StylePallete.ColorSurfaceContainerHighest);
      WriteBrushColor(ServiceStyle, 'color_on_primary_container', StylePallete.ColorOnPrimaryContainer);
      WriteBrushColor(ServiceStyle, 'color_on_surface008', StylePallete.ColorOnSurface008);
      WriteBrushColor(ServiceStyle, 'color_on_surface012', StylePallete.ColorOnSurface012);
      WriteBrushColor(ServiceStyle, 'color_on_surface060', StylePallete.ColorOnSurface060);
      WriteBrushColor(ServiceStyle, 'color_on_surface_variant', StylePallete.ColorOnSurfaceVariant);
      WriteBrushColor(ServiceStyle, 'color_on_surface_variant008', StylePallete.ColorOnSurfaceVariant008);
      WriteBrushColor(ServiceStyle, 'color_on_surface_variant012', StylePallete.ColorOnSurfaceVariant012);
      WriteBrushColor(ServiceStyle, 'color_secondary_container', StylePallete.ColorSecondaryContainer);
      WriteBrushColor(ServiceStyle, 'color_secondary_container', StylePallete.ColorSecondaryContainer000);
      WriteBrushColor(ServiceStyle, 'color_on_secondary_container', StylePallete.ColorOnSecondaryContainer);
      WriteBrushColor(ServiceStyle, 'color_on_secondary_container008', StylePallete.ColorOnSecondaryContainer008);
      WriteBrushColor(ServiceStyle, 'color_surface_container_low', StylePallete.ColorSurfaceContainerLow);
      WriteBrushColor(ServiceStyle, 'color_error', StylePallete.ColorError);
      WriteBrushColor(ServiceStyle, 'color_on_error', StylePallete.ColorOnError);
      TStyleManager.UpdateScenes;
    finally
      //
    end;
  end;
end;

constructor TMaterialDesignStyle3.Create(Style: TStyleBook);
begin
  inherited Create;
  FStyleBook := Style;
  FOriginalStyle := TMemoryStream.Create;
  FStyleBook.Styles[0].SaveToStream(FOriginalStyle, TStyleFormat.Binary);
end;

class function TMaterialDesignStyle3.DefaultLightPallete: TMaterialDesignStylePallete3;
begin
  Result.ColorPrimary := StringToAlphaColor('#FF6750A4');
  Result.ColorOnPrimary := StringToAlphaColor('#FFFFFFFF');
  Result.ColorSecondaryContainer := StringToAlphaColor('#FFE8DEF8');
  Result.ColorSurface := StringToAlphaColor('#FFFEF7FF');

  Result.ColorPrimary008 := GetColorWithAlpha(Result.ColorPrimary, 8);
  Result.ColorPrimary012 := GetColorWithAlpha(Result.ColorPrimary, 12);
  Result.ColorPrimaryContainer := StringToAlphaColor('#FF4F378B');
  Result.ColorOutline := StringToAlphaColor('#FF79747E');
  Result.ColorOnSurface := StringToAlphaColor('#FF1D1B20');
  Result.ColorOnSurface008 := GetColorWithAlpha(Result.ColorOnSurface, 8);
  Result.ColorOnSurface012 := GetColorWithAlpha(Result.ColorOnSurface, 12);
  Result.ColorOnSurface060 := GetColorWithAlpha(Result.ColorOnSurface, 60);
  Result.ColorOnSurfaceVariant := StringToAlphaColor('#FF49454F');
  Result.ColorOnSurfaceVariant008 := GetColorWithAlpha(Result.ColorOnSurfaceVariant, 8);
  Result.ColorOnSurfaceVariant012 := GetColorWithAlpha(Result.ColorOnSurfaceVariant, 12);
  Result.ColorCommonText := StringToAlphaColor('#FF000000');
  Result.ColorOutlineVariant := StringToAlphaColor('#FFCAC4D0');

  Result.ColorSurfaceContainer := StringToAlphaColor('#FFF3EDF7');
  Result.ColorSurfaceContainerHigh := StringToAlphaColor('#FFECE6F0');
  Result.ColorSurfaceContainerLow := StringToAlphaColor('#FFF7F2FA');
  Result.ColorSurfaceContainerHighest := StringToAlphaColor('#FFE6E0E9');

  Result.ColorOnPrimaryContainer := StringToAlphaColor('#FF21005D');
  Result.ColorOnSecondaryContainer := StringToAlphaColor('#FF1D192B');
  Result.ColorOnSecondaryContainer008 := GetColorWithAlpha(Result.ColorOnSecondaryContainer, 8);
  Result.ColorSecondaryContainer000 := GetColorWithAlpha(Result.ColorSecondaryContainer, 0);
  Result.ColorError := StringToAlphaColor('#FFB3261E');
  Result.ColorOnError := StringToAlphaColor('#FFFFFFFE');
end;

class function TMaterialDesignStyle3.DefaultDarkPallete: TMaterialDesignStylePallete3;
begin
  Result.ColorPrimary := StringToAlphaColor('#FFC63A3A');  //FFD0BCFF
  Result.ColorOnPrimary := StringToAlphaColor('#FF391111'); //FF381E72
  Result.ColorSecondaryContainer := StringToAlphaColor('#FF792323'); //FF4A4458
  Result.ColorSurface := StringToAlphaColor('#FF141218');

  Result.ColorPrimary008 := GetColorWithAlpha(Result.ColorPrimary, 8);
  Result.ColorPrimary012 := GetColorWithAlpha(Result.ColorPrimary, 12);
  Result.ColorPrimaryContainer := StringToAlphaColor('#FFEADDFF');
  Result.ColorOutline := StringToAlphaColor('#FF938F99');
  Result.ColorOnSurface := StringToAlphaColor('#FFE6E0E9');
  Result.ColorOnSurface008 := GetColorWithAlpha(Result.ColorOnSurface, 8);
  Result.ColorOnSurface012 := GetColorWithAlpha(Result.ColorOnSurface, 12);
  Result.ColorOnSurface060 := GetColorWithAlpha(Result.ColorOnSurface, 60);
  Result.ColorOnSurfaceVariant := StringToAlphaColor('#FFCAC4D0');
  Result.ColorOnSurfaceVariant008 := GetColorWithAlpha(Result.ColorOnSurfaceVariant, 8);
  Result.ColorOnSurfaceVariant012 := GetColorWithAlpha(Result.ColorOnSurfaceVariant, 12);
  Result.ColorCommonText := StringToAlphaColor('#FFFFFFFF');
  Result.ColorOutlineVariant := StringToAlphaColor('#FF49454F');
  Result.ColorSurfaceContainerHigh := StringToAlphaColor('#FF2B2930');
  Result.ColorSurfaceContainerHighest := StringToAlphaColor('#FF36343B');
  Result.ColorOnPrimaryContainer := StringToAlphaColor('#FFEADDFF');
  Result.ColorSurfaceContainer := StringToAlphaColor('#FF211F26');
  Result.ColorOnSecondaryContainer := StringToAlphaColor('#FFE8DEF8');
  Result.ColorOnSecondaryContainer008 := GetColorWithAlpha(Result.ColorOnSecondaryContainer, 8);
  Result.ColorSecondaryContainer000 := GetColorWithAlpha(Result.ColorSecondaryContainer, 0);
  Result.ColorSurfaceContainerLow := StringToAlphaColor('#FF1D1B20');
  Result.ColorError := StringToAlphaColor('#FFF2B8B5');
  Result.ColorOnError := StringToAlphaColor('#FF601410');
end;

destructor TMaterialDesignStyle3.Destroy;
begin
  FOriginalStyle.Free;
  FStyleBook := nil;
  inherited;
end;

function TMaterialDesignStyle3.FindColor(OldStyle, NewStyle: TMaterialDesignStylePallete3; const TargetColor: TAlphaColor; out FoundColor: TAlphaColor): Boolean;
begin
  Result := False;
  if TargetColor = OldStyle.ColorPrimary then
  begin
    FoundColor := NewStyle.ColorPrimary;
    Exit(True);
  end;
  if TargetColor = OldStyle.ColorOutline then
  begin
    FoundColor := NewStyle.ColorOutline;
    Exit(True);
  end;
  if TargetColor = OldStyle.ColorPrimaryContainer then
  begin
    FoundColor := NewStyle.ColorPrimaryContainer;
    Exit(True);
  end;
  if TargetColor = OldStyle.ColorSurface then
  begin
    FoundColor := NewStyle.ColorSurface;
    Exit(True);
  end;
  if TargetColor = OldStyle.ColorOnSurface then
  begin
    FoundColor := NewStyle.ColorOnSurface;
    Exit(True);
  end;
  if TargetColor = OldStyle.ColorOnPrimary then
  begin
    FoundColor := NewStyle.ColorOnPrimary;
    Exit(True);
  end;
  if TargetColor = OldStyle.ColorCommonText then
  begin
    FoundColor := NewStyle.ColorCommonText;
    Exit(True);
  end;
  if TargetColor = OldStyle.ColorOutlineVariant then
  begin
    FoundColor := NewStyle.ColorOutlineVariant;
    Exit(True);
  end;
  if TargetColor = OldStyle.ColorSurfaceContainerHighest then
  begin
    FoundColor := NewStyle.ColorSurfaceContainerHighest;
    Exit(True);
  end;
  if TargetColor = OldStyle.ColorSurfaceContainerHigh then
  begin
    FoundColor := NewStyle.ColorSurfaceContainerHigh;
    Exit(True);
  end;
  if TargetColor = OldStyle.ColorOnPrimaryContainer then
  begin
    FoundColor := NewStyle.ColorOnPrimaryContainer;
    Exit(True);
  end;
  if TargetColor = OldStyle.ColorOnSurface008 then
  begin
    FoundColor := NewStyle.ColorOnSurface008;
    Exit(True);
  end;
  if TargetColor = OldStyle.ColorOnSurface012 then
  begin
    FoundColor := NewStyle.ColorOnSurface012;
    Exit(True);
  end;
  if TargetColor = OldStyle.ColorOnSurface060 then
  begin
    FoundColor := NewStyle.ColorOnSurface060;
    Exit(True);
  end;
  if TargetColor = OldStyle.ColorOnSurfaceVariant then
  begin
    FoundColor := NewStyle.ColorOnSurfaceVariant;
    Exit(True);
  end;
  if TargetColor = OldStyle.ColorSurfaceContainer then
  begin
    FoundColor := NewStyle.ColorSurfaceContainer;
    Exit(True);
  end;
  if TargetColor = OldStyle.ColorPrimary008 then
  begin
    FoundColor := NewStyle.ColorPrimary008;
    Exit(True);
  end;
  if TargetColor = OldStyle.ColorPrimary012 then
  begin
    FoundColor := NewStyle.ColorPrimary012;
    Exit(True);
  end;
  if TargetColor = OldStyle.ColorSecondaryContainer then
  begin
    FoundColor := NewStyle.ColorSecondaryContainer;
    Exit(True);
  end;
  if TargetColor = OldStyle.ColorSecondaryContainer000 then
  begin
    FoundColor := NewStyle.ColorSecondaryContainer000;
    Exit(True);
  end;
  if TargetColor = OldStyle.ColorOnSurfaceVariant008 then
  begin
    FoundColor := NewStyle.ColorOnSurfaceVariant008;
    Exit(True);
  end;
  if TargetColor = OldStyle.ColorOnSurfaceVariant012 then
  begin
    FoundColor := NewStyle.ColorOnSurfaceVariant012;
    Exit(True);
  end;
  if TargetColor = OldStyle.ColorOnSecondaryContainer then
  begin
    FoundColor := NewStyle.ColorOnSecondaryContainer;
    Exit(True);
  end;
  if TargetColor = OldStyle.ColorOnSecondaryContainer008 then
  begin
    FoundColor := NewStyle.ColorOnSecondaryContainer008;
    Exit(True);
  end;
  if TargetColor = OldStyle.ColorSurfaceContainerLow then
  begin
    FoundColor := NewStyle.ColorSurfaceContainerLow;
    Exit(True);
  end;
  if TargetColor = OldStyle.ColorError then
  begin
    FoundColor := NewStyle.ColorError;
    Exit(True);
  end;
  if TargetColor = OldStyle.ColorOnError then
  begin
    FoundColor := NewStyle.ColorOnError;
    Exit(True);
  end;
end;

end.

