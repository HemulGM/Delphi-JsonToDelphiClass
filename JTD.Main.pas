unit JTD.Main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Layouts, FMX.Memo, System.Json, FMX.TreeView, Json.Mapper, FMX.Menus,
  FMX.Controls.Presentation, FMX.Edit, System.Threading, FMX.Objects,
  FMX.ScrollBox, FMX.Memo.Types, FMX.Effects, FMX.Filter.Effects, FMX.Memo.Style,
  ChatGPT.Code, FMX.TextLayout, FMX.Ani, HGM.MaterialDesignStyle,
  System.ImageList, FMX.ImgList, FMX.SVGIconImageList, FMX.TabControl,
  FMX.Platform.Win, FMX.SVGIconImage, JTD.Frame.JsonObject, JTD.Frame.JsonSchema;

const
  JsonValidatorUrl = 'https://jsonlint.com';

type
  TJSONError = record
    Path: string;
    Offset: Integer;
    Line: Integer;
    Position: Integer;
    Available: Boolean;
  end;

  TFormMain = class(TForm)
    MainPopupMenu: TPopupMenu;
    MenuItemFieldCaption: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItemRenameProp: TMenuItem;
    MemoPopupMenu: TPopupMenu;
    MenuItemJSONFormat: TMenuItem;
    MenuItemChangeClassName: TMenuItem;
    MenuItemChangeType: TMenuItem;
    MenuItemValidateJSON: TMenuItem;
    StyleBookMD3: TStyleBook;
    ImageList: TSVGIconImageList;
    SaveDialogUnit: TSaveDialog;
    MenuItemJSONUndo: TMenuItem;
    MenuItem1: TMenuItem;
    MenuItemJSONCut: TMenuItem;
    MenuItemJSONCopy: TMenuItem;
    MenuItemJSONPaste: TMenuItem;
    MenuItemJSONDelete: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItemJSONSelectAll: TMenuItem;
    MenuItem4: TMenuItem;
    TimerRepaint: TTimer;
    LayoutContent: TLayout;
    TabControlMode: TTabControl;
    TabItemJObject: TTabItem;
    TabItemJSchema: TTabItem;
    TabItemMenu: TTabItem;
    ButtonOpenJObject: TButton;
    ButtonOpenJSchema: TButton;
    LayoutHead: TLayout;
    ImageLogo: TImage;
    LabelTitle: TLabel;
    ButtonMenu: TButton;
    Layout8: TLayout;
    SVGIconImage1: TSVGIconImage;
    SVGIconImage2: TSVGIconImage;
    FrameJsonObject: TFrameJsonObject;
    FrameJsonSchema: TFrameJsonSchema;
    Label1: TLabel;
    Label2: TLabel;
    Layout1: TLayout;
    Layout2: TLayout;
    Image2: TImage;
    Label3: TLabel;
    LayoutMenu: TLayout;
    RectangleWork: TRectangle;
    AniIndicatorWork: TAniIndicator;
    Label4: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure TimerRepaintTimer(Sender: TObject);
    procedure ButtonOpenJObjectClick(Sender: TObject);
    procedure ButtonOpenJSchemaClick(Sender: TObject);
    procedure TabControlModeChange(Sender: TObject);
    procedure ButtonMenuClick(Sender: TObject);
  private
    FMDStyle3: TMaterialDesignStyle3;
    procedure DoWork(Sender: TObject);
    procedure EndWork(Sender: TObject);
    procedure SetStyle;
  public
  end;

const
  ProgramVersion = '1.1';

var
  FormMain: TFormMain;

implementation

{$R *.fmx}

uses
  System.IOUtils, System.Math, HGM.ObjectHolder, FMX.Text,
  {$IFDEF MSWINDOWS}
  Winapi.ShellAPI, Winapi.Windows,
  {$ENDIF MSWINDOWS}
  {$IFDEF DEBUG}
  RootUnit, Rest.Json,
  {$ENDIF}
  {$IFDEF POSIX}
  Posix.Stdlib,
  {$ENDIF POSIX} FMX.DialogService, FMX.BehaviorManager;

procedure TFormMain.SetStyle;
begin
  var Style := TMaterialDesignStyle3.DefaultDarkPallete;
  Style.ColorPrimary := $FF245eab;
  Style.ColorOnPrimary := $FFffffff;
  Style.ColorSecondaryContainer := $FFcee5ff;
  Style.ColorOnSecondaryContainer := $FF001d32;
  Style.ColorSurface := $FFECF5FD;
  Style.ColorSurfaceContainer := $FFECF5FD;

  Style.ColorOutline := $FF74777f;
  Style.ColorPrimaryContainer := $FFd6e3ff;
  Style.ColorOnSurface := $FF001b3d;
  Style.ColorOnSurfaceVariant := $FF44474e;
  Style.ColorOnPrimaryContainer := $FF001b3e;
  Style.ColorOutlineVariant := $FFe0e2ec;
  Style.ColorError := $FFba1a1a;
  Style.ColorOnError := $FFffffff;

  Style.ColorPrimary008 := GetColorWithAlpha(Style.ColorPrimary, 8);
  Style.ColorPrimary012 := GetColorWithAlpha(Style.ColorPrimary, 12);
  Style.ColorOnSurface008 := GetColorWithAlpha(Style.ColorOnSurface, 12);
  Style.ColorOnSurfaceVariant008 := GetColorWithAlpha(Style.ColorOnSurfaceVariant, 8);
  Style.ColorOnSurfaceVariant012 := GetColorWithAlpha(Style.ColorOnSurfaceVariant, 12);
  Style.ColorOnSecondaryContainer008 := GetColorWithAlpha(Style.ColorOnSecondaryContainer, 8);
  Style.ColorSecondaryContainer000 := GetColorWithAlpha(Style.ColorSecondaryContainer, 0);

  Style.ColorSurfaceContainerLow := ColorLighter(Style.ColorSurfaceContainer, 5);
  Style.ColorSurfaceContainerHigh := ColorDarker(Style.ColorSurfaceContainer, 5);
  Style.ColorSurfaceContainerHighest := ColorDarker(Style.ColorSurfaceContainer, 10);

  FMDStyle3.ApplyStyle(Style);
end;

procedure TFormMain.ButtonMenuClick(Sender: TObject);
begin
  TabControlMode.ActiveTab := TabItemMenu;
end;

procedure TFormMain.ButtonOpenJObjectClick(Sender: TObject);
begin
  TabControlMode.ActiveTab := TabItemJObject;
end;

procedure TFormMain.ButtonOpenJSchemaClick(Sender: TObject);
begin
  TabControlMode.ActiveTab := TabItemJSchema;
end;

procedure TFormMain.DoWork(Sender: TObject);
begin
  RectangleWork.Visible := True;
  if Sender is TControl then
    TControl(Sender).Enabled := False;
end;

procedure TFormMain.EndWork(Sender: TObject);
begin
  RectangleWork.Visible := False;
  if Sender is TControl then
    TControl(Sender).Enabled := True;
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  // Style
  FMDStyle3 := TMaterialDesignStyle3.Create(StyleBookMD3);
  SetStyle;

  TAnimation.AniFrameRate := 300;
  Caption := 'JsonToDelphiClass v' + ProgramVersion + ' | By HemulGM';

  TabControlMode.TabPosition := TTabPosition.None;
  TabControlMode.ActiveTab := TabItemMenu;
  TabControlModeChange(nil);

  RectangleWork.Visible := False;
  FrameJsonObject.OnDoWork := DoWork;
  FrameJsonObject.OnEndWork := EndWork;
  FrameJsonSchema.OnDoWork := DoWork;
  FrameJsonSchema.OnEndWork := EndWork;

  Width := 1400;
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  FMDStyle3.Free;
end;

procedure TFormMain.TabControlModeChange(Sender: TObject);
begin
  if TabControlMode.ActiveTab = TabItemMenu then
    TAnimator.AnimateFloat(ButtonMenu, 'Margins.Left', -(ButtonMenu.Width + ButtonMenu.ParentControl.Padding.Left))
  else
    TAnimator.AnimateFloat(ButtonMenu, 'Margins.Left', 0);
  //ButtonMenu.Visible := TabControlMode.ActiveTab <> TabItemMenu;
  if TabControlMode.ActiveTab = TabItemJObject then
  begin
    ButtonMenu.Text := 'JSON Object';
    //FrameJsonObject.Scale.Y := 0;
    //TAnimator.AnimateFloat(FrameJsonObject, 'Scale.Y', 1);
  end
  else if TabControlMode.ActiveTab = TabItemJSchema then
  begin
    ButtonMenu.Text := 'JSON Schema';
  end
  else
  begin
    LayoutMenu.Opacity := 0;
    TAnimator.AnimateFloat(LayoutMenu, 'Opacity', 1);
    ButtonMenu.Text := 'Back';
  end;
end;

procedure TFormMain.TimerRepaintTimer(Sender: TObject);
begin
  Invalidate;
end;

initialization
  {$IFDEF DEBUG}
  ReportMemoryLeaksOnShutdown := True;
  {$ENDIF}

end.

