program JsonToDelphiClass;

uses
  System.StartUpCopy,
  FMX.Forms,
  FMX.Types,
  JTD.Main in 'JTD.Main.pas' {FormMain},
  Json.Mapper in 'Json.Mapper.pas',
  ChatGPT.Code in 'ChatGPT.Code.pas',
  ChatGPT.Code.JSON in 'ChatGPT.Code.JSON.pas',
  FMX.Memo.Style in 'FMX.Memo.Style.pas',
  ChatGPT.Code.Pascal in 'ChatGPT.Code.Pascal.pas',
  HGM.MaterialDesignStyle in 'HGM.MaterialDesignStyle.pas',
  HGM.ObjectHolder in 'AsyncObjectHolder\HGM.ObjectHolder.pas',
  Json.Schema in 'Json.Schema.pas',
  JTD.Frame.JsonObject in 'JTD.Frame.JsonObject.pas' {FrameJsonObject: TFrame},
  JTD.Utils in 'JTD.Utils.pas',
  JTD.Frame.JsonSchema in 'JTD.Frame.JsonSchema.pas' {FrameJsonSchema: TFrame};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.

