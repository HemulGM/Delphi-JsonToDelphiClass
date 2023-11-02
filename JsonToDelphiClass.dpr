program JsonToDelphiClass;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMainForm in 'uMainForm.pas' {MainForm},
  Pkg.Json.Mapper in 'Pkg.Json.Mapper.pas',
  FMX.ConstrainedForm in 'FMX.ConstrainedForm.pas' {/  IdSSLOpenSSLHeaders,},
  ChatGPT.Code in 'ChatGPT.Code.pas',
  ChatGPT.Code.JSON in 'ChatGPT.Code.JSON.pas',
  FMX.Memo.Style in 'FMX.Memo.Style.pas',
  ChatGPT.Code.Pascal in 'ChatGPT.Code.Pascal.pas',
  HGM.MaterialDesignStyle in 'HGM.MaterialDesignStyle.pas',
  HGM.ObjectHolder in '..\..\AsyncObjectHolder\HGM.ObjectHolder.pas';

{$R *.res}

{$WEAKLINKRTTI OFF}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
