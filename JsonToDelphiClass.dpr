program JsonToDelphiClass;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMainForm in 'uMainForm.pas' {MainForm},
  Pkg.Json.Mapper in 'Pkg.Json.Mapper.pas',
  uSaveUnitForm in 'uSaveUnitForm.pas' {SaveUnitForm},
  uGitHub in 'uGitHub.pas',
  FMX.ConstrainedForm in 'FMX.ConstrainedForm.pas' {/  IdSSLOpenSSLHeaders,},
  uUpdate in 'uUpdate.pas',
  uUpdateForm in 'uUpdateForm.pas' {UpdateForm},
  ChatGPT.Code in 'ChatGPT.Code.pas',
  ChatGPT.Code.JSON in 'ChatGPT.Code.JSON.pas',
  FMX.Memo.Style in 'FMX.Memo.Style.pas',
  ChatGPT.Code.Pascal in 'ChatGPT.Code.Pascal.pas';

{$R *.res}

{$WEAKLINKRTTI OFF}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TSaveUnitForm, SaveUnitForm);
  Application.CreateForm(TUpdateForm, UpdateForm);
  Application.Run;
end.
