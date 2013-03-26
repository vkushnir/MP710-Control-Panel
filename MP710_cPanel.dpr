program MP710_cPanel;

uses
  Vcl.Forms,
  main in 'main.pas' {frmMain},
  utils in 'utils.pas',
  functions in 'functions.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'M710 Control Panel';
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
