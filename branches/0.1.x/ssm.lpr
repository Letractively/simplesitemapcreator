program ssm;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, main, LResources, synacode, httpsend, about, options;

{$IFDEF WINDOWS}{$R ssm.rc}{$ENDIF}

begin
  {$I ssm.lrs}
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.CreateForm(TfrmAbout, frmAbout);
  Application.CreateForm(TfrmOptions, frmOptions);
  Application.Run;
end.

