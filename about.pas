unit about;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, {$IFDEF MSWINDOWS} ShellAPI, Windows {$ELSE} Unix {$ENDIF};

type

  { TfrmAbout }

  TfrmAbout = class(TForm)
    btnOK: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    procedure btnOKClick(Sender: TObject);
    procedure Label4Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  frmAbout: TfrmAbout;

implementation

{ TfrmAbout }

procedure TfrmAbout.btnOKClick(Sender: TObject);
begin
  ModalResult := mrOK;
end;

procedure TfrmAbout.Label4Click(Sender: TObject);
begin
  {$IFDEF MSWINDOWS}
  ShellExecute(handle, Nil, 'http://www.matthewhipkin.co.uk', '', '.', SW_SHOWNORMAL);
  {$ELSE}
  Shell('xdg-open http://www.matthewhipkin.co.uk');
  {$ENDIF}
end;

initialization
  {$I about.lrs}

end.

