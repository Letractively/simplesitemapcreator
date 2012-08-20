unit about;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, LclIntf, XiPanel, XiButton;

type

  { TfrmAbout }

  TfrmAbout = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    procedure btnOKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Label4Click(Sender: TObject);
  private
    { private declarations }
    bgPanel: TXiPanel;
    btnOK: TXiButton;
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

procedure TfrmAbout.FormCreate(Sender: TObject);
begin
  bgPanel := TXiPanel.Create(Self);
  bgPanel.Parent := frmAbout;
  bgPanel.Align := alClient;
  bgPanel.BorderStyle := bsNone;
  bgPanel.BevelInner := bvNone;
  bgPanel.BevelOuter := bvNone;
  bgPanel.ColorScheme := XiPanel.csSky;
  bgPanel.SendToBack;
  Label1.Parent := bgPanel;
  Label2.Parent := bgPanel;
  Label3.Parent := bgPanel;
  Label4.Parent := bgPanel;
  Label5.Parent := bgPanel;
  Label6.Parent := bgPanel;
  btnOK := TXiButton.Create(self);
  btnOK.Parent := bgPanel;
  btnOK.Top := 176;
  btnOK.Left := 192;
  btnOK.Width := 75;
  btnOK.Height := 25;
  btnOK.Caption := 'OK';
  btnOK.ColorScheme := csNeoSky;
  btnOK.Visible := true;
  btnOK.BringToFront;
  btnOK.OnClick := @btnOKClick;
end;

procedure TfrmAbout.Label4Click(Sender: TObject);
begin
  OpenURL('http://www.matthewhipkin.co.uk');
end;

initialization
  {$I about.lrs}

end.

