unit about;
(*
Simple Sitemap Creator
Copyright (C) 2010-2012 Matthew Hipkin <http://www.matthewhipkin.co.uk>

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
*)
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

