unit about;
(*
Simple Sitemap Creator

Copyright (C) 2010 Matthew Hipkin <http://www.matthewhipkin.co.uk>
All rights reserved.

Redistribution and use in source and binary forms, with or without modification,
are permitted provided that the following conditions are met:

1. Redistributions of source code must retain the above copyright notice, this
   list of conditions and the following disclaimer.

2. Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

3. Neither the name of the copyright holder nor the names of its contributors
   may be used to endorse or promote products derived from this software without
   specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.

IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE
OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
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

