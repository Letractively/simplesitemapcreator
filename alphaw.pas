unit alphaw;
(*
Simple Sitemap Creator
Copyright (C) 2010 Matthew Hipkin <http://www.matthewhipkin.co.uk>

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
// TODO: DELETE THIS UNIT WHEN MATURE ENOUGH
uses
  Classes, SysUtils, FileUtil, IpHtml, LResources, Forms, Controls, Graphics,
  Dialogs, StdCtrls, LCLIntF;

type

  { TfrmAlphaWarning }

  TfrmAlphaWarning = class(TForm)
    btnOK: TButton;
    checkDontShow: TCheckBox;
    htmlPanel: TIpHtmlPanel;
    procedure btnOKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure htmlPanelHotClick(Sender: TObject);
  private
    { private declarations }
    html: TStrings;
  public
    { public declarations }
  end;

var
  frmAlphaWarning: TfrmAlphaWarning;

implementation

{ TfrmAlphaWarning }

procedure TfrmAlphaWarning.FormCreate(Sender: TObject);
begin
  html := TStringList.Create;
  html.Add('<html>');
  html.Add('<body link="#FF0000">');
  html.Add('<b>Important!</b><br><br>');
  html.Add('This software is very much alpha software at the moment, this means that it sort of works, but could go horribly wrong for no reason at all.<br><br>');
  html.Add('If you find this program trying to crawl the entire internet I would really like to know so I can fix it!<br><br>');
  html.Add('There are several ways to report bugs:<br>');
  html.Add('Raise an issue on the <a href="http://code.google.com/p/simplesitemapcreator">Google Code</a> project page.<br>');
  html.Add('Use the <a href="http://www.matthewhipkin.co.uk/contact/">contact form</a> on my website.<br>');
  html.Add('Send me a tweet on <a href="http://twitter.com/hippy2094">twitter</a>.');
  html.Add('</body>');
  html.Add('</html>');
  htmlPanel.SetHtmlFromStr(html.Text);
end;

procedure TfrmAlphaWarning.btnOKClick(Sender: TObject);
begin
  ModalResult := mrOK;
end;

procedure TfrmAlphaWarning.FormDestroy(Sender: TObject);
begin
  html.Free;
end;

procedure TfrmAlphaWarning.htmlPanelHotClick(Sender: TObject);
var
  NodeA: TIpHtmlNodeA;
begin
  if htmlPanel.HotNode is TIpHtmlNodeA then
  begin
    NodeA:=TIpHtmlNodeA(htmlPanel.HotNode);
    OpenURL(NodeA.HRef);
  end;
end;

initialization
  {$I alphaw.lrs}

end.

