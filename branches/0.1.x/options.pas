unit options;
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

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, EditBtn, Spin;

type

  { TfrmOptions }

  TfrmOptions = class(TForm)
    btnOK: TButton;
    btnCancel: TButton;
    checkDisableTheme: TCheckBox;
    checkIncludeImages: TCheckBox;
    textEditorFont: TEditButton;
    FontDialog1: TFontDialog;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    listIgores: TListBox;
    textEditorFontSize: TSpinEdit;
    procedure textEditorFontButtonClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  frmOptions: TfrmOptions;

implementation

{ TfrmOptions }

procedure TfrmOptions.textEditorFontButtonClick(Sender: TObject);
begin
  if FontDialog1.Execute then
  begin
    textEditorFont.Text := FontDialog1.Font.Name;
    if textEditorFontSize.Value <> FontDialog1.Font.Size then
      textEditorFontSize.Value := FontDialog1.Font.Size;
  end;
end;

initialization
  {$I options.lrs}

end.

