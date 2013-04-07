unit options;

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

