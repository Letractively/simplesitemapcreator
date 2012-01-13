unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, Menus, FileCtrl, StrUtils, FastHTMLParser, HTMLUtil,
  httpsend, SynEdit, SynHighlighterHTML;

{
  Makes use of Ararat Synapse http://www.ararat.cz/synapse/doku.php/start
  And TjsFastHTMLParser http://www.daniweb.com/forums/thread64972.html
}

type

  { TfrmMain }

  TfrmMain = class(TForm)
    btnGo: TButton;
    btnCancel: TButton;
    btnSave: TButton;
    btnClear: TButton;
    btnCopy: TButton;
    btnAbout: TButton;
    labelURL: TLabel;
    textURL: TFilterComboBox;
    labelInfo: TLabel;
    labelCount: TLabel;
    menuCopy: TMenuItem;
    menuClear: TMenuItem;
    menuSave: TMenuItem;
    panelWork: TPanel;
    editMenu: TPopupMenu;
    SaveDialog1: TSaveDialog;
    SynHTMLSyn1: TSynHTMLSyn;
    textHTML: TSynEdit;
    procedure btnAboutClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure btnGoClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure menuClearClick(Sender: TObject);
    procedure menuCopyClick(Sender: TObject);
    procedure menuSaveClick(Sender: TObject);
  private
    { private declarations }
    links: TList;
    stop: boolean;
    progdir: String;
    procedure OnTag(T: string);
    procedure OnText(T: String);
    function getURL(url: String): String;
    procedure addLink(link: String; title: String);
    procedure positionPanel;
  public
    { public declarations }
    inTag: Boolean;
    hrefTemp: String;
    procedure parseLinks(link: String);
    procedure addURL(url: String);
  end;

{ Simple type to store links }
type
  TLinkItem = record
    title: String;
    link: String;
    parsed: Boolean;
  end;

var
  frmMain: TfrmMain;

const
  APPVER = '0.1a';
  CURRVER = '20120113';

implementation

uses about;

{ TfrmMain }

{ Add URL to the drop down list }
procedure TfrmMain.addURL(url: String);
var
  x: integer;
  add: boolean;
begin
  add := true;
  // Check to see if the URL already exists
  for x := 0 to textURL.Items.Count-1 do
  begin
    if textURL.Items[x] = url then add := false;
  end;
  // If the URL was not found add it to the list
  if add then textURL.Items.Add(textURL.Text);
end;

{ Position the status panel at the centre of the form }
procedure TfrmMain.positionPanel;
begin
  with panelWork do
  begin
    Left := (frmMain.Width div 2) - (Width div 2);
    Top := (frmMain.Height div 2) - (Height div 2);
  end;
end;

{ GET the given link and parse the HTML for more A tags }
procedure TfrmMain.parseLinks(link: String);
var
  Parser: TjsFastHTMLParser;
  filec: TStrings;
begin
  // If cancel button clicked exit procedure
  if stop = true then exit;
  filec := TStringList.Create;
  // Retrieve the given URL
  filec.Text := getURL(link);
  // Parse the response
  Parser := tjsFastHTMLParser.Create(PChar(filec.Text));
  Parser.OnFoundTag := @OnTag;
  Parser.OnFoundText := @OnText;
  Parser.Exec;
  Parser.Free;
  filec.Free;
end;

{ Attempt to add a link to the parse list, checking to see if it's a local link
  and whether the link is already there }
procedure TfrmMain.addLink(link: String; title: String);
var
  x: integer;
  l: ^TLinkItem;
  add: Boolean;
begin
  add := true;
  // Check for a valid URL before adding
  if (AnsiLeftStr(link,7) = 'http://') and (Pos(textURL.Text,link) = 0) then add := false;
  if Pos('mailto:',link) > 0 then add := false;
  if link = '' then add := false;
  if (AnsiLeftStr(link,1) = '/') and (link <> '/') then link := Copy(link,2,Length(link)-1);
  // Make sure link is not already in the list
  for x := 0 to links.Count -1 do
  begin
    l := links[x];
    if l^.link = link then add := false;
  end;
  if add then
  begin
    // Add the link info to the list of links to be parsed
    new(l);
    l^.title := title;
    l^.link := link;
    l^.parsed := false;
    links.Add(l);
    Application.ProcessMessages;
    // Set status caption to show number of links found
    labelCount.Caption := IntToStr(links.Count) + ' links found';
    // Parse link
    parseLinks(textURL.Text + l^.link);
  end;
end;

{ Get the contents of specified link }
function TfrmMain.getURL(url: String): String;
var
  http: THTTPSend;
  l: TStrings;
begin
  http := THTTPSend.Create;
  l := TStringList.Create;
  http.UserAgent := 'Mozilla/4.0 (compatible; Simple Sitemap Creator '+APPVER+'; '+CURRVER+')';
  Application.ProcessMessages;
  if not HTTP.HTTPMethod('GET', url) then Result := ''
  else
  begin
    l.LoadFromStream(Http.Document);
    Result := l.Text;
  end;
  http.Free;
  l.Free;
end;

{ OnTagFound event, if its an A tag grab the href value }
procedure TfrmMain.OnTag(T: String);
var
  attrib: String;
begin
  hrefTemp := '';
  if LowerCase(Copy(T,1,2)) = '<a' then
  begin
    attrib := GetTagAttribute(T,'href');
    hrefTemp := GetAttributeValue(attrib);
    inTag := true;
  end;
  if LowerCase(T) = '</a>' then inTag := false;
end;

{ OnTextFound event }
procedure TfrmMain.OnText(T: String);
begin
  if inTag = true then
  begin
    addLink(hrefTemp,T);
  end;
end;

{ Setup stuff }
procedure TfrmMain.FormCreate(Sender: TObject);
begin
  // Set initial variables
  links := TList.Create;
  inTag := false;
  stop := false;
  frmMain.Caption := 'Simple Sitemap Creator '+APPVER;
  labelInfo.Caption := '';
  progdir := GetEnvironmentVariable('HOME')+'/.ssm/';
  // Check for program settings directory, if it doesn't exist create it
  if not DirectoryExists(progdir) then mkdir(progdir);
  // Load the URL history into the URL field
  if FileExists(progdir + 'history.txt') then textURL.Items.LoadFromFile(progdir + 'history.txt');
  textURL.Text := 'http://';
end;

{ Stuff to do on program close }
procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  // Save URL history to file
  textURL.Items.SaveToFile(progdir + 'history.txt');
end;

{ Form resize }
procedure TfrmMain.FormResize(Sender: TObject);
begin
  positionPanel;
end;

{ Clear button/menu click event }
procedure TfrmMain.menuClearClick(Sender: TObject);
begin
  textHTML.Lines.Clear;
  labelInfo.Caption := '';
end;

{ Copy button/menu click event }
procedure TfrmMain.menuCopyClick(Sender: TObject);
begin
  textHTML.SelectAll;
  textHTML.CopyToClipboard;
  labelInfo.Caption := 'Copied to clipboard.';
end;

{ Save button/menu click event }
procedure TfrmMain.menuSaveClick(Sender: TObject);
begin
  if (SaveDialog1.Execute) and (SaveDialog1.FileName <> '') then
  begin
    textHTML.Lines.SaveToFile(SaveDialog1.FileName);
    labelInfo.Caption := 'Saved to ' + SaveDialog1.FileName + '.';
  end;
end;

{ Go button/menu click event }
procedure TfrmMain.btnGoClick(Sender: TObject);
var
  x: integer;
  l: ^TLinkItem;
begin
  // Reset variables;
  stop := false;
  labelInfo.Caption := '';
  // Show and position the status panel
  panelWork.Visible := true;
  positionPanel;
  // Disable all but the Cancel button
  textURL.Enabled := false;
  textHTML.Enabled := false;
  btnGo.Enabled := false;
  btnSave.Enabled := false;
  btnClear.Enabled := false;
  btnCopy.Enabled := false;
  // If the entered URL doesn't have a trailing / add one
  if AnsiRightStr(textURL.Text,1) <> '/' then textURL.Text := textURL.Text + '/';
  // Add the URL to the history dropdown
  addURL(textURL.Text);
  // Process the site
  parseLinks(textURL.Text);
  // Clear the HTML editor
  textHTML.Lines.Clear;
  // Create an unordered list
  textHTML.Lines.Add('<ul>');
  // Loops through the links
  for x := 0 to links.Count -1 do
  begin
    // This keeps the program responding
    Application.ProcessMessages;
    // If the cancel button is clicked break the loop
    if stop = true then break;
    l := links[x];
    // Add a bullet item with the current link info
    textHTML.Lines.Add('  <li><a href="'+l^.link+'">'+l^.title+'</a></li>');
  end;
  // Set info caption
  labelInfo.Caption := IntToStr(links.Count) + ' links found.';
  if stop then labelInfo.Caption := 'Cancelled.';
  // Close list tag
  textHTML.Lines.Add('</ul>');
  // Clear list of links
  links.Clear;
  // Hide the status panel
  panelWork.Visible := false;
  // Re-enable the standard controls
  textURL.Enabled := true;
  textHTML.Enabled := true;
  btnGo.Enabled := true;
  btnSave.Enabled := true;
  btnClear.Enabled := true;
  btnCopy.Enabled := true;
end;

{ Cancel button/menu click event }
procedure TfrmMain.btnCancelClick(Sender: TObject);
begin
  stop := true;
  panelWork.Caption := 'Stopping';
end;

procedure TfrmMain.btnAboutClick(Sender: TObject);
begin
  frmAbout.ShowModal;
end;

initialization
  {$I main.lrs}

end.

