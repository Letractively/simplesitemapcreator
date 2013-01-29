unit main;
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
  StdCtrls, ExtCtrls, Menus, FileCtrl, StrUtils, httpsend, SynEdit, LCLIntF,
  ComCtrls{$IFDEF MSWINDOWS}, Windows{$ENDIF}, SynHighlighterHTML,
  SynHighlighterXML, XiPanel, XiButton, xmlparser;

{
  Makes use of Ararat Synapse http://www.ararat.cz/synapse/doku.php/start
  And XiControls http://www.matthewhipkin.co.uk/codelib/xicontrols/
}

type

  { TfrmMain }

  TfrmMain = class(TForm)
    labelUpdate: TLabel;
    labelURL: TLabel;
    PageControl1: TPageControl;
    SynXMLSyn1: TSynXMLSyn;
    textXML: TSynEdit;
    textHTML: TSynEdit;
    tabHTML: TTabSheet;
    tabGoogle: TTabSheet;
    textURL: TFilterComboBox;
    labelInfo: TLabel;
    labelCount: TLabel;
    menuCopy: TMenuItem;
    menuClear: TMenuItem;
    menuSave: TMenuItem;
    editMenu: TPopupMenu;
    SaveDialog1: TSaveDialog;
    SynHTMLSyn1: TSynHTMLSyn;
    updatesTimer: TTimer;
    procedure btnAboutClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure btnGoClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure labelUpdateClick(Sender: TObject);
    procedure menuClearClick(Sender: TObject);
    procedure menuCopyClick(Sender: TObject);
    procedure menuSaveClick(Sender: TObject);
    procedure updatesTimerTimer(Sender: TObject);
  private
    { private declarations }
    links: TList;
    stop: boolean;
    progdir: String;
    ignoreFiles: TStrings;
    bgPanel: TXiPanel;
    btnAbout: TXiButton;
    btnCopy: TXiButton;
    btnClear: TXiButton;
    btnSave: TXiButton;
    btnGo: TXiButton;
    btnCancel: TXiButton;
    updatePanel: TXiPanel;
    workPanel: TXiPanel;
    function getURL(url: String): String;
    procedure addLink(link: String; title: String);
    procedure positionPanel;
  public
    { public declarations }
    inTag: Boolean;
    hrefTemp: String;
    procedure parseLinks(url: String);
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
  APPVER = '0.1.5';
  CURRVER = 20130128;

implementation

uses about, alphaw;

{$IFDEF MSWINDOWS}
function getWinVer: String;
var
  VerInfo: TOSVersioninfo;
begin
  VerInfo.dwOSVersionInfoSize := SizeOf(TOSVersionInfo);
  GetVersionEx(VerInfo);
  Result := 'Windows '+IntToStr(VerInfo.dwMajorVersion) + '.' + IntToStr(VerInfo.dwMinorVersion)
end;
{$ENDIF}

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
  if updatePanel.Visible then
    updatePanel.Left := (frmMain.ClientWidth - updatePanel.Width) - 5;
  with workPanel do
  begin
    Left := (frmMain.Width div 2) - (Width div 2);
    Top := (frmMain.Height div 2) - (Height div 2);
  end;
end;

procedure TfrmMain.parseLinks(url: String);
var
  Parser: TXMLParser;
  hrefs: TStrings;
  html: String;
  link: String;
  title: String;
  add: Boolean;
  x: integer;
begin
  // If cancel button clicked exit procedure
  if stop = true then exit;
  // Ignore certain filetypes
  for x := 0 to ignoreFiles.Count -1 do
    if LowerCase(ExtractFileExt(url)) = ignoreFiles[x] then exit;
  // Retrieve the given URL
  html := getURL(url);
  // Set up variables
  hrefs := TStringList.Create;
  Parser := TXMLParser.Create(html);
  while Parser.Next do
  begin
    if Parser.TagType = ttBeginTag then
    begin
      if Parser.Name = 'a' then
      begin
        link := trim(Parser.Value['href']);
        title := Parser.ContentSpaceTrimText;
        if title = '' then title := link;
      end;
    end;
    add := true;
    for x := 0 to ignoreFiles.Count -1 do
      if Lowercase(ExtractFileExt(link)) = ignoreFiles[x] then add := false;
    if add = true then addLink(link,title);
  end;
  hrefs.Free;
  Parser.Free;
end;

{ GET the given link and parse the HTML for more A tags }
{procedure TfrmMain.parseLinks(url: String);
var
  h: String;
  t: String;
  i: integer;
  x: integer;
  hrefs: TStrings;
  html: String;
  inhref: Boolean;
  badChars: set of char;
  breakChars: set of char;
  link: String;
  title: String;
  ls: integer;
  add: Boolean;
  s, e: integer;
begin
  // If cancel button clicked exit procedure
  if stop = true then exit;
  // Ignore certain filetypes
  for x := 0 to ignoreFiles.Count -1 do
    if LowerCase(ExtractFileExt(url)) = ignoreFiles[x] then exit;
  // Retrieve the given URL
  html := getURL(url);
  // Set up variables;
  badChars := [#10,#13];
  breakChars := [' ','>','#','?'];
  inhref := false;
  hrefs := TStringList.Create;
  // Extract any <a></a> tags
  h := '';
  for i := 1 to Length(html) do
  begin
    //if (html[i] = '<') and (Lowercase(html[i+1]) = 'a') then
    if Lowercase(Copy(html,i,3)) = '<a ' then
    begin
      h := '';
      inhref := true;
    end;
    if Copy(html,i,4) = '</a>' then
    begin
      inhref := false;
      h := h + '</a>';
      hrefs.Add(h);
    end;
    if (inhref = true) and (html[i] in badChars = false) then h := h + html[i];
  end;
  // Process tags before adding to list
  for i := 0 to hrefs.Count -1 do
  begin
    (* If there is just plain text between the opening and closing tags just add
       the text, otherwise check for a title or alt attribute, if niether of
       those exist, we'll look for an <img> tag and take the title, alt or src
       to use *)
    t := hrefs[i];
    inhref := false;
    h := '';
    link := '';
    title := '';
    // Step 1: Retrieve link from href
    for x := 1 to Length(t) do
    begin
      (* HTML is a pain in the arse as attributes can be enclosed in quotes,
         apostrophes or nothing! *)
      if Copy(t,x,5) = 'href=' then
      begin
        h := '';
        inhref := true;
      end;
      if (t[x] in breakChars) and (inhref = true) then
      begin
        inhref := false;
        link := Copy(h,6,Length(h)-6);
        link := TrimSet(link,[' ','"','''']);
        ls := x+1;
      end;
      if inhref = true then h := h + t[x];
    end;
    // Step 2: Get text between <a></a> tags
    title := Copy(t,ls,(Length(t)-ls)-3);
    // There are tags inside
    if PosSet(['<','>'],title) > 0 then
    begin
      // Try and just extract an alt or title attribute
      if Pos('alt=',title) > 0 then
      begin
        s := Pos('alt=',title)+4;
        e := PosSetEx(['"',''''],title,s+1);
        title := Copy(title,s,e-s);
      end
      else if Pos('title=',title) > 0 then
      begin
        s := Pos('title=',title)+7;
        e := PosSetEx(['"',''''],title,s+1);
        title := Copy(title,s,e-s);
      end
      // No alt or title tags, try and grab an image filename
      else if Pos('<img',title) > 0 then
      begin
        s := Pos('src=',title)+4;
        e := PosSetEx(['"',''''],title,s+1);
        title := Copy(title,s,e-s);
      end;
      title := TrimSet(title,[' ','"','''']);
    end
    // If the title is empty just set it to the link
    else if title = '' then title := link;
    // Ignore certain filetypes
    add := true;
    for x := 0 to ignoreFiles.Count -1 do
      if Lowercase(ExtractFileExt(link)) = ignoreFiles[x] then add := false;
    if add = true then addLink(link,title);
  end;
  hrefs.Free;
end;       }

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
  if ((AnsiLeftStr(link,7) = 'http://') or (AnsiLeftStr(link,8) = 'https://')) and (Pos(textURL.Text,link) = 0) then add := false;
  if (Pos('mailto:',link) > 0) or (Pos('javascript:',link) > 0) then add := false;
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
    if Copy(l^.link,1,Length(textURL.Text)) <> textURL.Text then
      parseLinks(textURL.Text + l^.link)
    else
      parseLinks(l^.link)
  end;
end;

{ Get the contents of specified link }
function TfrmMain.getURL(url: String): String;
var
  http: THTTPSend;
  l: TStrings;
  OS: String;
begin
  {$ifdef Windows}
  OS := getWinVer;
  {$endif}
  {$ifdef Linux}
  OS := 'Linux';
  {$endif}
  {$ifdef FreeBSD}
  OS := 'FreeBSD';
  {$endif}
  {$ifdef Darwin}
  OS := 'OSX';
  {$endif}
  http := THTTPSend.Create;
  l := TStringList.Create;
  http.UserAgent := 'Mozilla/4.0 (compatible; Simple Sitemap Creator '+APPVER+'; ' + OS + '; '+IntToStr(CURRVER)+'; +http://www.matthewhipkin.co.uk/)';
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

{ Setup stuff }
procedure TfrmMain.FormCreate(Sender: TObject);
begin
  // Set initial variables
  links := TList.Create;
  inTag := false;
  stop := false;
  frmMain.Caption := 'Simple Sitemap Creator '+APPVER;
  Application.Title := frmMain.Caption;
  labelInfo.Caption := '';
  progdir := GetUserDir + '.ssm' + PathDelim;
  // Check for program settings directory, if it doesn't exist create it
  if not DirectoryExists(progdir) then mkdir(progdir);
  // Load the URL history into the URL field
  if FileExists(progdir + 'history.txt') then textURL.Items.LoadFromFile(progdir + 'history.txt');
  textURL.Text := 'http://';
  // Filetypes to ignore, add as needed
  ignoreFiles := TStringList.Create;
  ignoreFiles.Add('.png');
  ignoreFiles.Add('.jpg');
  ignoreFiles.Add('.gif');
  ignoreFiles.Add('.mp3');
  ignoreFiles.Add('.wav');
  ignoreFiles.Add('.exe');
  ignoreFiles.Add('.bin');
  ignoreFiles.Add('.zip');
  ignoreFiles.Add('.gz');
  ignoreFiles.Add('.bz2');
  ignoreFiles.Add('.dmg');
  ignoreFiles.Add('.rar');
  ignoreFiles.Add('.7z');
  ignoreFiles.Add('.arj');
  // UI tweaks
//  textHTML.Font.Name := 'Consolas';
  bgPanel := TXiPanel.Create(Self);
  bgPanel.Parent := frmMain;
  bgPanel.Align := alClient;
  bgPanel.BorderStyle := bsNone;
  bgPanel.BevelInner := bvNone;
  bgPanel.BevelOuter := bvNone;
  bgPanel.ColorScheme := XiPanel.csSky;
  bgPanel.SendToBack;
  labelURL.Parent := bgPanel;
  labelInfo.Parent := bgPanel;
  btnAbout := TXiButton.Create(Self);
  btnAbout.Parent := frmMain;
  btnAbout.Top := 352;
  btnAbout.Left := 8;
  btnAbout.Width := 75;
  btnAbout.Height := 25;
  btnAbout.Caption := 'About';
  btnAbout.ColorScheme := csNeoSky;
  btnAbout.Visible := true;
  btnAbout.BringToFront;
  btnAbout.Anchors := [akBottom,akLeft];
  btnAbout.OnClick := @btnAboutClick;
  btnCopy := TXiButton.Create(Self);
  btnCopy.Parent := frmMain;
  btnCopy.Top := 352;
  btnCopy.Left := 178;
  btnCopy.Width := 75;
  btnCopy.Height := 25;
  btnCopy.Caption := 'Copy';
  btnCopy.ColorScheme := csNeoSky;
  btnCopy.Visible := true;
  btnCopy.BringToFront;
  btnCopy.Anchors:=[akBottom,akRight];
  btnCopy.OnClick := @menuCopyClick;
  btnClear := TXiButton.Create(Self);
  btnClear.Parent := frmMain;
  btnClear.Top := 352;
  btnClear.Left := 258;
  btnClear.Width := 75;
  btnClear.Height := 25;
  btnClear.Caption := 'Clear';
  btnClear.ColorScheme := csNeoSky;
  btnClear.Visible := true;
  btnClear.BringToFront;
  btnClear.Anchors:=[akBottom,akRight];
  btnClear.OnClick := @menuClearClick;
  btnSave := TXiButton.Create(Self);
  btnSave.Parent := frmMain;
  btnSave.Top := 352;
  btnSave.Left := 338;
  btnSave.Width := 75;
  btnSave.Height := 25;
  btnSave.Caption := 'Save';
  btnSave.ColorScheme := csNeoSky;
  btnSave.Visible := true;
  btnSave.BringToFront;
  btnSave.Anchors:=[akBottom,akRight];
  btnSave.OnClick := @menuSaveClick;
  btnGo := TXiButton.Create(Self);
  btnGo.Parent := frmMain;
  btnGo.Top := 352;
  btnGo.Left := 415;
  btnGo.Width := 75;
  btnGo.Height := 25;
  btnGo.Caption := 'Go';
  btnGo.ColorScheme := csNeoGrass;
  btnGo.Visible := true;
  btnGo.BringToFront;
  btnGo.Anchors:=[akBottom,akRight];
  btnGo.OnClick := @btnGoClick;
  workPanel := TXiPanel.Create(Self);
  workPanel.Parent := frmMain;
  workPanel.Width := 170;
  workPanel.Height := 96;
  workPanel.Top := 0;
  workPanel.Left := 0;
  workPanel.ColorScheme := XiPanel.csGrass;
  workPanel.Visible := false;
//  workPanel.BringToFront;
  labelCount.Parent := workPanel;
  btnCancel := TXiButton.Create(Self);
  btnCancel.Parent := workPanel;
  btnCancel.Top := 64;
  btnCancel.Left := 48;
  btnCancel.Width := 75;
  btnCancel.Height := 25;
  btnCancel.Caption := 'Cancel';
  btnCancel.ColorScheme := csNeoRose;
  btnCancel.Visible := true;
  btnCancel.BringToFront;
  btnCancel.OnClick := @btnCancelClick;
  updatePanel := TXiPanel.Create(self);
  updatePanel.Parent := bgPanel;
  updatePanel.Top := 8;
  updatePanel.Height := 50;
  updatePanel.Width := 170;
  updatePanel.Left := 318;
  updatePanel.ColorScheme := XiPanel.csRose;
  labelUpdate.Parent := upDatePanel;
  labelUpdate.Align := alClient;
  labelUpdate.Alignment := taCenter;
  labelUpdate.Layout := tlCenter;
  labelUpdate.Caption := 'A new version is available';
  updatePanel.Visible := false;
  updatesTimer.Enabled := true;
//  textHTML.Font := textXML.Font;
  textHTML.Lines.Clear;
  textXML.Lines.Clear;
end;

{ Stuff to do on program close }
procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  // Save URL history to file
  textURL.Items.SaveToFile(progdir + 'history.txt');
  ignoreFiles.Free;
end;

{ Form resize }
procedure TfrmMain.FormResize(Sender: TObject);
begin
  positionPanel;
end;

procedure TfrmMain.FormShow(Sender: TObject);
var
  s: TStrings;
begin
  if FileExists(progdir + 'alpha.txt') then exit;
  s := TStringList.Create;
  if frmAlphaWarning.ShowModal = mrOK then
  begin
    if frmAlphaWarning.checkDontShow.Checked = true then
    begin
      s.Add('dontshow');
      s.SaveToFile(progdir + 'alpha.txt');
    end;
  end;
  s.Free;
end;

procedure TfrmMain.labelUpdateClick(Sender: TObject);
begin
  OpenURL('http://www.matthewhipkin.co.uk');
end;

{ Clear button/menu click event }
procedure TfrmMain.menuClearClick(Sender: TObject);
begin
  textHTML.Lines.Clear;
  textXML.Lines.Clear;
  labelInfo.Caption := '';
end;

{ Copy button/menu click event }
procedure TfrmMain.menuCopyClick(Sender: TObject);
begin
  if PageControl1.ActivePage = tabHTML then
  begin
    textHTML.SelectAll;
    textHTML.CopyToClipboard;
    labelInfo.Caption := 'Copied to clipboard.';
  end;
  if PageControl1.ActivePage = tabGoogle then
  begin
    textXML.SelectAll;
    textXML.CopyToClipboard;
    labelInfo.Caption := 'Copied to clipboard.';
  end;
end;

{ Save button/menu click event }
procedure TfrmMain.menuSaveClick(Sender: TObject);
begin
  if PageControl1.ActivePage = tabHTML then
  begin
    SaveDialog1.Filter := 'HTML Files (*.html)|*.html|All Files (*.*)|*.*';
    if (SaveDialog1.Execute) and (SaveDialog1.FileName <> '') then
    begin
      textHTML.Lines.SaveToFile(SaveDialog1.FileName);
      labelInfo.Caption := 'Saved to ' + SaveDialog1.FileName + '.';
    end;
  end;
  if PageControl1.ActivePage = tabGoogle then
  begin
    SaveDialog1.Filter := 'XML Files (*.xml)|*.xml|All Files (*.*)|*.*';
    if (SaveDialog1.Execute) and (SaveDialog1.FileName <> '') then
    begin
      textXML.Lines.SaveToFile(SaveDialog1.FileName);
      labelInfo.Caption := 'Saved to ' + SaveDialog1.FileName + '.';
    end;
  end;
end;

procedure TfrmMain.updatesTimerTimer(Sender: TObject);
var
  response: String;
  newVer: Boolean;
begin
  updatesTimer.Enabled := false;
  // Check for a new version comparing the CURRVER variable to the value returned
  newVer := false;
  try
    response := getURL('http://www.matthewhipkin.co.uk/ssmc.txt');
    response := trim(response);
    if CURRVER < StrToInt(response) then newVer := true;
  except
    newVer := false;
  end;
  if newVer then
  begin
    updatePanel.Visible := true;
  end;
end;

{ Go button/menu click event }
procedure TfrmMain.btnGoClick(Sender: TObject);
var
  x: integer;
  l: ^TLinkItem;
  today: String;
begin
  // Reset variables;
  stop := false;
  labelInfo.Caption := '';
  workPanel.Caption := 'Please wait';
  DateTimeToString(today,'yyyy-mm-dd',Now);
  // Show and position the status panel
  workPanel.Visible := true;
  workPanel.BringToFront;
  positionPanel;
  // Disable all but the Cancel button
  textURL.Enabled := false;
  textHTML.Enabled := false;
  btnGo.Enabled := false;
  btnSave.Enabled := false;
  btnClear.Enabled := false;
  btnCopy.Enabled := false;
  PageControl1.Enabled := false;
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
  // Clear the XML editor
  textXML.Lines.Clear;
  // Create XML header
  textXML.Lines.Add('<?xml version="1.0" encoding="UTF-8"?>');
  textXML.Lines.Add('<urlset xmlns="http://www.google.com/schemas/sitemap/0.84"');
  textXML.Lines.Add('xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"');
  textXML.Lines.Add('xsi:schemaLocation="http://www.google.com/schemas/sitemap/0.84');
  textXML.Lines.Add('http://www.google.com/schemas/sitemap/0.84/sitemap.xsd">');
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
    // Add an XML item
    textXML.Lines.Add('  <url>');
    textXML.Lines.Add('    <loc>'+l^.link+'</loc>');
    textXML.Lines.Add('    <lastmod>'+today+'</lastmod>');
    textXML.Lines.Add('  </url>');
  end;
  // Set info caption
  labelInfo.Caption := IntToStr(links.Count) + ' links found.';
  if stop then labelInfo.Caption := 'Cancelled.';
  // Close list tag
  textHTML.Lines.Add('</ul>');
  // End XML
  textXML.Lines.Add('</urlset>');
  // Clear list of links
  links.Clear;
  // Hide the status panel
  workPanel.Visible := false;
  // Re-enable the standard controls
  textURL.Enabled := true;
  textHTML.Enabled := true;
  btnGo.Enabled := true;
  btnSave.Enabled := true;
  btnClear.Enabled := true;
  btnCopy.Enabled := true;
  PageControl1.Enabled := true;
end;

{ Cancel button/menu click event }
procedure TfrmMain.btnCancelClick(Sender: TObject);
begin
  stop := true;
  workPanel.Caption := 'Stopping';
end;

{ Show about dialog }
procedure TfrmMain.btnAboutClick(Sender: TObject);
begin
  frmAbout.ShowModal;
end;

initialization
  {$I main.lrs}

end.

