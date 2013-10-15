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
{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, Menus, FileCtrl, StrUtils, httpsend, SynEdit, LCLIntF,
  ComCtrls, Buttons, SynHighlighterHTML, SynHighlighterXML, XiPanel, XiButton,
  xmlparser, {$IFDEF MSWINDOWS} Windows,{$ENDIF}resolve, IniFiles;

{
  Makes use of Ararat Synapse http://www.ararat.cz/synapse/
  Mythcode XML Parser http://www.mythcode.org
  And XiControls http://www.matthewhipkin.co.uk/codelib/xicontrols/
}

type
  TConfigOptions = record
    editorFont: String;
    editorFontSize: Integer;
    ignoreFiles: TStrings;
    disableCustomTheme: Boolean;
    includeImages: Boolean;
  end;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    labelUpdate: TLabel;
    labelURL: TLabel;
    PageControl1: TPageControl;
    btnOptions: TSpeedButton;
    textCSV: TSynEdit;
    SynXMLSyn1: TSynXMLSyn;
    tabCSV: TTabSheet;
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
    procedure btnOptionsClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormPaint(Sender: TObject);
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
    btnAbout: TXiButton;
    btnCopy: TXiButton;
    btnClear: TXiButton;
    btnSave: TXiButton;
    btnGo: TXiButton;
    btnCancel: TXiButton;
    updatePanel: TXiPanel;
    workPanel: TXiPanel;
    procedure addLink(link: String; title: String; ref: String; header: String);
    procedure positionPanel;
    procedure GradientFillRect(Canvas: TCanvas; Rect: TRect;
                    StartColor, EndColor: TColor; Direction: TFillDirection);
  public
    { public declarations }
    inTag: Boolean;
    hrefTemp: String;
    options: TConfigOptions;
    procedure parseLinks(url: String);
    procedure addURL(url: String);
    procedure setTitle(url: String; title: String);
    procedure loadConfig;
    procedure saveConfig;
  end;

{ Simple type to store links }
type
  TLinkItem = record
    title: String;
    link: String;
    referrer: String;
    modtime: String;
    parsed: Boolean;
  end;

type
  TArray = array of string;

var
  frmMain: TfrmMain;

const
  APPVER = '0.1.8';
  CURRVER = 20130905;

implementation

uses about, options;

{$IFDEF MSWINDOWS}
function getWinVer: String;
var
  VerInfo: TOSVersioninfo;
begin
  VerInfo.dwOSVersionInfoSize := SizeOf(TOSVersionInfo);
  GetVersionEx(VerInfo);
  Result := 'Windows NT '+IntToStr(VerInfo.dwMajorVersion) + '.' + IntToStr(VerInfo.dwMinorVersion)
end;
{$ENDIF}

function explode(cDelimiter,  sValue : string; iCount : integer) : TArray;
var
  s : string;
  i,p : integer;
begin
  s := sValue; i := 0;
  while length(s) > 0 do
  begin
    inc(i);
    SetLength(result, i);
    p := pos(cDelimiter,s);
    if ( p > 0 ) and ( ( i < iCount ) OR ( iCount = 0) ) then
    begin
      result[i - 1] := copy(s,0,p-1);
      s := copy(s,p + length(cDelimiter),length(s));
    end else
    begin
      result[i - 1] := s;
      s :=  '';
    end;
  end;
end;

function sortLinks(Item1, Item2: Pointer): Integer;
var
  Comp1: ^TLinkItem absolute Item1;
  Comp2: ^TLinkItem absolute Item2;
begin
  { This needs to be more complex if I am to sort by subdirectory }
  Result := CompareText(Comp1^.link,Comp2^.link);
end;

procedure saveDebug(s: String);
var
  dFile: TStrings;
  f: String;
begin
  {$IFDEF MSWINDOWS}
  f := 'C:\tmp\debug.txt';
  {$ELSE}
  f := '/tmp/debug.txt';
  {$ENDIF}
  dFile := TStringList.Create;
  if FileExists(f) then dFile.LoadFromFile(f);
  dFile.Add(s);
  dFile.SaveToFile(f);
  dFile.Free;
end;

{ Get the contents of specified link }
procedure getURL(url: String; var response: String; var headers: String);
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
  http.UserAgent := 'Mozilla/4.0 (compatible; Simple Sitemap Creator '+APPVER+'; ' + OS + '; '+IntToStr(CURRVER)+'; +http://www.matthewhipkin.co.uk/apps/simplesitemapcreator/)';
  Application.ProcessMessages;
  if not HTTP.HTTPMethod('GET', url) then response := ''
  else
  begin
    l.LoadFromStream(Http.Document);
    response := l.Text;
    headers := http.Headers.Text;
  end;
  http.Free;
  l.Free;
end;

function getDate(header: String): String;
var
  lines: TStrings;
  x: integer;
  modline: String;
  tmps: String;
  ele: TArray;
  today: String;
  m: Array[1..12] of String[3];
begin
  //showmessage(header);
  //saveDebug(header);
  m[1] := 'Jan';
  m[2] := 'Feb';
  m[3] := 'Mar';
  m[4] := 'Apr';
  m[5] := 'May';
  m[6] := 'Jun';
  m[7] := 'Jul';
  m[8] := 'Aug';
  m[9] := 'Sep';
  m[10] := 'Oct';
  m[11] := 'Nov';
  m[12] := 'Dec';
  DateTimeToString(today,'yyyy-mm-dd',Now);
  lines := TStringList.Create;
  lines.Text := header;
  modline := '';
  for x := 0 to lines.Count -1 do
  begin
    if AnsiStartsStr('Last-Modified',lines[x]) then
    begin
      modline := lines[x];
      break;
    end;
  end;
  if modline <> '' then
  begin
    // Last-Modified: Sun, 17 Feb 2013 13:04:46 GMT
    x := Pos(': ',modline) + 2;
    tmps := Trim(Copy(modline,x,Length(modline) - x + 1));
    ele := explode(' ',tmps,0);
    // year at 3, month at 2, day at 1
    for x := 1 to 12 do
      if m[x] = ele[2] then
      begin
        if x < 10 then ele[2] := '0' + IntToStr(x)
        else ele[2] := IntToStr(x);
        break;
      end;
    Result := ele[3] + '-' + ele[2] + '-' + ele[1];
    //showmessage(tmps);
  end
  else Result := today;
  lines.Free;
end;

{ TfrmMain }

procedure TfrmMain.loadConfig;
var
  conf: TIniFile;
begin
  conf := TIniFile.Create(progdir + 'ssmc.ini');
  //options.disableCustomTheme := true;
  options.ignoreFiles := TStringList.Create;
  conf.Free;
end;

procedure TfrmMain.saveConfig;
var
  conf: TIniFile;
begin
  conf := TIniFile.Create(progdir + 'ssmc.ini');
  conf.WriteString('program','editorfont',options.editorFont);
  conf.WriteInteger('program','editorfontsize',options.editorFontSize);
  conf.WriteBool('program','includeimages',options.includeImages);
  conf.WriteBool('program','disabletheme',options.disableCustomTheme);
  conf.Free;
end;

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

procedure TfrmMain.setTitle(url: String; title: String);
var
  i: integer;
  l: ^TLinkItem;
begin
  if links.Count > 0 then
  begin
    for i := 0 to links.Count -1 do
    begin
      l := links[i];
      if l^.link = url then break;
    end;
    l^.title := title;
  end;
end;

{ GET the given link and parse the HTML for more A tags }
procedure TfrmMain.parseLinks(url: String);
var
  Parser: TXMLParser;
  html: String;
  header: String;
  link: String;
  title: String;
  add: Boolean;
  x,i: integer;
  tmp: String;
begin
  // If cancel button clicked exit procedure
  if stop = true then
  begin
    links.Clear;
    exit;
  end;
  // Ignore certain filetypes
  for x := 0 to ignoreFiles.Count -1 do
    if LowerCase(ExtractFileExt(url)) = ignoreFiles[x] then exit;
  // Retrieve the given URL
  getURL(url,html,header);
  // Set up variables
  Parser := TXMLParser.Create(html);
  while Parser.Next do
  begin
    Application.ProcessMessages;
    if Parser.TagType = ttBeginTag then
    begin
      if Lowercase(Parser.Name) = 'a' then
      begin
        link := trim(Parser.Value['href']);
      end;
      if Lowercase(Parser.Name) = 'title' then
        title := Parser.ContentSpaceTrimText;
    end;
    add := true;
    setTitle(url,title);
    for x := 0 to ignoreFiles.Count -1 do
      if Lowercase(ExtractFileExt(link)) = ignoreFiles[x] then add := false;
    if add = true then
    begin
      // Trim links of anchors etc
      x := Pos('#',link);
      if x > 0 then
      begin
        tmp := '';
        for i := 1 to x do
          tmp := tmp + link[x];
        link := tmp;
      end;
      addLink(link,title,url,header);
    end;
  end;
  Parser.Free;
end;

{ Attempt to add a link to the parse list, checking to see if it's a local link
  and whether the link is already there

  We pass the referring URL in the hope of being able to calculate the path to
  the file }
procedure TfrmMain.addLink(link: String; title: String; ref: String; header: String);
var
  x: integer;
  l: ^TLinkItem;
  tmp: TArray;
  proto: String;
  tmps: String;
  U: TURIParser;
  refU: TURiParser;
begin
  // Are we supposed to have stopped?
  if stop = true then exit;
  // URL methods we're not interested in
  if AnsiStartsStr('mailto:',link) then exit;
  if AnsiStartsStr('javascript:',link) then exit;
  if AnsiStartsStr('skype:',link) then exit;
  // Not interested in anchors
  if AnsiStartsStr('#',link) then exit;
  // Not interested in blank links
  if link = '' then exit;
  // Does the link start with http:// or https://
  if (AnsiStartsStr('http://',link) = true) or (AnsiStartsStr('https://',link) = true) then
  begin
    // If so, is it the same URL as is specified in textURL.Text?
    if AnsiStartsStr(textURL.Text,link) = false then exit;
  end
  // It doesn't start with http:// or https:// but is a local link
  else
  begin
    //saveDebug('link='+link+',ref='+ref);
    // TODO: Make more use of this TURIParser class
    U := TURIParser.Create(Nil);
    refU := TURIParser.Create(nil);
    U.ParseURI(textURL.Text);
    refU.ParseURI(ref);
    if U.Document <> '' then
    begin
      tmps := AnsiReplaceStr(ref,refU.Document,link);
      link := tmps;
    end
    else
    begin
      //saveDebug('link='+link+' ref='+ref);
      if refU.Document <> '' then
        tmps := AnsiReplaceStr(ref,refU.Document,link)
      else
        tmps := ref + link;
      link := tmps;
      //saveDebug('newlink='+link);
    end;
    U.Free;
    refU.Free;
  end;
  // Clean up any possible occurances of ../ ./ or //
  link := AnsiReplaceStr(link,'../','/');
  link := AnsiReplaceStr(link,'./','/');
  // Nasty way to clean up double slashes
  proto := Copy(link,1,Pos('://',link)+2);
  tmp := explode('/',link,0);
  tmps := proto;
  for x := 1 to High(tmp) do
  begin
    if tmp[x] <> '' then tmps := tmps + tmp[x] + '/';
  end;
  if not AnsiEndsStr('/',link) then tmps := Copy(tmps,1,Length(tmps)-1);
  link := tmps;
  // Make sure link is not already in the list
  for x := 0 to links.Count -1 do
  begin
    l := links[x];
    if l^.link = link then exit;
  end;
  // Add the link info to the list of links to be parsed
  new(l);
  l^.title := title;
  l^.link := link;
  l^.referrer := ref;
  l^.parsed := false;
  l^.modtime := getDate(header);
  links.Add(l);
  Application.ProcessMessages;
  // Set status caption to show number of links found
  labelCount.Caption := IntToStr(links.Count) + ' links found';
  // Parse link
  parseLinks(l^.link);
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
  ignoreFiles.Add('.tar');
  ignoreFiles.Add('.ogg');
  ignoreFiles.Add('.avi');
  ignoreFiles.Add('.mp4');
  //loadConfig;
  // UI tweaks
  if not options.disableCustomTheme then
  begin
    btnAbout := TXiButton.Create(Self);
    btnAbout.Parent := frmMain;
    btnAbout.Left := 8;
    btnAbout.Width := 75;
    btnAbout.Height := 25;
    btnAbout.Top := frmMain.ClientHeight - btnAbout.Height - 5;
    btnAbout.Caption := 'About';
    btnAbout.ColorScheme := csNeoSky;
    btnAbout.Visible := true;
    btnAbout.BringToFront;
    btnAbout.Anchors := [akBottom,akLeft];
    btnAbout.OnClick := btnAboutClick;
    btnCopy := TXiButton.Create(Self);
    btnCopy.Parent := frmMain;
    btnCopy.Left := 178;
    btnCopy.Width := 75;
    btnCopy.Height := 25;
    btnCopy.Top := frmMain.ClientHeight - btnCopy.Height - 5;
    btnCopy.Caption := 'Copy';
    btnCopy.ColorScheme := csNeoSky;
    btnCopy.Visible := true;
    btnCopy.BringToFront;
    btnCopy.Anchors:=[akBottom,akRight];
    btnCopy.OnClick := menuCopyClick;
    btnClear := TXiButton.Create(Self);
    btnClear.Parent := frmMain;
    btnClear.Left := 258;
    btnClear.Width := 75;
    btnClear.Height := 25;
    btnClear.Top := frmMain.ClientHeight - btnClear.Height - 5;
    btnClear.Caption := 'Clear';
    btnClear.ColorScheme := csNeoSky;
    btnClear.Visible := true;
    btnClear.BringToFront;
    btnClear.Anchors:=[akBottom,akRight];
    btnClear.OnClick := menuClearClick;
    btnSave := TXiButton.Create(Self);
    btnSave.Parent := frmMain;
    btnSave.Left := 338;
    btnSave.Width := 75;
    btnSave.Height := 25;
    btnSave.Top := frmMain.ClientHeight - btnSave.Height - 5;
    btnSave.Caption := 'Save';
    btnSave.ColorScheme := csNeoSky;
    btnSave.Visible := true;
    btnSave.BringToFront;
    btnSave.Anchors:=[akBottom,akRight];
    btnSave.OnClick := menuSaveClick;
    btnGo := TXiButton.Create(Self);
    btnGo.Parent := frmMain;
    btnGo.Left := 415;
    btnGo.Width := 75;
    btnGo.Height := 25;
    btnGo.Top := frmMain.ClientHeight - btnGo.Height - 5;
    btnGo.Caption := 'Go';
    btnGo.ColorScheme := csNeoGrass;
    btnGo.Visible := true;
    btnGo.BringToFront;
    btnGo.Anchors:=[akBottom,akRight];
    btnGo.OnClick := btnGoClick;
  end;
  workPanel := TXiPanel.Create(Self);
  workPanel.Parent := frmMain;
  workPanel.Width := 170;
  workPanel.Height := 96;
  workPanel.Top := 0;
  workPanel.Left := 0;
  workPanel.ColorScheme := XiPanel.csGrass;
  workPanel.Visible := false;
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
  btnCancel.OnClick := btnCancelClick;
  updatePanel := TXiPanel.Create(self);
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
  textHTML.Lines.Clear;
  textXML.Lines.Clear;
  textCSV.Lines.Clear;
  PageControl1.ActivePage := tabHTML;
end;

{ Stuff to do on program close }
procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  // Save URL history to file
  textURL.Items.SaveToFile(progdir + 'history.txt');
  ignoreFiles.Free;
end;

procedure TfrmMain.FormPaint(Sender: TObject);
begin
  if not options.disableCustomTheme then
    GradientFillRect(frmMain.Canvas, Classes.Rect(0, 0, frmMain.ClientWidth,
      frmMain.ClientHeight), clWhite, $00FFE0C1, fdVertical);
end;

{ Form resize }
procedure TfrmMain.FormResize(Sender: TObject);
begin
  positionPanel;
end;

procedure TfrmMain.FormShow(Sender: TObject);
begin

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
  if PageControl1.ActivePage = tabCSV then
  begin
    SaveDialog1.Filter := 'CSV Files (*.csv)|*.csv|All Files (*.*)|*.*';
    if (SaveDialog1.Execute) and (SaveDialog1.FileName <> '') then
    begin
      textCSV.Lines.SaveToFile(SaveDialog1.FileName);
      labelInfo.Caption := 'Saved to ' + SaveDialog1.FileName + '.';
    end;
  end;
end;

procedure TfrmMain.updatesTimerTimer(Sender: TObject);
var
  response: String;
  header: String;
  newVer: Boolean;
begin
  updatesTimer.Enabled := false;
  // Check for a new version comparing the CURRVER variable to the value returned
  newVer := false;
  try
    getURL('http://www.matthewhipkin.co.uk/ssmc.txt',response,header);
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
  U: TURIParser;
  commentText: TStrings;
  startTime: TDateTime;
  endTime: TDateTime;
  ss: String;
  tmp: String;
begin
  // Start the clock!
  startTime := Now;
  // Reset variables;
  commentText := TStringList.Create;
  stop := false;
  labelInfo.Caption := '';
  workPanel.Caption := 'Please wait';
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
  if (AnsiRightStr(textURL.Text,1) <> '/') then
  begin
    U:=TURIParser.Create(Nil);
    U.ParseURI(textURL.Text);
    if U.Document = '' then textURL.Text := textURL.Text + '/';
    U.Free;
  end;
  // Add the URL to the history dropdown
  addURL(textURL.Text);
  // Process the site
  parseLinks(textURL.Text);
  // Stop the clock!
  endTime := Now;
  // Sort links
  links.Sort(@sortLinks);
  // Generate comments text header
  DateTimeToString(ss,'yyyy-mm-dd hh:nn',startTime);
  commentText.Add('<!-- Sitemap for '+textURL.Text+' generated at ' + ss + '-->');
  tmp := '';
  DateTimeToString(ss,'hh',(endTime-startTime));
  tmp := tmp + ss + 'h ';
  DateTimeToString(ss,'nn',(endTime-startTime));
  tmp := tmp + ss + 'm ';
  DateTimeToString(ss,'ss',(endTime-startTime));
  tmp := tmp + ss + 's';
  commentText.Add('<!-- ' + IntToStr(links.Count) + ' links discovered, runtime '+tmp+' -->');
  // Clear the HTML editor
  textHTML.Lines.Clear;
  textHTML.Lines.AddStrings(commentText);
  // Create an unordered list
  textHTML.Lines.Add('<ul>');
  // Clear the XML editor
  textXML.Lines.Clear;
  // Create XML header
  textXML.Lines.Add('<?xml version="1.0" encoding="UTF-8"?>');
  textXML.Lines.AddStrings(commentText);
  textXML.Lines.Add('<urlset xmlns="http://www.google.com/schemas/sitemap/0.84"');
  textXML.Lines.Add('xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"');
  textXML.Lines.Add('xsi:schemaLocation="http://www.google.com/schemas/sitemap/0.84');
  textXML.Lines.Add('http://www.google.com/schemas/sitemap/0.84/sitemap.xsd">');
  // Clear the CSV editor
  textCSV.Lines.Clear;
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
    textXML.Lines.Add('    <lastmod>'+l^.modtime+'</lastmod>');
    textXML.Lines.Add('  </url>');
    // Add a CSV item
    textCSV.Lines.Add('"'+l^.link+'","'+l^.title+'"');
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
  commentText.Free;
end;

procedure TfrmMain.btnOptionsClick(Sender: TObject);
begin
  if frmOptions.ShowModal = mrOK then
  begin
    options.disableCustomTheme := frmOptions.checkDisableTheme.Checked;
    options.editorFont := frmOptions.textEditorFont.Text;
    options.editorFontSize := frmOptions.textEditorFontSize.Value;
    options.ignoreFiles.Clear;
    options.ignoreFiles.AddStrings(frmOptions.listIgores.Items);
    options.includeImages := frmOptions.checkIncludeImages.Checked;
    saveConfig;
  end;
end;

{ Cancel button/menu click event }
procedure TfrmMain.btnCancelClick(Sender: TObject);
begin
  stop := true;
  workPanel.Caption := 'Stopping';
  Application.ProcessMessages;
end;

{ Show about dialog }
procedure TfrmMain.btnAboutClick(Sender: TObject);
begin
  frmAbout.ShowModal;
end;

{ Taken from XiPanel.pas
  due to an incompatibility with Lazarus/FreePascal its better to paint the
  gradient directly onto the form's canvas }

procedure TfrmMain.GradientFillRect(Canvas: TCanvas; Rect: TRect;
                StartColor, EndColor: TColor; Direction: TFillDirection);
var
  Steps: Integer;
  StartR, StartG, StartB, EndR, EndG, EndB: Byte;
  CrrR, CrrG, CrrB: Double;
  IncR, IncG, incB: Double;
  i: integer;
begin
  case Direction of
    fdVertical:   Steps:= Rect.Bottom - Rect.Top;
    fdHorizontal: Steps:= Rect.Right - Rect.Left;
    fdDiagonal:   Steps:= Rect.Bottom - Rect.Top + Rect.Right - Rect.Left;
  end;

  StartR:= GetRValue(StartColor);  EndR:= GetRValue(EndColor);
  StartG:= GetGValue(StartColor);  EndG:= GetGValue(EndColor);
  StartB:= GetBValue(StartColor);  EndB:= GetBValue(EndColor);

  IncR:= (EndR - StartR) / steps;
  IncG:= (EndG - StartG) / steps;
  IncB:= (EndB - StartB) / steps;

  CrrR:= StartR;
  CrrG:= StartG;
  CrrB:= StartB;

  for i:= 0 to Steps do begin
    Canvas.Pen.Color:= RGB(Round(CrrR), Round(CrrG), Round(CrrB));
    case Direction of
      fdVertical:   begin
                      Canvas.MoveTo(Rect.Left, i);
                      Canvas.LineTo(Rect.Right + Rect.Left, i);
                    end;
      fdHorizontal: begin
                      Canvas.MoveTo(i, Rect.Top);
                      Canvas.LineTo(i, Rect.Top + Rect.Bottom);
                    end;
      fdDiagonal:   begin
                      Canvas.MoveTo(i, Rect.Top);
                      Canvas.LineTo(Rect.Left, i);
                    end;
    end;
    CrrR:= CrrR + IncR;
    CrrG:= CrrG + IncG;
    CrrB:= CrrB + IncB;
  end;
end;


initialization
  {$I main.lrs}

end.

