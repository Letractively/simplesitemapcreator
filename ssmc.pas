program ssmc;
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
uses CustApp, Classes, Sysutils, Strutils, xmlparser, 
     httpsend{$IFDEF MSWINDOWS}, Windows{$ENDIF};
     
(* THIS IS THE COMMAND LINE ONLY VERSION OF THE APPLICATION, THIS FILE IS 
   NOT RELATED AT ALL TO THE LAZARUS PROJECT *)
   
(* To build:
	    fpc ssmc.pas -Fusynapse *)
 
(*  Makes use of Ararat Synapse http://www.ararat.cz/synapse/doku.php/start
    And XiControls http://www.matthewhipkin.co.uk/codelib/xicontrols/ *)

{ Simple type to store links }
type
  TLinkItem = record
    title: String;
    link: String;
    parsed: Boolean;
  end;

type

  { THApplication }

  THApplication = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure parseLinks(url: String);
    procedure addLink(link: String; title: String);
    procedure showUsage;
    procedure checkUpdates;
  end;

const
  APPVER = '0.1.6';
  CURRVER = 20130206;

var
  Application: THApplication;
  links: TList;
  ignoreFiles: TStrings;
  url: String;
  outfile: String;
  outformat: String;

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

{ Get the contents of specified link }
function getURL(url: String): String;
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
  http.UserAgent := 'Mozilla/4.0 (compatible; Simple Sitemap Creator '+APPVER+' (cli); ' + OS + '; '+IntToStr(CURRVER)+'; +http://www.matthewhipkin.co.uk/apps/simplesitemapcreator/)';
  if not HTTP.HTTPMethod('GET', url) then Result := ''
  else
  begin
    l.LoadFromStream(Http.Document);
    Result := l.Text;
  end;
  http.Free;
  l.Free;
end;

{ GET the given link and parse the HTML for more A tags }
procedure THApplication.parseLinks(url: String);
var
  Parser: TXMLParser;
  html: String;
  link: String;
  title: String;
  add: Boolean;
  x: integer;
begin
  // If cancel button clicked exit procedure
//  if stop = true then exit;
  // Ignore certain filetypes
  for x := 0 to ignoreFiles.Count -1 do
    if LowerCase(ExtractFileExt(url)) = ignoreFiles[x] then exit;
  // Retrieve the given URL
  html := getURL(url);
  // Set up variables
  Parser := TXMLParser.Create(html);
  while Parser.Next do
  begin
    if Parser.TagType = ttBeginTag then
    begin
      if Parser.Name = 'a' then
      begin
        link := trim(Parser.Value['href']);
        title := Parser.ContentSpaceTrimText;
        if title = '' then
          title := Parser.Value['title'];
      end;
      if (Parser.Name = 'img') and (title = '') then
        title := Parser.Value['alt'];
      if (Parser.Name = 'img') and (title = '') then
        title := Parser.Value['title'];
      if (Parser.Name = 'img') and (title = '') then
        title := Parser.Value['src'];
    end;
    add := true;
    for x := 0 to ignoreFiles.Count -1 do
      if Lowercase(ExtractFileExt(link)) = ignoreFiles[x] then add := false;
    if add = true then addLink(link,title);
  end;
  Parser.Free;
end;

{ Attempt to add a link to the parse list, checking to see if it's a local link
  and whether the link is already there }
procedure THApplication.addLink(link: String; title: String);
var
  x: integer;
  l: ^TLinkItem;
  add: Boolean;
  edit: Boolean;
  lt: String;
begin
  add := true;
  edit := false;
  // Check for a valid URL before adding
  if ((AnsiLeftStr(link,7) = 'http://') or (AnsiLeftStr(link,8) = 'https://')) and (Pos(url,link) = 0) then add := false;
  if (Pos('mailto:',link) > 0) or (Pos('javascript:',link) > 0) then add := false;
  if AnsiLeftStr(link,1) = '#' then add := false;
  if link = '' then add := false;
  if (AnsiLeftStr(link,1) = '/') and (link <> '/') then link := Copy(link,2,Length(link)-1);
  // Make sure link is not already in the list
  for x := 0 to links.Count -1 do
  begin
    l := links[x];
    if l^.link = link then
    begin
      add := false;
      if l^.title = '' then edit := true;
      break;
    end;
  end;
  if (add = true) and (edit = false) then
  begin
    // Add the link info to the list of links to be parsed
    new(l);
    l^.title := title;
    l^.link := link;
    l^.parsed := false;
    links.Add(l);
    // Set status caption to show number of links found
    lt := IntToStr(links.Count) + ' links found';
    write(Chr(27),'[',Length(lt),'D',lt);
    // Parse link
    if Copy(l^.link,1,Length(url)) <> url then
      parseLinks(url + l^.link)
    else
      parseLinks(l^.link)
  end;
  // Edit link that has a blank title
  if (edit = true) and (add = false) then
  begin
    if (title <> '') and (l^.title = '') then
    begin
      l^.title := title;
    end;
  end;
end;

procedure THApplication.DoRun;
var 
  x: integer;
  l: ^TLinkItem;
  today: String;  
  textHTML: TStrings;
  textXML: TStrings;
  cont: Boolean;
begin
  cont := true;
  // parse parameters
  if HasOption('url') then url := GetOptionValue('url');
  if HasOption('outfile') then outfile := GetOptionValue('outfile');
  if HasOption('outformat') then outformat := GetOptionValue('outformat');

  writeln('Simple Sitemap Creator ',APPVER);
  writeln('(c) 2010 Matthew Hipkin <',Chr(27),'[4;31mhttp://www.matthewhipkin.co.uk',Chr(27),'[0m>');
  writeln;
  
  checkUpdates;
  
  if url = '' then cont := false;
  if outfile = '' then cont := false;
  if outformat = '' then cont := false;
  
  if cont then
  begin
  
    textHTML := TStringList.Create;
    textXML := TStringList.Create;
  
    DateTimeToString(today,'yyyy-mm-dd',Now); 
    if AnsiRightStr(url,1) <> '/' then url := url + '/'; 
    parseLinks(url);
  
    // Clear the HTML editor
    textHTML.Clear;
    // Create an unordered list
    textHTML.Add('<ul>');
    // Clear the XML editor
    textXML.Clear;
    // Create XML header
    textXML.Add('<?xml version="1.0" encoding="UTF-8"?>');
    textXML.Add('<urlset xmlns="http://www.google.com/schemas/sitemap/0.84"');
    textXML.Add('xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"');
    textXML.Add('xsi:schemaLocation="http://www.google.com/schemas/sitemap/0.84');
    textXML.Add('http://www.google.com/schemas/sitemap/0.84/sitemap.xsd">');
    // Loops through the links
    for x := 0 to links.Count -1 do
    begin
      // If the cancel button is clicked break the loop
      l := links[x];
      // Add a bullet item with the current link info
      textHTML.Add('  <li><a href="'+l^.link+'">'+l^.title+'</a></li>');
      // Add an XML item
      textXML.Add('  <url>');
      textXML.Add('    <loc>'+l^.link+'</loc>');
      textXML.Add('    <lastmod>'+today+'</lastmod>');
      textXML.Add('  </url>');
    end;
    // Set info caption
    writeln;
    // Close list tag
    textHTML.Add('</ul>');
    // End XML
    textXML.Add('</urlset>');
    // Clear list of links
    links.Clear;   
    writeln('Saving output to ',outfile); 
    if Lowercase(outformat) = 'xml' then textXML.SaveToFile(outfile)
    else textHTML.SaveToFile(outfile);
    writeln('Done!');
    textHTML.Free;
    textXML.Free;
  end
  else
    showUsage;
  Terminate;
end;

procedure THApplication.showUsage;
begin
  writeln('Usage:');
  writeln('  --url=		specify which URL to crawl');
  writeln('  --outfile= 		output file to save to');
  writeln('  --outformat=html|xml	save output as HTML or XML');
end;

procedure THApplication.checkUpdates;
var
  response: String;
  newVer: Boolean;
begin
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
    writeln('A new version is available, please visit ',Chr(27),'[4;31mhttp://www.matthewhipkin.co.uk',Chr(27),'[0m');
    writeln;
  end;
end;

constructor THApplication.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor THApplication.Destroy;
begin
  inherited Destroy;
end;

begin
  // Setup variables
  links := TList.Create;
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
  Application:=THApplication.Create(nil);
  Application.Title:='Simple Sitemap Creator';
  Application.Run;
  Application.Free;
  links.Free;
  ignoreFiles.Free;
end.