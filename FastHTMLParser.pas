// TODO:
{
  - OnDone event when parser is finished
  - advanced parsing NAME=VALUE pairs
}
{
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

                    FastHTMLParser unit to parse HTML
                  (disect html into its tags and text.)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 TITLE        : Fast HTML Parser (modified)
 CLASS        : TjsFastHTMLParser
 VERSION      : 0.4La

 AUTHOR       : James Azarja
                http://www.jazarsoft.com/

 CONTRIBUTORS : L505
                http://z505.com

                YourName Here...


 LEGAL        : Copyright (C) 2004 Jazarsoft, All Rights Reserved.
                Modified 2005 Lars (L505)

--------------------------------------------------------------------------------

  - Modified for use as a pure command line unit (no dialogs) for freepascal.
  - Also added UPPERCASE tags so that when you check for <font> it returns all
    tags like <FONT> and <FoNt> and <font>

 Use it for what reasons:
    -make your own web browsers,
    -make your own text copies of web pages for caching purposes
    -Grab content from websites -without- using regular expressions
    -Seems to be MUCH MUCH FASTER than regular expressions, as it is after all
     a true parser
    -convert website tables into spreadsheets (parse <TD> and <TR>, turn in to
     CSV or similar)
    -convert websites into text files (parse all text, and tags <BR> <P> )
    -convert website tables into CSV/Database (<parse <TD> and <TR>)
    -find certain info from a web page.. i.e. all the bold text or hyperlinks in
     a page.
    -Parse websites remotely from a CGI app using something like Sockets or
     Synapse and SynWrap to first get the HTML site. This would allow you to
     dynamically parse info from websites and display data on your site in real
     time.
    -HTML editor.. WYSIWYG or a partial WYSIWYG editor. Ambitious, but possible.
    -HTML property editor. Not completely wysiwyg but ability to edit proprties
     of tags. Work would need to be done to parse each property in a tag.


--------------------------------------------------------------------------------
 LICENSE/TERMS
--------------------------------------------------------------------------------

 This code may be used and modified by anyone so long as  this header and
 copyright  information remains intact.

 The code is provided "AS-IS" and without WARRANTY OF ANY KIND,
 expressed, implied or otherwise, including and without limitation, any
 warranty of merchantability or fitness for a  particular purpose. 

 In no event shall the author be liable for any special, incidental,
 indirect or consequential damages whatsoever (including, without
 limitation, damages for loss of profits, business interruption, loss
 of information, or any other loss), whether or not advised of the
 possibility of damage, and on any theory of liability, arising out of
 or in connection with the use or inability to use this software.  


--------------------------------------------------------------------------------
 HISTORY:
--------------------------------------------------------------------------------

 0.1     -  James:
             Initial Development
             mostly based on Peter Irlam works & ideas

 0.2     -  James:
             Some minor bug has fixed

 0.3     -  James:
             Some jsHTMLUtil function bug has been fixed

 0.4     -  James:
             jsHTMLUtil Tag Attributes bug has been fixed
             thanks to Dmitry [mail@vader.ru]

 0.4L.1a -  L505:
             Made unit work with freepascal, added UPCASE (case insensitive)
             exec function

 0.4L.1b -  L505:
             Changed case insensitive version to a new class instead of
             the old ExecUpcase

                                                                                                                                                          //
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

}

{$ifdef FPC}
  {$MODE Delphi} {H+}
{$endif}

unit FastHTMLParser;


interface

uses
 {$IFDEF KOL_MCK}
  KOL;
 {$else}
  SysUtils;
 {$ENDIF}

type
  // TODO: When values in tags are found in HTML
  //(i.e. align="top" - value found in this event is "top")
  TOnFoundTagValue = procedure(value: string) of object;                                    

  // when tag content is found in HTML, including names and values
  // case insensitive analysis available via CaseInsensitiveTag
  TOnFoundTagNoCase = procedure(CaseInsensitiveTag, ActualTag: string) of object;

  // when tag content is found in HTML, including names and values
  // case sensitive
  TOnFoundTag = procedure(ActualTag: string) of object;

  // when text is found in the HTML
  TOnFoundText = procedure(Text: string) of object;

  // regular fast html parser, case sensitive
  TjsFastHTMLParser = class(TObject)                            
    public
      OnFoundTag: TOnFoundTag;
      OnFoundText: TOnFoundText;
      Raw: Pchar;
      constructor Create(sRaw: string);overload;
      constructor Create(pRaw: PChar);overload;
      procedure Exec;
      procedure NilOnFoundTag(ActualTag: string);
      procedure NilOnFoundText(Text: string);
  end;

  // Lars's modified html parser, case insensitive tag retrieval.
  THTMLParserNoCase = class(TObject)
    public
      OnFoundTag: TOnFoundTagNoCase;
      OnFoundText: TOnFoundText;
      Raw: Pchar;
      constructor Create(sRaw: string);overload;
      constructor Create(pRaw: PChar);overload;
      procedure Exec;
      procedure NilOnFoundTag(ActualTag: string);
      procedure NilOnFoundText(Text: string);
  end;


implementation


function CopyBuffer(StartIndex: PChar; Length: Integer): string;
var
  S: string;
begin
  SetLength(S, Length);
  StrLCopy(@S[1], StartIndex, Length);
  Result:= S;
end;


procedure TjsFastHTMLParser.NilOnFoundTag(ActualTag: string);
begin end;

procedure TjsFastHTMLParser.NilOnFoundText(Text: string);
begin end;

constructor TjsFastHTMLParser.Create(sRaw: string);
begin
  if sRaw = '' then exit;
  Raw:= Pchar(sRaw);
end;

constructor TjsFastHTMLParser.Create(pRaw: Pchar);
begin
  if pRaw = '' then exit;
  if pRaw = nil then exit;  
  Raw:= pRaw;
end;

// Main tag parsing procedure (case sensitive)
// i.e. there IS a difference between <FONT> and <font> and <fOnT>
procedure TjsFastHTMLParser.Exec;
var
  L: Integer;
  TL: Integer;
  I: Integer;
  Done: Boolean;
  TagStart,
  TextStart,
  P: PChar;   // Pointer to current char.
  C: Char;
begin
  { set nil events once rather than checking for nil each time tag is found }
  if not assigned(OnFoundText) then
    OnFoundText:= NilOnFoundText;
  if not assigned(OnFoundTag) then
    OnFoundText:= NilOnFoundTag;

  TL:= StrLen(Raw);
  I:= 0;
  P:= Raw;
  Done:= False;
  if P <> nil then
  begin
    TagStart:= nil;
    repeat
      TextStart:= P;
      { Get next tag position }
      while Not (P^ in [ '<', #0 ]) do
      begin
        Inc(P); Inc(I);
        if I>= TL then
        begin
          Done:= True;
          Break;
        end;
      end;
      if Done then Break;

      { Is there any text before ? }
      if (TextStart <> nil) and (P > TextStart) then
      begin
        L:= P - TextStart;
        { Yes, copy to buffer }
//        if ( assigned(OnFoundText) ) then //L505: optimized
          OnFoundText( CopyBuffer(TextStart, L ) );
      end else
      begin
        TextStart:= nil;
      end;
      { No }

      TagStart:= P;
      while Not (P^ in [ '>', #0]) do
      begin

        // Find string in tag
        if (P^ = '"') or (P^ = '''') then
        begin
          C:= P^;
          Inc(P);Inc(I); // Skip current char " or '

          // Skip until string end
          while Not (P^ in [C, #0]) do
          begin
            Inc(P); Inc(I);
          end;
        end;
        Inc(P); Inc(I);
        if I >= TL then
        begin
          Done:= True;
          Break;
        end;
      end;
      if Done then Break;
      { Copy this tag to buffer }
      L:= P - TagStart + 1;

//      if ( Assigned(OnFoundTag) ) then //L505: optimized
        OnFoundTag( CopyBuffer(TagStart, L) );

      Inc(P); Inc(I);
      if I >= TL then Break;

    until (Done);
  end;
end;



{ ************************ THTMLParserNoCase ********************************* }

constructor THTMLParserNoCase.Create(pRaw: Pchar);
begin
  if pRaw = '' then exit;
  if pRaw = nil then exit;
  Raw:= pRaw;
end;

constructor THTMLParserNoCase.Create(sRaw: string);
begin
  if sRaw = '' then exit;
  Raw:= Pchar(sRaw);
end;

{ default dummy "do nothing" events if events are unassigned }
procedure THTMLParserNoCase.NilOnFoundTag(ActualTag: string);
begin end;

procedure THTMLParserNoCase.NilOnFoundText(Text: string);
begin end;


// L505: parse case insensitive
//       Developer receives <FONT> rather than <font> or <fOnT> in OnFoundTag
procedure THTMLParserNoCase.Exec;
var
  L: Integer;
  TL: Integer;
  I: Integer;
  Done: Boolean;
  TagStart,
  TextStart,
  P: PChar;   // Pointer to current char.
  C: Char;
begin
  { set nil events once rather than checking for nil each time tag is found }
  if not assigned(OnFoundText) then
    OnFoundText:= NilOnFoundText;
  if not assigned(OnFoundTag) then
    OnFoundText:= NilOnFoundTag;

  TL:= StrLen(Raw);
  I:= 0;
  P:= Raw;
  Done:= False;
  if P <> nil then
  begin
    TagStart:= nil;
    repeat
      TextStart:= P;
      { Get next tag position }
      while Not (P^ in [ '<', #0 ]) do
      begin
        Inc(P); Inc(I);
        if I >= TL then
        begin
          Done:= True;
          Break;
        end;
      end;
      if Done then Break;

      { Is there any text before ? }
      if (TextStart <> nil) and (P > TextStart) then
      begin
        L:= P - TextStart;
        { Yes, copy to buffer }
        OnFoundText( CopyBuffer(TextStart, L) );
      end else
      begin
        TextStart:= nil;
      end;
      { No }

      TagStart:= P;
      while Not (P^ in [ '>', #0]) do
      begin
        // Find string in tag
        if (P^ = '"') or (P^ = '''') then
        begin
          C:= P^;
          Inc(P); Inc(I); // Skip current char " or '

          // Skip until string end
          while Not (P^ in [C, #0]) do
          begin
            Inc(P);Inc(I);
          end;
        end;

        Inc(P);Inc(I);
        if I >= TL then
        begin
          Done:= True;
          Break;
        end;
      end;
      if Done then Break;

      { Copy this tag to buffer }
      L:= P - TagStart + 1;
      OnFoundTag( uppercase(CopyBuffer(TagStart, L )), CopyBuffer(TagStart, L ) ); //L505: added uppercase
      Inc(P); Inc(I);
      if I >= TL then Break;
    until (Done);
  end;
end;


end.







{ ************************ THTMLParserAdvanced ******************************* }

  THTMLParserAdvanced = class(TObject)
    public
      OnFoundTag   : TOnFoundTagNoCase;
      OnFoundText  : TOnFoundText;
      Raw          : Pchar;
//      constructor Create(sRaw: string);overload;
//      constructor Create(pRaw: PChar);overload;
      procedure Exec;
  end;


// UNFINISHED

{ L505: Added this function for advanced parsing.  Similar to the other parser
       classes but offers more power than first parser. Has tag parsing
       (name value pairs) and case insensitivity
 UNFINISHED }
procedure THTMLParserAdvanced.Exec;
var
  L: Integer;
  TL: Integer;
  I: Integer;
  Done: Boolean;
  TagStart,
  TextStart,
  P: PChar;   // Pointer to current char.
  C: Char;
begin
  TL:= StrLen(Raw);
  I:= 0;
  P:= Raw;
  Done:= False;
  if P <> nil then
  begin
    TagStart:= nil;
    repeat
      TextStart:= P;
      { Get next tag position }
      while Not (P^ in [ '<', #0 ]) do
      begin
        Inc(P); Inc(I);
        if I >= TL then
        begin
          Done:= True;
          Break;
        end;
      end;
      if Done then Break;

      { Is there any text before ? }
      if (TextStart <> nil) and (P > TextStart) then
      begin
        L:= P - TextStart;
        { Yes, copy to buffer }
        if (assigned(OnFoundText)) then
          OnFoundText( CopyBuffer(TextStart, L) );
      end else
      begin
        TextStart:= nil;
      end;
      { No }

      TagStart:= P;
      while Not (P^ in [ '>', #0]) do
      begin
        // Find string in tag
        if (P^ = '"') or (P^ = '''') then
        begin
          C:= P^;
          Inc(P); Inc(I); // Skip current char " or '

          // Skip until string end
          while Not (P^ in [C, #0]) do
          begin
            Inc(P);Inc(I);
          end;
        end;

        Inc(P);Inc(I);
        if I >= TL then
        begin
          Done:= True;
          Break;
        end;
      end;
      if Done then Break;
      { Copy this tag to buffer }
      L:= P - TagStart + 1;

//      if ( Assigned(OnFoundTag) ) then
        OnFoundTag( uppercase(CopyBuffer(TagStart, L )), CopyBuffer(TagStart, L ) ); //L505: added upppercase

      Inc(P); Inc(I);
      if I >= TL then Break;

    until (Done);
  end;
end;

