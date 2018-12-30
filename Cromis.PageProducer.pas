{
  ==================================================================================
  Cromis.PageProducer.pas
  ----------------------------
  Copyright © 2008-2010 Kacin Iztok (iztok.kacin@gmail.com)

  Feel free to do anything you want with this source code, but don't
  distribute modified versions without clearly marking them as such.
  ==================================================================================
  Page producing component. It reads content from file or stream and processes special tags.
  It can take parameters and can hold internal content inside the tag markers. Very Fast
  ==================================================================================
  7/3/2008:
    * Initial implementation.                                                              
  8/3/2008:
    * Added new tag type "$" for extended tags. Kept the TPageProducer compatibility
  11/3/2008
    * support for two types of parameter notation (without or with " chars)
    * reorganized the command parsing code
  27/11/2008
    * support for extra tags that allows to parse some of the html or similar code
  30/09/2009
    * unicode compatibility, treat all string and chars according to delphi version
  26/01/2010
    * Extra tags can have in depth processing
  01/10/2010
    * Unlimited in depth parsing even for same nested tags
    * reorganized how trim inner content works
    * can control how much indentation the inner content has
  18/12/2013
    * Parse inner contenct is now part of the callback handler
    * Fixed automatic inner content parsing
  19/12/2013
    * Improved parameters parsing and support
  ==================================================================================
}
unit Cromis.PageProducer;

interface

uses
  SysUtils, Classes, Math,

  // library units
  Cromis.Streams,
  Cromis.Unicode;

const
  SIZE_BUFFER = 65536;

  CHAR_TAG_CMDNEW  = '$';
  CHAR_TAG_CMDOLD  = '#';
  CHAR_TAG_BEGIN   = '<';
  CHAR_TAG_CLOSE   = '>';
  CHAR_TAG_END     = '/';

  CHAR_PARAM_DELIM = '=';
  CHAR_PARAM_CLOSE = '"';

  CHAR_INDENT_BEGIN = '[';
  CHAR_INDENT_CLOSE = ']';

type
  TSourceType = (stStream, stString);
  
  TContentTagEvent = procedure(const Sender: TObject;
                               const TagString: string;
                               const TagParams: TStrings;
                               const PageParams: TStrings;
                               const TagContent: string;
                               const ContentStream: TStream;
                               var ParseInnerContent: Boolean
                               ) of Object;

  TContentReader = class
  private
    FBytesRead: Integer;
    FSourceStream: TStream;
    FEndOfContent: Boolean;
    FContentPosition: Integer;
    FContentBuffer: array [0..SIZE_BUFFER - 1] of Byte;
  public
    constructor Create;
    function GetNextChar: Char;
    function PeekNextChar: Char;
    property EndOfContent: Boolean read FEndOfContent write FEndOfContent;
    property SourceStream: TStream read FSourceStream write FSourceStream;
  end;

  TPageProducerEx = class
  private
    FTagName: string;
    FCurrChar: Char;
    FExtraTags: TStringList;
    FTagContent: string;
    FExtraData: Pointer;
    FSourceType: TSourceType;
    FParseAllTags: Boolean;
    FSourceString: string;
    FSourceStream: TStream;
    FOnContentTag: TContentTagEvent;
    FContentIndent: Integer;
    FExtraTagCheck: Boolean;
    FTagParameters: TStringList;
    FContentReader: TContentReader;
    FPageParameters: TStringList;
    FTrimInnerContent: Boolean;
    FTrimTrailingCRLF: Boolean;
    FExtraTagsExtended: Boolean;
    procedure TrimRightEx(var TagContent: string);
    procedure ParseCurrentTagName(var TagName: string;
                                  var EndIndicator: Boolean;
                                  var ContenIndent: Integer);
    procedure ParseCurrentTagParameters(var EndIndicator: Boolean);
    procedure ParseInnerTagContent(const TagContent: string; const DstStream: TStream);
    procedure ParseCurrentTagContent(var EndIndicator: Boolean; const Extended: Boolean);
    function CheckForClosingTagMatch(var EndTagContent: string; var TagCount: Integer): Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    function ProduceContent: string; overload;
    procedure ProduceContent(var DstString: string); overload;
    procedure ProduceContent(const DstStream: TStream); overload;
    procedure ProduceContent(const SrcStream, DstStream: TStream); overload;
    property ExtraTags: TStringList read FExtraTags;
    property PageParams: TStringList read FPageParameters;
    property ExtraData: Pointer read FExtraData write FExtraData;
    property SourceType: TSourceType read FSourceType write FSourceType;
    property ParseAllTags: Boolean read FParseAllTags write FParseAllTags;
    property SourceString: string read FSourceString write FSourceString;
    property SourceStream: TStream read FSourceStream write FSourceStream;
    property OnContentTag: TContentTagEvent read FOnContentTag write FOnContentTag;
    property TrimInnerContent: Boolean read FTrimInnerContent write FTrimInnerContent;
    property TrimTrailingCRLF: Boolean read FTrimTrailingCRLF write FTrimTrailingCRLF;
    property ExtraTagsExtended: Boolean read FExtraTagsExtended write FExtraTagsExtended;
  end;

implementation

{ TPageProducerEx }

function TPageProducerEx.CheckForClosingTagMatch(var EndTagContent: string; var TagCount: Integer): Boolean;
begin
  EndTagContent := '';

  while not FContentReader.EndOfContent do
  begin
    FCurrChar := FContentReader.GetNextChar;

    case FCurrChar <> CHAR_TAG_CLOSE of
      True: EndTagContent := EndTagContent + FCurrChar;
      False: Break;
    end;
  end;

  TagCount := TagCount - Integer(EndTagContent = FTagName);
  Result := TagCount = 0;
end;

constructor TPageProducerEx.Create;
begin
  FPageParameters := TStringList.Create;
  FTagParameters := TStringList.Create;
  FExtraTags := TStringList.Create;

  FPageParameters.CaseSensitive := False;
  FTagParameters.CaseSensitive := False;
  FTrimInnerContent := False;
end;

destructor TPageProducerEx.Destroy;
begin
  FreeAndNil(FExtraTags);
  FreeAndNil(FTagParameters);
  FreeAndNil(FPageParameters);

  inherited;
end;

procedure TPageProducerEx.ParseCurrentTagContent(var EndIndicator: Boolean; const Extended: Boolean);
var
  NestedTag: string;
  SpaceCount: Integer;
  IndentString: string;
  EndTagContent: string;
  ContentIndent: Integer;
  NestedTagCount: Integer;
  FoundLineStart: Boolean;
  FoundLineContent: Boolean;
  FoundContentStart: Boolean;
begin
  FoundContentStart := False;
  FoundLineContent := False;
  FoundLineStart := False;
  NestedTagCount := 1;
  FTagContent := '';
  SpaceCount := 0;

  // parse the tag content
  if not EndIndicator and Extended then
  begin
    while not FContentReader.EndOfContent do
    begin
      EndIndicator := FCurrChar = CHAR_TAG_BEGIN;
      FCurrChar := FContentReader.GetNextChar;

      // check for start
      if not FoundContentStart then
      begin
         // check if the start was alread found and set it
        if not (CharInSet(FCurrChar, [#13, #10, #32])) then
        begin
          FoundContentStart := True;
          FoundLineStart := True;
        end;
      end;

      if FTrimInnerContent and CharInSet(FCurrChar, [#13, #10, #32])
      then
      begin
        if CharInSet(FCurrChar, [#13, #10]) and FoundContentStart then
        begin
          FTagContent :=  FTagContent + FCurrChar;
          FoundLineContent := False;
          FoundLineStart := True;
          SpaceCount := 0;
        end
        else if FCurrChar = #32 then
        begin
          if (FContentIndent = -1) or (SpaceCount < FContentIndent) or FoundLineContent then
          begin
            FTagContent :=  FTagContent + FCurrChar;
            Inc(SpaceCount);
          end;
        end;
      end
      else
      begin
        if FoundLineStart and FTrimInnerContent then
        begin
          FTagContent := FTagContent + StringOfChar(#32, Max(0, FContentIndent - SpaceCount));
          FoundLineContent := True;
          FoundLineStart := False;
        end;
        
        if not CharInSet(FCurrChar, [CHAR_TAG_END, CHAR_TAG_BEGIN]) then
        begin
          if EndIndicator and not (FCurrChar = CHAR_TAG_END) then
          begin
            FTagContent := FTagContent + CHAR_TAG_BEGIN;

            if FCurrChar = CHAR_TAG_CMDNEW then
            begin
              ParseCurrentTagName(NestedTag, EndIndicator, ContentIndent);
              FTagContent := FTagContent + CHAR_TAG_CMDNEW + NestedTag;
              NestedTagCount := NestedTagCount + Integer(NestedTag = FTagName);

              if ContentIndent > -1 then
              begin
                IndentString := Format('%s%d', [CHAR_INDENT_BEGIN, ContentIndent]);
                FTagContent := FTagContent + IndentString;
              end;
            end;
          end;

          FTagContent := FTagContent + FCurrChar;
        end
        else if EndIndicator and (FCurrChar = CHAR_TAG_END) then
        begin
          case CheckForClosingTagMatch(EndTagContent, NestedTagCount) of
            False: FTagContent := FTagContent + CHAR_TAG_BEGIN +
                                                CHAR_TAG_END +
                                                EndTagContent +
                                                CHAR_TAG_CLOSE;
            True: Break;
          end;
        end
        else if FCurrChar = CHAR_TAG_END then
          FTagContent := FTagContent + FCurrChar;
      end;
    end;

    // always trim spaces null chars
    TrimRightEx(FTagContent);

    // check if we need to trim
    if FTrimTrailingCRLF then
    begin
      if FContentReader.PeekNextChar = #13 then
        FCurrChar := FContentReader.GetNextChar;
      if FContentReader.PeekNextChar = #10 then
        FCurrChar := FContentReader.GetNextChar;
    end;
  end;
end;

procedure TPageProducerEx.ParseCurrentTagName(var TagName: string;
                                              var EndIndicator: Boolean;
                                              var ContenIndent: Integer);
var
  Indent: string;
begin
  if FExtraTagCheck then
    TagName := FCurrChar
  else
    TagName := '';

  // parse the name of the tag
  while not FContentReader.EndOfContent do
  begin
    EndIndicator := FCurrChar = CHAR_TAG_END;
    FCurrChar := FContentReader.GetNextChar;
    ContenIndent := -1;
    Indent := '';

    if FCurrChar = CHAR_INDENT_BEGIN then
    begin
      while not FContentReader.EndOfContent do
      begin
        FCurrChar := FContentReader.GetNextChar;

        case FCurrChar <> CHAR_INDENT_CLOSE of
          True: Indent := Indent + FCurrChar;
          False: Break;
        end;
      end;

      // always calculate the current indent
      ContenIndent := StrToIntDef(Indent, -1);
    end;

    case not CharInSet(FCurrChar, [#32, CHAR_TAG_CLOSE, CHAR_INDENT_CLOSE]) of
      True: TagName := TagName + FCurrChar;
      False: Break;
    end;
  end;
end;

procedure TPageProducerEx.ParseCurrentTagParameters(var EndIndicator: Boolean);
var
  ParamStr: string;
  ParsingParams: Boolean;
  BeginParamValue: Boolean;

  procedure SetParsedParameter(const EndOfParameters: Boolean);
  begin
    if Trim(ParamStr) <> EmptyStr then
    begin
      if EndOfParameters and EndIndicator then
        SetLength(ParamStr, Length(ParamStr) - 1);

      FTagParameters.Add(ParamStr);
    end;

    BeginParamValue := False;
    ParsingParams := False;
    ParamStr := '';
  end;

begin
  BeginParamValue := False;
  ParsingParams := False;
  FTagParameters.Clear;

  if FCurrChar <> CHAR_TAG_CLOSE then
  begin
    ParamStr := EmptyStr;

    // parse the tag parameters
    while not FContentReader.EndOfContent do
    begin
      EndIndicator := FCurrChar = CHAR_TAG_END;
      FCurrChar := FContentReader.GetNextChar;

      // break if we encountered close char
      if (FCurrChar = CHAR_TAG_CLOSE) and not BeginParamValue then
      begin
        SetParsedParameter(True);
        Break;
      end;

      // check all the possible states and combinations
      if (FCurrChar = CHAR_PARAM_CLOSE) and not BeginParamValue then
        BeginParamValue := True
      else if (FCurrChar = CHAR_PARAM_CLOSE) and BeginParamValue then
        SetParsedParameter(False)
      else if not (FCurrChar = #32) or BeginParamValue then
      begin
        ParamStr := ParamStr + FCurrChar;
        ParsingParams := True;
      end
      else if FCurrChar = #32 then
        SetParsedParameter(False);
    end;
  end;
end;

procedure TPageProducerEx.ParseInnerTagContent(const TagContent: string; const DstStream: TStream);
var
  ContentParser: TPageProducerEx;
begin
  ContentParser := TPageProducerEx.Create;
  try
    ContentParser.OnContentTag := Self.OnContentTag;
    ContentParser.ExtraTags.Assign(FExtraTags);
    ContentParser.SourceString := TagContent;
    ContentParser.FSourceType := stString;

    // parse the content. can be recursive
    ContentParser.ProduceContent(DstStream);
  finally
    ContentParser.Free;
  end;
end;

function TPageProducerEx.ProduceContent: string;
begin
  ProduceContent(Result);
end;

procedure TPageProducerEx.ProduceContent(var DstString: string);
var
  MS: TMemoryStream;
begin
  MS := TMemoryStream.Create;
  try
    Self.ProduceContent(MS);
    {$IFDEF UNICODE}
      DstString := UTF8ToString(ReadFromStreamAsUTF8(MS));
    {$ELSE}
      DstString := ReadFromStreamAsUTF8(MS);
    {$ENDIF}
  finally
    MS.Free;
  end;
end;

procedure TPageProducerEx.ProduceContent(const DstStream: TStream);
var
  PrevChar: Char;
  EndIndicator: Boolean;
  ExtendedCommand: Boolean;
  ParseInnerContent: Boolean;
  UTF8BOM: array [0..2] of Byte;
begin
  if FSourceType = stString then
  begin
    FSourceStream := TMemoryStream.Create;
    Cromis.Streams.WriteToStreamAsUTF8(FSourceStream, FSourceString);
  end;

  if (FSourceType = stStream) and (FSourceStream = nil) then
    raise Exception.Create('No source stream specified!');

  if DstStream = nil then
    raise Exception.Create('No destination stream specified!');

  if not Assigned(FOnContentTag) then
    raise Exception.Create('OnContentTag not assigned!');

  FContentReader := TContentReader.Create;
  try
    FContentReader.SourceStream := FSourceStream;
    FSourceStream.Position := 0;
    FCurrChar := #0;

    if FSourceStream.Position = 0 then
    begin
      FSourceStream.Read(UTF8BOM, SizeOf(UTF8BOM));

      if not ((UTF8BOM[0] = 239) and
              (UTF8BOM[1] = 187) and
              (UTF8BOM[2] = 191)) then
        FSourceStream.Position := 0;
    end;

    while not FContentReader.EndOfContent do
    begin
      PrevChar := FCurrChar;
      FCurrChar := FContentReader.GetNextChar;

      if not FContentReader.EndOfContent then
      begin
        if (PrevChar = CHAR_TAG_BEGIN) and CharInSet(FCurrChar, [CHAR_TAG_CMDNEW, CHAR_TAG_CMDOLD]) then
        begin
          ExtendedCommand := FCurrChar = CHAR_TAG_CMDNEW;
          ParseInnerContent := True;
          EndIndicator := False;
          FContentIndent := -1;

          ParseCurrentTagName(FTagName, EndIndicator, FContentIndent);
          ParseCurrentTagParameters(EndIndicator);
          ParseCurrentTagContent(EndIndicator, ExtendedCommand);

          // call the listening application and pass all the parameters
          FOnContentTag(Self, FTagName, FTagParameters, FPageParameters, FTagContent, DstStream, ParseInnerContent);

          if ParseInnerContent and (Trim(FTagContent) <> '') then
            ParseInnerTagContent(FTagContent, DstStream);
        end
        else if (PrevChar = CHAR_TAG_BEGIN) then
        begin
          // do we check for extra tags
          FExtraTagCheck := ((FExtraTags.Count > 0) and (FCurrChar <> CHAR_TAG_END)) or FParseAllTags;
          EndIndicator := False;

          if FExtraTagCheck then
          begin
            ParseCurrentTagName(FTagName, EndIndicator, FContentIndent);

            case not FParseAllTags of
              True: FExtraTagCheck := FExtraTags.IndexOf(FTagName) > -1;
              False: FExtraTagCheck := True;
            end;

            if FExtraTagCheck then
            begin
              ParseInnerContent := True;
              ParseCurrentTagParameters(EndIndicator);
              ParseCurrentTagContent(EndIndicator, FExtraTagsExtended);
              FOnContentTag(Self, FTagName, FTagParameters, FPageParameters, FTagContent, DstStream, ParseInnerContent);

              if ParseInnerContent and (Trim(FTagContent) <> '') then
                ParseInnerTagContent(FTagContent, DstStream);
            end
            else
            begin
              // write both chars and tag to the stream
              DstStream.Write(PrevChar, SizeOf(AnsiChar));
              DstStream.Write(FTagName[1], SizeOf(AnsiChar) * Length(FTagName));
              DstStream.Write(FCurrChar, SizeOf(AnsiChar));
            end;
          end
          else
          begin
            // write both chars to the stream
            DstStream.Write(PrevChar, SizeOf(AnsiChar));
            DstStream.Write(FCurrChar, SizeOf(AnsiChar));
          end;
        end
        else if FCurrChar <> CHAR_TAG_BEGIN then
          DstStream.Write(FCurrChar, SizeOf(AnsiChar));
      end;
    end;

    // clean behind ourselves
    if FSourceType = stString then
    begin
      FreeAndNil(FSourceStream);
      FSourceString := '';
    end;
  finally
    FContentReader.Free;
  end;
end;

procedure TPageProducerEx.ProduceContent(const SrcStream, DstStream: TStream);
begin
  SourceStream := SrcStream;
  ProduceContent(DstStream);
end;

procedure TPageProducerEx.TrimRightEx(var TagContent: string);
var
  StrLength: Integer;
begin
  StrLength := Length(TagContent);

  while (StrLength > 0) and (CharInSet(TagContent[StrLength], [#32, #0])) do
    Dec(StrLength);

  if StrLength < Length(TagContent) then
    SetLength(TagContent, StrLength);
end;

{ TContentReader }

constructor TContentReader.Create;
begin
  FContentPosition := SIZE_BUFFER;
  FEndOfContent := False;
end;

function TContentReader.GetNextChar: Char;
begin
  Result := #0;

  if FContentPosition >= FBytesRead then
  begin
    // read the next block of bytes from the source stream
    FBytesRead := FSourceStream.Read(FContentBuffer, SIZE_BUFFER);

    case FBytesRead > 0 of
      True: FContentPosition := 0;
      False: FEndOfContent := True;
    end;
  end;

  if not FEndOfContent then
  begin
    Result := Chr(FContentBuffer[FContentPosition]);
    Inc(FContentPosition);
  end;
end;

function TContentReader.PeekNextChar: Char;
begin
  Result := #0;

  if FContentPosition >= FBytesRead then
  begin
    // read the next block of bytes from the source stream
    FBytesRead := FSourceStream.Read(FContentBuffer, SIZE_BUFFER);

    case FBytesRead > 0 of
      True: FContentPosition := 0;
      False: FEndOfContent := True;
    end;
  end;

  if not FEndOfContent then
    Result := Chr(FContentBuffer[FContentPosition]);
end;

end.
