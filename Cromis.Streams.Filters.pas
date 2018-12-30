unit Cromis.Streams.Filters;

interface

uses
  SysUtils, Classes, Contnrs;

type
  TStreamFilters = class;

  TCustomFilter = class
  protected
    FID: AnsiString;
  public
    procedure InputFilter(const InputStream, OutputStream: TStream); virtual; abstract;
    procedure OutputFilter(const InputStream, OutputStream: TStream); virtual; abstract;
    property ID: AnsiString read FID;
  end;

  IUsedFilters = Interface(IInterface)
  ['{C5E58525-BA97-4CC9-ADF1-C35844A1BEB5}']
    function FilterList: TStreamFilters;
  end;

  TStreamFilters = class
  private
    FFilterList: TObjectList;
    function GetFilters(const Index: Integer): TCustomFilter;
    function GetCount: Integer;
    function GetOwnFilters: Boolean;
    procedure SetOwnFilters(const Value: Boolean);
  public
    constructor Create(const OwnFilters: Boolean = True);
    destructor Destroy; override;
    procedure RemoveAll;
    procedure AddFilter(const Filter: TCustomFilter);
    procedure RemoveFilter(const Filter: TCustomFilter);
    function IsFiltered(const Stream: TStream): Boolean;
    function FindFilter(const ID: AnsiString): TCustomFilter;
    procedure ApplyInput(const InStream, OutStream: TStream);
    function ApplyOutput(const InStream, OutStream: TStream): IUsedFilters;
    property Filters[const Index: Integer]: TCustomFilter read GetFilters; default;
    property OwnFilters: Boolean read GetOwnFilters write SetOwnFilters;
    property Count: Integer read GetCount;
  end;

type
  TFilterClass = class of TCustomFilter;

  // function to check if the stream is filtered
  function IsFiltered(const Stream: TStream): Boolean;

implementation

type
  TFilterMark = array [0..2] of Cardinal;

type
  TUsedFilters = class(TInterfacedObject, IUsedFilters)
  private
    FFilterList: TStreamFilters;
  public
    constructor Create;
    destructor Destroy; override;
    function FilterList: TStreamFilters;
  end;

var
  FilterMark: TFilterMark = (0,0,0);

function IsFiltered(const Stream: TStream): Boolean;
var
  OldPosition: Int64;
  CompareMark: TFilterMark;
begin
  OldPosition := Stream.Position;
  try
    Stream.Seek(0, soFromBeginning);
    Stream.Read(CompareMark[0], SizeOf(TFilterMark));
    Result := CompareMem(@CompareMark[0], @FilterMark[0], SizeOf(TFilterMark));
  finally
    Stream.Position := OldPosition;
  end;
end;

{ TStreamFilters }

procedure TStreamFilters.AddFilter(const Filter: TCustomFilter);
begin
  if Trim(string(Filter.ID)) = '' then
    raise Exception.Create('Filter needs to have a valid ID');

  FFilterList.Add(Filter);
end;

procedure TStreamFilters.ApplyInput(const InStream, OutStream: TStream);
var
  I: Integer;
  Input: TMemoryStream;
  Output: TMemoryStream;
  NameSize: Cardinal;
  FilterData: TMemoryStream;
  FilterCount: Cardinal;
begin
  if FFilterList.Count > 0 then
  begin
    Input := TMemoryStream.Create;
    try
      Output := TMemoryStream.Create;
      try
        FilterData := TMemoryStream.Create;
        try
          InStream.Position := 0;
          FilterCount := 0;

          for I := 0 to FFilterList.Count - 1 do
          begin
            case I = 0 of
              True: Filters[I].InputFilter(InStream, Output);
              False: Filters[I].InputFilter(Input, Output);
            end;

            NameSize := Length(Filters[I].ID) * SizeOf(AnsiChar);
            FilterData.Write(NameSize, SizeOf(Cardinal));
            FilterData.Write(Filters[I].ID[1], NameSize);
            Inc(FilterCount);

            if I < FFilterList.Count - 1 then
            begin
              Input.Clear;
              Input.CopyFrom(Output, 0);
              Input.Position := 0;
              Output.Clear
            end;
          end;

          OutStream.Size := 0;
          Output.Position := 0;
          FilterData.Position := 0;
          // copy last output to storage
          OutStream.Write(FilterMark[0], SizeOf(TFilterMark));
          OutStream.Write(FilterCount, SizeOf(Cardinal));
          OutStream.CopyFrom(FilterData, FilterData.Size);
          OutStream.CopyFrom(Output, Output.Size);
        finally
          FilterData.Free;
        end;
      finally
        Output.Free;
      end;
    finally
      Input.Free;
    end;
  end;
end;

function TStreamFilters.ApplyOutput(const InStream, OutStream: TStream): IUsedFilters;
var
  I: Integer;
  Input: TMemoryStream;
  Output: TMemoryStream;
  NameSize: Cardinal;
  FilterName: AnsiString;
  FilterCount: Cardinal;
  OldPosition: Int64;
  CompareMark: TFilterMark;
  ChainFilter: TCustomFilter;
begin
  Result := TUsedFilters.Create;

  if FFilterList.Count > 0 then
  begin
    Input := TMemoryStream.Create;
    try
      Output := TMemoryStream.Create;
      try
        if InStream.Size > SizeOf(TFilterMark) then
        begin
          OldPosition := InStream.Position;
          InStream.Seek(0, soFromBeginning);
          InStream.Read(CompareMark[0], SizeOf(TFilterMark));

          if not CompareMem(@CompareMark[0], @FilterMark[0], SizeOf(TFilterMark)) then
          begin
            InStream.Position := OldPosition;
            Exit;
          end;

          if InStream.Read(FilterCount, SizeOf(Cardinal)) = SizeOf(Cardinal) then
          begin
            for I := 0 to FilterCount - 1 do
            begin
              if InStream.Read(NameSize, SizeOf(Cardinal)) < SizeOf(Cardinal) then
                raise Exception.Create('Filtered data is corrupted');

              // set the length of filter name
              SetLength(FilterName, NameSize);

              if InStream.Read(FilterName[1], NameSize) < SizeOf(Cardinal) then
                raise Exception.Create('Filtered data is corrupted');

              ChainFilter := FindFilter(FilterName);

              if ChainFilter = nil then
                raise Exception.CreateFmt('Required filter with ID %s is not on the list of available filters', [FilterName]);

              // add filter to output list
              Result.FilterList.AddFilter(ChainFilter);
            end;

            for I := Result.FilterList.Count - 1 downto 0 do
            begin
              case I = Result.FilterList.Count - 1 of
                True: Result.FilterList[I].OutputFilter(InStream, Output);
                False: Result.FilterList[I].OutputFilter(Input, Output);
              end;

              if I > 0 then
              begin
                Input.Clear;
                Input.CopyFrom(Output, 0);
                Input.Position := 0;
                Output.Clear;
              end;

              OutStream.Size := 0;
              Output.Position := 0;
              // copy last output to storage
              OutStream.CopyFrom(Output, Output.Size);
              OutStream.Position := 0;
            end;
          end;
        end;
      finally
        Output.Free;
      end;
    finally
      Input.Free;
    end;
  end;
end;

constructor TStreamFilters.Create(const OwnFilters: Boolean = True);
begin
  FFilterList := TObjectList.Create(OwnFilters);
end;

destructor TStreamFilters.Destroy;
begin
  FreeAndNil(FFilterList);

  inherited;
end;

function TStreamFilters.FindFilter(const ID: AnsiString): TCustomFilter;
var
  I: Integer;
begin
  Result := nil;

  for I := 0 to FFilterList.Count - 1 do
  begin
    if SameText(string(TCustomFilter(FFilterList[I]).ID), string(ID)) then
    begin
      Result := TCustomFilter(FFilterList[I]);
      Exit;
    end;
  end;
end;

function TStreamFilters.GetCount: Integer;
begin
  Result := FFilterList.Count;
end;

function TStreamFilters.GetFilters(const Index: Integer): TCustomFilter;
begin
  Result := TCustomFilter(FFilterList[Index]);
end;

function TStreamFilters.GetOwnFilters: Boolean;
begin
  Result := FFilterList.OwnsObjects;
end;

function TStreamFilters.IsFiltered(const Stream: TStream): Boolean;
begin
  Result := IsFiltered(Stream);
end;

procedure TStreamFilters.RemoveAll;
begin
  FFilterList.Clear;
end;

procedure TStreamFilters.RemoveFilter(const Filter: TCustomFilter);
begin
  FFilterList.Remove(Filter);
end;

procedure TStreamFilters.SetOwnFilters(const Value: Boolean);
begin
  FFilterList.OwnsObjects := Value;
end;

{ TUsedFilters }

constructor TUsedFilters.Create;
begin
  FFilterList := TStreamFilters.Create(False);
end;

destructor TUsedFilters.Destroy;
begin
  FreeAndNil(FFilterList);

  inherited;
end;

function TUsedFilters.FilterList: TStreamFilters;
begin
  Result := FFilterList;
end;

end.
