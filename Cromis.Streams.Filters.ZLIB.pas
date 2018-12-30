unit Cromis.Streams.Filters.ZLIB;

interface

uses
  SysUtils, Classes, Zlib,

  // cromis units
  Cromis.Streams.Filters;

type
  TZLIBFilter = class(TCustomFilter)
  public
    constructor Create;
    procedure InputFilter(const InputStream, OutputStream: TStream); override;
    procedure OutputFilter(const InputStream, OutputStream: TStream); override;
  end;

  // filter data constructor
  function ZLIBFilter: TCustomFilter;

implementation

function ZLIBFilter: TCustomFilter;
begin
  Result := TZLIBFilter.Create;
end;

{ TZLIBFilter }

constructor TZLIBFilter.Create;
begin
  FID := 'A6F9B382118148248E10F0597FE69D96';
end;

procedure TZLIBFilter.InputFilter(const InputStream, OutputStream: TStream);
const
  BufferSize = 4096;
var
  Count: Integer;
  ZStream: TCompressionStream;
  Buffer: array[0..BufferSize - 1] of Byte;
begin
  inherited;

  ZStream := TCompressionStream.Create(clMax, OutputStream);
  try
    while True do
    begin
      Count := InputStream.Read(Buffer[0], BufferSize);

      if Count <> 0 then
        ZStream.Write(Buffer[0], Count)
      else
        Break;
    end;
  finally
    ZStream.Free;
  end;
end;

procedure TZLIBFilter.OutputFilter(const InputStream, OutputStream: TStream);
const
  BufferSize = 4096;
var
  Count: Integer;
  ZStream: TDecompressionStream;
  Buffer: array[0..BufferSize - 1] of Byte;
begin
  inherited;

  ZStream := TDecompressionStream.Create(InputStream);
  try
    while True do
    begin
      Count := ZStream.Read(Buffer[0], BufferSize);

      if Count <> 0 then
        OutputStream.WriteBuffer(Buffer[0], Count)
      else
        Break;
    end;
  finally
    ZStream.Free;
  end;
end;

end.
