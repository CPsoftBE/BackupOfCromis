unit Cromis.Streams.Filters.XTEA;

interface

uses
  SysUtils, Classes,

  // cromis library units
  Cromis.Streams, Cromis.XTEA, Cromis.Streams.Filters, Cromis.Unicode;

type
  TXTEAFilter = class(TCustomFilter)
  private
    FKey: TBytes;
  public
    constructor Create(const Key: TBytes); overload;
    constructor Create(const Key: ustring); overload;
    procedure InputFilter(const InputStream, OutputStream: TStream); override;
    procedure OutputFilter(const InputStream, OutputStream: TStream); override;
  end;

  // filter data constructor
  function XTEAFilter(const Key: TBytes): TCustomFilter; overload;
  function XTEAFilter(const Key: ustring): TCustomFilter; overload;

implementation

  // filter data constructor
function XTEAFilter(const Key: TBytes): TCustomFilter; overload;
begin
  Result := TXTEAFilter.Create(Key);
end;

function XTEAFilter(const Key: ustring): TCustomFilter; overload;
begin
  Result := TXTEAFilter.Create(Key);
end;

{ TXTEAFilter }

constructor TXTEAFilter.Create(const Key: TBytes);
begin
  FID := '32D80BECF7F64C8890AC6F7469736001';
  FKey := Key;
end;

constructor TXTEAFilter.Create(const Key: ustring);
begin
  Create(GetBytesFromUnicodeString(Key));
end;

procedure TXTEAFilter.InputFilter(const InputStream, OutputStream: TStream);
begin
  inherited;

  XTeaEncryptStream(InputStream, OutputStream, FKey);
end;

procedure TXTEAFilter.OutputFilter(const InputStream, OutputStream: TStream);
begin
  inherited;

  XTeaDecryptStream(InputStream, OutputStream, FKey);
end;

end.
