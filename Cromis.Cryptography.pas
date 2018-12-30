unit Cromis.Cryptography;

interface

uses
  Windows, SysUtils;

function GetNewFormatedGUID: string;

implementation

function GetNewGUID: string;
var
  NewGUID: TGUID;
begin
  // create GUID
  CreateGUID(NewGUID);
  Result := GUIDToString(NewGUID);
end;

function FormatGUID(const GUID: string): string;
var
  I: Integer;
  TempStr: string;
const
  RemoveSet = [#123, #125, #45];
begin
  for I := 1 to length(GUID) do
  begin
    if not (AnsiChar(GUID[I]) in RemoveSet) then
      Tempstr := Tempstr + GUID[I];
  end;
  Result := TempStr;
end;

function GetNewFormatedGUID: string;
begin
  Result := FormatGUID(GetNewGUID);
end;

end.
