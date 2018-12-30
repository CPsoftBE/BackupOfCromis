unit Cromis.MVC.Common;

interface

uses
  SysUtils, Classes, IniFiles,

  // cromis units
  Cromis.AnyValue;

const
  cMIME = 'MIME';

type
  TDispatchResult = (drSuccess, drDoLogin, drRedirectToLogin, drForbiden, drFailed);

  ISessionContent = Interface(IInterface)
  ['{0C2C2ADE-4B0D-4150-B2E0-51901492AECC}']
    function GetData: TStream;
    function GetEncoding: string;
    function GetHTMLRoot: string;
    function GetExitCode: Cardinal;
    procedure SetHTMLRoot(const Value: string);
    procedure SetEncoding(const Value: string);
    procedure SetExitCode(const Value: Cardinal);
    property ExitCode: Cardinal read GetExitCode write SetExitCode;
    property HTMLRoot: string read GetHTMLRoot write SetHTMLRoot;
    property Encoding: string read GetEncoding write SetEncoding;
    property Data: TStream read GetData;
    procedure Clear;
  end;

  ISessionData = Interface(IInterface)
  ['{9C3CC7B7-B853-4D50-BBC2-F36051335F12}']
    function GetID: string;
    function GetValues: IValueList;
    function GetAuthenticated: Boolean;
    function GetSessionContent: ISessionContent;
    function GetDispatchResult: TDispatchResult;
    procedure SetAuthenticated(const Value: Boolean);
    procedure SetDispatchResult(const Value: TDispatchResult);
    property DispatchResult: TDispatchResult read GetDispatchResult write SetDispatchResult;
    property Authenticated: Boolean read GetAuthenticated write SetAuthenticated;
    property SessionContent: ISessionContent read GetSessionContent;
    property Values: IValueList read GetValues;
    property ID: string read GetID;
  end;

  IMVCData = Interface(IInterface)
  ['{9C61B770-FC1B-4BDF-ACE1-F735D758B0FF}']
    function GetValues: IValueList;
    function GetActiveOnly: Boolean;
    function GetParameters: IValueList;
    function GetUpdateViews: Boolean;
    procedure SetActiveOnly(const Value: Boolean);
    procedure SetUpdateViews(const Value: Boolean);
    property UpdateViews: Boolean read GetUpdateViews write SetUpdateViews;
    property ActiveOnly: Boolean read GetActiveOnly write SetActiveOnly;
    property Parameters: IValueList read GetParameters;
    property Values: IValueList read GetValues;
  end;

  IMVCObserver = Interface(IInterface)
  ['{633D8A96-AC42-489C-8E10-A387113EBB29}']
    procedure UpdateView(const Section, ID: string; const Data: IMVCData);
    function IsActive: Boolean;
  end;

  IMimeTypes = Interface(IInterface)
  ['{43C821F4-4499-4698-A3F2-32AA83EFC4D1}']
    function GetMime(const Ext: string): string;
    procedure SetMime(const Ext: string; const Value: string);
    property Mime[const Ext: string]: string read GetMime write SetMime;
  end;

  // acquire different datafunctions
  function AcquireSessionData(const SessionID: string): ISessionData;
  function AcquireMimeTypes: IMimeTypes;
  function AcquireMVCData: IMVCData;

implementation

type
  TSessionContent = class(TInterfacedObject, ISessionContent)
  private
    FEncoding: string;
    FHTMLRoot: string;
    FExitCode: Cardinal;
    FData: TMemoryStream;
    function GetData: TStream;
    function GetEncoding: string;
    function GetHTMLRoot: string;
    function GetExitCode: Cardinal;
    procedure SetHTMLRoot(const Value: string);
    procedure SetEncoding(const Value: string);
    procedure SetExitCode(const Value: Cardinal);
  public
    constructor Create;
    destructor Destroy; override;
    property ExitCode: Cardinal read GetExitCode write SetExitCode;
    property HTMLRoot: string read GetHTMLRoot write SetHTMLRoot;
    property Encoding: string read GetEncoding write SetEncoding;
    property Data: TStream read GetData;
    procedure Clear;
  end;

  TSessionData = class(TInterfacedObject, ISessionData)
  private
    FID: string;
    FValues: IValueList;
    FAuthenticated: Boolean;
    FSessionContent: ISessionContent;
    FDispatchResult: TDispatchResult;
    function GetID: string;
    function GetValues: IValueList;
    function GetAuthenticated: Boolean;
    function GetDispatchResult: TDispatchResult;
    function GetSessionContent: ISessionContent;
    procedure SetAuthenticated(const Value: Boolean);
    procedure SetDispatchResult(const Value: TDispatchResult);
  public
    constructor Create(const ID: string);
    property DispatchResult: TDispatchResult read GetDispatchResult write SetDispatchResult;
    property Authenticated: Boolean read GetAuthenticated write SetAuthenticated;
    property SessionContent: ISessionContent read GetSessionContent;
    property Values: IValueList read GetValues;
    property ID: string read GetID;
  end;

  TMVCData = class(TInterfacedObject, IMVCData)
  private
    FValues: IValueList;
    FParameters: IValueList;
    FActiveOnly: Boolean;
    FUpdateViews: Boolean;
    function GetValues: IValueList;
    function GetActiveOnly: Boolean;
    function GetParameters: IValueList;
    function GetUpdateViews: Boolean;
    procedure SetActiveOnly(const Value: Boolean);
    procedure SetUpdateViews(const Value: Boolean);
  public
    constructor Create;
    property Values: IValueList read GetValues;
    property Parameters: IValueList read GetParameters;
    property ActiveOnly: Boolean read GetActiveOnly write SetActiveOnly;
    property UpdateViews: Boolean read GetUpdateViews write SetUpdateViews;
  end;

  TMimeTypes = class(TInterfacedObject, IMimeTypes)
  private
    FMimeTypes: TMemIniFile;
    function GetMime(const Ext: string): string;
    procedure SetMime(const Ext: string; const Value: string);
  public
    constructor Create;
    destructor Destroy; override;
    property Mime[const Ext: string]: string read GetMime write SetMime;
  end;

var
  MimeTypes: IMimeTypes;

function AcquireMVCData: IMVCData;
begin
  Result := TMVCData.Create;
end;

function AcquireMimeTypes: IMimeTypes;
begin
  if MimeTypes = nil then
    MimeTypes := TMimeTypes.Create;
  Result := MimeTypes;
end;

function AcquireSessionData(const SessionID: string): ISessionData;
begin
  Result := TSessionData.Create(SessionID);
end;

{ TMVCData }

constructor TMVCData.Create;
begin
  FParameters := AcquireValueList;
  FValues := AcquireValueList;
  FUpdateViews := True;
  FActiveOnly := False;
end;

function TMVCData.GetUpdateViews: Boolean;
begin
  Result := FUpdateViews;
end;

function TMVCData.GetValues: IValueList;
begin
  Result := FValues;
end;

function TMVCData.GetActiveOnly: Boolean;
begin
  Result := FActiveOnly;
end;

function TMVCData.GetParameters: IValueList;
begin
  Result := FParameters;
end;

procedure TMVCData.SetUpdateViews(const Value: Boolean);
begin
  FUpdateViews := Value;
end;

procedure TMVCData.SetActiveOnly(const Value: Boolean);
begin
  FActiveOnly := Value;
end;

{ TSessionData }

constructor TSessionData.Create(const ID: string);
begin
  FSessionContent := TSessionContent.Create;
  FValues := AcquireValueList;
  FID := ID;
end;

function TSessionData.GetValues: IValueList;
begin
  Result := FValues;
end;

function TSessionData.GetAuthenticated: Boolean;
begin
  Result := FAuthenticated;
end;

function TSessionData.GetDispatchResult: TDispatchResult;
begin
  Result := FDispatchResult;
end;

procedure TSessionData.SetAuthenticated(const Value: Boolean);
begin
  FAuthenticated := Value;
end;

procedure TSessionData.SetDispatchResult(const Value: TDispatchResult);
begin
  FDispatchResult := Value;
end;

function TSessionData.GetID: string;
begin
  Result := FID;
end;

function TSessionData.GetSessionContent: ISessionContent;
begin
  Result := FSessionContent;
end;

{ TSessionContent }

procedure TSessionContent.Clear;
begin
  FEncoding := '';
  FData.Clear;
end;

constructor TSessionContent.Create;
begin
  FData := TMemoryStream.Create;
end;

destructor TSessionContent.Destroy;
begin
  FreeAndNil(FData);

  inherited;
end;

function TSessionContent.GetData: TStream;
begin
  Result := FData;
end;

function TSessionContent.GetEncoding: string;
begin
  Result := FEncoding;
end;

function TSessionContent.GetExitCode: Cardinal;
begin
  Result := FExitCode;
end;

function TSessionContent.GetHTMLRoot: string;
begin
  Result := FHTMLRoot;
end;

procedure TSessionContent.SetEncoding(const Value: string);
begin
  FEncoding := Value;
end;

procedure TSessionContent.SetExitCode(const Value: Cardinal);
begin
  FExitCode := Value;
end;

procedure TSessionContent.SetHTMLRoot(const Value: string);
begin
  FHTMLRoot := Value;
end;

{ TMimeTypes }

constructor TMimeTypes.Create;
begin
  FMimeTypes := TMemIniFile.Create('');

  FMimeTypes.WriteString(cMIME, 'TXT', 'text/plain');
  FMimeTypes.WriteString(cMIME, 'HTM', 'text/html');
  FMimeTypes.WriteString(cMIME, 'HTML', 'text/html');
  FMimeTypes.WriteString(cMIME, 'BMP', 'image/x-ms-bmp');
  FMimeTypes.WriteString(cMIME, 'JPG', 'image/jpeg');
  FMimeTypes.WriteString(cMIME, 'JPEG', 'image/jpeg');
  FMimeTypes.WriteString(cMIME, 'JPE', 'image/jpeg');
  FMimeTypes.WriteString(cMIME, 'IEF', 'image/ief');
  FMimeTypes.WriteString(cMIME, 'TIF', 'image/tiff');
  FMimeTypes.WriteString(cMIME, 'TIFF', 'image/tiff');
  FMimeTypes.WriteString(cMIME, 'PNG', 'image/x-png');
  FMimeTypes.WriteString(cMIME, 'GIF', 'image/gif');
  FMimeTypes.WriteString(cMIME, 'CSS', 'text/css');
  FMimeTypes.WriteString(cMIME, 'JS', 'text/javascript');
  FMimeTypes.WriteString(cMIME, 'LS', 'text/javascript');
  FMimeTypes.WriteString(cMIME, 'MOCHA', 'text/javascript');
end;

destructor TMimeTypes.Destroy;
begin
  FreeAndNil(FMimeTypes);

  inherited;
end;

function TMimeTypes.GetMime(const Ext: string): string;
begin
  Result := FMimeTypes.ReadString(cMIME, Ext, '');
end;

procedure TMimeTypes.SetMime(const Ext, Value: string);
begin
  FMimeTypes.WriteString(cMIME, Ext, Value);
end;

end.
