unit DocFilters.Main;

interface

uses
  Windows, SysUtils, Classes, Controls, Forms, Dialogs, StdCtrls, Menus,

  // cromis units
  Cromis.SimpleStorage, Cromis.SSF.XTEA, Cromis.SSF.Zlib;

type
  TfMain = class(TForm)
    btnEncrypt: TButton;
    btnDecrypt: TButton;
    btnChainIn: TButton;
    btnCompress: TButton;
    btnDecompress: TButton;
    btnChainOut: TButton;
    procedure btnCompressClick(Sender: TObject);
    procedure btnDecompressClick(Sender: TObject);
    procedure btnChainOutClick(Sender: TObject);
    procedure btnChainInClick(Sender: TObject);
    procedure btnDecryptClick(Sender: TObject);
    procedure btnEncryptClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  fMain: TfMain;

implementation

{$R *.dfm}

procedure TfMain.btnChainInClick(Sender: TObject);
var
  DataPath: string;
  NormalXML: ISimpleStorage;
  NormalXMLFile: string;
  FilteredXMLFileOne: string;
  FilteredXMLFileTwo: string;
  DocumentFilterChain: IDocumentFilterChain;
begin
  DataPath := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) + 'Data';
  FilteredXMLFileOne := IncludeTrailingPathDelimiter(DataPath) + 'FilteredXML_Sample1.xml';
  FilteredXMLFileTwo := IncludeTrailingPathDelimiter(DataPath) + 'FilteredXML_Sample2.xml';
  NormalXMLFile := IncludeTrailingPathDelimiter(DataPath) + 'DecryptedXML.xml';
  NormalXML := StorageFromFile(NormalXMLFile);

  { first way to do the chaining }

  DocumentFilterChain := CreateDocumentFilterChain;
  DocumentFilterChain.AddFilter(CompressedStorage);
  DocumentFilterChain.AddFilter(EncryptedStorage('MyStrongKey'));
  DocumentFilterChain.SaveToFile(NormalXML, FilteredXMLFileOne);

  { second way to do the chaining }

  NormalXML.Filter(ZLIB).Filter(XTEA('MyStrongKey')).SaveToFile(FilteredXMLFileTwo);
end;

procedure TfMain.btnChainOutClick(Sender: TObject);
var
  DataPath: string;
  NormalXML: ISimpleStorage;
  NormalXMLFileOne: string;
  NormalXMLFileTwo: string;
  FilteredXMLFileOne: string;
  FilteredXMLFileTwo: string;
  DocumentFilterChain: IDocumentFilterChain;
begin
  DataPath := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) + 'Data';
  FilteredXMLFileOne := IncludeTrailingPathDelimiter(DataPath) + 'FilteredXML_Sample1.xml';
  FilteredXMLFileTwo := IncludeTrailingPathDelimiter(DataPath) + 'FilteredXML_Sample2.xml';
  NormalXMLFileOne := IncludeTrailingPathDelimiter(DataPath) + 'NormalXML_Sample1.xml';
  NormalXMLFileTwo := IncludeTrailingPathDelimiter(DataPath) + 'NormalXML_Sample2.xml';

  { first way to do the chaining }

  DocumentFilterChain := CreateDocumentFilterChain;
  DocumentFilterChain.AddFilter(CompressedStorage);
  DocumentFilterChain.AddFilter(EncryptedStorage('MyStrongKey'));
  NormalXML := DocumentFilterChain.LoadFromFile(FilteredXMLFileOne);
  NormalXML.SaveToFile(NormalXMLFileOne);

  { second way to do the chaining }

  NormalXML := CreateStorage;
  NormalXML.Filter(ZLIB).Filter(XTEA('MyStrongKey')).LoadFromFile(FilteredXMLFileTwo);
  NormalXML.SaveToFile(NormalXMLFileTwo);
end;

procedure TfMain.btnCompressClick(Sender: TObject);
var
  DataPath: string;
  DecompressedXMLFile: string;
  CompressedXMLFileOne: string;
  CompressedXMLFileTwo: string;
  DecompressedXML: ISimpleStorage;
begin
  DataPath := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) + 'Data';
  DecompressedXMLFile := IncludeTrailingPathDelimiter(DataPath) + 'DecryptedXML.xml';
  CompressedXMLFileOne := IncludeTrailingPathDelimiter(DataPath) + 'CompressedXML_Sample1.xml';
  CompressedXMLFileTwo := IncludeTrailingPathDelimiter(DataPath) + 'CompressedXML_Sample2.xml';
  DecompressedXML := StorageFromFile(DecompressedXMLFile);

  { first way to do the compression }

  CompressedStorage.SaveToFile(DecompressedXML, CompressedXMLFileOne);

  { second way to do the compression }

  DecompressedXML.Filter(ZLIB).SaveToFile(CompressedXMLFileTwo);
end;

procedure TfMain.btnDecompressClick(Sender: TObject);
var
  DataPath: string;
  DecompressedXML: ISimpleStorage;
  CompressedXMLFileOne: string;
  CompressedXMLFileTwo: string;
  DecompressedXMLFileOne: string;
  DecompressedXMLFileTwo: string;
begin
  DataPath := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) + 'Data';
  CompressedXMLFileOne := IncludeTrailingPathDelimiter(DataPath) + 'CompressedXML_Sample1.xml';
  CompressedXMLFileTwo := IncludeTrailingPathDelimiter(DataPath) + 'CompressedXML_Sample2.xml';
  DecompressedXMLFileOne := IncludeTrailingPathDelimiter(DataPath) + 'DecompressedXML_Sample1.xml';
  DecompressedXMLFileTwo := IncludeTrailingPathDelimiter(DataPath) + 'DecompressedXML_Sample2.xml';

  { first way to do the decompression }

  DecompressedXML := CompressedStorage.LoadFromFile(CompressedXMLFileOne);
  DecompressedXML.SaveToFile(DecompressedXMLFileOne);

  { second way to do the decompression }

  DecompressedXML := CreateStorage;
  DecompressedXML.Filter(ZLIB).LoadFromFile(CompressedXMLFileTwo);
  DecompressedXML.SaveToFile(DecompressedXMLFileTwo);
end;

procedure TfMain.btnDecryptClick(Sender: TObject);
var
  DataPath: string;
  DecryptedXML: ISimpleStorage;
  DecryptedXMLFileOne: string;
  DecryptedXMLFileTwo: string;
  EncryptedXMLFileOne: string;
  EncryptedXMLFileTwo: string;
begin
  DataPath := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) + 'Data';
  EncryptedXMLFileOne := IncludeTrailingPathDelimiter(DataPath) + 'EncryptedXML_Sample1.xml';
  EncryptedXMLFileTwo := IncludeTrailingPathDelimiter(DataPath) + 'EncryptedXML_Sample2.xml';
  DecryptedXMLFileOne := IncludeTrailingPathDelimiter(DataPath) + 'DecryptedXML_Sample1.xml';
  DecryptedXMLFileTwo := IncludeTrailingPathDelimiter(DataPath) + 'DecryptedXML_Sample2.xml';

  { first way to do the decryption }

  DecryptedXML := EncryptedStorage('MyStrongKey').LoadFromFile(EncryptedXMLFileOne);
  DecryptedXML.SaveToFile(DecryptedXMLFileOne);

  { second way to do the decryption }

  DecryptedXML := CreateStorage;
  DecryptedXML.Filter(XTEA('MyStrongKey')).LoadFromFile(EncryptedXMLFileTwo);
  DecryptedXML.SaveToFile(DecryptedXMLFileTwo);
end;

procedure TfMain.btnEncryptClick(Sender: TObject);
var
  DataPath: string;
  DecryptedXMLFile: string;
  EncryptedXMLFileOne: string;
  EncryptedXMLFileTwo: string;
  DecryptedXML: ISimpleStorage;
begin
  DataPath := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) + 'Data';
  DecryptedXMLFile := IncludeTrailingPathDelimiter(DataPath) + 'DecryptedXML.xml';
  EncryptedXMLFileOne := IncludeTrailingPathDelimiter(DataPath) + 'EncryptedXML_Sample1.xml';
  EncryptedXMLFileTwo := IncludeTrailingPathDelimiter(DataPath) + 'EncryptedXML_Sample2.xml';
  DecryptedXML := StorageFromFile(DecryptedXMLFile);

  { first way to do the encryption }

  EncryptedStorage('MyStrongKey').SaveToFile(DecryptedXML, EncryptedXMLFileOne);

  { second way to do the encryption }

  DecryptedXML.Filter(XTEA('MyStrongKey')).SaveToFile(EncryptedXMLFileTwo);
end;

end.
