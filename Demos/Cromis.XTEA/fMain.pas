unit fMain;

interface

uses
  Windows, SysUtils, Classes, Controls, Forms, Dialogs, StdCtrls, XPMan,

  // library units
  Cromis.XTEA;

type
  TForm1 = class(TForm)
    mmOriginal: TMemo;
    mmDecrypted: TMemo;
    btnEnrypt: TButton;
    XPManifest: TXPManifest;
    btnDecrypt: TButton;
    mmEncrypted: TMemo;
    procedure btnEnryptClick(Sender: TObject);
    procedure btnDecryptClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
    FEncodedBytes: TMemoryStream;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.btnDecryptClick(Sender: TObject);
var
  OutStream: TMemoryStream;
begin
  OutStream := TMemoryStream.Create;
  try
    FEncodedBytes.Seek(0, soFromBeginning);

    XTeaDecryptStream(FEncodedBytes, OutStream, GetBytesFromAnsiString('1234567890'));
    OutStream.Seek(0, soFromBeginning);

    mmEncrypted.Lines.Text := 'The memo content is now decoded!';
    mmDecrypted.Lines.LoadFromStream(OutStream);
  finally
    OutStream.Free;
  end;
end;

procedure TForm1.btnEnryptClick(Sender: TObject);
var
  InStream: TMemoryStream;
begin
  InStream := TMemoryStream.Create;
  try
    FEncodedBytes.Clear;
    mmDecrypted.Lines.Clear;
    mmOriginal.Lines.SaveToStream(InStream);
    InStream.Seek(0, soFromBeginning);
    XTeaEncryptStream(InStream, FEncodedBytes, GetBytesFromAnsiString('1234567890'));

    mmEncrypted.Lines.Text := 'The memo content is now encoded!'
  finally
    InStream.Free;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FEncodedBytes := TMemoryStream.Create;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FEncodedBytes);
end;

end.
