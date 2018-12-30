unit f_Main;

interface

uses
  Windows, SysUtils, Classes, Controls, Forms, StdCtrls, Menus, ExtCtrls, Dialogs,

  // cromis units
  Cromis.PageProducer;

type
  TfMain = class(TForm)
    OpenDialog: TOpenDialog;
    mmSource: TMemo;
    MainMenu1: TMainMenu;
    miFileGroup: TMenuItem;
    miOpenFile: TMenuItem;
    miActionGroup: TMenuItem;
    miParseToStream: TMenuItem;
    mmDestination: TMemo;
    spVertical: TSplitter;
    procedure miParseToStreamClick(Sender: TObject);
    procedure miOpenFileClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
    PageProducer: TPageProducerEx;
    procedure OnContentTag(const Sender: TObject;
                           const TagString: string;
                           const TagParams: TStrings;
                           const PageParams: TStrings;
                           const TagContent: string;
                           const ContentStream: TStream;
                           var ParseInnerContent: Boolean);
  public
    { Public declarations }
  end;

var
  fMain: TfMain;

implementation

{$R *.dfm}

procedure TfMain.FormCreate(Sender: TObject);
begin
  PageProducer := TPageProducerEx.Create;
  PageProducer.TrimTrailingCRLF := True;
end;

procedure TfMain.FormDestroy(Sender: TObject);
begin
  FreeAndNil(PageProducer);
end;

procedure TfMain.miOpenFileClick(Sender: TObject);
begin
  if OpenDialog.Execute then
    mmSource.Lines.LoadFromFile(OpenDialog.FileName);
end;

procedure TfMain.miParseToStreamClick(Sender: TObject);
var
  InStream: TMemoryStream;
  OutStream: TMemoryStream;
begin
  OutStream := TMemoryStream.Create;
  try
    InStream := TMemoryStream.Create;
    try
      mmSource.Lines.SaveToStream(InStream);
      InStream.Seek(0, soFromBeginning);

      PageProducer.ExtraTags.Add('table');

      PageProducer.OnContentTag := OnContentTag;
      PageProducer.SourceStream := InStream;
      PageProducer.ProduceContent(OutStream);

      OutStream.Seek(0, soFromBeginning);
      mmDestination.Lines.LoadFromStream(OutStream);
    finally
      InStream.Free;
    end;
  finally
    OutStream.Free;
  end;
end;

procedure TfMain.OnContentTag(const Sender: TObject;
                              const TagString: string;
                              const TagParams: TStrings;
                              const PageParams: TStrings;
                              const TagContent: string;
                              const ContentStream: TStream;
                              var ParseInnerContent: Boolean);
begin
  {
  case TagContent <> '' of
    True: ContentStream.Write(TagContent[1], Length(TagContent));
    False: ContentStream.Write('To je TAG', 9);
  end;
  }

  ContentStream.Write('To je TAG', 9);

  if TagString = 'table' then
  begin
    TagParams.Text;
  end;
end;

end.
