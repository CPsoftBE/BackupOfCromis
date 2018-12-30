unit Builder.Main;

interface

uses
  Windows,  SysUtils, Classes, Controls, Forms, StdCtrls, Dialogs,

  // cromis units
  Cromis.SimpleStorage, Cromis.SimpleStorage.Builder;

type
  TfMain = class(TForm)
    btnExample1: TButton;
    btnExample2: TButton;
    mmResultingXML: TMemo;
    procedure btnExample1Click(Sender: TObject);
    procedure btnExample2Click(Sender: TObject);
  private
     procedure OnAddElements(const Element: IElement);
  public
  end;

var
  fMain: TfMain;

implementation

{$R *.dfm}

procedure TfMain.btnExample1Click(Sender: TObject);
var
  MyStorage: ISimpleStorage;
begin
  MyStorage := CreateStorage('Books');

  CreateBuilder(MyStorage).Construct(
  [
    AddElement('English',
    [
      AddElement('Science',
      [
        AddElement('Mathematics', 5),
        AddElement('Physics', 0),
        AddElement('Computers', 8),
        AddElement('Engineering',  4),
        AddElement('Electoronics',  8),
        AddElement('Biology',
        [
          AddElement('Microbiology',  4),
          AddElement('Undefined',  3),
          {$IF CompilerVersion >= 20}
            Add(
              procedure(const Element: IElement)
              begin
                // add few elements here
                Element.Append('InsertedNode').AsInteger := 1;
                Element.Append('InsertedNode').AsInteger := 2;
                Element.Append('InsertedNode').AsInteger := 3;
              end
            )
          {$ELSE}
            Add(OnAddElements)
          {$IFEND}
        ])
      ]),
      AddElement('Musical',
      [
        AddElement('Science', 5),
        AddElement('Physics', 0),
        AddElement('Computers', 8)
      ])
    ])
  ]);

  mmResultingXML.Lines.Text := MyStorage.Content(True);
end;

procedure TfMain.btnExample2Click(Sender: TObject);
var
  MyStorage: ISimpleStorage;
begin
  MyStorage := CreateStorage('Demografija');

  CreateBuilder(MyStorage).Construct(
  [
    AddElement('Males',
    [
      AddElement('Janez', 'Novak'),
      AddElement('Darko', 'Gazvoda'),
      AddElement('Mitja', 'Dežela'),
      AddElement('Tilen', 'Medved')
    ]),
    AddElement('Females',
    [
      AddElement('Person', [AddAttr('Name', 'Marija'), AddValue('Gorenjska')]),
      AddElement('Person', [AddAttr('Name', 'Tanja'), AddValue('Notranjska')]),
      AddElement('Person', [AddAttr('Name', 'Maja'), AddValue('Primorska')]),
      AddElement('Person', [AddAttr('Name', 'Tadeja'), AddValue('Pomurje')]),
      AddElement('Person', [AddAttr('Name', 'Irma'), AddValue('Posoèje')]),
      AddElement('Person', [AddAttr('Name', 'Marija'), AddValue('Notranjska')])
    ])
  ]);

  mmResultingXML.Lines.Text := MyStorage.Content(True);
end;

procedure TfMain.OnAddElements(const Element: IElement);
begin
  // add few elements here
  Element.Append('InsertedNode').AsInteger := 1;
  Element.Append('InsertedNode').AsInteger := 2;
  Element.Append('InsertedNode').AsInteger := 3;
end;

end.
