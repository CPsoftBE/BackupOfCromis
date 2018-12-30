unit Adapters.Main;

interface

uses
  SysUtils, Classes, Controls, Forms, DB, DBClient, StdCtrls, DBTables, Grids, DBGrids,
  Provider, Menus, 

  // simple storage units and adapters units
  Cromis.SimpleStorage, Cromis.SSA.DataSet;

type
  TfMain = class(TForm)
    mmXML: TMemo;
    DataSource: TDataSource;
    DataSetProvider: TDataSetProvider;
    ClientDS: TClientDataSet;
    MainMenu: TMainMenu;
    mgAction: TMenuItem;
    miDataSetToSS: TMenuItem;
    DBGrid: TDBGrid;
    miSSToDataset: TMenuItem;
    procedure miDataSetToSSClick(Sender: TObject);
    procedure miSSToDatasetClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  fMain: TfMain;

implementation

{$R *.dfm}

procedure TfMain.FormCreate(Sender: TObject);
begin
  ClientDS.FieldDefs.Add('ID', ftInteger, 0, False);
  ClientDS.FieldDefs.Add('Name', ftString, 10, False);
  ClientDS.FieldDefs.Add('Data', ftString, 10, False);
  ClientDS.CreateDataset;
end;

procedure TfMain.miDataSetToSSClick(Sender: TObject);
var
  I: Integer;
  SS: ISimpleStorage;
begin
  ClientDS.Open;
  ClientDS.EmptyDataSet;

  for I := 1 to 100 do
    ClientDS.InsertRecord([I, 'SomeName', 'SomeData']);

  SS := CreateStorage;
  SS.Ensure('MemTable').Adapter(DataSet).Load(ClientDS);

  mmXML.Lines.Text := SS.Content(True);
end;

procedure TfMain.miSSToDatasetClick(Sender: TObject);
var
  I: Integer;
  SS: ISimpleStorage;
  NewField: IElement;
  NewRecord: IElement;
begin
  ClientDS.Active := True;
  ClientDS.EmptyDataSet;
  SS := CreateStorage;

  for I := 1 to 100 do
  begin
    NewRecord := SS.Ensure('MemTable').Append('Record');

    NewField := NewRecord.Append('Field');
    NewField.EnsureAttr('Name').AsString := 'ID';
    NewField.EnsureAttr('Type').AsString := 'ftInteger';
    NewField.AsInteger := I;

    NewField := NewRecord.Append('Field');
    NewField.EnsureAttr('Name').AsString := 'Name';
    NewField.EnsureAttr('Type').AsString := 'ftString';
    NewField.AsString := 'SomeName';

    NewField := NewRecord.Append('Field');
    NewField.EnsureAttr('Name').AsString := 'Data';
    NewField.EnsureAttr('Type').AsString := 'ftString';
    NewField.AsString := 'SomeData';
  end;

  SS.Get('MemTable').Adapter(DataSet).Save(ClientDS);
  mmXML.Lines.Text := SS.Content(True);
end;

end.
