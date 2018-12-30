unit Collection.Main;

interface

uses
  Windows, SysUtils, Classes, Controls, Forms, Dialogs, StdCtrls, ComCtrls,

  // cromis units
  Cromis.SimpleStorage, Cromis.SimpleStorage.Collection;

type
  TDeviceObject = class
    InBound: Int64;
    OutBound: Int64;
  end;

  TfMain = class(TForm)
    btnEnumerateDevice: TButton;
    btnSelectRandomDocuments: TButton;
    btnCalcualteSum: TButton;
    lvCollectionLog: TListView;
    procedure btnSelectRandomDocumentsClick(Sender: TObject);
    procedure btnEnumerateDeviceClick(Sender: TObject);
    procedure btnCalcualteSumClick(Sender: TObject);
  private
    procedure OnDeviceCallback(const Document: IDocument;
                               const Element: IElement;
                               const Data: Pointer);
  public
    { Public declarations }
  end;

var
  fMain: TfMain;

implementation

{$R *.dfm}

procedure TfMain.btnEnumerateDeviceClick(Sender: TObject);
var
  DataPath: string;
  ListItem: TListItem;
  Documents: IDocuments;
begin
  DataPath := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) + 'Data';
  lvCollectionLog.Clear;

  Documents := CreateCollection(DataPath, False);
  Documents.Get('//Device[@ID="3FBF5000735108010E1212674B5D036A"]',
    procedure(const Document: IDocument; const Element: IElement; const Data: Pointer)
    begin
      ListItem := lvCollectionLog.Items.Add;
      ListItem.SubItems.Add(Element.GetAttr('InBound').AsString);
      ListItem.SubItems.Add(Element.GetAttr('OutBound').AsString);
      ListItem.Caption := ExtractFileName(Document.Path);
    end
  );
end;

procedure TfMain.btnSelectRandomDocumentsClick(Sender: TObject);
var
  Document: IDocument;
  ListItem: TListItem;
  DataPath: string;
  Element: IElement;
  Subset: IDocuments;
begin
  DataPath := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) + 'Data';
  lvCollectionLog.Clear;
  Randomize;

  Subset := CreateCollection(DataPath, False).Get('//Device[@ID="3FBF5000735108010E1212674B5D036A"]',
    procedure(const Document: IDocument; var Include: Boolean; const Data: Pointer)
    begin
      Include := Random(10) > 5;
    end
  );

  for Document in Subset do
  begin
    Element := Document.Data.Get('//Device[@ID="3FBF5000735108010E1212674B5D036A"]');

    ListItem := lvCollectionLog.Items.Add;
    ListItem.SubItems.Add(Element.GetAttr('InBound').AsString);
    ListItem.SubItems.Add(Element.GetAttr('OutBound').AsString);
    ListItem.Caption := ExtractFileName(Document.Path);
  end;
end;

procedure TfMain.btnCalcualteSumClick(Sender: TObject);
var
  I: Integer;
  DataPath: string;
  ListItem: TListItem;
  HashTable: TStringList;
  Documents: IDocuments;
  DeviceObject: TDeviceObject;
begin
  DataPath := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) + 'Data';
  lvCollectionLog.Clear;

  HashTable := TStringList.Create;
  try
    //HashTable.OwnsObjects := True;
    Documents := CreateCollection(DataPath, False);
    Documents.Get('//Device', OnDeviceCallback, HashTable);

    for I := 0 to HashTable.Count - 1 do
    begin
      DeviceObject := TDeviceObject(HashTable.Objects[I]);

      ListItem := lvCollectionLog.Items.Add;
      ListItem.SubItems.Add(IntToStr(DeviceObject.InBound));
      ListItem.SubItems.Add(IntToStr(DeviceObject.OutBound));
      ListItem.Caption := HashTable[I];
    end;
  finally
    HashTable.Free;
  end;

end;

procedure TfMain.OnDeviceCallback(const Document: IDocument; const Element: IElement; const Data: Pointer);
var
  DeviceIdx: Integer;
  HashTable: TStringList;
  DeviceObject: TDeviceObject;
begin
  HashTable := TStringList(Data);
  DeviceIdx := HashTable.IndexOf(Element.GetAttr('ID').AsString);

  if DeviceIdx = -1 then
    DeviceIdx := HashTable.AddObject(Element.GetAttr('ID').AsString, TDeviceObject.Create);

  DeviceObject := TDeviceObject(HashTable.Objects[DeviceIdx]);
  DeviceObject.InBound := DeviceObject.InBound + Element.GetAttr('InBound').AsInteger;
  DeviceObject.OutBound := DeviceObject.OutBound + Element.GetAttr('OutBound').AsInteger;
end;

end.
