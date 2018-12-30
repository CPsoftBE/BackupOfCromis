unit SimpleStorage.Main;

interface

uses
  Windows, SysUtils, Classes, Controls, Forms, Dialogs, ExtCtrls, StdCtrls, XPMan, jpeg,

  // cromis units (added compression filter and excryption filter)
  Cromis.SimpleStorage, Cromis.SSF.Zlib, Cromis.SSF.XTEA, OmniXML;

const
  DIR_DATA = 'Data\';

  FILE_DATA_ONE = 'Sample_One.xml';
  FILE_DATA_TWO = 'Sample_Two.xml';
  FILE_DATA_IMG = 'Motherboard.jpg';
  FILE_DATA_THREE = 'Sample_Three.xml';

type
  TfMain = class(TForm)
    mmSourceData: TMemo;
    mmResultsData: TMemo;
    spData: TSplitter;
    pnActions: TPanel;
    imgLogo: TImage;
    rgDemoSelection: TRadioGroup;
    XPManifest: TXPManifest;
    procedure rgDemoSelectionClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    FRootDir: string;
  public
    { Public declarations }
  end;

var
  fMain: TfMain;

implementation

{$R *.dfm}

procedure TfMain.FormCreate(Sender: TObject);
begin
  FRootDir := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)));
end;

procedure TfMain.rgDemoSelectionClick(Sender: TObject);
var
  MS: TMemoryStream;
  Jpeg: TJPEGImage;
  Element: IElement;
  Node1, Node2: IElement;
  SrcStorage: ISimpleStorage;
  TrgStorage: ISimpleStorage;
  LegacyDoc: IXMLDocument;
  ElementValue: string;
begin
  if rgDemoSelection.ItemIndex > -1 then
  begin
    mmSourceData.Lines.Clear;
    mmResultsData.Lines.Clear;

    //****************************************
    // here we just select one single value
    //****************************************
    if rgDemoSelection.ItemIndex = 0 then
    begin
      // show the original content
      mmSourceData.Lines.LoadFromFile(FRootDir + DIR_DATA + FILE_DATA_ONE);

      // create the storage and select the single value of interest
      SrcStorage := StorageFromFile(FRootDir + DIR_DATA + FILE_DATA_ONE);

      // this is the exact path to the wanded element
      Element := SrcStorage.Get('English/Science/Computers');
      mmResultsData.Lines.Add(Format('%s : %s', [Element.Name, Element.AsString]));

      // this is the path with the predicate but the same result
      Element := SrcStorage.Get('English/Science/*[@Difficulty="Medium"]');
      mmResultsData.Lines.Add(Format('%s : %s', [Element.Name, Element.AsString]));

      Node1 := SrcStorage.Get('English/Science');
      Node2 := SrcStorage.Get('English/Science/Computers');

      Node1.Remove(Node2);

      mmResultsData.Lines.Text := SrcStorage.Content(True);
    end
    //****************************************
    // here we read all values of a single node
    //****************************************
    else if rgDemoSelection.ItemIndex = 1 then
    begin
      // show the original content
      mmSourceData.Lines.LoadFromFile(FRootDir + DIR_DATA + FILE_DATA_ONE);

      // create the storage and select the single value of interest
      SrcStorage := StorageFromFile(FRootDir + DIR_DATA + FILE_DATA_ONE);

      for Element in SrcStorage.Get('English/Science').Values do
        mmResultsData.Lines.Add(Format('%s : %s', [Element.Name, Element.AsString]));
    end
    //****************************************
    // here we read all elements of a single node
    //****************************************
    else if rgDemoSelection.ItemIndex = 2 then
    begin
      // show the original content
      mmSourceData.Lines.LoadFromFile(FRootDir + DIR_DATA + FILE_DATA_ONE);

      // create the storage and select the single value of interest
      SrcStorage := StorageFromFile(FRootDir + DIR_DATA + FILE_DATA_ONE);

      for Element in SrcStorage.Get('English/Science').Elements do
      begin
        if Element.ElementType = etValue then
          mmResultsData.Lines.Add(Format('%s : %s', [Element.Name, Element.AsString]))
        else
          mmResultsData.Lines.Add(Format('%s : %s', [Element.Name, 'Key Element']))
      end;

      mmResultsData.Lines.Add('---------------------------------------------------');
      ElementValue := SrcStorage.Get('English/Science').Elements.Item[3].AsString;
      mmResultsData.Lines.Add(Format('Value at Index 3 : %s', [ElementValue]));
    end
    //****************************************
    // here we add some values to the storage
    //****************************************
    else if rgDemoSelection.ItemIndex = 3 then
    begin
      // show the original content
      mmSourceData.Lines.LoadFromFile(FRootDir + DIR_DATA + FILE_DATA_ONE);

      // create the storage and select the single value of interest
      SrcStorage := StorageFromFile(FRootDir + DIR_DATA + FILE_DATA_ONE);
      SrcStorage.Ensure('English/Science/Arheology').AsInteger := 3;
      SrcStorage.Ensure('English/Science/Geology').AsInteger := 15;

      mmResultsData.Lines.Text := SrcStorage.Content(True);
    end
    //****************************************
    // here we assing one node to another one
    //****************************************
    else if rgDemoSelection.ItemIndex = 4 then
    begin
      // show the original content
      mmSourceData.Lines.LoadFromFile(FRootDir + DIR_DATA + FILE_DATA_ONE);

      // create the storage and select the single value of interest
      SrcStorage := StorageFromFile(FRootDir + DIR_DATA + FILE_DATA_TWO);
      TrgStorage := StorageFromFile(FRootDir + DIR_DATA + FILE_DATA_ONE);
      TrgStorage.Get('English').Append(SrcStorage, True);

      TrgStorage.Get('English/Musical').Append(SrcStorage.Get('Females').Values);
      mmResultsData.Lines.Text := TrgStorage.Content(True);
    end
    //****************************************
    // here we write and read some binary data
    //****************************************
    else if rgDemoSelection.ItemIndex = 5 then
    begin
      MS := TMemoryStream.Create;
      try
        Jpeg := TJPEGImage.Create;
        try
          Jpeg.LoadFromFile(FRootDir + DIR_DATA + FILE_DATA_IMG);
          Jpeg.SaveToStream(MS);
          MS.Position := 0;

          // create the storage and select the single value of interest
          SrcStorage := CreateStorage('BinaryStorage');
          SrcStorage.Ensure('Image').AsBinary.LoadFromStream(MS);
          mmSourceData.Lines.Text := SrcStorage.Content;

          Jpeg.LoadFromStream(SrcStorage.Get('Image').AsBinary.Stream);
          imgLogo.Picture.Assign(Jpeg);
        finally
          Jpeg.Free;
        end;
      finally
        MS.Free;
      end;
    end
    //****************************************
    // here we read all just certaion values of
    // a single node based on attribute value
    //****************************************
    else if rgDemoSelection.ItemIndex = 6 then
    begin
      // show the original content
      mmSourceData.Lines.LoadFromFile(FRootDir + DIR_DATA + FILE_DATA_ONE);

      // create the storage and select the single value of interest
      SrcStorage := StorageFromFile(FRootDir + DIR_DATA + FILE_DATA_ONE);

      // this is the first way where we select a single node and then enumerate
      for Element in SrcStorage.Get('English/Science').Values('*[@Difficulty="High"]') do
        mmResultsData.Lines.Add(Format('%s : %s', [Element.Name, Element.AsString]));

      mmResultsData.Lines.Add('--------------------------------------------------');

      // this is the second way where we enumerate from root but deep into the tree
      for Element in SrcStorage.Values('English/Science/*[@Difficulty="High"]') do
        mmResultsData.Lines.Add(Format('%s : %s', [Element.Name, Element.AsString]));

      mmResultsData.Lines.Add('--------------------------------------------------');

      // this is the third way where we enumerate from single node but only one node
      for Element in SrcStorage.Get('English/Science').Values('Engineering') do
        mmResultsData.Lines.Add(Format('%s : %s', [Element.Name, Element.AsString]));
    end
    //****************************************
    // compress and decompremss some binary data
    //****************************************
    else if rgDemoSelection.ItemIndex = 7 then
    begin
      MS := TMemoryStream.Create;
      try
        Jpeg := TJPEGImage.Create;
        try
          Jpeg.LoadFromFile(FRootDir + DIR_DATA + FILE_DATA_IMG);
          Jpeg.SaveToStream(MS);
          MS.Position := 0;

          // create the storage and select the single value of interest
          SrcStorage := CreateStorage('BinaryStorage');
          SrcStorage.Ensure('Image').Filter(ZLIB).AsBinary.LoadFromStream(MS);

          mmSourceData.Lines.Text := SrcStorage.Content;

          Jpeg.LoadFromStream(SrcStorage.Get('Image').Filter(ZLIB).AsBinary.Stream);
          imgLogo.Picture.Assign(Jpeg);
        finally
          Jpeg.Free;
        end;
      finally
        MS.Free;
      end;
    end
    //****************************************
    // encrypt and decrypt some binary data
    //****************************************
    else if rgDemoSelection.ItemIndex = 8 then
    begin
      MS := TMemoryStream.Create;
      try
        Jpeg := TJPEGImage.Create;
        try
          Jpeg.LoadFromFile(FRootDir + DIR_DATA + FILE_DATA_IMG);
          Jpeg.SaveToStream(MS);
          MS.Position := 0;

          // create the storage and select the single value of interest
          SrcStorage := CreateStorage('BinaryStorage');
          SrcStorage.Ensure('Image').Filter(XTEA('MyStrongKey')).AsBinary.LoadFromStream(MS);

          mmSourceData.Lines.Text := SrcStorage.Content;

          Jpeg.LoadFromStream(SrcStorage.Get('Image').Filter(XTEA('MyStrongKey')).AsBinary.Stream);
          imgLogo.Picture.Assign(Jpeg);
        finally
          Jpeg.Free;
        end;
      finally
        MS.Free;
      end;
    end
    //**********************************************
    // filter chaining example (compress -> enrcypt)
    //**********************************************
    else if rgDemoSelection.ItemIndex = 9 then
    begin
      MS := TMemoryStream.Create;
      try
        Jpeg := TJPEGImage.Create;
        try
          Jpeg.LoadFromFile(FRootDir + DIR_DATA + FILE_DATA_IMG);
          Jpeg.SaveToStream(MS);
          MS.Position := 0;

          // create the storage and select the single value of interest
          SrcStorage := CreateStorage('BinaryStorage');
          SrcStorage.Ensure('Image').Filter(ZLIB).Filter(XTEA('MyStrongKey')).AsBinary.LoadFromStream(MS);

          mmSourceData.Lines.Text := SrcStorage.Content;

          Jpeg.LoadFromStream(SrcStorage.Get('Image').Filter(ZLIB).Filter(XTEA('MyStrongKey')).AsBinary.Stream);
          imgLogo.Picture.Assign(Jpeg);
        finally
          Jpeg.Free;
        end;
      finally
        MS.Free;
      end;
    end
    else if rgDemoSelection.ItemIndex = 10 then
    begin
      // show the original content
      mmSourceData.Lines.LoadFromFile(FRootDir + DIR_DATA + FILE_DATA_ONE);

      // create the storage and select the single value of interest
      SrcStorage := StorageFromFile(FRootDir + DIR_DATA + FILE_DATA_ONE);
      SrcStorage.Ensure('English/Science/Arheology').AsCData.Data.AsInteger := 3;
      SrcStorage.Ensure('English/Science/Geology').AsCData.Data.AsInteger := 15;

      mmResultsData.Lines.Text := SrcStorage.Content(True);
    end
    else if rgDemoSelection.ItemIndex = 11 then
    begin
      // show the original content
      mmSourceData.Lines.LoadFromFile(FRootDir + DIR_DATA + FILE_DATA_ONE);

      SrcStorage := StorageFromFile(FRootDir + DIR_DATA + FILE_DATA_ONE);
      TrgStorage := StorageFromFile(FRootDir + DIR_DATA + FILE_DATA_TWO);
      SrcStorage.Get('English/Musical').Assign(TrgStorage.Get('Females'), True);

      mmResultsData.Lines.Text := SrcStorage.Content(True);
    end
    else if rgDemoSelection.ItemIndex = 12 then
    begin
      // show the original content
      mmSourceData.Lines.LoadFromFile(FRootDir + DIR_DATA + FILE_DATA_ONE);

      SrcStorage := StorageFromFile(FRootDir + DIR_DATA + FILE_DATA_ONE);
      TrgStorage := StorageFromFile(FRootDir + DIR_DATA + FILE_DATA_TWO);
      SrcStorage.Get('English/Musical').Merge(TrgStorage.Get('Females'));

      mmResultsData.Lines.Text := SrcStorage.Content(True);
    end
    else if rgDemoSelection.ItemIndex = 13 then
    begin
      SrcStorage := StorageFromFile(FRootDir + DIR_DATA + FILE_DATA_THREE);

      if not SrcStorage.LastLoadStatus.Success then
      begin
        mmResultsData.Lines.Clear;
        mmResultsData.Lines.Add(Format('Line %d, Pos: %d - %s',
          [SrcStorage.LastLoadStatus.Error.Line,
           SrcStorage.LastLoadStatus.Error.LinePos,
           SrcStorage.LastLoadStatus.Error.Reason]));
      end;
    end
    else if rgDemoSelection.ItemIndex = 14 then
    begin
      LegacyDoc := CreateXMLDoc;
      LegacyDoc.Load(FRootDir + DIR_DATA + FILE_DATA_ONE);

      SrcStorage := StorageFromXMLDocument(LegacyDoc);

      // this is the exact path to the wanded element
      Element := SrcStorage.Get('English/Science/Computers');
      mmResultsData.Lines.Add(Format('%s : %s', [Element.Name, Element.AsString]));

      // get the element from raw IXMLNode and use it in a SimpleStorage manner
      Element := ElementFromXMLNode(LegacyDoc.SelectSingleNode('/Books/English/Science/Engineering'));
      mmResultsData.Lines.Add(Format('%s : %s', [Element.Name, Element.AsString]));
    end;
  end;
end;

end.
