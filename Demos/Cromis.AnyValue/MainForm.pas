unit MainForm;

interface

uses
  Windows, SysUtils, Variants, Classes, Controls, Forms, Dialogs, StdCtrls, ComCtrls, Math,

  // cromis units
  Cromis.AnyValue, Cromis.StringUtils;

type
  TfMain = class(TForm)
    pcAnyValue: TPageControl;
    tsAnyValueDemo: TTabSheet;
    tsAnyArrayDemo: TTabSheet;
    lbAnyValue: TListBox;
    gbAnyArray: TGroupBox;
    btnCreateArray: TButton;
    btnPushItems: TButton;
    btnPopItem: TButton;
    btnSliceArray: TButton;
    btnIndexOf: TButton;
    btnContains: TButton;
    eCreateArray: TEdit;
    ePushItems: TEdit;
    eSliceArray: TEdit;
    eIndexOf: TEdit;
    eContains: TEdit;
    btnReverseItems: TButton;
    btnSortASC: TButton;
    btnSortDSC: TButton;
    btnClone: TButton;
    btnDeleteItem: TButton;
    eDeleteItem: TEdit;
    btnClear: TButton;
    btnStreamOps: TButton;
    btnAddArrays: TButton;
    btnAddNamedValue: TButton;
    eAddNamedValue: TEdit;
    btnFindNamedValue: TButton;
    eFindNamedValue: TEdit;
    lbTestAnyValue: TListBox;
    btnTestAnyValue: TButton;
    btnInsert: TButton;
    eInsertItems: TEdit;
    eSliceSize: TEdit;
    lbSliceInfo: TListBox;
    lbSliceData: TListBox;
    btnUnshiftItems: TButton;
    btnShiftItem: TButton;
    eUnshiftItems: TEdit;
    btnEnumerate: TButton;
    procedure btnCreateArrayClick(Sender: TObject);
    procedure btnPushItemsClick(Sender: TObject);
    procedure btnPopItemClick(Sender: TObject);
    procedure btnSliceArrayClick(Sender: TObject);
    procedure btnIndexOfClick(Sender: TObject);
    procedure btnContainsClick(Sender: TObject);
    procedure btnSortDSCClick(Sender: TObject);
    procedure btnSortASCClick(Sender: TObject);
    procedure btnReverseItemsClick(Sender: TObject);
    procedure btnCloneClick(Sender: TObject);
    procedure btnDeleteItemClick(Sender: TObject);
    procedure btnClearClick(Sender: TObject);
    procedure btnStreamOpsClick(Sender: TObject);
    procedure btnAddArraysClick(Sender: TObject);
    procedure btnAddNamedValueClick(Sender: TObject);
    procedure btnFindNamedValueClick(Sender: TObject);
    procedure btnTestAnyValueClick(Sender: TObject);
    procedure btnInsertClick(Sender: TObject);
    procedure btnUnshiftItemsClick(Sender: TObject);
    procedure btnShiftItemClick(Sender: TObject);
    procedure btnEnumerateClick(Sender: TObject);
  private
    FAnyArray: IAnyArray;
    procedure UpdateArrayInfo(const AnyArray: IAnyArray);
  end;

var
  fMain: TfMain;

implementation

{$R *.dfm}

procedure TfMain.btnCloneClick(Sender: TObject);
begin
  lbAnyValue.Items.Add(FAnyArray.Clone.GetAsString);
end;

procedure TfMain.btnContainsClick(Sender: TObject);
begin
  lbAnyValue.Items.Add(BoolToStr(FAnyArray.Contains(eIndexOf.Text), True));
end;

procedure TfMain.btnCreateArrayClick(Sender: TObject);
var
  I: Integer;
begin
  FAnyArray := CreateAnyArray(StrToInt(eSliceSize.Text));
  FAnyArray.ArrayMode := amSlicedArray;
  FAnyArray.SliceBufferMpl := 1.5;

  FAnyArray.Push(eCreateArray.Text, ',');
  UpdateArrayInfo(FAnyArray);

  for I := 0 to fMain.ComponentCount - 1 do
    if fMain.Components[I].ClassType = TButton then
      TButton(fMain.Components[I]).Enabled := True;
end;

procedure TfMain.btnDeleteItemClick(Sender: TObject);
begin
  FAnyArray.DeleteValue(eDeleteItem.Text);
  UpdateArrayInfo(FAnyArray);
end;

procedure TfMain.btnEnumerateClick(Sender: TObject);
var
  Element: PAnyValue;
  TempList: TStringList;
begin
  TempList := TStringList.Create;
  try
    for Element in FAnyArray.Enum.Forward do
      TempList.Add(Element.AsString);
    lbAnyValue.Items.Add(TempList.CommaText);
    TempList.Clear;

    for Element in FAnyArray.Enum.Reverse do
      TempList.Add(Element.AsString);
    lbAnyValue.Items.Add(TempList.CommaText);
    TempList.Clear;
  finally
    TempList.Free;
  end;
end;

procedure TfMain.btnFindNamedValueClick(Sender: TObject);
var
  Value: TAnyValue;
begin
  Value := FAnyArray.FindNamed(eFindNamedValue.Text);
  lbAnyValue.Items.Add(Value.AsString);
end;

procedure TfMain.btnIndexOfClick(Sender: TObject);
begin
  lbAnyValue.Items.Add(IntToStr(FAnyArray.IndexOf(eIndexOf.Text)));
end;

procedure TfMain.btnInsertClick(Sender: TObject);
begin
  FAnyArray.Insert(2, eInsertItems.Text, ',');
  UpdateArrayInfo(FAnyArray);
end;

procedure TfMain.btnPopItemClick(Sender: TObject);
begin
  FAnyArray.Pop;
  UpdateArrayInfo(FAnyArray);
end;

procedure TfMain.btnPushItemsClick(Sender: TObject);
begin
  FAnyArray.Push(ePushItems.Text, ',');
  UpdateArrayInfo(FAnyArray);
end;

procedure TfMain.btnReverseItemsClick(Sender: TObject);
begin
  FAnyArray.Reverse;
  UpdateArrayInfo(FAnyArray);
end;

procedure TfMain.btnShiftItemClick(Sender: TObject);
begin
  FAnyArray.Shift;
  UpdateArrayInfo(FAnyArray);
end;

procedure TfMain.btnSliceArrayClick(Sender: TObject);
var
  SlicedArray: IAnyArray;
begin
  SlicedArray := FAnyArray.Slice(StrToInt(StrBefore(',', eSliceArray.Text)),
                                 StrToInt(StrAfter(',', eSliceArray.Text)));
  UpdateArrayInfo(SlicedArray);
end;

{$IF CompilerVersion < 20}
function SortASC(Item1, Item2: PAnyValue): Integer;
begin
  Result := StrToInt64Def(Item1.AsString, -1) - StrToInt64Def(Item2.AsString, -1);
end;

function SortDSC(Item1, Item2: PAnyValue): Integer;
begin
  Result := StrToInt64Def(Item2.AsString, -1) - StrToInt64Def(Item1.AsString, -1);
end;
{$IFEND}

procedure TfMain.btnSortASCClick(Sender: TObject);
begin
{$IF CompilerVersion >= 20}
  FAnyArray.Sort
  (
    function(Item1, Item2: PAnyValue): Integer
    begin
      Result := StrToInt64Def(Item1.AsString, -1) - StrToInt64Def(Item2.AsString, -1);
    end
  );
  UpdateArrayInfo(FAnyArray);
{$ELSE}
  FAnyArray.Sort(SortASC);
{$IFEND}
end;

procedure TfMain.btnSortDSCClick(Sender: TObject);
begin
{$IF CompilerVersion >= 20}
  FAnyArray.Sort
  (
    function(Item1, Item2: PAnyValue): Integer
    begin
      Result := StrToInt64Def(Item2.AsString, -1) - StrToInt64Def(Item1.AsString, -1);
    end
  );
  UpdateArrayInfo(FAnyArray);
{$ELSE}
  FAnyArray.Sort(SortDSC);
{$IFEND}
end;

procedure TfMain.btnStreamOpsClick(Sender: TObject);
var
  MS: TMemoryStream;
  SecondArray: IAnyArray;
begin
  MS := TMemoryStream.Create;
  try
    FAnyArray.SaveToStream(MS);
    MS.Seek(0, soFromBeginning);
    SecondArray := CreateAnyArray;
    SecondArray.LoadFromStream(MS);
    UpdateArrayInfo(SecondArray);
  finally
    MS.Free;
  end;
end;

procedure TfMain.btnTestAnyValueClick(Sender: TObject);
var
  AnyValue: TAnyValue;
  DummyValue: TAnyValue;
  TestVariant: Variant;
begin
  lbTestAnyValue.Clear;
  AnyValue.AsInteger :=  High(Integer);
  DummyValue.AsInteger := AnyValue.AsInteger;
  lbTestAnyValue.Items.Add(Format('AnyValue as Integer: %s', [DummyValue.AsString]));
  AnyValue.AsFloat := MaxExtended;
  DummyValue.AsFloat := AnyValue.AsFloat;
  lbTestAnyValue.Items.Add(Format('AnyValue as Float: %s', [DummyValue.AsString]));
  AnyValue.AsDouble := MaxDouble;
  DummyValue.AsDouble := AnyValue.AsDouble;
  lbTestAnyValue.Items.Add(Format('AnyValue as Double: %s',[DummyValue.AsString]));
  AnyValue.AsString := 'string';
  DummyValue.AsString := AnyValue.AsString;
  lbTestAnyValue.Items.Add(Format('AnyValue as string: %s', [DummyValue.AsString]));
{$IFDEF UNICODE}
  AnyValue.AsAnsiString := 'AnsiString';
  DummyValue.AsAnsiString := AnyValue.AsAnsiString;
  lbTestAnyValue.Items.Add(Format('AnyValue as AnsiString: %s', [DummyValue.AsString]));
{$ENDIF}
  AnyValue.AsWideString := 'WideString';
  DummyValue.AsWideString := AnyValue.AsWideString;
  lbTestAnyValue.Items.Add(Format('AnyValue as WideString: %s', [DummyValue.AsString]));
  AnyValue.AsDateTime := Now;
  DummyValue.AsDateTime := AnyValue.AsDateTime;
  lbTestAnyValue.Items.Add(Format('AnyValue as DateTime: %s', [DummyValue.AsString]));
  AnyValue.AsInt64 := High(Int64);
  DummyValue.AsInt64 := AnyValue.AsInt64;
  lbTestAnyValue.Items.Add(Format('AnyValue as Int64: %s', [DummyValue.AsString]));
  AnyValue.AsBoolean := True;
  DummyValue.AsBoolean := AnyValue.AsBoolean;
  lbTestAnyValue.Items.Add(Format('AnyValue as Boolean: %s', [DummyValue.AsString]));
  AnyValue.AsCardinal := High(Cardinal);
  DummyValue.AsCardinal := AnyValue.AsCardinal;
  lbTestAnyValue.Items.Add(Format('AnyValue as Cardinal: %s', [DummyValue.AsString]));
  AnyValue.AsPointer := btnTestAnyValue;
  DummyValue.AsPointer := AnyValue.AsPointer;
  lbTestAnyValue.Items.Add(Format('AnyValue as Pointer: %s', [DummyValue.AsString]));
  AnyValue.AsObject := btnTestAnyValue;
  DummyValue.AsObject := AnyValue.AsObject;
  lbTestAnyValue.Items.Add(Format('AnyValue as Object: %s', [DummyValue.AsString]));
  TestVariant := High(Integer);
  AnyValue.AsVariant := TestVariant;
  DummyValue.AsVariant := AnyValue.AsVariant;
  lbTestAnyValue.Items.Add(Format('AnyValue as Variant: %s', [DummyValue.AsString]));
{$IF CompilerVersion >= 20}
  AnyValue.EnsureAsArray.Push([5, 1.3, Now]);
{$ELSE}
  AnyValue.EnsureAsArray.Push([5, 1.3, DateTimeToStr(Now)]);
{$IFEND}
  DummyValue.EnsureAsArray.Assign(AnyValue.GetAsArray);
  lbTestAnyValue.Items.Add(Format('AnyValue as Array: %s', [DummyValue.AsString]));
  AnyValue.EnsureAsArray.Clear;
  AnyValue['Name'] := 'Value';
  DummyValue.EnsureAsArray.Assign(AnyValue.GetAsArray);
  lbTestAnyValue.Items.Add(Format('AnyValue as NamedValue: %s', [DummyValue.AsString]));
end;

procedure TfMain.btnUnshiftItemsClick(Sender: TObject);
begin
  FAnyArray.Unshift(eUnshiftItems.Text, ',');
  UpdateArrayInfo(FAnyArray);
end;

procedure TfMain.UpdateArrayInfo(const AnyArray: IAnyArray);
var
  I, K: Integer;
  SliceData: TStringList;
  CurrentSlice: PArraySlice;
begin
  lbAnyValue.Items.Add(AnyArray.GetAsString);

  lbSliceInfo.Clear;
  lbSliceData.Clear;

  SliceData := TStringList.Create;
  try
    lbSliceInfo.Items.Add(Format('Count %d:', [FAnyArray.Count]));

    for I := 0 to FAnyArray.SliceCount - 1 do
    begin
      CurrentSlice := FAnyArray.RawData^[I];
      lbSliceInfo.Items.Add(Format('Slice %d: %d - %d', [CurrentSlice.Index,
                                                         CurrentSlice.Start,
                                                         CurrentSlice.Last]));
      SliceData.Clear;

      for K := 0 to Length(CurrentSlice.Data) - 1 do
      begin
        case CurrentSlice.Data[K].GetValueType of
          avtNone: SliceData.Add('NA');
          else
            SliceData.Add(CurrentSlice.Data[K].AsString);
        end;
      end;

      lbSliceData.Items.Add(SliceData.CommaText);
    end;
  finally
    SliceData.Free;
  end;

  Application.ProcessMessages;
end;

procedure TfMain.btnAddArraysClick(Sender: TObject);
begin
  FAnyArray.Push([5, '4.5', AnyValues([7, '5', 3, AnyValues([1.2, 3, '5'])])]);
  UpdateArrayInfo(FAnyArray);
end;

procedure TfMain.btnAddNamedValueClick(Sender: TObject);
begin
  FAnyArray.AddNamed(StrBefore(',', eAddNamedValue.Text), StrAfter(',', eAddNamedValue.Text));
  UpdateArrayInfo(FAnyArray);
end;

procedure TfMain.btnClearClick(Sender: TObject);
begin
  FAnyArray.Clear;
  lbAnyValue.Clear;
  lbSliceInfo.Clear;
  lbSliceData.Clear;
end;

end.
