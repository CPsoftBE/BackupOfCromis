unit SpeedTest.Main;

interface

uses
  Windows, SysUtils, Variants, Classes, Controls, Forms, StdCtrls, ComCtrls, XPMan, ExtCtrls,

  // additional units
  Math, RTTI, diagnostics,

  // generics units
  Generics.Collections, Generics.Defaults,

  // cromis units
  Cromis.AnyValue;

type
  TfMain = class(TForm)
    lbSpeedResults: TListBox;
    lbSliceData: TListBox;
    sbSpeedTest: TStatusBar;
    XPManifest: TXPManifest;
    pnCommands: TPanel;
    btnTestLargeSize: TButton;
    btnTestSmallSize: TButton;
    cbDynamicArray: TRadioButton;
    cbSlicedArray: TRadioButton;
    eSliceSize: TEdit;
    eSliceBufferMpl: TEdit;
    lbSliceBufferMpl: TLabel;
    lbSliceSize: TLabel;
    procedure btnTestLargeSizeClick(Sender: TObject);
    procedure btnTestSmallSizeClick(Sender: TObject);
  private
    procedure WriteToLog(const Msg: string);
    procedure WriteSlicesInfo(const AnyArray: IAnyArray);
  end;

{ Declare a new custom comparer. }
  TAnyValueComparer = class(TComparer<TAnyValue>)
  public
    function Compare(const Left, Right: TAnyValue): Integer; override;
  end;

var
  fMain: TfMain;

implementation

{$R *.dfm}

procedure TfMain.btnTestLargeSizeClick(Sender: TObject);
var
  I: Integer;
  EnumValue: TAnyValue;
  EnumTValue: TValue;
  EnumVariant: Variant;
  EnumPointer: PAnyValue;
  TempValueA: Integer;
  TempValueB: Integer;
  StopWatch: TStopWatch;
  TValueList: TList<TValue>;
  AnyArrayList: IAnyArray;
  AnyValueList: TList<TAnyValue>;
  VariantsList: TList<Variant>;
begin
  Randomize;
  lbSliceData.Clear;
  lbSpeedResults.Clear;
  // set decimal separator
  {$IF CompilerVersion >= 22}Formatsettings.{$IFEND}DecimalSeparator := '.';

  AnyArrayList := CreateAnyArray;
  try
    if cbDynamicArray.Checked then
    begin
      AnyArrayList.ArrayMode := amDynamicArray;
    end
    else
    begin
      AnyArrayList.ArrayMode := amSlicedArray;
      AnyArrayList.SliceBufferMpl := StrToFloat(eSliceBufferMpl.Text);
      AnyArrayList.SliceSize := StrToInt(eSliceSize.Text);
    end;

    WriteToLog('********************* BEGIN AnyArrayList *********************');
    TempValueB := 0;
    StopWatch := TStopWatch.StartNew;
    for I := 0 to 1000000 - 1 do
      AnyArrayList.Push(Random(1000000));
    StopWatch.Stop;
    WriteToLog(Format('IAnyArray, add 1000000: %d', [stopwatch.ElapsedMilliseconds]));
    StopWatch := TStopWatch.StartNew;
    AnyArrayList.Sort
    (
      function(Item1, Item2: PAnyValue): Integer
      begin
        Result := Item1.AsInteger - Item2.AsInteger;
      end
    );
    StopWatch.Stop;
    WriteToLog(Format('IAnyArray, sort ASC: %d', [stopwatch.ElapsedMilliseconds]));
    StopWatch := TStopWatch.StartNew;
    for I := 0 to 5000 - 1 do
      AnyArrayList.DeleteIndex(Random(AnyArrayList.Count - 1));
    StopWatch.Stop;
    WriteToLog(Format('IAnyArray, delete 5000 by index: %d', [stopwatch.ElapsedMilliseconds]));
    StopWatch := TStopWatch.StartNew;
    for I := 0 to 5000 - 1 do
      AnyArrayList.Insert(Random(AnyArrayList.Count - 1), I);
    StopWatch.Stop;
    WriteToLog(Format('IAnyArray, insert 5000 by index: %d', [stopwatch.ElapsedMilliseconds]));
    StopWatch := TStopWatch.StartNew;
    for I := 0 to 1000000 - 1 do
      TempValueA := AnyArrayList.Item[Random(AnyArrayList.Count - 1)].AsInteger;
    StopWatch.Stop;
    // use TempValue;
    Sleep(Min(0, TempValueA));
    WriteToLog(Format('IAnyArray, 1000000 random access by index: %d', [stopwatch.ElapsedMilliseconds]));
    StopWatch := TStopWatch.StartNew;
    for I := 0 to 1000000 - 1 do
      TempValueA := AnyArrayList.RawItem(Random(AnyArrayList.Count - 1))^;
    StopWatch.Stop;
    // use TempValue;
    Sleep(Min(0, TempValueA));
    WriteToLog(Format('IAnyArray, 1000000 raw random access by index: %d', [stopwatch.ElapsedMilliseconds]));
    StopWatch := TStopWatch.StartNew;
    for I := 0 to 1000000 - 1 do
      TempValueA := AnyArrayList.Item[I].AsInteger;
    StopWatch.Stop;
    // use TempValue;
    Sleep(Min(0, TempValueA));
    WriteToLog(Format('IAnyArray, 1000000 iterations by index: %d', [stopwatch.ElapsedMilliseconds]));
    StopWatch := TStopWatch.StartNew;
    for I := 0 to 1000000 - 1 do
      TempValueA := AnyArrayList.RawItem(I)^;
    StopWatch.Stop;
    // use TempValue;
    Sleep(Min(0, TempValueA));
    WriteToLog(Format('IAnyArray, 1000000 raw iterations by index: %d', [stopwatch.ElapsedMilliseconds]));
    StopWatch := TStopWatch.StartNew;
    for EnumPointer in AnyArrayList.Enum.Forward do
      TempValueB := EnumPointer.AsInteger;
    StopWatch.Stop;
    // use TempValue;
    Sleep(Min(0, TempValueB));
    WriteToLog(Format('IAnyArray, pointers enumeration: %d', [stopwatch.ElapsedMilliseconds]));
    WriteToLog('********************* END AnyArrayList *********************');
    // write current slice info to list
    WriteSlicesInfo(AnyArrayList);
  finally
    AnyArrayList := nil;
  end;


  AnyValueList := TList<TAnyValue>.Create;
  try
    WriteToLog('********************* BEGIN TList<TAnyValue> *********************');
    StopWatch := TStopWatch.StartNew;
    for I := 0 to 1000000 - 1 do
      AnyValueList.Add(Random(1000000));
    StopWatch.Stop;
    WriteToLog(Format('TList<TAnyValue>, add 1000000: %d', [stopwatch.ElapsedMilliseconds]));
    StopWatch := TStopWatch.StartNew;
    AnyValueList.Sort
    (
      TDelegatedComparer<TAnyValue>.Create
      (
        function(const Left, Right: TAnyValue): Integer
        begin
          Result := Left.AsInteger - Right.AsInteger;
        end
      )
    );
    StopWatch.Stop;
    WriteToLog(Format('TList<TAnyValue>, sort ASC: %d', [stopwatch.ElapsedMilliseconds]));
    StopWatch := TStopWatch.StartNew;
    for I := 0 to 5000 - 1 do
      AnyValueList.Delete(Random(AnyValueList.Count - 1));
    StopWatch.Stop;
    WriteToLog(Format('TList<TAnyValue>, delete 5000 by index: %d', [stopwatch.ElapsedMilliseconds]));
    StopWatch := TStopWatch.StartNew;
    for I := 0 to 5000 - 1 do
      AnyValueList.Insert(Random(AnyValueList.Count - 1), I);
    StopWatch.Stop;
    WriteToLog(Format('TList<TAnyValue>, insert 5000 by index: %d', [stopwatch.ElapsedMilliseconds]));
    StopWatch := TStopWatch.StartNew;
    for I := 0 to 1000000 - 1 do
      TempValueA := AnyValueList[Random(AnyValueList.Count - 1)];
    StopWatch.Stop;
    // use TempValue;
    Sleep(Min(0, TempValueA));
    WriteToLog(Format('TList<TAnyValue>, 1000000 random access by index: %d', [stopwatch.ElapsedMilliseconds]));
    StopWatch := TStopWatch.StartNew;
    for I := 0 to AnyValueList.Count - 1 do
      TempValueB := AnyValueList[I];
    StopWatch.Stop;
    // use TempValue;
    Sleep(Min(0, TempValueB));
    WriteToLog(Format('TList<TAnyValue>, 1000000 iteration by index: %d', [stopwatch.ElapsedMilliseconds]));
    StopWatch := TStopWatch.StartNew;
    for EnumValue in AnyValueList do
      TempValueB := EnumValue.AsInteger;
    StopWatch.Stop;
    // use TempValue;
    Sleep(Min(0, TempValueB));
    WriteToLog(Format('TList<TAnyValue>, values enumeration: %d', [stopwatch.ElapsedMilliseconds]));
    WriteToLog('********************* END TList<TAnyValue> *********************');
  finally
    AnyValueList.Free;
  end;

  VariantsList := TList<Variant>.Create;
  try
    WriteToLog('********************* BEGIN TList<Variant> *********************');
    StopWatch := TStopWatch.StartNew;
    //for I := 0 to 1000000 - 1 do
    //  VariantsList.Add(I);
    for I := 0 to 1000000 - 1 do
      VariantsList.Add(Random(1000000));
    StopWatch.Stop;
    WriteToLog(Format('TList<Variant>, add 1000000: %d', [stopwatch.ElapsedMilliseconds]));
    StopWatch := TStopWatch.StartNew;
    VariantsList.Sort
    (
      TDelegatedComparer<Variant>.Create
      (
        function(const Left, Right: Variant): Integer
        begin
          Result := Left - Right;
        end
      )
    );
    for I := 0 to 5000 - 1 do
      VariantsList.Delete(Random(VariantsList.Count - 1));
    StopWatch.Stop;
    WriteToLog(Format('TList<Variant>, delete 5000 by index: %d', [stopwatch.ElapsedMilliseconds]));
    StopWatch := TStopWatch.StartNew;
    for I := 0 to 5000 - 1 do
      VariantsList.Insert(Random(VariantsList.Count - 1), I);
    StopWatch.Stop;
    WriteToLog(Format('TList<Variant>, insert 5000 by index: %d', [stopwatch.ElapsedMilliseconds]));
    StopWatch := TStopWatch.StartNew;
    for I := 0 to 1000000 - 1 do
      TempValueA := VariantsList[Random(VariantsList.Count - 1)];
    StopWatch.Stop;
    // use TempValue;
    Sleep(Min(0, TempValueA));
    WriteToLog(Format('TList<Variant>, 1000000 random access by index: %d', [stopwatch.ElapsedMilliseconds]));
    StopWatch := TStopWatch.StartNew;
    for I := 0 to VariantsList.Count - 1 do
      TempValueB := VariantsList[I];
    StopWatch.Stop;
    // use TempValue;
    Sleep(Min(0, TempValueB));
    WriteToLog(Format('TList<Variant>, 1000000 iteration by index: %d', [stopwatch.ElapsedMilliseconds]));
    StopWatch := TStopWatch.StartNew;
    for EnumVariant in VariantsList do
      TempValueB := EnumVariant;
    StopWatch.Stop;
    // use TempValue;
    Sleep(Min(0, TempValueB));
    WriteToLog(Format('TList<Variant>, values enumeration: %d', [stopwatch.ElapsedMilliseconds]));
    WriteToLog('********************* END TList<Variant> *********************');
  finally
    VariantsList.Free;
  end;

  TValueList := TList<TValue>.Create;
  try
    WriteToLog('********************* BEGIN TList<TValue> *********************');
    StopWatch := TStopWatch.StartNew;
    for I := 0 to 1000000 - 1 do
      TValueList.Add(I);
    for I := 0 to 1000000 - 1 do
      TValueList.Add(1000000);
    StopWatch.Stop;
    WriteToLog(Format('TList<TValue>, add 1000000: %d', [stopwatch.ElapsedMilliseconds]));
    StopWatch := TStopWatch.StartNew;
    TValueList.Sort
    (
      TDelegatedComparer<TValue>.Create
      (
        function(const Left, Right: TValue): Integer
        begin
          Result := Left.AsInteger - Right.AsInteger;
        end
      )
    );
    for I := 0 to 5000 - 1 do
      TValueList.Delete(Random(TValueList.Count - 1));
    StopWatch.Stop;
    WriteToLog(Format('TList<TValue>, delete 5000 by index: %d', [stopwatch.ElapsedMilliseconds]));
    StopWatch := TStopWatch.StartNew;
    for I := 0 to 5000 - 1 do
      TValueList.Insert(Random(TValueList.Count - 1), I);
    StopWatch.Stop;
    WriteToLog(Format('TList<TValue>, insert 5000 by index: %d', [stopwatch.ElapsedMilliseconds]));
    StopWatch := TStopWatch.StartNew;
    for I := 0 to 1000000 - 1 do
      TempValueA := TValueList[Random(TValueList.Count - 1)].AsInteger;
    StopWatch.Stop;
    // use TempValue;
    Sleep(Min(0, TempValueA));
    WriteToLog(Format('TList<TValue>, 1000000 random access by index: %d', [stopwatch.ElapsedMilliseconds]));
    StopWatch := TStopWatch.StartNew;
    for I := 0 to TValueList.Count - 1 do
      TempValueB := TValueList[I].AsInteger;
    StopWatch.Stop;
    // use TempValue;
    Sleep(Min(0, TempValueB));
    WriteToLog(Format('TList<TValue>, 1000000 iteration by index: %d', [stopwatch.ElapsedMilliseconds]));
    StopWatch := TStopWatch.StartNew;
    for EnumTValue in TValueList do
      TempValueB := EnumTValue.AsInteger;
    StopWatch.Stop;
    // use TempValue;
    Sleep(Min(0, TempValueB));
    WriteToLog(Format('TList<TValue>, values enumeration: %d', [stopwatch.ElapsedMilliseconds]));
    WriteToLog('********************* END TList<TValue> *********************');
  finally
    TValueList.Free;
  end;
end;

procedure TfMain.btnTestSmallSizeClick(Sender: TObject);
var
  I, K: Integer;
  EnumValue: TAnyValue;
  EnumTValue: TValue;
  EnumVariant: Variant;
  EnumPointer: PAnyValue;
  TempValueA: Integer;
  TempValueB: Integer;
  StopWatch: TStopWatch;
  TValueList: TList<TValue>;
  AnyArrayList: IAnyArray;
  AnyValueList: TList<TAnyValue>;
  VariantsList: TList<Variant>;
begin
  Randomize;
  lbSliceData.Clear;
  lbSpeedResults.Clear;
  // set decimal separator
  {$IF CompilerVersion >= 22}Formatsettings.{$IFEND}DecimalSeparator := '.';

  AnyArrayList := CreateAnyArray;
  try
    if cbDynamicArray.Checked then
    begin
      AnyArrayList.ArrayMode := amDynamicArray;
    end
    else
    begin
      AnyArrayList.ArrayMode := amSlicedArray;
      AnyArrayList.SliceBufferMpl := StrToFloat(eSliceBufferMpl.Text);
      AnyArrayList.SliceSize := StrToInt(eSliceSize.Text);
    end;

    WriteToLog('********************* BEGIN AnyArrayList *********************');
    TempValueB := 0;
    StopWatch := TStopWatch.StartNew;
    for K := 1 to 200 do
    begin
      AnyArrayList.Clear;
      for I := 0 to 5000 - 1 do
        AnyArrayList.Push(I);
    end;
    StopWatch.Stop;
    WriteToLog(Format('IAnyArray, add 5000 X 200: %d', [stopwatch.ElapsedMilliseconds]));
    StopWatch := TStopWatch.StartNew;
    for K := 1 to 10 do
    begin
      for I := 0 to 1000 - 1 do
        AnyArrayList.DeleteIndex(Random(AnyArrayList.Count - 1));
      for I := 0 to 1000 - 1 do
        AnyArrayList.Push(I);
    end;
    StopWatch.Stop;
    WriteToLog(Format('IAnyArray, delete 1000 X 10 by index: %d', [stopwatch.ElapsedMilliseconds]));
    StopWatch := TStopWatch.StartNew;
    for K := 1 to 10 do
    begin
      for I := 0 to 1000 - 1 do
        AnyArrayList.Insert(Random(AnyArrayList.Count - 1), I);
      for I := 0 to 1000 - 1 do
        AnyArrayList.DeleteIndex(Random(AnyArrayList.Count - 1));
    end;
    StopWatch.Stop;
    WriteToLog(Format('IAnyArray, insert 1000 X 10 by index: %d', [stopwatch.ElapsedMilliseconds]));
    StopWatch := TStopWatch.StartNew;
    for I := 0 to 1000000 - 1 do
      TempValueA := AnyArrayList.Item[Random(AnyArrayList.Count - 1)].AsInteger;
    StopWatch.Stop;
    // use TempValue;
    Sleep(Min(0, TempValueA));
    WriteToLog(Format('IAnyArray, 1000000 random access by index: %d', [stopwatch.ElapsedMilliseconds]));
    StopWatch := TStopWatch.StartNew;
    for I := 0 to 1000000 - 1 do
      TempValueA := AnyArrayList.RawItem(Random(AnyArrayList.Count - 1)).AsInteger;
    StopWatch.Stop;
    // use TempValue;
    Sleep(Min(0, TempValueA));
    WriteToLog(Format('IAnyArray, 1000000 random raw access by index: %d', [stopwatch.ElapsedMilliseconds]));
    StopWatch := TStopWatch.StartNew;
    for K := 1 to 200 do
    begin
      for I := 0 to AnyArrayList.Count - 1 do
        TempValueB := AnyArrayList[I].AsInteger;
    end;
    StopWatch.Stop;
    // use TempValue;
    Sleep(Min(0, TempValueB));
    WriteToLog(Format('IAnyArray, 1000000 iterations by index X 200: %d', [stopwatch.ElapsedMilliseconds]));
    StopWatch := TStopWatch.StartNew;
    for K := 1 to 200 do
    begin
      for I := 0 to AnyArrayList.Count - 1 do
        TempValueB := AnyArrayList.RawItem(I).AsInteger;
    end;
    StopWatch.Stop;
    // use TempValue;
    Sleep(Min(0, TempValueB));
    WriteToLog(Format('IAnyArray, 1000000 raw iterations by index X 200: %d', [stopwatch.ElapsedMilliseconds]));
    StopWatch := TStopWatch.StartNew;
    for K := 1 to 200 do
    begin
      for EnumPointer in AnyArrayList.Enum.Forward do
        TempValueB := EnumPointer^.AsInteger;
    end;
    StopWatch.Stop;
    WriteToLog(Format('IAnyArray, pointers enumeration X 200: %d', [stopwatch.ElapsedMilliseconds]));
    WriteToLog('********************* END AnyArrayList *********************');

    // write current slice info to list
    WriteSlicesInfo(AnyArrayList);
  finally
    AnyArrayList := nil;
  end;

  AnyValueList := TList<TAnyValue>.Create;
  try
    WriteToLog('********************* BEGIN TList<TAnyValue> *********************');
    StopWatch := TStopWatch.StartNew;
    for K := 1 to 200 do
    begin
      AnyValueList.Clear;
      for I := 0 to 5000 - 1 do
        AnyValueList.Add(I);
    end;
    StopWatch.Stop;
    WriteToLog(Format('TList<TAnyValue>, add 5000 X 200: %d', [stopwatch.ElapsedMilliseconds]));
    StopWatch := TStopWatch.StartNew;
    for K := 1 to 10 do
    begin
      for I := 0 to 1000 - 1 do
        AnyValueList.Delete(Random(AnyValueList.Count - 1));
      for I := 0 to 1000 - 1 do
        AnyValueList.Add(I);
    end;
    StopWatch.Stop;
    WriteToLog(Format('TList<TAnyValue>, delete 1000 X 10 by index: %d', [stopwatch.ElapsedMilliseconds]));
    StopWatch := TStopWatch.StartNew;
    for K := 1 to 10 do
    begin
      for I := 0 to 1000 - 1 do
        AnyValueList.Insert(Random(AnyValueList.Count - 1), I);
      for I := 0 to 1000 - 1 do
        AnyValueList.Delete(Random(AnyValueList.Count - 1));
    end;
    StopWatch.Stop;
    WriteToLog(Format('TList<TAnyValue>, insert 1000 X 10 by index: %d', [stopwatch.ElapsedMilliseconds]));
    StopWatch := TStopWatch.StartNew;
    for I := 0 to 1000000 - 1 do
      TempValueA := AnyValueList[Random(AnyValueList.Count - 1)];
    StopWatch.Stop;
    // use TempValue;
    Sleep(Min(0, TempValueA));
    WriteToLog(Format('TList<TAnyValue>, 1000000 random access by index: %d', [stopwatch.ElapsedMilliseconds]));
    StopWatch := TStopWatch.StartNew;
    for K := 1 to 200 do
    begin
      for I := 0 to AnyValueList.Count - 1 do
        TempValueB := AnyValueList[I].AsInteger;
    end;
    StopWatch.Stop;
    // use TempValue;
    Sleep(Min(0, TempValueB));
    WriteToLog(Format('TList<TAnyValue>, 1000000 iterations by index X 200: %d', [stopwatch.ElapsedMilliseconds]));
    StopWatch := TStopWatch.StartNew;
    for K := 1 to 200 do
    begin
      for I := 0 to AnyValueList.Count - 1 do
        TempValueB := AnyValueList[I];
    end;
    StopWatch.Stop;
    // use TempValue;
    Sleep(Min(0, TempValueB));
    WriteToLog(Format('TList<TAnyValue>, enumeration X 200: %d', [stopwatch.ElapsedMilliseconds]));
    WriteToLog('********************* END TList<TAnyValue> *********************');
  finally
    AnyValueList.Free;
  end;

  VariantsList := TList<Variant>.Create;
  try
    WriteToLog('********************* BEGIN TList<Variant> *********************');
    StopWatch := TStopWatch.StartNew;
    for K := 1 to 200 do
    begin
      VariantsList.Clear;
      for I := 0 to 5000 - 1 do
        VariantsList.Add(I);
    end;
    StopWatch.Stop;
    WriteToLog(Format('TList<Variant>, add 5000 X 200: %d', [stopwatch.ElapsedMilliseconds]));
    StopWatch := TStopWatch.StartNew;
    for K := 1 to 10 do
    begin
      for I := 0 to 1000 - 1 do
        VariantsList.Delete(Random(VariantsList.Count - 1));
      for I := 0 to 1000 - 1 do
        VariantsList.Add(I);
    end;
    StopWatch.Stop;
    WriteToLog(Format('TList<Variant>, delete 1000 X 10 by index: %d', [stopwatch.ElapsedMilliseconds]));
    StopWatch := TStopWatch.StartNew;
    for K := 1 to 10 do
    begin
      for I := 0 to 1000 - 1 do
        VariantsList.Insert(Random(VariantsList.Count - 1), I);
      for I := 0 to 1000 - 1 do
        VariantsList.Delete(Random(VariantsList.Count - 1));
    end;
    StopWatch.Stop;
    WriteToLog(Format('TList<Variant>, insert 1000 X 10 by index: %d', [stopwatch.ElapsedMilliseconds]));
    StopWatch := TStopWatch.StartNew;
    for I := 0 to 1000000 - 1 do
      TempValueA := VariantsList[Random(VariantsList.Count - 1)];
    StopWatch.Stop;
    // use TempValue;
    Sleep(Min(0, TempValueA));
    WriteToLog(Format('TList<Variant>, 1000000 random access by index: %d', [stopwatch.ElapsedMilliseconds]));
    StopWatch := TStopWatch.StartNew;
    for K := 1 to 200 do
    begin
      for I := 0 to VariantsList.Count - 1 do
        TempValueB := VariantsList[I];
    end;
    StopWatch.Stop;
    WriteToLog(Format('TList<Variant>, 5000 iterantions by index X 200: %d', [stopwatch.ElapsedMilliseconds]));
    StopWatch := TStopWatch.StartNew;
    for K := 1 to 200 do
    begin
      for EnumVariant in VariantsList do
        TempValueB := EnumVariant;
    end;
    StopWatch.Stop;
    // use TempValue;
    Sleep(Min(0, TempValueB));
    WriteToLog(Format('TList<Variant>, enumeration X 200: %d', [stopwatch.ElapsedMilliseconds]));
    WriteToLog('********************* END TList<Variant> *********************');
  finally
    VariantsList.Free;
  end;

  TValueList := TList<TValue>.Create;
  try
    WriteToLog('********************* BEGIN TList<TValue> *********************');
    StopWatch := TStopWatch.StartNew;
    for K := 1 to 200 do
    begin
      TValueList.Clear;
      for I := 0 to 5000 - 1 do
        TValueList.Add(I);
    end;
    StopWatch.Stop;
    WriteToLog(Format('TList<TValue>, add 5000 X 200: %d', [stopwatch.ElapsedMilliseconds]));
    StopWatch := TStopWatch.StartNew;
    for K := 1 to 10 do
    begin
      for I := 0 to 1000 - 1 do
        TValueList.Delete(Random(TValueList.Count - 1));
      for I := 0 to 1000 - 1 do
        TValueList.Add(I);
    end;
    StopWatch.Stop;
    WriteToLog(Format('TList<TValue>, delete 1000 X 10 by index: %d', [stopwatch.ElapsedMilliseconds]));
    StopWatch := TStopWatch.StartNew;
    for K := 1 to 10 do
    begin
      for I := 0 to 1000 - 1 do
        TValueList.Insert(Random(TValueList.Count - 1), I);
      for I := 0 to 1000 - 1 do
        TValueList.Delete(Random(TValueList.Count - 1));
    end;
    StopWatch.Stop;
    WriteToLog(Format('TList<TValue>, insert 1000 X 10 by index: %d', [stopwatch.ElapsedMilliseconds]));
    StopWatch := TStopWatch.StartNew;
    for I := 0 to 1000000 - 1 do
      TempValueA := TValueList[Random(TValueList.Count - 1)].AsInteger;
    StopWatch.Stop;
    // use TempValue;
    Sleep(Min(0, TempValueA));
    WriteToLog(Format('TList<TValue>, 1000000 random access by index: %d', [stopwatch.ElapsedMilliseconds]));
    StopWatch := TStopWatch.StartNew;
    for K := 1 to 200 do
    begin
      for I := 0 to TValueList.Count - 1 do
        TempValueB := TValueList[I].AsInteger;
    end;
    StopWatch.Stop;
    // use TempValue;
    Sleep(Min(0, TempValueB));
    WriteToLog(Format('TList<TValue>, 5000 iterantions by index X 200: %d', [stopwatch.ElapsedMilliseconds]));
    StopWatch := TStopWatch.StartNew;
    for K := 1 to 200 do
    begin
      for EnumTValue in TValueList do
        TempValueB := EnumTValue.AsInteger;
    end;
    StopWatch.Stop;
    // use TempValue;
    Sleep(Min(0, TempValueB));
    WriteToLog(Format('TList<TValue>, enumeration X 200: %d', [stopwatch.ElapsedMilliseconds]));
    WriteToLog('********************* END TList<TValue> *********************');
  finally
    TValueList.Free;
  end;
end;

procedure TfMain.WriteSlicesInfo(const AnyArray: IAnyArray);
var
  I: Integer;
  ItemCount: Integer;
  CurrentSlice: PArraySlice;
begin
  lbSliceData.Clear;

  for I := 0 to AnyArray.SliceCount - 1 do
  begin
    CurrentSlice := AnyArray.RawData^[I];
    ItemCount := CurrentSlice.Last - CurrentSlice.Start;
    lbSliceData.Items.Add(Format('Slice %d: %d - %d [%d]', [CurrentSlice.Index,
                                                            CurrentSlice.Start,
                                                            CurrentSlice.Last,
                                                            ItemCount]));
  end;
end;

procedure TfMain.WriteToLog(const Msg: string);
begin
  lbSpeedResults.Items.Add(Msg);
  Application.ProcessMessages;
end;

{ TAnyValueComparer }

function TAnyValueComparer.Compare(const Left, Right: TAnyValue): Integer;
begin
  Result := Left.AsInteger - Right.AsInteger;
end;

end.
