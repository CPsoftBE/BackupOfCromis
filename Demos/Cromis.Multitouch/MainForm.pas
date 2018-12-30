unit MainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  System.Generics.Collections, System.UIConsts,

  // FMX units
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Layouts,
  FMX.ListBox, FMX.Platform, FMX.Gestures, FMX.Objects, FMX.StdCtrls,

{$IFDEF IOS}
  iOSApi.UIKit, FMX.Platform.iOS,
{$ENDIF}

  // multitouch support
  Cromis.Multitouch.Custom;

type
  TfMain = class(TForm)
    PaintBox: TPaintBox;
    pnTopBar: TPanel;
    btnClear: TButton;
    lbTouchUp: TLabel;
    lbTouchMove: TLabel;
    lbTouchDown: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure PaintBoxPaint(Sender: TObject; Canvas: TCanvas);
    procedure FormShow(Sender: TObject);
    procedure btnClearClick(Sender: TObject);
  private
    FIsPainting: Boolean;
    FBitmapBuffer: TBitmap;
    procedure OnTouchEvent(const Event: TTouchEvent);
    function PointsToString(const Event: TTouchEvent): string;
  public
    { Public declarations }
  end;

var
  fMain: TfMain;

implementation

{$R *.fmx}

procedure TfMain.btnClearClick(Sender: TObject);
begin
  FBitmapBuffer.Canvas.BeginScene;
  try
    FBitmapBuffer.Canvas.Clear(claWhite);
  finally
    FBitmapBuffer.Canvas.EndScene;
  end;

  // repaint box
  PaintBox.Repaint;
end;

procedure TfMain.FormCreate(Sender: TObject);
begin
  FBitmapBuffer := TBitmap.Create;
  FBitmapBuffer.Width := Trunc(PaintBox.Width);
  FBitmapBuffer.Height := Trunc(PaintBox.Height);

  FBitmapBuffer.Canvas.Fill.Color := claBlue;
  FBitmapBuffer.Canvas.Stroke.Color:= claBlue;
  FBitmapBuffer.Canvas.Fill.Kind := TBrushKind.bkSolid;
  FBitmapBuffer.Canvas.Stroke.Dash:= TStrokeDash.sdSolid;

  InitializeTouchListener;
  TouchEventListener.AddHandler(OnTouchEvent);
end;

procedure TfMain.FormDestroy(Sender: TObject);
begin
  FinalizeTouchListener;
  FreeAndNil(FBitmapBuffer);
end;

procedure TfMain.FormShow(Sender: TObject);
{$IFDEF IOS}
var
  V: UIView;
{$ENDIF}
begin
{$IFDEF IOS}
  V := WindowHandleToPlatform(Handle).View;
  V.setMultipleTouchEnabled(True);
{$ENDIF}

  FBitmapBuffer.Canvas.BeginScene;
  try
    FBitmapBuffer.Canvas.Clear(claWhite);
  finally
    FBitmapBuffer.Canvas.EndScene;
  end;

  PaintBox.Canvas.BeginScene;
  try
    PaintBox.Canvas.Clear(claWhite);
  finally
    PaintBox.Canvas.EndScene;
  end;

  PaintBox.Repaint;
end;

procedure TfMain.OnTouchEvent(const Event: TTouchEvent);
const
  cSize = 10;
var
  I: Integer;
  X: Single;
  Y: Single;
begin
  lbTouchUp.Text := 'UP: N/A';
  lbTouchDown.Text := 'DOWN: N/A';
  lbTouchMove.Text := 'MOVE: N/A';

  case Event.EventType of
    teDown:
      begin
        lbTouchDown.Text := Format('DOWN: %s', [PointsToString(Event)]);
        FIsPainting := True;
      end;
    teMove:
      begin
        if FIsPainting then
        begin
          for I := Low(Event.Points) to High(Event.Points) do
          begin
            X := Event.Points[I].RelPosition.X;
            Y := Event.Points[I].RelPosition.Y;

            FBitmapBuffer.Canvas.BeginScene;
            try
              FBitmapBuffer.Canvas.FillEllipse(RectF(X - cSize / 2, Y - cSize / 2, X + cSize / 2, Y + cSize / 2), 255);
            finally
              FBitmapBuffer.Canvas.EndScene;
            end;

            lbTouchMove.Text := Format('MOVE: %s', [PointsToString(Event)]);
            PaintBox.Repaint;
          end;
        end;
      end;
    teUp:
      begin
        lbTouchUp.Text := Format('UP: %s', [PointsToString(Event)]);
        FIsPainting := Length(Event.Points) > 1;
      end;
  end;
end;

procedure TfMain.PaintBoxPaint(Sender: TObject; Canvas: TCanvas);
var
  SourceRect: TRectF;
begin
  Canvas.BeginScene;
  try
    SourceRect := RectF(0, 0, FBitmapBuffer.Width, FBitmapBuffer.Height);
    Canvas.DrawBitmap(FBitmapBuffer, SourceRect, SourceRect, 255, True);
  finally
    Canvas.EndScene;
  end;
end;

function TfMain.PointsToString(const Event: TTouchEvent): string;
var
  I: Integer;
begin
  Result := '';

  for I := 0 to Length(Event.Points) - 1 do
  begin
    if not (pfTouchUp in Event.Points[I].Flags) then
    begin
      if Result = '' then
        Result := Format('[%d - > %f:%f]', [Event.Points[I].ID,
                                            Event.Points[I].Position.X,
                                            Event.Points[I].Position.Y])
      else
        Result := Result + ' ' + Format('[%d - > %f:%f]', [Event.Points[I].ID,
                                                           Event.Points[I].Position.X,
                                                           Event.Points[I].Position.Y]);
    end;
  end;
end;

end.
