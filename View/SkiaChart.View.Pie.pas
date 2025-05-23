unit SkiaChart.View.Pie;

interface

uses
  System.Generics.Collections,

  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  System.Skia, FMX.Ani, FMX.Controls.Presentation, FMX.Objects, FMX.Skia,
  FMX.Layouts;

type
  TPieSlice = record
    Value: Double; // Valor da fatia
    Color: TAlphaColor; // Cor da fatia
    Text: string; // Rótulo da fatia
    Enabled: Boolean;

    constructor Create(AValue: Double; AColor: TAlphaColor; AText: string; AEnabled: Boolean = True);
  end;

  TLayoutLegend = class(TLayout)
  private
    FRectColor: TRectangle;
    FLbl: TLabel;
    FIndex: Integer;

    function GetColor: TAlphaColor;
    procedure SetColor(const Value: TAlphaColor);
    { Private declarations }
  protected
    procedure Painting; override;
    procedure Click; override;
    procedure Tap(const Point: TPointF); override;
    { Protected declarations }
  public
    constructor Create(AOwner: TComponent); override;

    property Color: TAlphaColor read GetColor write SetColor;
    property Text: TLabel read FLbl;
    property &Index: Integer read FIndex write FIndex;

    { Public declarations }
  end;

  TFrmSkiaChartPie = class(TFrame)
    lytLegend: TLayout;
    skChart: TSkPaintBox;
    lytSelectedSlice: TLayout;
    rctSelectedSliceBackground: TRectangle;
    lytSelectedSliceBackground: TLayout;
    lblSelectedSliceText: TLabel;
    lytSelectedSliceBottom: TLayout;
    lblSelectedSliceValue: TLabel;
    lytSelectedSliceColor: TLayout;
    rctSelectedSliceColor: TRectangle;
    caniSelectedSlice: TColorAnimation;
    tmrLabel: TTimer;
    faniSelectedSliceX: TFloatAnimation;
    faniSelectedSliceY: TFloatAnimation;
    tmrAnimation: TTimer;
    procedure tmrAnimationTimer(Sender: TObject);
    procedure skChartDraw(ASender: TObject; const ACanvas: ISkCanvas;
      const ADest: TRectF; const AOpacity: Single);
    procedure skChartMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure rctSelectedSliceBackgroundClick(Sender: TObject);
    procedure rctSelectedSliceBackgroundTap(Sender: TObject;
      const Point: TPointF);
    procedure tmrLabelTimer(Sender: TObject);
    procedure lytSelectedSliceBottomPainting(Sender: TObject; Canvas: TCanvas;
      const ARect: TRectF);
    procedure lytLegendPainting(Sender: TObject; Canvas: TCanvas;
      const ARect: TRectF);
  private
    const
    CStartAngle = 270; // Start from 270 (12 hours)
    CAryColors: array [0 .. 10] of LongWord = (
      $FF36A2EB, // 0
      $FFFE6383,
      $FFFE9F3E,
      $FFFFCB55,
      $FF4AC0C0,
      $FF9966FF,
      $FFCACACA,
      $FF4CAF50,
      $FFFFF59D,
      $FFE1F5FE,
      $FFFF4081 // Hot pink
      );

  var
    FSlices: TArray<TPieSlice>;
    FAnimationAngle: Single; // Current animation angle
    FAnimationSpeed: Single; // Animation speed (radius per tick)
    FSelectedSlice: Integer; // selected slice index (-1 = none)
    FObjLstLegend: TObjectList<TLayoutLegend>;

    procedure UpdateLegend(AIndex: Integer; ACenter: TPointF; ARadius: Single; ATotal: Single);
    procedure OnLegendTap(Sender: TObject; const APoint: TPointF);
    { Private declarations }
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure StartAnimation;

    /// <summary> Add a slice with default color
    /// </summary>
    procedure SliceAdd(AValue: Double; AText: string); overload;
    /// <summary> Add a slice with custom color
    /// </summary>
    procedure SliceAdd(AValue: Double; AColor: TAlphaColor; AText: string); overload;
    { Public declarations }
  end;

implementation

uses
  System.Math;

{$R *.fmx}

{ TPieSlice }

constructor TPieSlice.Create(AValue: Double; AColor: TAlphaColor; AText: string;
  AEnabled: Boolean);
begin
  Value := AValue;
  Color := AColor;
  Text := AText;
  Enabled := AEnabled;
end;

{ TLayoutLegend }

procedure TLayoutLegend.Click;
begin
{$IFDEF MSWINDOWS}
  Tap(TPointF.Create(0, 0));
{$ELSE}
  inherited;
{$ENDIF}
end;

constructor TLayoutLegend.Create(AOwner: TComponent);
begin
  inherited;
  HitTest := True;
  Height := 25;
  var
  LLytColor := TLayout.Create(Self);
  LLytColor.Parent := Self;
  LLytColor.Align := TAlignLayout.MostLeft;
  LLytColor.Width := 50;
  LLytColor.TabStop := False;
  LLytColor.HitTest := False;
  LLytColor.Margins.Left := 10;

  FRectColor := TRectangle.Create(Self);
  FRectColor.Parent := LLytColor;
  FRectColor.Align := TAlignLayout.VertCenter;
  FRectColor.Stroke.Kind := TBrushKind.None;
  FRectColor.Height := 20;
  FRectColor.Margins.Left := 5;
  FRectColor.Margins.Right := 5;
  FRectColor.HitTest := False;

  FLbl := TLabel.Create(Self);
  FLbl.Parent := Self;
  FLbl.Align := TAlignLayout.Left;
  FLbl.AutoSize := True;
  FLbl.WordWrap := False;
  FLbl.Margins.Left := 5;
  FLbl.HitTest := False;
  FLbl.StyledSettings := FLbl.StyledSettings - [TStyledSetting.Style];
end;

function TLayoutLegend.GetColor: TAlphaColor;
begin
  Result := FRectColor.Fill.Color;
end;

procedure TLayoutLegend.Painting;
begin
  inherited;
  Width := FLbl.Position.X + FLbl.Width + 10;
end;

procedure TLayoutLegend.SetColor(const Value: TAlphaColor);
begin
  FRectColor.Fill.Color := Value;
end;

procedure TLayoutLegend.Tap(const Point: TPointF);
begin
  inherited;
  if (TFontStyle.fsStrikeOut in FLbl.TextSettings.Font.Style) then
    FLbl.TextSettings.Font.Style := FLbl.TextSettings.Font.Style - [TFontStyle.fsStrikeOut]
  else
    FLbl.TextSettings.Font.Style := FLbl.TextSettings.Font.Style + [TFontStyle.fsStrikeOut];
end;

{ TFrmSkiaChartPie }

constructor TFrmSkiaChartPie.Create(AOwner: TComponent);
begin
  inherited;
  SetLength(FSlices, 0);
  FObjLstLegend := TObjectList<TLayoutLegend>.Create;
end;

destructor TFrmSkiaChartPie.Destroy;
begin
  if Assigned(FObjLstLegend) then
  begin
    try
      while FObjLstLegend.Count > 0 do
        FObjLstLegend.ExtractAt(0);
      FreeAndNil(FObjLstLegend);
    except

    end;
  end;
  inherited;
end;

procedure TFrmSkiaChartPie.lytLegendPainting(Sender: TObject; Canvas: TCanvas;
  const ARect: TRectF);
const
  CHeight = 25;
begin
  // Legend button position
  lytLegend.OnPainting := nil;
  try
    var
    LHeight := CHeight;
    var
    LPos : Single := 0;
    for var i := 0 to Pred(FObjLstLegend.Count) do
    begin
      var
      LLyt := FObjLstLegend[i];
      var
      LPosNew := LPos + LLyt.Width;
      if LPosNew > (lytLegend.Width - 5) then
      begin // Add new line
        LHeight := LHeight + CHeight;
        LPos := 0;
      end;
      LLyt.Position.Y := LHeight - CHeight;
      LLyt.Position.X := LPos;
      LPos := LLyt.Position.X + LLyt.Width;
    end;
    lytLegend.Height := LHeight + 5;
  finally
    lytLegend.OnPainting := lytLegendPainting;
  end;
end;

procedure TFrmSkiaChartPie.lytSelectedSliceBottomPainting(Sender: TObject;
  Canvas: TCanvas; const ARect: TRectF);
begin
  lytSelectedSlice.Height := lytSelectedSliceBottom.Position.Y + lytSelectedSliceBottom.Height + 5;
end;

procedure TFrmSkiaChartPie.OnLegendTap(Sender: TObject; const APoint: TPointF);
begin
  FSlices[TLayoutLegend(Sender).&Index].Enabled := not FSlices[TLayoutLegend(Sender).&Index].Enabled;
  skChart.Redraw;
end;

procedure TFrmSkiaChartPie.rctSelectedSliceBackgroundClick(Sender: TObject);
begin
{$IFDEF MSWINDOWS}
  if (Sender.InheritsFrom(TControl)) and (Assigned(TControl(Sender).OnTap)) then
    TControl(Sender).OnTap(Sender, TPointF.Create(0, 0));
{$ENDIF}
end;

procedure TFrmSkiaChartPie.rctSelectedSliceBackgroundTap(Sender: TObject;
  const Point: TPointF);
begin
  if tmrLabel.Enabled then
    Exit;
  FSelectedSlice := -1;
  UpdateLegend(FSelectedSlice, TPointF.Create(0, 0), 0, 0);
end;

procedure TFrmSkiaChartPie.skChartDraw(ASender: TObject;
  const ACanvas: ISkCanvas; const ADest: TRectF; const AOpacity: Single);
var
  LCenter: TPointF;
  LRadius: Single;
  LStartAngle, LSweepAngle, LTotal, LCurrentAngle: Single;
  LSlice: TPieSlice;
  LPaint: ISkPaint;
  LPathBuilder: ISkPathBuilder;
  LPath: ISkPath;
  LRect: TRectF;
  LFont: ISkFont;
  i: Integer;
begin
  // Center and radius of chart
  LCenter := TPointF.Create(ADest.Width / 2, ADest.Height / 2);
  LRadius := Min(ADest.Width, ADest.Height) / 2 * 0.8; // 80% of current size

  // Sum of enabled values
  LTotal := 0;
  for LSlice in FSlices do
    if LSlice.Enabled then
    begin
      LTotal := LTotal + LSlice.Value
    end
    else
    begin
      StrToInt('1');
    end;

  // Initialize
  LPaint := TSkPaint.Create;
  LPaint.AntiAlias := True;
  LPaint.Style := TSkPaintStyle.Fill;

  // Paint the slices based on the animation angle
  LStartAngle := CStartAngle;
  LCurrentAngle := 0;
  for i := 0 to High(FSlices) do
  begin
    if not FSlices[i].Enabled then
      Continue;
    // Check the Slice's angle
    LSweepAngle := (FSlices[i].Value / LTotal) * 360;

    // Check if paint the Slice (artially or complete)
    if (LCurrentAngle < FAnimationAngle) then
    begin
      var
      LDrawAngle := Min(LSweepAngle, FAnimationAngle - LCurrentAngle); // Angle to draw
      if LDrawAngle > 0 then
      begin
        // Slice color
        LPaint.Color := FSlices[i].Color;

        // draw the path
        LPathBuilder := TSkPathBuilder.Create;
        try
          LRect := TRectF.Create(LCenter.X - LRadius, LCenter.Y - LRadius, LCenter.X + LRadius, LCenter.Y + LRadius);
          LPathBuilder.MoveTo(LCenter); // Start from center
          LPathBuilder.ArcTo(LRect, LStartAngle, LDrawAngle, False); // Add arc
          LPathBuilder.LineTo(LCenter); // Back to center
          LPathBuilder.Close; // close path
          LPath := LPathBuilder.Detach;
          ACanvas.DrawPath(LPath, LPaint);
        finally
          LPathBuilder := nil;
        end;
      end;
    end;

    // Update angles
    LStartAngle := LStartAngle + LSweepAngle;
    LCurrentAngle := LCurrentAngle + LSweepAngle;
  end;

  UpdateLegend(FSelectedSlice, LCenter, LRadius, LTotal);
end;

procedure TFrmSkiaChartPie.skChartMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Single);
var
  LCenter: TPointF;
  LRadius: Single;
  LTotal, LCurrentAngle, LSweepAngle: Single;
  LAngle: Single;
  LDistance: Single;
  LSlice: TPieSlice;
  i: Integer;
begin
  // Check the center and radius
  LCenter := TPointF.Create(skChart.Width / 2, skChart.Height / 2);
  LRadius := Min(skChart.Width, skChart.Height) / 2 * 0.8;

  // Calculate the Clicked angle based on the center
  LAngle := ArcTan2(Y - LCenter.Y, X - LCenter.X) * 180 / Pi;
  if LAngle < 0 then
    LAngle := LAngle + 360; // Normalizar para 0-360 graus

  // Fix the angle based on stating angle 270
  LAngle := LAngle - 270;
  if LAngle < 0 then
    LAngle := LAngle + 360;

  // Check if the click is in the chart radius
  LDistance := Sqrt(Sqr(X - LCenter.X) + Sqr(Y - LCenter.Y));
  if LDistance <= LRadius then
  begin
    // Sum of enabled values
    LTotal := 0;
    for LSlice in FSlices do
      if LSlice.Enabled then
        LTotal := LTotal + LSlice.Value;

    // Find the clicked slice
    LCurrentAngle := 0;
    for i := 0 to High(FSlices) do
    begin
      if not FSlices[i].Enabled then
        Continue;
      LSweepAngle := (FSlices[i].Value / LTotal) * 360;
      if (LAngle >= LCurrentAngle) and (LAngle < LCurrentAngle + LSweepAngle) then
      begin
        if i = FSelectedSlice then
          FSelectedSlice := -1 // Deselect if second click
        else
          FSelectedSlice := i; // Select the slice
        Break;
      end;
      LCurrentAngle := LCurrentAngle + LSweepAngle;
    end;
    UpdateLegend(FSelectedSlice, LCenter, LRadius, LTotal);
    skChart.Redraw;
  end
  else
  begin
    FSelectedSlice := -1; // Deselect
    UpdateLegend(FSelectedSlice, LCenter, LRadius, LTotal);
  end;
end;

procedure TFrmSkiaChartPie.StartAnimation;
begin
  // Config animation
  FAnimationAngle := 0;
  FAnimationSpeed := 5;
  tmrAnimation.Interval := 3;
  FSelectedSlice := -1;
  lytSelectedSlice.Visible := False;
  tmrAnimation.Enabled := True;
  skChart.Redraw;
end;

procedure TFrmSkiaChartPie.tmrAnimationTimer(Sender: TObject);
begin
  FAnimationAngle := FAnimationAngle + FAnimationSpeed;
  if FAnimationAngle >= 360 then
  begin
    tmrAnimation.Enabled := False; // Stop animation
    FAnimationAngle := 360; // Full circle
  end;
  FAnimationSpeed := FAnimationSpeed * 1.05;
  skChart.Redraw;
end;

procedure TFrmSkiaChartPie.tmrLabelTimer(Sender: TObject);
begin
  tmrLabel.Enabled := False;
end;

procedure TFrmSkiaChartPie.UpdateLegend(AIndex: Integer; ACenter: TPointF;
  ARadius, ATotal: Single);
var
  LStartAngle, LSweepAngle, LAngle: Single;
  LCurrentAngle: Single;
  LLabelPos: TPointF;
  i: Integer;
begin
  if (AIndex < 0) or (not FSlices[AIndex].Enabled) then
  begin
    lytSelectedSlice.Visible := False;
    tmrLabel.Enabled := False;
    Exit;
  end;

  // Calculate the Legend position based on selected Slice
  LStartAngle := CStartAngle;
  LCurrentAngle := 0;
  for i := 0 to High(FSlices) do
  begin
    if not FSlices[i].Enabled then
      Continue;
    LSweepAngle := (FSlices[i].Value / ATotal) * 360;
    if i = AIndex then
    begin
      LAngle := DegToRad(LStartAngle + LSweepAngle / 2);
      LLabelPos := TPointF.Create(
        ACenter.X + Cos(LAngle) * ARadius * 0.7,
        ACenter.Y + Sin(LAngle) * ARadius * 0.7
        );
      lblSelectedSliceText.Text := FSlices[i].Text;
      lblSelectedSliceValue.Text := 'R$ ' + FormatFloat('##0.,00', FSlices[i].Value);
      var
      LPosX := LLabelPos.X - lytSelectedSlice.Width / 2; // Horizontal center
      var
      LPosY := LLabelPos.Y - lytSelectedSlice.Height / 2; // Vertical center
      var
      LColor := FSlices[i].Color;
      tmrLabel.Enabled := False;
      if lytSelectedSlice.Visible then
      begin
        faniSelectedSliceX.StartValue := lytSelectedSlice.Position.X;
        faniSelectedSliceY.StartValue := lytSelectedSlice.Position.Y;
        caniSelectedSlice.StartValue := rctSelectedSliceColor.Fill.Color;
        faniSelectedSliceX.StopValue := LPosX;
        faniSelectedSliceY.StopValue := LPosY;
        caniSelectedSlice.StopValue := LColor;
        faniSelectedSliceX.Start;
        faniSelectedSliceY.Start;
        caniSelectedSlice.Start;
      end
      else
      begin
        rctSelectedSliceColor.Fill.Color := LColor;
        lytSelectedSlice.Position.X := LPosX;
        lytSelectedSlice.Position.Y := LPosY;
        lytSelectedSlice.Visible := True;
      end;
      tmrLabel.Enabled := True;
      Break;
    end;
    LStartAngle := LStartAngle + LSweepAngle;
    LCurrentAngle := LCurrentAngle + LSweepAngle;
  end;
end;

procedure TFrmSkiaChartPie.SliceAdd(AValue: Double; AText: string);
begin
  var
  LIndex := Length(FSlices);
  if LIndex > Length(CAryColors) then
    LIndex := LIndex - Length(CAryColors);
  SliceAdd(AValue, CAryColors[LIndex], AText);
end;

procedure TFrmSkiaChartPie.SliceAdd(AValue: Double; AColor: TAlphaColor;
  AText: string);
begin
  var
  LIndex := Length(FSlices);
  SetLength(FSlices, Succ(LIndex));
  FSlices[LIndex] := TPieSlice.Create(AValue, AColor, AText);

  var
  LLyt := TLayoutLegend.Create(Self);
  LLyt.Parent := lytLegend;
  LLyt.OnTap := OnLegendTap;
  LLyt.Text.Text := FSlices[LIndex].Text;
  LLyt.Color := FSlices[LIndex].Color;
  LLyt.&Index := LIndex;
  FObjLstLegend.Add(LLyt)
end;

end.
