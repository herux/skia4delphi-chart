unit FormMain;

interface

uses
  System.Generics.Collections,

  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, System.Skia,
  FMX.Skia, FMX.Controls.Presentation, FMX.StdCtrls, FMX.Layouts, FMX.Objects,
  FMX.Ani;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Layout1: TLayout;
    chkPie: TCheckBox;
    tbarLegendSize: TTrackBar;
    lytChartType: TLayout;
    lytOptions: TLayout;
    procedure Button1Click(Sender: TObject);
    procedure tbarLegendSizeChange(Sender: TObject);
  private
    FFrmChart: TFrame;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  SkiaChart.View.Pie;

{$R *.fmx}

procedure TForm1.Button1Click(Sender: TObject);
begin
  if not Assigned(FFrmChart) then
  begin
    if chkPie.IsChecked then
    begin
      FFrmChart := TFrmSkiaChartPie.Create(Self);
      FFrmChart.Parent := Self;
      FFrmChart.Align := TAlignLayout.Client;
      TFrmSkiaChartPie(FFrmChart).SliceAdd(32415, 'Category 1');
      TFrmSkiaChartPie(FFrmChart).SliceAdd(10000, 'Category 2');
      TFrmSkiaChartPie(FFrmChart).SliceAdd(5000, 'Category 3');
      TFrmSkiaChartPie(FFrmChart).SliceAdd(10661.88, 'Category 4');
      TFrmSkiaChartPie(FFrmChart).SliceAdd(5000, 'Category 5');
      TFrmSkiaChartPie(FFrmChart).SliceAdd(1518, 'Category 6');
      TFrmSkiaChartPie(FFrmChart).SliceAdd(14206.67, 'Category 7');
    end
    else
    begin
      // yet to come
    end;
  end;
  if Assigned(FFrmChart) then
    TFrmSkiaChartPie(FFrmChart).StartAnimation;
end;

procedure TForm1.tbarLegendSizeChange(Sender: TObject);
begin
  TFrmSkiaChartPie(FFrmChart).LegendSize := tbarLegendSize.Value / 100;
end;

end.
