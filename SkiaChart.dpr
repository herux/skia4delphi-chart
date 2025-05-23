program SkiaChart;

uses
  System.StartUpCopy,
  FMX.Forms,
  FMX.Skia,
  FormMain in 'FormMain.pas' {Form1},
  SkiaChart.View.Pie in 'View\SkiaChart.View.Pie.pas' {FrmSkiaChartPie: TFrame};

{$R *.res}

begin
  GlobalUseSkia := True;
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
