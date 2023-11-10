program InpView;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, usetup, vinfo, uAbout, uView
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TSetup, Setup);
  Application.CreateForm(TfAbout, fAbout);
  Application.CreateForm(TiView, iView);
  Application.Run;
end.

