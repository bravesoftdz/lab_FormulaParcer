program project1;

{$mode objfpc}{$H+}

uses
      {$IFDEF UNIX}{$IFDEF UseCThreads}
      cthreads,
      {$ENDIF}{$ENDIF}
      Interfaces, // this includes the LCL widgetset
      Forms, Unit1, u_formula_parcer, u_formula_fstandard
      { you can add units after this };

{$R *.res}

begin
      RequireDerivedFormResource := True;
      Application.Initialize;
  Application.CreateForm(TFMyFormulaTester, FMyFormulaTester);
      Application.Run;
end.

