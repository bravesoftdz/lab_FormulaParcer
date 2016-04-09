unit Unit1;

{$mode objfpc}{$H+}

interface

uses
      Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
      Lazutf8, u_formula_parcer;

type

      TDoFormulaResult = procedure (AName:String;AData:String; var AOut:TStringList);

      { TFMyFormulaTester }

      TFMyFormulaTester = class(TForm)
            Edit1: TEdit;
            Memo1: TMemo;
            procedure Edit1Change(Sender: TObject);
            procedure FormCreate(Sender: TObject);
            procedure FormDestroy(Sender: TObject);
      private
            { private declarations }
      public
            { public declarations }
      end;

const
      QUOTE1 = '&quot;';
      QUOTE2 = '\"';
      QUOTE3 = '"';
      QUOTEA = '(';
      QUOTEB = ')';
      QUOTEC = ',';
      BAFFLE = '\';
var
      FMyFormulaTester: TFMyFormulaTester;
      Params:TStringList;
      MyFormula:TAssiFormulaParcer;

implementation

{$R *.lfm}

{ TFMyFormulaTester }

procedure TFMyFormulaTester.Edit1Change(Sender: TObject);
var
     AOut:String;
begin
    Params.Clear;
    AOut:='error';
    MyFormula.Execute(nil, Edit1.Text, AOut);
    Params.Add(MyFormula.ExecutionLog);
    Params.Add('Result='+AOut);
    //GetCmdParams(Edit1.Text, Params);
    Memo1.Lines.Text:=Params.Text;
end;

procedure TFMyFormulaTester.FormCreate(Sender: TObject);
begin
    MyFormula:=TAssiFormulaParcer.Create;
    Params:=TStringList.Create;
end;

procedure TFMyFormulaTester.FormDestroy(Sender: TObject);
begin
    Params.Free;
    MyFormula.Free;
end;

end.

