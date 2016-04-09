unit u_formula_fstandard;

{$mode objfpc}{$H+}

//************************************************************
 //
 //    Модуль u_formula_fstandard
 //    Copyright (c) 2016  Pichugin M.
 //    This file is part of the Assi System.
 //
 //    Разработчик: Pichugin M. (e-mail: pichugin_m@mail.ru)
 //    (2016-02-27)
 //
//************************************************************

interface

uses
      Classes, SysUtils, LazUTF8, u_formula_parcer;

{
    ======Стандартные формулы======

    Sum(X1,X2,X3,...,Xn)
    Difference(X1,X2,X3,...,Xn)
    Product(X1,X2,X3,...,Xn)
    Quotient(X1,X2,X3,...,Xn)

    ReplaceEqual(Text,OldText1,NewText1,OldText2,NewText2,...,,OldTextN,NewTextN)
    ReplaceAll(Text,OldText,NewText)
    Replace(Text,OldText,NewText)
    Concat(N1,N2,N3,...,Nn)

    If(Check,TrueResult,FalseResult)
    Equal(N1,N2,N3,...,Nn)
}

procedure SetDefaultFormula(AParcer:TAssiFormulaParcer);

implementation

{ Стандартные формулы }

//Сравнение параметров
procedure Formula_Equal(AParcer:TAssiFormulaParcer; Sender:Pointer; AData:String;
  var AOutData:String; var AResult:Boolean);
var
  i,k     :Integer;
  BParams :TStringList;
begin
  k:=0;
  BParams :=TStringList.Create;
  GetFormulaData(AData, BParams);
  if BParams.Count>1 then
  begin
       for i:=1 to BParams.Count-1 do
       begin
           if CompareText(BParams.Strings[i-1], BParams.Strings[i])<>0 then
           begin
              inc(k);
           end;
       end;
       if k=0 then
         AOutData:=FORMULA_RESULT_TRUE
       else
         AOutData:=FORMULA_RESULT_FALSE;
  end
  else begin
    AResult:=False;
    AOutData:=FORMULA_RESULT_FALSE;
  end;
  BParams.Free;
end;

//Условие
procedure Formula_If(AParcer:TAssiFormulaParcer; Sender:Pointer; AData:String;
  var AOutData:String; var AResult:Boolean);
var
  i,k     :Integer;
  BParams :TStringList;
begin
  k:=0;
  BParams :=TStringList.Create;
  GetFormulaData(AData, BParams);
  if BParams.Count=3 then
  begin
       if CompareText(BParams.Strings[0], FORMULA_RESULT_TRUE)=0 then
       begin
          AOutData:=BParams.Strings[1];
       end
       else begin
          AOutData:=BParams.Strings[2];
       end;
  end
  else begin
    AResult:=False;
    AOutData:=FORMULA_RESULT_FALSE;
  end;
  BParams.Free;
end;

//Сложение, Сумма
procedure Formula_Sum(AParcer:TAssiFormulaParcer; Sender:Pointer; AData:String;
  var AOutData:String; var AResult:Boolean);
var
  i       :Integer;
  BParams :TStringList;
  x       :Double;
begin
  AOutData:='0';
  BParams :=TStringList.Create;
  GetFormulaData(AData, BParams);
  if BParams.Count>1 then
  begin
       x:=0;
       for i:=0 to BParams.Count-1 do
       begin
           if isCorrectNumber(BParams.Strings[i]) then
           begin
               x:=x+StrToFloat(GetCorrectNumber(BParams.Strings[i]));
           end
           else begin
               AResult:=False;
               break;
           end;
       end;
       AOutData:=FloatToStr(x);
  end
  else begin
    AResult:=False;
  end;
  BParams.Free;
end;

//Вычитание, Разность
procedure Formula_Difference(AParcer:TAssiFormulaParcer; Sender:Pointer; AData:String;
  var AOutData:String; var AResult:Boolean);
var
  i       :Integer;
  BParams :TStringList;
  x       :Double;
begin
  AOutData:='0';
  BParams :=TStringList.Create;
  GetFormulaData(AData, BParams);
  if BParams.Count>1 then
  begin
       x:=0;
       if isCorrectNumber(BParams.Strings[0]) then
       begin
         x:=StrToFloat(GetCorrectNumber(BParams.Strings[0]));
         for i:=1 to BParams.Count-1 do
         begin
             if isCorrectNumber(BParams.Strings[i]) then
             begin
                 x:=x-StrToFloat(GetCorrectNumber(BParams.Strings[i]));
             end
             else begin
                 AResult:=False;
                 break;
             end;
         end;
       end;
       AOutData:=FloatToStr(x);
  end
  else begin
    AResult:=False;
  end;
  BParams.Free;
end;

//Умножение, Произведение
procedure Formula_Product(AParcer:TAssiFormulaParcer; Sender:Pointer; AData:String;
  var AOutData:String; var AResult:Boolean);
var
  i       :Integer;
  BParams :TStringList;
  x       :Double;
begin
  AOutData:='0';
  BParams :=TStringList.Create;
  GetFormulaData(AData, BParams);
  if BParams.Count>1 then
  begin
       x:=1;
       for i:=0 to BParams.Count-1 do
       begin
           if isCorrectNumber(BParams.Strings[i]) then
           begin
               x:=x*StrToFloat(GetCorrectNumber(BParams.Strings[i]));
           end
           else begin
               AResult:=False;
               break;
           end;
       end;
       AOutData:=FloatToStr(x);
  end
  else begin
    AResult:=False;
  end;
  BParams.Free;
end;

//Деление, Частное
procedure Formula_Quotient(AParcer:TAssiFormulaParcer; Sender:Pointer; AData:String;
  var AOutData:String; var AResult:Boolean);
var
  i       :Integer;
  BParams :TStringList;
  x       :Double;
begin
  AOutData:='0';
  BParams :=TStringList.Create;
  GetFormulaData(AData, BParams);
  if BParams.Count>1 then
  begin
       x:=0;
       if isCorrectNumber(BParams.Strings[0]) then
       begin
         x:=StrToFloat(GetCorrectNumber(BParams.Strings[0]));
         for i:=1 to BParams.Count-1 do
         begin
             if isCorrectNumber(BParams.Strings[i]) then
             begin
                 x:=x/StrToFloat(GetCorrectNumber(BParams.Strings[i]));
             end
             else begin
                 AResult:=False;
                 break;
             end;
         end;
       end;
       AOutData:=FloatToStr(x);
  end
  else begin
    AResult:=False;
  end;
  BParams.Free;
end;

//Объединение
procedure Formula_Concat(AParcer:TAssiFormulaParcer; Sender:Pointer; AData:String;
  var AOutData:String; var AResult:Boolean);
var
  i       :Integer;
  BParams :TStringList;
begin
  BParams :=TStringList.Create;
  GetFormulaData(AData, BParams);
  if BParams.Count>1 then
  begin
       for i:=0 to BParams.Count-1 do
       begin
           AOutData:=AOutData+BParams.Strings[i];
       end;
  end
  else begin
    AResult:=False;
  end;
  BParams.Free;
end;

//Замена
procedure Formula_ReplaceEqual(AParcer:TAssiFormulaParcer; Sender:Pointer; AData:String;
  var AOutData:String; var AResult:Boolean);
var
  i       :Integer;
  BParams :TStringList;
begin
  BParams :=TStringList.Create;
  GetFormulaData(AData, BParams);
  if BParams.Count>=3 then
  begin
       i:=1;
       AOutData:=BParams.Strings[0];
       while i<BParams.Count do
       begin
           if (CompareText(BParams.Strings[0], BParams.Strings[i])=0)
               and(BParams.Count>i+1) then
           begin
              AOutData:=BParams.Strings[i+1];
              break;
           end
           else begin
             i:=i+2;
           end;
       end;
  end
  else begin
    AResult:=False;
  end;
  BParams.Free;
end;

//Замена
procedure Formula_Replace(AParcer:TAssiFormulaParcer; Sender:Pointer; AData:String;
  var AOutData:String; var AResult:Boolean);
var
  BParams :TStringList;
begin

  BParams :=TStringList.Create;
  GetFormulaData(AData, BParams);
  if BParams.Count=3 then
  begin
       AOutData:=StringReplace(BParams.Strings[0],
       BParams.Strings[1],BParams.Strings[2],[rfIgnoreCase]);
  end
  else begin
    AResult:=False;
  end;
  BParams.Free;
end;

//Замена
procedure Formula_ReplaceAll(AParcer:TAssiFormulaParcer; Sender:Pointer; AData:String;
  var AOutData:String; var AResult:Boolean);
var
  BParams :TStringList;
begin
  BParams :=TStringList.Create;
  GetFormulaData(AData, BParams);
  if BParams.Count=3 then
  begin
       AOutData:=StringReplace(BParams.Strings[0],BParams.Strings[1],
       BParams.Strings[2],[rfIgnoreCase,rfReplaceAll]);
  end
  else begin
    AResult:=False;
  end;
  BParams.Free;
end;

//Test
procedure Formula_Test(AParcer:TAssiFormulaParcer; Sender:Pointer; AData:String;
  var AOutData:String; var AResult:Boolean);
begin
  AOutData:='1,1,1';
end;

{ Прочее }

procedure SetDefaultFormula(AParcer:TAssiFormulaParcer);
var
   i:integer;
begin
   AParcer.FormulaInitNew(11);
   i:=0;
   AParcer.FormulaInsert(i,'Equal',         @Formula_Equal);
   inc(i);//1
   AParcer.FormulaInsert(i,'If',            @Formula_if);
   inc(i);
   AParcer.FormulaInsert(i,'Concat',        @Formula_Concat);
   inc(i);
   AParcer.FormulaInsert(i,'Replace',       @Formula_Replace);
   inc(i);
   AParcer.FormulaInsert(i,'ReplaceAll',    @Formula_ReplaceAll);
   inc(i);
   AParcer.FormulaInsert(i,'ReplaceEqual',  @Formula_ReplaceEqual);
   inc(i);//6
   AParcer.FormulaInsert(i,'Sum',           @Formula_Sum);
   inc(i);
   AParcer.FormulaInsert(i,'Difference',    @Formula_Difference);
   inc(i);
   AParcer.FormulaInsert(i,'Product',       @Formula_Product);
   inc(i);
   AParcer.FormulaInsert(i,'Quotient',      @Formula_Quotient);
   inc(i);//10
   AParcer.FormulaInsert(i,'Yest',      @Formula_Test);
   inc(i);//10

   AParcer.FormulaSynonymAdd('Sum',         'math_s');
   AParcer.FormulaSynonymAdd('Difference',  'math_d');
   AParcer.FormulaSynonymAdd('Product',     'math_p');
   AParcer.FormulaSynonymAdd('Quotient',    'math_q');

   {
   AParcer.FormulaSynonymAdd('Sum',         'Сложить');
   AParcer.FormulaSynonymAdd('Difference',  'Вычесть');
   AParcer.FormulaSynonymAdd('Product',     'Умножить');
   AParcer.FormulaSynonymAdd('Quotient',    'Разделить');

   AParcer.FormulaSynonymAdd('Sum',         'Сумма');
   AParcer.FormulaSynonymAdd('Difference',  'Разность');
   AParcer.FormulaSynonymAdd('Product',     'Произведение');
   AParcer.FormulaSynonymAdd('Quotient',    'Частное');

   AParcer.FormulaSynonymAdd('If',          'Если');
   AParcer.FormulaSynonymAdd('Concat',      'Сцепить');
   AParcer.FormulaSynonymAdd('Equal',       'Равно');
   }
end;

end.

