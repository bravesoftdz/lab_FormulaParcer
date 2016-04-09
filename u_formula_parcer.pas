unit u_formula_parcer;

{$mode objfpc}{$H+}

//************************************************************
 //
 //    Модуль u_formula_parcer
 //    Copyright (c) 2016  Pichugin M.
 //    This file is part of the Assi System.
 //
 //    Разработчик: Pichugin M. (e-mail: pichugin_m@mail.ru)
 //    (2016-02-27)
 //
//************************************************************

interface

uses
      Classes, SysUtils, LazUTF8;

type

      { Forward Declarartions }

      TAssiFormulaParcer = class;

      { Data types }

      TDoFormulaResult = procedure (AParcer:TAssiFormulaParcer; Sender:Pointer;
                                   AData:String; var AOutData:String;
                                   var AResult:Boolean);
      { TAssiFormulaParcer }

      TAssiFormulaParcer = class(TObject)
      private
        FFormulaDevLog       :TStringList;
        FArrayFormulaName    :Array of string;
        FArrayFormulaPointer :Array of Pointer;
        FArraySynonymNameOriginal    :Array of string;
        FArraySynonymNameAlternative :Array of string;
        function GetLog: String;
        procedure ParcePossibleFormula(AData:String;
                             var AOutName:String; var AOutData:String);
        //Создание стандартных формул
        procedure FormulaAddDefault();
        //Выполнение передаваемой формулы для указанного объекта и вывод результата
        function DoFormulaResult(Sender:Pointer; AData:String; var AOut:String):boolean;
        function GetFormulaIndex(AName:String):Integer;
      protected

      public
        property ExecutionLog:String read GetLog;
        //Выполнение передаваемой формулы для указанного объекта и вывод результата
        function Execute(Sender:Pointer; AData:String; var AOut:String):boolean;
        procedure FormulaSynonymAdd(AOriginalName, ASynonymName:String);
        //добавление пользовательской формулы
        procedure FormulaAdd(AName:String; AFunc:Pointer);
        procedure FormulaInitNew(Count:Integer);
        procedure FormulaInsert(Index:integer; AName:String; AFunc:Pointer);

        constructor Create;
        destructor Destroy; override;
      end;

const
      QUOTE1                       = '&quot;';
      QUOTE2                       = '\"';
      QUOTE3                       = '"';
      FORMULA_CHRPARAMSBEGIN       = '(';
      FORMULA_CHRPARAMSEND         = ')';
      FORMULA_CHRPARAMSSEPARETER   = ',';
      FORMULA_BAFFLE               = '\';
      FORMULA_RESULT_TRUE          = 'True';
      FORMULA_RESULT_FALSE         = 'False';

      //Разделение строки на отдельные параметры
      procedure GetFormulaData(AData:String; var AParams:TStringList);
      //Проверка, является ли текст числом
      function isCorrectNumber(AData:String):boolean;
      //Замена точек на запятые в тексте
      function GetCorrectNumber(AData:String):String;

implementation

uses
      u_formula_fstandard;

function isCorrectNumber(AData:String):boolean;
const
  CHRDOT1='.';
  CHRDOT2=',';
var
  LStr,
  i,
  CountA,
  CountB,
  CountC    :Integer;
  tmpChrs,
  tmpChr    :String;
begin
  LStr  :=UTF8Length(AData);
  Result:=(LStr>0);
  CountA:=0;
  CountB:=0;
  CountC:=0;
  tmpChrs:='0123456789-'+CHRDOT1+CHRDOT2;
  for i:=1 to LStr do
  begin
     tmpChr:=UTF8Copy(AData,i,1);
     if UTF8Pos(tmpChr,tmpChrs,1)=0 then
     begin
        Result:=False;
        break;
     end;

     if (UTF8Pos('-',AData,1)>1)
        or((UTF8Pos('-',AData,1)>0)and(CountA>1)) then
     begin
        Result:=False;
        break;
     end
     else if ((UTF8Pos('-',AData,1)>0)and(CountA=0)) then
     begin
        inc(CountA);
     end;

     if (UTF8Pos(CHRDOT1,AData,1)=1)
        or((UTF8Pos(CHRDOT1,AData,1)>0)and(CountB>1)) then
     begin
        Result:=False;
        break;
     end
     else if ((UTF8Pos(CHRDOT1,AData,1)>0)and(CountB=0)) then
     begin
        inc(CountB);
     end;

     if (UTF8Pos(CHRDOT2,AData,1)=1)
         or((UTF8Pos(CHRDOT2,AData,1)>0)and(CountC>1)) then
     begin
        Result:=False;
        break;
     end
     else if ((UTF8Pos(CHRDOT2,AData,1)>0)and(CountC=0)) then
     begin
        inc(CountC);
     end;
  end;
end;

function GetCorrectNumber(AData:String):String;
const
  CHRDOT1='.';
  CHRDOT2=',';
begin
  Result:=StringReplace(AData,CHRDOT1,CHRDOT2,[]);
end;

procedure GetFormulaData(AData:String; var AParams:TStringList);
var
  LStr,
  i,
  iA        :Integer;
  tmpStr,
  tmpChr    :String;
  QuoteOpen :Integer;
  BParams   :TStringList;
begin
  tmpStr:='';
  if AData<>'' then
  begin
       LStr      :=UTF8Length(AData);
       iA        :=UTF8Pos(FORMULA_CHRPARAMSBEGIN,AData,1);
       QuoteOpen :=0;
       tmpStr    :='';
       for i:=1 to LStr do
       begin
           tmpChr:=UTF8Copy(AData,i,1);
           if (UTF8CompareStr(tmpChr,FORMULA_CHRPARAMSBEGIN)=0) then
           begin
              inc(QuoteOpen);
           end;
           if (UTF8CompareStr(tmpChr,FORMULA_CHRPARAMSEND)=0)
              and(QuoteOpen>0) then
           begin
              dec(QuoteOpen);
           end;
           if (QuoteOpen>0)
              or(UTF8CompareStr(tmpChr,FORMULA_CHRPARAMSSEPARETER)<>0) then
           begin
              tmpStr    :=tmpStr+UTF8Copy(AData,i,1);
           end
           else if (QuoteOpen=0)
                and(UTF8CompareStr(tmpChr,FORMULA_CHRPARAMSSEPARETER)=0) then
           begin
              AParams.Add(tmpStr);
              tmpStr    :='';
           end;
       end;
       if tmpStr<>'' then
       begin
          AParams.Add(tmpStr);
          tmpStr    :='';
       end;
  end;
end;

{ Проверки }

function isCorrectFormulaName(AData:String):boolean;
var
  LStr,
  i         :Integer;
  tmpChr    :String;
begin
  Result:=True;
  if AData<>'' then
  begin
       LStr  :=UTF8Length(AData);
       for i:=1 to LStr do
       begin
           tmpChr:=UTF8Copy(AData,i,1);
           if UTF8Pos(tmpChr,' ~`*!@#$%^&?><][}{;:|\/)(-+',1)>0 then
           begin
              Result:=False;
              break;
           end;
       end;
  end;
end;

{ TAssiFormulaParcer }

procedure TAssiFormulaParcer.ParcePossibleFormula(AData:String;
                             var AOutName:String; var AOutData:String);
var
  i,y   :Integer;
  tmpStr:String;
begin
  if AData<>'' then
  begin
       i         :=UTF8Pos(FORMULA_CHRPARAMSBEGIN,AData,1);
       y         :=UTF8Length(AData);
       if (i>2)and(UTF8Copy(AData,1,i-1)<>FORMULA_BAFFLE)
       and(UTF8Copy(AData,y,1)=FORMULA_CHRPARAMSEND) then
       begin
          tmpStr    :=UTF8Copy(AData,1,i-1);
          AOutName  :=tmpStr;
          y         :=y-UTF8Length(tmpStr);
          tmpStr    :=UTF8Copy(AData,i+1,y-2);
          AOutData  :=tmpStr;
       end
       else if (UTF8Copy(AData,1,1)=FORMULA_CHRPARAMSBEGIN)
       and(UTF8Copy(AData,y,1)=FORMULA_CHRPARAMSEND) then
       begin
          tmpStr    :=UTF8Copy(AData,2,y-1);
          AOutData  :=tmpStr;
       end
       else begin
          AOutData  :=AData;
       end;
  end;
end;

function TAssiFormulaParcer.GetLog: String;
begin
  Result:=Self.FFormulaDevLog.Text;
end;

function TAssiFormulaParcer.DoFormulaResult(Sender:Pointer; AData:String; var AOut:String):boolean;
var
  dfrProcResult :Boolean;
  dfrProc       :TDoFormulaResult;
  BParams       :TStringList;
  i             :Integer;
  tmpStr,
  tmpData,
  tmpName,
  tmpOut,
  tmpOut2       :String;
begin
  Result  :=True;
  BParams :=TStringList.Create;
  GetFormulaData(AData, BParams);
  if BParams.Count=1 then
  begin
    tmpName :='';
    tmpData :='';
    ParcePossibleFormula(AData,tmpName,tmpData);
    if tmpName<>'' then
    begin
       Result :=isCorrectFormulaName(tmpName);
       if Result then
       begin
       //Есть формула, есть ее параметры
       BParams.Clear;
       tmpOut2:='';
       GetFormulaData(tmpData, BParams);

       for i:=0 to BParams.Count-1 do
       begin
           tmpOut :='';
           Result :=DoFormulaResult(Sender, BParams.Strings[i], tmpOut);
           if Result then
           begin
             if tmpOut2='' then
               tmpOut2:=tmpOut
             else
               tmpOut2:=tmpOut2+FORMULA_CHRPARAMSSEPARETER+tmpOut;
           end
           else begin
               break;
           end;
       end;
       if Result then
       begin
           tmpOut        :='';
           dfrProcResult :=True;
           i             :=GetFormulaIndex(tmpName);
           if i>-1 then
           begin
              dfrProc:=TDoFormulaResult(FArrayFormulaPointer[i]);
              dfrProc(Self, Sender, tmpOut2, tmpOut, dfrProcResult);
           end
           else begin
              dfrProcResult:=False;
           end;
           //for testing
           //tmpOut:=inttostr(round(random(100)));
           if Assigned(FFormulaDevLog)then
           FFormulaDevLog.Add(tmpOut+'='+tmpName+'|'+tmpOut2);

           if dfrProcResult then
           AOut  :=tmpOut;
           Result:=dfrProcResult;
       end;
       end;
    end
    else begin
       //Есть значение
       AOut   :=tmpData;
       Result :=True;
    end;
  end
  else begin
    //Ошибка формулы
    Result:=False;
  end;
  BParams.Free;
end;

function TAssiFormulaParcer.GetFormulaIndex(AName: String): Integer;
var
  i:integer;
begin
   Result:=-1;
   AName:=UTF8UpperCase(AName);
   for i:=0 to high(FArraySynonymNameAlternative) do
   begin
       if CompareText(FArraySynonymNameAlternative[i], AName)=0 then
       begin
          AName:=FArraySynonymNameOriginal[i];
          break;
       end;
   end;
   for i:=0 to high(FArrayFormulaName) do
   begin
       if CompareText(FArrayFormulaName[i], AName)=0 then
       begin
          Result:=i;
          break;
       end;
   end;
end;

function TAssiFormulaParcer.Execute(Sender:Pointer;
                             AData:String; var AOut:String):boolean;
begin
   FFormulaDevLog.Clear;
   Result:=DoFormulaResult(Sender,AData,AOut);
end;

procedure TAssiFormulaParcer.FormulaSynonymAdd(AOriginalName,ASynonymName:String);
var
   i:integer;
begin
   i:=Length(FArraySynonymNameOriginal);
   inc(i);
   SetLength(FArraySynonymNameOriginal,i);
   SetLength(FArraySynonymNameAlternative,i);
   dec(i);
   FArraySynonymNameOriginal[i]    :=UTF8UpperCase(AOriginalName);
   FArraySynonymNameAlternative[i] :=UTF8UpperCase(ASynonymName);
end;

procedure TAssiFormulaParcer.FormulaAddDefault;
begin
  SetDefaultFormula(Self);
end;

procedure TAssiFormulaParcer.FormulaAdd(AName:String; AFunc:Pointer);
var
   i:integer;
begin
   i:=Length(FArrayFormulaName);
   inc(i);
   SetLength(FArrayFormulaName,i);
   SetLength(FArrayFormulaPointer,i);
   dec(i);
   FArrayFormulaName[i]    :=UTF8UpperCase(AName);
   FArrayFormulaPointer[i] :=AFunc;
end;

procedure TAssiFormulaParcer.FormulaInitNew(Count: Integer);
var
   i:integer;
begin
   i:=Length(FArrayFormulaName);
   i:=i+Count;
   SetLength(FArrayFormulaName,i);
   SetLength(FArrayFormulaPointer,i);
end;

procedure TAssiFormulaParcer.FormulaInsert(Index: integer; AName: String;
  AFunc: Pointer);
begin
  FArrayFormulaName[Index]    :=UTF8UpperCase(AName);
  FArrayFormulaPointer[Index] :=AFunc;
end;

constructor TAssiFormulaParcer.Create;
begin
  inherited Create;
  SetLength(FArraySynonymNameOriginal,0);
  SetLength(FArraySynonymNameAlternative,0);
  FFormulaDevLog:=TStringList.Create;
  FormulaAddDefault();
end;

destructor TAssiFormulaParcer.Destroy;
begin
  FFormulaDevLog.Free;
  inherited Destroy;
end;

end.
