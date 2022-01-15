unit TestBusServiceData;

interface

uses
  DUnitX.TestFramework, BusServiceData, BusServiceDataTreeViewConsts;

const
  LINE1 = '"A2B Bus and Coach Limited","127","1101110"';
  LINE2 = '"A2B Bus and Coach Limited","128","1111101"';
  LINE3 = '"A2B Bus and Coach Limited","E02","1111100"';
  LINE4 = 'SomeCompany,SomeService,0000011';
  LINE5 = 'SomeCompany,SomeService,000001';
  LINE6 = 'SomeCompany,SomeService';
  LINE7 = '???';
type
  //obviously this is only a beginning of the full testing coverage - all the boundary conditions need to be identified and that takes time ..
  [TestFixture]
  TTestBusServiceData = class
  private
    function GetInput(const ALines: string): string;
    function GetError(const ALines: string): string;
    function GetWeekSections(const ALines: string): TBusServiceData.TWeekSections;
    function GetOperatorNames(const ALines: string; AWeekSection: TBusServiceData.TWeekSection ): string;
    function GetA2BServices(const ALines: string; AWeekSection: TBusServiceData.TWeekSection ): string;
  public
    [TestCase('Test123','123')]
    [TestCase('Test234','234')]
    [TestCase('Test567','567')]
    [TestCase('Test1234567','1234567 ')]
    procedure TestError(const ALines: string);
    [TestCase('Test123','123')]
    [TestCase('Test34','34')]
    procedure Test(const ALines: string);
  end;

implementation

uses Classes, SysUtils;

{ TTestBusServiceData }

function TTestBusServiceData.GetA2BServices(const ALines: string; AWeekSection: TBusServiceData.TWeekSection): string;
begin
  result := '';
  for var i := Low(ALines) to High(ALines) do
    case ALines[i] of
    '1':  if AWeekSection in [wsWorkday, wsSaturday] then result := result + '127'#13#10;
    '2':  if AWeekSection in [wsWorkday, wsSunday] then result := result + '128'#13#10;
    '3':  if AWeekSection in [wsWorkday] then result := result + 'E02'#13#10;
    end
end;

function TTestBusServiceData.GetError(const ALines: string): string;
begin
  result := '';
  for var i := Low(ALines) to High(ALines) do
    case ALines[i] of
    '5': result := result + Format(ERROR_PARSING_CSV_LINE_S, [LINE5]) + #13#10;
    '6': result := result + Format(ERROR_PARSING_CSV_LINE_S, [LINE6]) + #13#10;
    '7': result := result + Format(ERROR_PARSING_CSV_LINE_S, [LINE7]) + #13#10;
    end;
end;

function TTestBusServiceData.GetInput(const ALines: string): string;
begin
  result := '';
  for var i := Low(ALines) to High(ALines) do
    case ALines[i] of
    '1': result := result + LINE1 + #13#10;
    '2': result := result + LINE2 + #13#10;
    '3': result := result + LINE3 + #13#10;
    '4': result := result + LINE4 + #13#10;
    '5': result := result + LINE5 + #13#10;
    '6': result := result + LINE6 + #13#10;
    '7': result := result + LINE7 + #13#10;
    end;
end;

function TTestBusServiceData.GetOperatorNames(const ALines: string; AWeekSection: TBusServiceData.TWeekSection ): string;
begin
  var operatorNames := TStringList.Create;
  try
    for var i := Low(ALines) to High(ALines) do
      case ALines[i] of
      '1':  if AWeekSection in [wsWorkday, wsSaturday] then if operatorNames.IndexOf('A2B Bus and Coach Limited') = -1 then operatorNames.Add('A2B Bus and Coach Limited');
      '2':  if AWeekSection in [wsWorkday, wsSunday] then if operatorNames.IndexOf('A2B Bus and Coach Limited') = -1 then operatorNames.Add('A2B Bus and Coach Limited');
      '3':  if AWeekSection in [wsWorkday] then if operatorNames.IndexOf('A2B Bus and Coach Limited') = -1 then operatorNames.Add('A2B Bus and Coach Limited');
      '4':  if AWeekSection in [wsSaturday, wsSunday]  then if operatorNames.IndexOf('SomeCompany') = -1 then operatorNames.Add('SomeCompany');
      end;
      result := operatorNames.Text

  finally
    operatorNames.Free
  end;
end;

function TTestBusServiceData.GetWeekSections(const ALines: string): TBusServiceData.TWeekSections;
begin
  result := [];
  for var i := Low(ALines) to High(ALines) do
    case ALines[i] of
    '1': result := result + [wsWorkDay, wsSaturday] ;
    '2': result := result + [wsWorkDay, wsSunday] ;
    '3': result := result + [wsWorkDay] ;
    '4': result := result + [wsSaturday, wsSunday] ;
    end;
end;

procedure TTestBusServiceData.Test(const ALines: string);
begin
  var aText := GetInput(ALines);
  var aStream := TStringStream.Create(aText);
  try
    var aReader := TStreamReader.Create(aStream);
    try
      var aData := TBusServiceData.LoadFromStreamReader(aReader,
                                                        procedure(const AError: string)
                                                        begin
                                                        end,
                                                        procedure(const ALine: string)
                                                        begin
                                                        end);
      try
        var aExpectedWeekSections := GetWeekSections(ALines);
        var aWeekSections := aData.GetWeekSections;
        Assert.AreEqual<TBusServiceData.TWeekSections>(aExpectedWeekSections, aWeekSections);
        var aExpectedOperatorNames := GetOperatorNames(ALines, wsSaturday);
        var aOperatorNames := '';
        aData.GetOperatorNames(wsSaturday,
                               procedure(const AOperatorName: string)
                               begin
                                 aOperatorNames := aOperatorNames + AOperatorName + #13#10
                               end
                               );
        Assert.AreEqual(aExpectedOperatorNames, aOperatorNames);
        aExpectedOperatorNames := GetOperatorNames(ALines, wsSunday);
        aOperatorNames := '';
        aData.GetOperatorNames(wsSunday,
                               procedure(const AOperatorName: string)
                               begin
                                 aOperatorNames := aOperatorNames + AOperatorName + #13#10
                               end
                               );
        Assert.AreEqual(aExpectedOperatorNames, aOperatorNames);
        var aExpectedServices := GetA2bServices(ALines, wsSaturday);
        var aServices := '';
        aData.GetServices(wsSaturday, 'A2B Bus and Coach Limited',
                          procedure(const ID: TGUID; const AServiceNumber: string; AWeekDays: TBusServiceData.TWeekDays)
                          begin
                            aServices := aServices + AServiceNumber + #13#10
                          end);
        Assert.AreEqual(aExpectedServices, aServices);
        aExpectedServices := GetA2bServices(ALines, wsSunday);
        aServices := '';
        aData.GetServices(wsSunday, 'A2B Bus and Coach Limited',
                          procedure(const ID: TGUID; const AServiceNumber: string; AWeekDays: TBusServiceData.TWeekDays)
                          begin
                            aServices := aServices + AServiceNumber + #13#10
                          end);
        Assert.AreEqual(aExpectedServices, aServices);
        aExpectedServices := GetA2bServices(ALines, wsWorkday);
        aServices := '';
        aData.GetServices(wsWorkday, 'A2B Bus and Coach Limited',
                          procedure(const ID: TGUID; const AServiceNumber: string; AWeekDays: TBusServiceData.TWeekDays)
                          begin
                            aServices := aServices + AServiceNumber + #13#10
                          end);
        Assert.AreEqual(aExpectedServices, aServices);
      finally
        aData.Free
      end
    finally
      aReader.Free
    end
  finally
    aStream.Free
  end
end;

procedure TTestBusServiceData.TestError(const ALines: string);
begin
  var aText := GetInput(ALines);
  var aStream := TStringStream.Create(aText);
  try
    var aExpectedErrors := GetError(ALines);
    var aErrors: string := '';
    var aReader := TStreamReader.Create(aStream);
    try
      var aData := TBusServiceData.LoadFromStreamReader(aReader,
                                                        procedure(const AError: string)
                                                        begin
                                                          aErrors := aErrors + AError +#13#10
                                                        end,
                                                        procedure(const ALine: string)
                                                        begin

                                                        end);
      try
        aText := aErrors;
        Assert.AreEqual(AExpectedErrors, aErrors)
      finally
        aData.Free
      end
    finally
      aReader.Free
    end
  finally
    aStream.Free
  end
end;

initialization
  TDUnitX.RegisterTestFixture(TTestBusServiceData);

end.
