unit BusServiceData;

interface

uses Classes, Generics.Collections;

type
  TStringProc = reference to procedure(const S: string);
  TGUIDProc = reference to procedure(const AGUID: TGUID);
  TBusServiceData = class
  public type
   TWeekSection = (wsWorkDay, wsSaturday, wsSunday);
   TWeekSections = set of TWeekSection;
   TWeekDay = (wdMonday, wdTuesday, wdWednesday, wdThursday, wdFriday, wdSaturday, wdSunday);
   TWeekDays = set of TWeekDay;
   TServiceProc = reference to procedure(const ID: TGUID; const AServiceNumber: string; AWeekDays: TWeekDays);
  {$REGION 'Internals'}
  strict private
   class function GetWeekDaySection(const AWeekDay: TWeekDay): TWeekSection;
   class function GetWeekDaysSections(const AWeekDays: TWeekDays): TWeekSections;
  strict private type
   TItem  = class
     ID: TGUID;
     OperatorName: string;
     ServiceNumber: string;
     WeekDays: TWeekDays;
     function GetWeekSections: TWeekSections;
   end;
   TItemsByID = TDictionary<string, TItem>;
   TItems = TList<TItem>;
  strict private const
   WeekSectionDays: array[TWeekSection] of TWeekDays = ([wdMonday, wdTuesday, wdWednesday, wdThursday, wdFriday], [wdSaturday], [wdSunday]);
  strict private
   FItems: TItems;
   FItemsByID: TItemsByID;
   FOnLog: TStringProc;
  strict private
   function ParseCSVLine(const ALine: string; out AItem: TItem): boolean;
   function ParseWeekDays(const AWeekDaysString: string; out AWeekDays: TWeekDays): boolean; //assuming positions in AWeekDaysString based on Ord(TWeekDay)
   procedure Log(const ALine: string);
  public
   destructor Destroy;override;
  {$ENDREGION}
  public
   constructor LoadFromStreamReader(const AReader: TStreamReader; AOnError: TStringProc; AOnLog: TStringProc);
  public
    class function OrdToWeekSection(const AWeekSectionOrd: Integer): TWeekSection;
  public
    //these are all not yet optimised, but can easily be by caching connections in the constructor.
    //the only future looking thing done here, was to idemtify individual services by an ID, on which the indexes can be based.
    function GetWeekSections: TWeekSections;
    procedure GetOperatorNames(const AWeekSection: TWeekSection; AAddProc: TStringProc);
    procedure GetServiceIDs(const AWeekSection: TWeekSection; const AOperatorName: string; AAddProc: TGUIDProc);
    procedure GetServices(const AWeekSection: TWeekSection; const AOperatorName: string; AAddProc: TServiceProc);
    function GetService(const AID: TGUID; AAddProc: TServiceProc): boolean;
  end;


implementation

uses SysUtils, BusServiceDataTreeViewConsts;

{ TBusServiceData }

destructor TBusServiceData.Destroy;
begin
  FreeAndNil(FItemsByID);
  FreeAndNil(FItems);
  inherited
end;

procedure TBusServiceData.GetOperatorNames(const AWeekSection: TWeekSection; AAddProc: TStringProc);
begin
  with TStringList.Create do try
    Sorted := TRUE;
    for var aItem in FItems do
      if IndexOf(aItem.OperatorName) = -1 then
        if aWeekSection in aItem.GetWeekSections then begin
          AAddProc(aItem.OperatorName);
          Add(aItem.OperatorName)
        end
  finally
    Free
  end
end;

function TBusServiceData.GetService(const AID: TGUID; AAddProc: TServiceProc): boolean;
begin
  var aItem: TItem;
  result := FItemsByID.TryGetValue(GUIDToString(AID), aItem);
  if result then
    AAddProc(aItem.ID, aItem.ServiceNumber, aItem.WeekDays)

end;

procedure TBusServiceData.GetServiceIDs(const AWeekSection: TWeekSection; const AOperatorName: string; AAddProc: TGUIDProc);
begin
  for var aItem in FItems do
    if CompareText(aItem.OperatorName, AOperatorName) = 0 then
      if aWeekSection in aItem.GetWeekSections then
        AAddProc(aItem.ID)
end;

procedure TBusServiceData.GetServices(const AWeekSection: TWeekSection; const AOperatorName: string; AAddProc: TServiceProc);
begin
  for var aItem in FItems do
    if CompareText(aItem.OperatorName, AOperatorName) = 0 then
      if aWeekSection in aItem.GetWeekSections then
        AAddProc(aItem.ID, aItem.ServiceNumber, aItem.WeekDays)

end;

class function TBusServiceData.GetWeekDaySection(const AWeekDay: TWeekDay): TWeekSection;
begin
  for var aWeekSection in [Low(TWeekSection)..High(TWeekSection)] do
    if AWeekDay in WeekSectionDays[aWeekSection] then
        Exit(aWeekSection);
  raise EInvalidOpException.CreateFmt('%s in GetWeekDaySection : Ord(AWeekDay) = %d', [INTERNAL_ERROR, ord(AWeekDay)]);

end;

class function TBusServiceData.GetWeekDaysSections(const AWeekDays: TWeekDays): TWeekSections;
begin
  result := [];
  for var aWeekDay in AWeekDays do begin
    var aWeekDaySection := GetWeekDaySection(aWeekDay);
    Include(result, aWeekDaySection);
  end
end;

function TBusServiceData.GetWeekSections: TWeekSections;
begin
  result := [];
  for var aItem in FItems do
     result := result + aItem.GetWeekSections
end;

constructor TBusServiceData.LoadFromStreamReader;
begin
  inherited Create;
  FOnLog := AOnLog;
  with AReader do begin
    Rewind;
    FItems := TItems.Create;
    FItemsByID := TItemsByID.Create;

    while not EndOfStream do begin
      var aLine := ReadLine;
      {$IFDEF LOG_ALL}Log(aLine); {$ENDIF}
      var aItem := TItem.Create;
      try
        if ParseCSVLine(aLine, aItem) then begin
          FItems.Add(aItem);
          FItemsByID.Add(GUIDToString(aItem.ID), aItem);
          aItem := nil
        end else begin
          var aError := Format(ERROR_PARSING_CSV_LINE_S, [aLine]);
          AOnError(aError);
          Log(aError)
        end
      finally
        aItem.Free
      end;
    end
  end
end;

procedure TBusServiceData.Log(const ALine: string);
begin
  if Assigned(FOnLog) then
    FOnLog(ALine)
end;



class function TBusServiceData.OrdToWeekSection(const AWeekSectionOrd: Integer): TWeekSection;
begin
  result := TWeekSection(AWeekSectionOrd)
end;

function TBusServiceData.ParseCSVLine(const ALine: string; out AItem: TItem): boolean;
  function UnQuote(const AText: string): string;
  begin
    if (Length(AText) > 1) and (AText[1] = AText[High(AText)]) then
      if AText[1] in ['"', '''']  then
        Exit( Copy(AText, 2, AText.Length-2) );
    result := AText
  end;

begin
  with TStringList.Create do try
    StrictDelimiter := TRUE;
    CommaText := ALine;
    result := FALSE;
    if Count > 0 then
      AItem.OperatorName := UnQuote(Strings[0]);
    if Count > 1 then
      AItem.ServiceNumber := UnQuote(Strings[1]);
    var aWeekDays: string;
    if Count > 2 then begin
      aWeekDays := UnQuote(Strings[2]);
      result := ParseWeekDays(aWeekDays, AItem.WeekDays);
    end;
    if not result then
      Log(Format('OperatorName:%s, ServiceNumber:%s, WeekDays:%s', [AItem.OperatorName, AItem.ServiceNumber, aWeekDays]));
    if result then
      AItem.ID := TGUID.NewGuid
  finally
    Free
  end
end;

function TBusServiceData.ParseWeekDays(const AWeekDaysString: string;
  out AWeekDays: TWeekDays): boolean;
begin
  result := High(AWeekDaysString) = Ord(High(TWeekDay))+1;
  if result then begin
    for var d := Low(TWeekDay) to High(TWeekDay) do
      if AWeekDaysString[Ord(d)+1] = '1' then
        Include(AWeekDays, d)
      else
      if AWeekDaysString[Ord(d)+1] = '0' then
      else
        Exit(FALSE)
  end
end;

{ TBusServiceData.TItem }

function TBusServiceData.TItem.GetWeekSections: TWeekSections;
begin
  result := TBusServiceData.GetWeekDaysSections(WeekDays)
end;

end.
