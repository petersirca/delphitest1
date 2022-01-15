unit BusServiceDataTreeViewMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, System.Actions, Vcl.ActnList,
  Vcl.PlatformDefaultStyleActnCtrls, Vcl.ActnMan, Vcl.StdCtrls, Vcl.ExtCtrls,
  Vcl.ComCtrls, Vcl.Menus, BusServiceData;

type
  TBusServiceDataTreeViewMainForm = class(TForm)
    actions: TActionManager;
    openInputFile: TAction;
    inputFileOpenDialog: TFileOpenDialog;
    MainMenu: TMainMenu;
    mainFileMenu: TMenuItem;
    fileOpenMenu: TMenuItem;
    treeView: TTreeView;
    logMemo: TMemo;
    procedure openInputFileExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure treeViewExpanding(Sender: TObject; Node: TTreeNode; var AllowExpansion: Boolean);
  private type
    ELoadError = class(Exception);
  private
    FData: TBusServiceData;
  private
    procedure Clear;
    function FormatServiceForDisplay(const AServiceNumber: string; const AWeekDays: TBusServiceData.TWeekDays): string;
    function FormatWeekDaysForDisplay(const AWeekDays: TBusServiceData.TWeekDays): string;
    function FormatWeekSectionForDisplay(const AWeekSection: TBusServiceData.TWeekSection): string;
    procedure LoadInputFile(const AFileName: string);
    procedure Log(const AMessage: string);
    procedure PopulateTree;
  public
  end;

var
  BusServiceDataTreeViewMainForm: TBusServiceDataTreeViewMainForm;

implementation

uses TypInfo, BusServiceDataTreeViewConsts;

{$R *.dfm}

procedure TBusServiceDataTreeViewMainForm.Clear;
begin
  with treeView.Items do begin
    BeginUpdate;
    try
      Clear
    finally
      EndUpdate
    end;
  end;
  FreeAndNil(FData)
end;

function TBusServiceDataTreeViewMainForm.FormatServiceForDisplay;
begin
  result := Format('%s: %s', [AServiceNumber, FormatWeekDaysForDisplay(AWeekDays)])
end;

function TBusServiceDataTreeViewMainForm.FormatWeekDaysForDisplay(const AWeekDays: TBusServiceData.TWeekDays): string;
const WeekDaysInitials: array[TBusServiceData.TWeekDay] of string = (MON_INITIAL, TUE_INITIAL, WED_INITIAL, THU_INITIAL, FRI_INIIAL, SAT_INITIAL, SUN_INITIAL);
begin
  result := '';
  for var wd := Low(TBusServiceData.TWeekDay) to High(TBusServiceData.TWeekDay) do
    if wd in AWeekDays then
      result := result + WeekDaysInitials[wd]
    else
      result := result + '-'

end;

function TBusServiceDataTreeViewMainForm.FormatWeekSectionForDisplay(const AWeekSection: TBusServiceData.TWeekSection): string;
const WeekDaySectionsDisplays: array[TBusServiceData.TWeekSection] of string = (MON_FRI_SECTION, SAT_SECTION, SUN_SECTION);
begin
  result := WeekDaySectionsDisplays[AWeekSection]
end;

procedure TBusServiceDataTreeViewMainForm.FormCreate(Sender: TObject);
begin
  inputFileOpenDialog.DefaultFolder :=  ExtractFilePath(Application.ExeName)
end;

procedure TBusServiceDataTreeViewMainForm.LoadInputFile(const AFileName: string);
begin
  Clear;
  Screen.Cursor := crHourGlass;
  try
    var aInputStream := TFileStream.Create(AFileName, fmOpenRead);
    try
      var aInputReader := TStreamReader.Create(aInputStream);
      try
        var aErrorCount := 0;
        FData := TBusServiceData.LoadFromStreamReader(aInputReader,
                                                      procedure(const AError: string)
                                                      begin
                                                        Inc(aErrorCount)
                                                      end,
                                                      procedure(const aLine: string)
                                                      begin
                                                        Log(aLine)
                                                      end);
        PopulateTree;
        if aErrorCount > 0 then
          raise ELoadError.CreateFmt(LOAD_ERRORS_FORMAT_STR_DS, [aErrorCount,AFileName]);
      finally
        aInputReader.Free
      end
    finally
      aInputStream.Free
    end
  finally
    screen.Cursor := crDefault
  end
end;

procedure TBusServiceDataTreeViewMainForm.Log(const AMessage: string);
begin
  logMemo.Lines.Add(AMessage);
  logMemo.Show
end;

procedure TBusServiceDataTreeViewMainForm.openInputFileExecute(Sender: TObject);
begin
  with inputFileOpenDialog do
    if Execute then try
      LoadInputFile(FileName);
      Caption := Format(FORM_CAPTION_FORMAT_STR_SS, [ ChangeFileExt(ExtractFileName(Application.ExeName), ''), FileName]);
    except
      on E: Exception do
        Caption := Format(LOAD_EXCEPTION_FORMAT_STR_SSS, [E.ClassName, E.Message, FileName])
    end

end;

procedure TBusServiceDataTreeViewMainForm.PopulateTree;
begin
  if not Assigned(FData) then
    raise EInvalidOpException.Create(INTERNAL_ERROR);

  with treeView.Items do begin
    BeginUpdate;
    try
      Clear;
      for var aWeekSection in FData.GetWeekSections do begin
        var aNode := AddChildObject(nil, FormatWeekSectionForDisplay(aWeekSection), Pointer(NativeInt(Ord(aWeekSection))));
        AddChild(aNode, '')
      end
    finally
      EndUpdate
    end;
  end;
end;

procedure TBusServiceDataTreeViewMainForm.treeViewExpanding(Sender: TObject; Node: TTreeNode; var AllowExpansion: Boolean);
begin
  with treeView.Items do begin
    BeginUpdate;
    try
      Node.DeleteChildren;

      with FData do
        case Node.Level of
        0: GetOperatorNames(OrdToWeekSection(NativeInt(Node.Data)),
                            procedure (const AOperatorName: string)
                            begin
                              var aNode := AddChild(Node, AOperatorName);
                              AddChild(aNode, '')
                            end);
        1: GetServices(OrdToWeekSection(NativeInt(Node.Parent.Data)), Node.Text,
                       procedure(const AID: TGUID; const AServiceNumber: string; AWeekDays: TBusServiceData.TWeekDays)
                       begin
                         AddChild(Node, FormatServiceForDisplay(AServiceNumber, AWeekDays))
                       end);

        end
    finally
      EndUpdate
    end
  end
end;

end.
