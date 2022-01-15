program BusServiceDataTreeView;

uses
  Vcl.Forms,
  BusServiceDataTreeViewMain in 'BusServiceDataTreeViewMain.pas' {BusServiceDataTreeViewMainForm},
  BusServiceData in 'BusServiceData.pas',
  BusServiceDataTreeViewConsts in 'BusServiceDataTreeViewConsts.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TBusServiceDataTreeViewMainForm, BusServiceDataTreeViewMainForm);
  Application.Run;
end.
