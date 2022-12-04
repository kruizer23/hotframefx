unit MainUnit;
//***************************************************************************************
//  Description: Program's main window.
//    File Name: mainunit.pas
//
//---------------------------------------------------------------------------------------
//                          C O P Y R I G H T
//---------------------------------------------------------------------------------------
//              Copyright 2022 (c) by Frank Voorburg   All rights reserved.
//
//   This software has been carefully tested, but is not guaranteed for any particular
// purpose. The author does not offer any warranties and does not guarantee the accuracy,
//   adequacy, or completeness of the software and is not responsible for any errors or
//              omissions or the results obtained from use of the software.
//
//---------------------------------------------------------------------------------------
//                            L I C E N S E
//---------------------------------------------------------------------------------------
// This file is part of HotFrameFx. HotFrameFx is free software: you can redistribute it
// and/or modify it under the terms of the GNU General Public License as published by the
// Free Software Foundation, either version 3 of the License, or (at your option) any
// later version.
//
// HotFrameFx is distributed in the hope that it will be useful, but WITHOUT ANY
// WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
// PARTICULAR PURPOSE. See the GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along with this
// program.  If not, see <http://www.gnu.org/licenses/>.
//
//***************************************************************************************
{$mode objfpc}{$H+}

interface
//***************************************************************************************
// Global includes
//***************************************************************************************
uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, CornerEdge,
  Menus, ActnList, Buttons, KeyBindingUnit, AboutUnit, HotAction, AppSettings;

//***************************************************************************************
// Type Definitions
//***************************************************************************************
type

  { TMainForm }

  TMainForm = class(TForm)
    ActAbout: TAction;
    ActOpenFromTray: TAction;
    ActManual: TAction;
    ActPreferences: TAction;
    ActQuit: TAction;
    ActionList: TActionList;
    BtnKeyBinding: TButton;
    ImageList: TImageList;
    ImgScreen: TImage;
    MainMenu: TMainMenu;
    MnuTrayQuit: TMenuItem;
    MnuTrayOpen: TMenuItem;
    MnuTraySep: TMenuItem;
    MnuAbout: TMenuItem;
    MnuSep2: TMenuItem;
    MnuManual: TMenuItem;
    MnuHelp: TMenuItem;
    MnuQuit: TMenuItem;
    MnuSep1: TMenuItem;
    MnuPreferences: TMenuItem;
    MnuFile: TMenuItem;
    PnlScreen: TPanel;
    BtnTopLeft: TSpeedButton;
    TrayPopup: TPopupMenu;
    TrayIcon: TTrayIcon;
    procedure ActAboutExecute(Sender: TObject);
    procedure ActOpenFromTrayExecute(Sender: TObject);
    procedure ActQuitExecute(Sender: TObject);
    procedure BtnKeyBindingClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormWindowStateChange(Sender: TObject);
  private
    FAppSettings : TAppSetings;
    FCornerEdge: TCornerEdge;
    FQuitRequest: Boolean;
    FFirstTimeShow: Boolean;
    procedure OnHotCorner(Sender: TObject; Corner: TCorner);
    procedure OnHotEdge(Sender: TObject; Edge: TEdge);
    procedure DoAction(ActionStr: string);
  public

  end;

//***************************************************************************************
// Global data declarations
//***************************************************************************************
var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

//***************************************************************************************
// NAME:           FormCreate
// PARAMETER:      Sender Signal source.
// DESCRIPTION:    Called when the form is created.
//
//***************************************************************************************
procedure TMainForm.FormCreate(Sender: TObject);
begin
  // Initialize fields.
  FQuitRequest := False;
  FFirstTimeShow := True;
  // Construct and load the application settings.
  FAppSettings := TAppSetings.Create;
  FAppSettings.Load;
  // Construct and configure the hot corner and edge detection object.
  FCornerEdge := TCornerEdge.Create;
  FCornerEdge.OnHotCorner := @OnHotCorner;
  FCornerEdge.OnHotEdge := @OnHotEdge;
  FCornerEdge.Sensitivity := FAppSettings.Sensitivity;
end;

//***************************************************************************************
// NAME:           BtnKeyBindingClick
// PARAMETER:      Sender Signal source.
// DESCRIPTION:    Called when the button is clicked.
//
//***************************************************************************************
procedure TMainForm.BtnKeyBindingClick(Sender: TObject);
var
  KeyBindingForm: TKeyBindingForm;
begin
  KeyBindingForm := TKeyBindingForm.Create(Self);
  KeyBindingForm.KeyBinding := 'alt+ctrl+tab';
  if KeyBindingForm.ShowModal = mrOK then
  begin
    ShowMessage('Key: ' + KeyBindingForm.KeyBinding +
                ' App: ' + ExtractFileName(KeyBindingForm.AppName));
  end;
  KeyBindingForm.Free;
end;

//***************************************************************************************
// NAME:           FormCloseQuery
// PARAMETER:      Sender Signal source.
//                 CanClose Set to true if it's okay to actually close the form, set it
//                 to false otherwise.
// DESCRIPTION:    Called when the form is about to be closed.
//
//***************************************************************************************
procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  // Only actually close the window if the user specifically requested this.
  if FQuitRequest then
  begin
    // Okay to close the window, because the user wants to quit.
    CanClose := True
  end
  else
  begin
    // If they didn't explicitly request to quit, then they probably just clicked the 'X'
    // on the window. Ask them if they want to minimize to tray instead of closing the
    // window.
    if MessageDlg('Question', 'Minimize to the system tray?', mtConfirmation,
                  [mbYes, mbNo], 0) = mrYes then
    begin
      // Minimize to the system tray, instead of closing the window.
      CanClose := False;
      Application.Minimize;
    end
    // Okay to close the window.
    else
    begin
      CanClose := True;
    end;
  end;
end;

//***************************************************************************************
// NAME:           ActQuitExecute
// PARAMETER:      Sender Signal source.
// DESCRIPTION:    Exits the program.
//
//***************************************************************************************
procedure TMainForm.ActQuitExecute(Sender: TObject);
begin
  // Make sure to save the application settings.
  FAppSettings.Save;
  // Set flag to inidicate the request to quit the application.
  FQuitRequest := True;
  // Quit the application.
  Close;
end;

//***************************************************************************************
// NAME:           ActAboutExecute
// PARAMETER:      Sender Signal source.
// DESCRIPTION:    Display program's about dialog.
//
//**************************************************************************************
procedure TMainForm.ActAboutExecute(Sender: TObject);
var
  AboutDialog: TAboutDialog;
begin
  // create the dialog
  AboutDialog := TAboutDialog.Create(Self);
  // make sure it is centered
  AboutDialog.Position := poScreenCenter;
  // show it in the modal state. we are not interested in the result at this point
  AboutDialog.ShowModal;
  // release the dialog
  AboutDialog.Free;
end;

//***************************************************************************************
// NAME:           ActOpenFromTrayExecute
// PARAMETER:      Sender Signal source.
// DESCRIPTION:    Opens the window from the system tray.
//
//**************************************************************************************
procedure TMainForm.ActOpenFromTrayExecute(Sender: TObject);
begin
  WindowState := wsNormal;
  Show;
end;

//***************************************************************************************
// NAME:           FormDestroy
// PARAMETER:      Sender Signal source.
// DESCRIPTION:    Called when the form is destroyed.
//
//***************************************************************************************
procedure TMainForm.FormDestroy(Sender: TObject);
begin
  // Release the hot corner and edge detection object.
  FCornerEdge.Free;
  // Release the application settings object.
  FAppSettings.Free;
end;

//***************************************************************************************
// NAME:           FormShow
// PARAMETER:      Sender Signal source.
// DESCRIPTION:    Called when the form is shown.
//
//***************************************************************************************
procedure TMainForm.FormShow(Sender: TObject);
begin
  // First time that the form gets shown after its creation?
  if FFirstTimeShow then
  begin
    // Reset flag to make sure the follow part only runs once after startup.
    FFirstTimeShow := False;
    // If this is not the first time ever that this application gets strarted, then
    // automatically minimize to send it to the system tray.
    if not FAppSettings.FirstRun then
      Application.Minimize;
  end;
end;

//***************************************************************************************
// NAME:           FormWindowStateChange
// PARAMETER:      Sender Signal source.
// DESCRIPTION:    Called when the window state changes.
//
//***************************************************************************************
procedure TMainForm.FormWindowStateChange(Sender: TObject);
begin
  // Did the window just get minimized?
  if WindowState = wsMinimized then
    // Also hide the window to truly minimize to the system tray.
    Hide;
end;

//***************************************************************************************
// NAME:           OnHotCorner
// PARAMETER:      Sender Source of the event.
//                 Corner The hot corner that triggered the event.
// DESCRIPTION:    Event handler that gets called upon hot corner detection.
//
//***************************************************************************************
procedure TMainForm.OnHotCorner(Sender: TObject; Corner: TCorner);
begin
  // Process based on the specific corner.
  case Corner of
    coTopLeft:     DoAction(FAppSettings.ActionTopLeft);
    coTopRight:    DoAction(FAppSettings.ActionTopRight);
    coBottomLeft:  DoAction(FAppSettings.ActionBottomLeft);
    coBottomRight: DoAction(FAppSettings.ActionBottomRight);
  end;
end;

//***************************************************************************************
// NAME:           OnHotEdge
// PARAMETER:      Sender Source of the event.
//                 Edge The hot edge that triggered the event.
// DESCRIPTION:    Event handler that gets called upon hot edge detection.
//
//***************************************************************************************
procedure TMainForm.OnHotEdge(Sender: TObject; Edge: TEdge);
begin
  // Process based on the specific edge.
  case Edge of
    edTop:    DoAction(FAppSettings.ActionTop);
    edBottom: DoAction(FAppSettings.ActionBottom);
    edLeft:   DoAction(FAppSettings.ActionLeft);
    edRight:  DoAction(FAppSettings.ActionRight);
  end;
end;

//***************************************************************************************
// NAME:           DoAction
// PARAMETERS:     ActionStr String representation of the action to perform.
// DESCRIPTION:    Executes the specified action.
//
//***************************************************************************************
procedure TMainForm.DoAction(ActionStr: string);
var
  HotAction: THotAction;
begin
  HotAction := THotAction.Create;
  HotAction.Text := ActionStr;
  HotAction.Execute;
  FreeAndNil(HotAction);
end;

end.
//********************************** end of mainunit.pas ********************************

