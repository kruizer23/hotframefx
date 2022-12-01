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
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, CornerEdge,
  MouseAndKeyInput, LCLType, ExtCtrls, Menus, ActnList, Buttons, KeyBindingUnit,
  AboutUnit;

//***************************************************************************************
// Type Definitions
//***************************************************************************************
type

  { TMainForm }

  TMainForm = class(TForm)
    ActAbout: TAction;
    ActManual: TAction;
    ActPreferences: TAction;
    ActQuit: TAction;
    ActionList: TActionList;
    BtnKeyBinding: TButton;
    ImageList: TImageList;
    ImgScreen: TImage;
    MainMenu: TMainMenu;
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
    procedure ActAboutExecute(Sender: TObject);
    procedure ActQuitExecute(Sender: TObject);
    procedure BtnKeyBindingClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FCornerEdge: TCornerEdge;
    procedure OnHotCorner(Sender: TObject; Corner: TCorner);
    procedure OnHotEdge(Sender: TObject; Edge: TEdge);
    procedure ShowTaskView;
    procedure ShowAppsView;
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
// DESCRIPTION:    Called when the form is created
//
//***************************************************************************************
procedure TMainForm.FormCreate(Sender: TObject);
begin
  // Construct and configure the hot corner and edge detection object.
  FCornerEdge := TCornerEdge.Create;
  FCornerEdge.OnHotCorner := @OnHotCorner;
  FCornerEdge.OnHotEdge := @OnHotEdge;
  FCornerEdge.Sensitivity := seHigh;
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
  KeyBindingForm.KeyBinding.KeyBindingStr := 'alt+ctrl+tab';
  if KeyBindingForm.ShowModal = mrOK then
  begin
    // TODO Process the new key binding from KeyBindingForm.KeyBinding.KeyBindingStr.
  end;
  KeyBindingForm.Free;
end;

//***************************************************************************************
// NAME:           ActQuitExecute
// PARAMETER:      Sender Signal source.
// DESCRIPTION:    Exits the program.
//
//***************************************************************************************
procedure TMainForm.ActQuitExecute(Sender: TObject);
begin
  // Quit the application.
  Close;
end;

//***************************************************************************************
// NAME:           ActAboutExecute
// PARAMETER:      Sender Signal source.
// RETURN VALUE:   None.
// DESCRIPTION:    Display program's about dialog.
//
//***************************************************************************************
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
// NAME:           FormDestroy
// PARAMETER:      Sender Signal source.
// DESCRIPTION:    Called when the form is destroyed
//
//***************************************************************************************
procedure TMainForm.FormDestroy(Sender: TObject);
begin
  // Release the hot corner and edge detection object.
  FCornerEdge.Free;
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
  if Corner = coTopLeft then
  begin
    ShowTaskView;
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
  if Edge = edBottom then
  begin
    ShowAppsView;
  end;
end;

//***************************************************************************************
// NAME:           ShowTaskView
// DESCRIPTION:    Simulates pressing the LWIN + TAB key, which is the keyboard shortcut
//                 in Windows for showing the task view.
//
//***************************************************************************************
procedure TMainForm.ShowTaskView;
begin
  // Press the left Super key, followed by the TAB key.
  KeyInput.Down(VK_LWIN);
  KeyInput.Down(VK_TAB);
  // Let go of the keys in reverse order.
  KeyInput.Up(VK_TAB);
  KeyInput.Up(VK_LWIN);
end;

//***************************************************************************************
// NAME:           ShowAppsView
// DESCRIPTION:    Simulates pressing the ALT + CTRL + TAB key, which is the keyboard
//                 shortcut in Windows for showing the open applications.
//
//***************************************************************************************
procedure TMainForm.ShowAppsView;
begin
  // Press the ALT and CTRL keys, followed by the TAB key.
  KeyInput.Down(VK_MENU);
  KeyInput.Down(VK_CONTROL);
  KeyInput.Down(VK_TAB);
  // Let go of the keys in reverse order.
  KeyInput.Up(VK_TAB);
  KeyInput.Up(VK_CONTROL);
  KeyInput.Up(VK_MENU);
end;

end.
//********************************** end of mainunit.pas ********************************

