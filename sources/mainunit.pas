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
// This file is part of TheWhiteSheet. TheWhiteSheet is free software: you can
// redistribute it and/or modify it under the terms of the GNU General Public License as
// published by the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// TheWhiteSheet is distributed in the hope that it will be useful, but WITHOUT ANY
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
  MouseAndKeyInput, LCLType, KeyBindingUnit;

//***************************************************************************************
// Type Definitions
//***************************************************************************************
type

  { TMainForm }

  TMainForm = class(TForm)
    BtnKeyBinding: TButton;
    MmoEventInfo: TMemo;
    procedure BtnKeyBindingClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FCornerEdge: TCornerEdge;
    procedure OnHotCorner(Sender: TObject; Corner: TCorner);
    procedure OnHotEdge(Sender: TObject; Edge: TEdge);
    procedure ShowTaskView;
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
    MmoEventInfo.Lines.Add('Key binding dialog mrOK: ' +
                           KeyBindingForm.KeyBinding.KeyBindingStr);
  end;
  KeyBindingForm.Free;
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
  case Corner of
    coTopLeft:     MmoEventInfo.Lines.Add('Top left hot corner');
    coTopRight:    MmoEventInfo.Lines.Add('Top right hot corner');
    coBottomLeft:  MmoEventInfo.Lines.Add('Bottom left hot corner');
    coBottomRight: MmoEventInfo.Lines.Add('Bottom right hot corner');
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
  case Edge of
    edLeft:   MmoEventInfo.Lines.Add('Left hot edge');
    edRight:  MmoEventInfo.Lines.Add('Right hot edge');
    edTop:    MmoEventInfo.Lines.Add('Top hot edge');
    edBottom: MmoEventInfo.Lines.Add('Bottom hot edge');
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

end.
//********************************** end of mainunit.pas ********************************

