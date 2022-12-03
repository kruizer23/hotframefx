unit KeyBindingUnit;
//***************************************************************************************
//  Description: Key binding detection dialog.
//    File Name: keybindingunit.pas
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
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  GrabKey, PickApp;

//***************************************************************************************
// Type Definitions
//***************************************************************************************
type

{ TKeyBindingForm }

  TKeyBindingForm = class(TForm)
    BtnCancel: TButton;
    BtnOk: TButton;
    FrmGrabKey: TGrabKeyFrame;
    GrbKey: TGroupBox;
    GrbApp: TGroupBox;
    FrmPickApp: TPickAppFrame;
    PnlButtons: TPanel;
  private
    function GetAppName: string;
    function GetKeyBinding: string;
    procedure SetAppName(AValue: string);
    procedure SetKeyBinding(AValue: string);
  public
    property KeyBinding: string read GetKeyBinding write SetKeyBinding;
    property AppName: string read GetAppName write SetAppName;
  end;


implementation

{$R *.lfm}

{ TKeyBindingForm }

//***************************************************************************************
// NAME:           GetAppName
// RETURN VALUE:   Application name string.
// DESCRIPTION:    Getter for the application.
//
//***************************************************************************************
function TKeyBindingForm.GetAppName: string;
begin
  Result := FrmPickApp.FileName;
end;

//***************************************************************************************
// NAME:           GetKeyBinding
// RETURN VALUE:   Key binding string.
// DESCRIPTION:    Getter for the string representation of the key binding.
//
//***************************************************************************************
function TKeyBindingForm.GetKeyBinding: string;
begin
  // Obtain the key string from the frame.
  Result := FrmGrabKey.KeyBinding;
end;

//***************************************************************************************
// NAME:           SetAppName
// PARAMETER:      Application name string.
// DESCRIPTION:    Setter for the application name.
//
//***************************************************************************************
procedure TKeyBindingForm.SetAppName(AValue: string);
begin
  FrmPickApp.FileName := AValue;
end;

//***************************************************************************************
// NAME:           SetKeyBinding
// PARAMETER:      Key binding string.
// DESCRIPTION:    Setter for the key binding in its string representation.
//
//***************************************************************************************
procedure TKeyBindingForm.SetKeyBinding(AValue: string);
begin
  // Update the frame.
  FrmGrabKey.KeyBinding := AValue;
end;

end.
//********************************** end of keybindingunit.pas **************************


