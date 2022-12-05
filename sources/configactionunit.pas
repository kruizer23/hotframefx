unit ConfigActionUnit;
//***************************************************************************************
//  Description: Action configuration form.
//    File Name: configactionunit.pas
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

  { TConfigActionForm }

  TConfigActionForm = class(TForm)
    BtnCancel: TButton;
    BtnOk: TButton;
    FrmGrabKey: TGrabKeyFrame;
    GrbFrames: TGroupBox;
    LblActionType: TLabel;
    FrmPickApp: TPickAppFrame;
    PnlBottom: TPanel;
    PnlRadioButtons: TPanel;
    PnlTop: TPanel;
    RdbLaunchApp: TRadioButton;
    RdbSendKeys: TRadioButton;
    procedure FormCreate(Sender: TObject);
    procedure RdbLaunchAppChange(Sender: TObject);
    procedure RdbSendKeysChange(Sender: TObject);
  private
    function GetActionText: string;
    procedure SetActionText(AValue: string);

  public
    property ActionText: string read GetActionText write SetActionText;
  end;

implementation

{$R *.lfm}

{ TConfigActionForm }

//***************************************************************************************
// NAME:           RdbSendKeysChange
// PARAMETER:      Sender Signal source.
// DESCRIPTION:    Called when the state of the radio button changes.
//
//***************************************************************************************
procedure TConfigActionForm.RdbSendKeysChange(Sender: TObject);
begin
  // Is the radio button now checked?
  if RdbSendKeys.Checked then
  begin
    // Make sure the right frame is shown.
    FrmGrabKey.Visible := True;
    FrmPickApp.Visible := False;
  end;
end;

//***************************************************************************************
// NAME:           GetActionText
// RETURN VALUE:   String representation of the action.
// DESCRIPTION:    Getter for the string representation of the action.
//
//***************************************************************************************
function TConfigActionForm.GetActionText: string;
begin
  // Is the frame to selecting a key binding visible?
  if FrmGrabKey.Visible then
  begin
    Result := FrmGrabKey.KeyBinding;
  end
  // Frame for selecting an application is visible.
  else
  begin
    Result := FrmPickApp.FileName;
  end;
end;

//***************************************************************************************
// NAME:           SetActionText
// PARAMETER:      AValue String representation of the action.
// DESCRIPTION:    Setter for the string representation of the action.
//
//***************************************************************************************
procedure TConfigActionForm.SetActionText(AValue: string);
begin
  // Empty action text?
  if AValue = '' then
  begin
    FrmPickApp.FileName := '';
    FrmGrabKey.KeyBinding := '';
    RdbSendKeys.Checked := True;
    RdbLaunchApp.Checked := False;
  end
  // Not an empty action text.
  else
  begin
    // If the action text is an existing filename, then it much be of the type to launch
    // an application.
    if FileExists(AValue) then
    begin
      FrmPickApp.FileName := AValue;
      FrmPickApp.InitialDir := ExtractFileDir(AValue);
      FrmGrabKey.KeyBinding := '';
      RdbLaunchApp.Checked := True;
      RdbSendKeys.Checked := False;
    end
    // Not an existing filename. It is probably a key binding.
    else
    begin
      FrmGrabKey.KeyBinding := AValue;
      FrmPickApp.FileName := '';
      RdbSendKeys.Checked := True;
      RdbLaunchApp.Checked := False;
    end;
  end;
end;

//***************************************************************************************
// NAME:           RdbLaunchAppChange
// PARAMETER:      Sender Signal source.
// DESCRIPTION:    Called when the state of the radio button changes.
//
//***************************************************************************************
procedure TConfigActionForm.RdbLaunchAppChange(Sender: TObject);
begin
  // Is the radio button now checked?
  if RdbLaunchApp.Checked then
  begin
    // Make sure the right frame is shown.
    FrmPickApp.Visible := True;
    FrmGrabKey.Visible := False;
  end;
end;

//***************************************************************************************
// NAME:           FormCreate
// PARAMETER:      Sender Signal source.
// DESCRIPTION:    Called when the form is created.
//
//***************************************************************************************
procedure TConfigActionForm.FormCreate(Sender: TObject);
begin
  // Modify the alignment of the frames. In design mode both frames are showns for
  // convenience.
  FrmGrabKey.Align := alClient;
  FrmPickApp.Align := alClient;
  // Resize the form to the best fit.
  AutoSize := True;
end;

end.
//********************************** end of configactionunit.pas ************************


