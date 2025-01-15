unit SettingsUnit;
//***************************************************************************************
//  Description: Application settings form.
//    File Name: settingsunit.pas
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
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  CornerEdge;

//***************************************************************************************
// Type Definitions
//***************************************************************************************
type

  { TSettingsForm }

  TSettingsForm = class(TForm)
    BtnCancel: TButton;
    BtnOk: TButton;
    ChbAutostart: TCheckBox;
    ChbDisableFullscreen: TCheckBox;
    ChbIgnoreWithMousePressed: TCheckBox;
    CmbSensitivity: TComboBox;
    LblFullscreen: TLabel;
    LblMouseButtons: TLabel;
    LblSensitivity: TLabel;
    LblAutostart: TLabel;
    PnlBottom: TPanel;
    procedure FormCreate(Sender: TObject);
  private
    function GetAutoStart: Boolean;
    function GetDisableInFullscreen: Boolean;
    function GetSensitivity: TSensitivity;
    function GetIgnoreWithMousePressed: Boolean;
    procedure SetAutoStart(AValue: Boolean);
    procedure SetDisableInFullscreen(AValue: Boolean);
    procedure SetSensitivity(AValue: TSensitivity);
    procedure SetIgnoreWithMousePressed(AValue: Boolean);
  public
    property Sensitivity: TSensitivity read GetSensitivity write SetSensitivity;
    property AutoStart: Boolean read GetAutoStart write SetAutoStart;
    property DisableInFullscreen: Boolean read GetDisableInFullscreen write SetDisableInFullscreen;
    property IgnoreWithMousePressed: Boolean read GetIgnoreWithMousePressed write SetIgnoreWithMousePressed;
  end;

implementation

{$R *.lfm}

//***************************************************************************************
// Type Definitions
//***************************************************************************************
type
  TSensitivityMap = record
    Text: string;
    Code: TSensitivity;
  end;


//***************************************************************************************
// Constant data declarations
//***************************************************************************************
const
  SensitivityMap: array[0..4] of TSensitivityMap =
  (
    (Text: 'Lowest'; Code: seLowest),
    (Text: 'Lower'; Code: seLower),
    (Text: 'Medium'; Code: seMedium),
    (Text: 'Higher'; Code: seHigher),
    (Text: 'Highest'; Code: seHighest)
  );

{ TSettingsForm }

//***************************************************************************************
// NAME:           FormCreate
// PARAMETER:      Sender Signal source.
// DESCRIPTION:    Called when the form is created.
//
//***************************************************************************************
procedure TSettingsForm.FormCreate(Sender: TObject);
var
  Idx: integer;
begin
  // Populate the items in the sensitivity combox box.
  for Idx := Low(SensitivityMap) to High(SensitivityMap) do
  begin
    CmbSensitivity.Items.Add(SensitivityMap[Idx].Text);
  end;
  CmbSensitivity.ItemIndex := 0;
end;

//***************************************************************************************
// NAME:           GetAutoStart
// RETURN VALUE    True if the application should autostart with the operating system,
//                 False otherwise.
// DESCRIPTION:    Getter for the autostart setting.
//
//***************************************************************************************
function TSettingsForm.GetAutoStart: Boolean;
begin
  Result := ChbAutostart.Checked;
end;

//***************************************************************************************
// NAME:           GetDisableInFullscreen
// RETURN VALUE    True if corner and frame actions should be disabled when another
//                 application runs in fullscreen mode, false otherwise.
// DESCRIPTION:    Getter for the disable-in-fullscreen setting.
//
//***************************************************************************************
function TSettingsForm.GetDisableInFullscreen: Boolean;
begin
  Result := ChbDisableFullscreen.Checked;
end;

//***************************************************************************************
// NAME:           GetSensitivity
// RETURN VALUE    Sensitivity setting for the hot corners / edges.
// DESCRIPTION:    Getter for the hot corners / edges sensitivity setting.
//
//***************************************************************************************
function TSettingsForm.GetSensitivity: TSensitivity;
begin
  Result := SensitivityMap[CmbSensitivity.ItemIndex].Code;
end;

//***************************************************************************************
// NAME:           GetIgnoreWithMousePressed
// RETURN VALUE    True if corner and frame event should be ignored when a mouse button
//                 is pressed, false otherwise.
// DESCRIPTION:    Getter for the ignore-with-mouse-button-pressed setting.
//
//***************************************************************************************
function TSettingsForm.GetIgnoreWithMousePressed: Boolean;
begin
  Result := ChbIgnoreWithMousePressed.Checked;
end;

//***************************************************************************************
// NAME:           SetAutoStart
// PARAMETER:      AValue True if the application should autostart with the operating
//                 system, False otherwise.
// DESCRIPTION:    Setter for the autostart setting.
//
//***************************************************************************************
procedure TSettingsForm.SetAutoStart(AValue: Boolean);
begin
  ChbAutostart.Checked := AValue;
end;

//***************************************************************************************
// NAME:           SetDisableInFullscreen
// PARAMETER:      AValue True if corner and frame actions should be disabled when
//                 another application runs in fullscreen mode, false otherwise.
// DESCRIPTION:    Setter for the disable-in-fullscreen setting.
//
//***************************************************************************************
procedure TSettingsForm.SetDisableInFullscreen(AValue: Boolean);
begin
  ChbDisableFullscreen.Checked := AValue;
end;

//***************************************************************************************
// NAME:           SetSensitivity
// RETURN VALUE    AValue Sensitivity setting for the hot corners / edges.
// DESCRIPTION:    Setter for the hot corners / edges sensitivity setting.
//
//***************************************************************************************
procedure TSettingsForm.SetSensitivity(AValue: TSensitivity);
var
  Idx: integer;
begin
  // Loop through sensitivity map to find the combobox item that belong to this one.
  for Idx := Low(SensitivityMap) to High(SensitivityMap) do
  begin
    // Is this sensitivity a match?
    if SensitivityMap[Idx].Code = AValue then
    begin
      // Select this one in the combo box and stop the loop.
      CmbSensitivity.ItemIndex := Idx;
      Break;
    end;
  end;
end;

//***************************************************************************************
// NAME:           SetIgnoreWithMousePressed
// PARAMETER:      AValue True if corner and frame events should be ignored when a mouse
//                 button is pressed, False otherwise.
// DESCRIPTION:    Setter for the ignore-with-mouse-button-pressed setting.
//
//***************************************************************************************
procedure TSettingsForm.SetIgnoreWithMousePressed(AValue: Boolean);
begin
  ChbIgnoreWithMousePressed.Checked := AValue;
end;

end.
//********************************** end of settingsunit.pas ****************************

