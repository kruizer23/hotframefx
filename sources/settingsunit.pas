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
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, CornerEdge;

//***************************************************************************************
// Type Definitions
//***************************************************************************************
type

  { TSettingsForm }

  TSettingsForm = class(TForm)
  private
    function GetAutoStart: Boolean;
    function GetSensitivity: TSensitivity;
    procedure SetAutoStart(AValue: Boolean);
    procedure SetSensitivity(AValue: TSensitivity);

  public
    property Sensitivity: TSensitivity read GetSensitivity write SetSensitivity;
    property AutoStart: Boolean read GetAutoStart write SetAutoStart;
  end;

implementation

{$R *.lfm}

{ TSettingsForm }

//***************************************************************************************
// NAME:           GetAutoStart
// RETURN VALUE    True if the application should autostart with the operating system,
//                 False otherwise.
// DESCRIPTION:    Getter for the autostart setting.
//
//***************************************************************************************
function TSettingsForm.GetAutoStart: Boolean;
begin
  // TODO Update.
  Result := False;
end;

//***************************************************************************************
// NAME:           GetSensitivity
// RETURN VALUE    Sensitivity setting for the hot corners / edges.
// DESCRIPTION:    Getter for the hot corners / edges sensitivity setting.
//
//***************************************************************************************
function TSettingsForm.GetSensitivity: TSensitivity;
begin
  // TODO Update.
  Result := seMedium;
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
  // TODO Update.
  AValue := AValue;
end;

//***************************************************************************************
// NAME:           SetSensitivity
// RETURN VALUE    AValue Sensitivity setting for the hot corners / edges.
// DESCRIPTION:    Setter for the hot corners / edges sensitivity setting.
//
//***************************************************************************************
procedure TSettingsForm.SetSensitivity(AValue: TSensitivity);
begin
  // TODO Update.
  AValue := AValue;
end;

end.
//********************************** end of settingsunit.pas ****************************

