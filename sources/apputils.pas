unit AppUtils;
//***************************************************************************************
//  Description: Application settings class.
//    File Name: appsettings.pas
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
  Classes, SysUtils;

//***************************************************************************************
// Type Definitions
//***************************************************************************************
type

  { TAppUtils }

  TAppUtils = class(TObject)
  public
    class function IsOtherAppFullscreen : Boolean;
  end;

implementation

{$IFDEF Windows}
type
  TQUERY_USER_NOTIFICATION_STATE = (
    QUNS_NOT_PRESENT = 1,
    QUNS_BUSY = 2,
    QUNS_RUNNING_D3D_FULL_SCREEN = 3,
    QUNS_PRESENTATION_MODE = 4,
    QUNS_ACCEPTS_NOTIFICATIONS = 5,
    QUNS_QUIET_TIME = 6,
    QUNS_APP = 7);

function SHQueryUserNotificationState(out pquns: TQUERY_USER_NOTIFICATION_STATE): HResult; stdcall; external 'shell32.dll';
{$ENDIF}

{ TAppUtils }

//***************************************************************************************
// NAME:           ReadAutoStartFromRegistry
// RETURN VALUE:   True if autostart is enabled, False otherwise.
// DESCRIPTION:    Utility function to determine if another (typically the foreground)
//                 application is running in fullscreen mode. For example a game or a
//                 presentation.
//
//***************************************************************************************
class function TAppUtils.IsOtherAppFullscreen: Boolean;
var
  QUNState: TQUERY_USER_NOTIFICATION_STATE;
begin
  // Initialize the result.
  Result := False;
  {$IFDEF Windows}
  // Make sure we're running on Windows 7 or newer otherwise SHQueryUserNotificationState
  // is not available.
  if CheckWin32Version(6, 1) then
  begin
    // Request the user notification state.
    if SHQueryUserNotificationState(QUNState) = S_OK then
    begin
      // Use the notification state to infer the fullscreen state of other running apps.
      if (QUNState = QUNS_BUSY) or // F11 fullscreen or video games
         (QUNState = QUNS_RUNNING_D3D_FULL_SCREEN) or // Direct3D app in exclusive mode
         (QUNState = QUNS_PRESENTATION_MODE) then // Special fullscreen presentation mode
      begin
        // Update the result.
        Result := True;
      end;
    end;
  end;
  {$ENDIF}
end;

end.
//********************************** end of apputils.pas ********************************

