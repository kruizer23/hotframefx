unit MouseButtons;
//***************************************************************************************
//  Description: Access desktop wide mouse button state information.
//    File Name: mousebuttons.pas
//
//---------------------------------------------------------------------------------------
//                          C O P Y R I G H T
//---------------------------------------------------------------------------------------
//              Copyright 2025 (c) by Frank Voorburg   All rights reserved.
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
  Classes, SysUtils, Windows, LazUtilities, SyncObjs;

//***************************************************************************************
// Type Definitions
//***************************************************************************************
type
  { TMouseButtons }
  TMouseButtons = class (TObject)
  private
  public
    constructor Create;
    destructor Destroy; override;
    function IsLeftButtonDown: Boolean;
    function IsRightButtonDown: Boolean;
    function IsMiddleButtonDown: Boolean;
    function IsAnyButtonDown: Boolean;
  end;

//***************************************************************************************
// Global data declarations
//***************************************************************************************
var
  MouseButtonInfo: TMouseButtons;

implementation
//***************************************************************************************
// Local Type Definitions
//***************************************************************************************
type
  { TMouseButtonsSingleton }
  {$warn 3018 off} // Okay for singleton to not have a public constructor.
  TMouseButtonsSingleton = class sealed
  private
    class var FInstance: TMouseButtonsSingleton;
    FHook: Cardinal;
    FCritSec: TCriticalSection;
    FLeftButtonDown: Boolean;
    FRightButtonDown: Boolean;
    FMiddleButtonDown: Boolean;
    constructor CreateSingleton;
    class destructor ClassDestroy;
    class function Instance: TMouseButtonsSingleton; static;
    procedure SetLeftButtonDown(AValue: Boolean);
    function GetLeftButtonDown: Boolean;
    procedure SetRightButtonDown(AValue: Boolean);
    function GetRightButtonDown: Boolean;
    procedure SetMiddleButtonDown(AValue: Boolean);
    function GetMiddleButtonDown: Boolean;
    property LeftButtonDown: Boolean read GetLeftButtonDown write SetLeftButtonDown;
    property RightButtonDown: Boolean read GetRightButtonDown write SetRightButtonDown;
    property MiddleButtonDown: Boolean read GetMiddleButtonDown write SetMiddleButtonDown;
  public
    constructor Create;
    destructor Destroy; override;
  end;
  {$warn 3018 on} // enable suppressed warning again.

//***************************************************************************************
// NAME:           LowLevelMouseHookProc
// DESCRIPTION:    Hook function to receiving low level mouse related information. Needed
//                 for accessing mouse up/down events from the entire desktop.
//
//***************************************************************************************
function LowLevelMouseHookProc(nCode: LongInt; wParam: Int64; lParam: Int64): Int64; stdcall;
begin
  // Hook procedures are installed in chains for particular hook types. CallNextHookEx
  // calls the next hook in the chain. Calling CallNextHookEx is optional, but it is
  // highly recommended; otherwise, other applications that have installed hooks will not
  // receive hook notifications and may behave incorrectly as a result. You should call
  // CallNextHookEx unless you absolutely need to prevent the notification from being
  // seen by other applications.
  Result := CallNextHookEx(TMouseButtonsSingleton.Instance.FHook, nCode, wParam, lParam);
  // Process mouse button up/down events. Make sure to change the button down flags
  // using their properties as this then uses a critical section. Critical section is
  // needed when accessing these button down flags, because they are shared resources
  // and this hook function can be called asynchronously from the application execution
  // loop.
  case wParam of
    WM_LBUTTONDOWN: TMouseButtonsSingleton.Instance.LeftButtonDown := True;
    WM_LBUTTONUP: TMouseButtonsSingleton.Instance.LeftButtonDown := False;
    WM_RBUTTONDOWN: TMouseButtonsSingleton.Instance.RightButtonDown := True;
    WM_RBUTTONUP: TMouseButtonsSingleton.Instance.RightButtonDown := False;
    WM_MBUTTONDOWN: TMouseButtonsSingleton.Instance.MiddleButtonDown := True;
    WM_MBUTTONUP: TMouseButtonsSingleton.Instance.MiddleButtonDown := False;
  end;
end;

{ TMouseButtonsSingleton }

//***************************************************************************************
// NAME:           ClassDestroy
// DESCRIPTION:    Class constructor that is called automatically during finalization.
//
//***************************************************************************************
class destructor TMouseButtonsSingleton.ClassDestroy;
begin
  // Only free the singleton if it was previously instanced.
  if Assigned(FInstance) then
  begin
    FInstance.Free;
  end;
end;

//***************************************************************************************
// NAME:           Instance
// DESCRIPTION:    Method for accessing the singleton instance.
//
//***************************************************************************************
class function TMouseButtonsSingleton.Instance: TMouseButtonsSingleton;
begin
  // Only construct the object once.
  if not Assigned(FInstance) then
  begin
    FInstance := TMouseButtonsSingleton.CreateSingleton;
  end;
  // Update the result.
  Result := FInstance;
end;

//***************************************************************************************
// NAME:           Create
// DESCRIPTION:    All TObject descendants should have a Create constructor. However,
//                 this one shouldn't be used for a singleton. This explicit Create
//                 constructor makes sure an exception is raised, in case something does
//                 try to create an instance of the class using Create.
//
//***************************************************************************************
constructor TMouseButtonsSingleton.Create;
var
  ExceptionMsg: String;
begin
  // Trigger exception as the constructor should not be directly used.
  ExceptionMsg := 'TMouseButtonsSingleton cannot be directly constructed';
  raise ENoConstructException.Create(ExceptionMsg);
end;

//***************************************************************************************
// NAME:           CreateSingleton
// DESCRIPTION:    Actual constructor. Note that this constructor is private, making sure
//                 that only the class itself can use it.
//
//***************************************************************************************
constructor TMouseButtonsSingleton.CreateSingleton;
const
  WH_MOUSE_LL = 14;
begin
  // Call inherited constructor.
  inherited Create;
  // Create the critical section.
  FCritSec := TCriticalSection.Create;
  // Initialize button down flags.
  FLeftButtonDown := False;
  FRightButtonDown := False;
  FMiddleButtonDown := False;
  // Register the hook function.
  FHook := SetWindowsHookEx(WH_MOUSE_LL, @LowLevelMouseHookProc, hInstance, 0);
end;

//***************************************************************************************
// NAME:           Destroy
// DESCRIPTION:    Object destructor. Calls TObjects's destructor
//
//***************************************************************************************
destructor TMouseButtonsSingleton.Destroy;
begin
  // Unregister the hook function.
  UnhookWindowsHookEx(FHook);
  // Release the critical section object.
  FCritSec.Free;
  // Call inherited destructor.
  inherited Destroy;
end;

//***************************************************************************************
// NAME:           SetLeftButtonDown
// PARAMETER:      AValue True to flag the mouse button as pressed, False otherwise.
// DESCRIPTION:    Setter for the LeftButtonDown property.
//
//***************************************************************************************
procedure TMouseButtonsSingleton.SetLeftButtonDown(AValue: Boolean);
begin
  // Enter the critical section.
  FCritSec.Enter;
  try
    FLeftButtonDown := AValue;
  finally
    // Exit the critical section
    FCritSec.Leave;
  end;
end;

//***************************************************************************************
// NAME:           GetLeftButtonDown
// RETURN VALUE:   True if the mouse button is pressed, False otherwise.
// DESCRIPTION:    Getter for the LeftButtonDown property.
//
//***************************************************************************************
function TMouseButtonsSingleton.GetLeftButtonDown: Boolean;
begin
  // Enter the critical section.
  FCritSec.Enter;
  try
    Result := FLeftButtonDown;
  finally
    // Exit the critical section
    FCritSec.Leave;
  end;
end;

//***************************************************************************************
// NAME:           SetRightButtonDown
// PARAMETER:      AValue True to flag the mouse button as pressed, False otherwise.
// DESCRIPTION:    Setter for the RightButtonDown property.
//
//***************************************************************************************
procedure TMouseButtonsSingleton.SetRightButtonDown(AValue: Boolean);
begin
  // Enter the critical section.
  FCritSec.Enter;
  try
    FRightButtonDown := AValue;
  finally
    // Exit the critical section
    FCritSec.Leave;
  end;
end;

//***************************************************************************************
// NAME:           GetRightButtonDown
// RETURN VALUE:   True if the mouse button is pressed, False otherwise.
// DESCRIPTION:    Getter for the RightButtonDown property.
//
//***************************************************************************************
function TMouseButtonsSingleton.GetRightButtonDown: Boolean;
begin
  // Enter the critical section.
  FCritSec.Enter;
  try
    Result := FRightButtonDown;
  finally
    // Exit the critical section
    FCritSec.Leave;
  end;
end;

//***************************************************************************************
// NAME:           SetMiddleButtonDown
// PARAMETER:      AValue True to flag the mouse button as pressed, False otherwise.
// DESCRIPTION:    Setter for the MiddleButtonDown property.
//
//***************************************************************************************
procedure TMouseButtonsSingleton.SetMiddleButtonDown(AValue: Boolean);
begin
  // Enter the critical section.
  FCritSec.Enter;
  try
    FMiddleButtonDown := AValue;
  finally
    // Exit the critical section
    FCritSec.Leave;
  end;
end;

//***************************************************************************************
// NAME:           GetMiddleButtonDown
// RETURN VALUE:   True if the mouse button is pressed, False otherwise.
// DESCRIPTION:    Getter for the MiddleButtonDown property.
//
//***************************************************************************************
function TMouseButtonsSingleton.GetMiddleButtonDown: Boolean;
begin
  // Enter the critical section.
  FCritSec.Enter;
  try
    Result := FMiddleButtonDown;
  finally
    // Exit the critical section
    FCritSec.Leave;
  end;
end;

{ TMouseButtons }

//***************************************************************************************
// NAME:           Create
// DESCRIPTION:    Object constructor. Calls TObjects's constructor and initializes
//                 the fields to their default values.
//
//***************************************************************************************
constructor TMouseButtons.Create;
begin
  // Call inherited constructor.
  inherited Create;
  // Create the singleton instance. Not abolutely needed, but this way the singleton
  // gets created right away with this object and not when the singleton instance is
  // first accessed.
  TMouseButtonsSingleton.Instance;
end;

//***************************************************************************************
// NAME:           Destroy
// DESCRIPTION:    Object destructor. Calls TObjects's destructor
//
//***************************************************************************************
destructor TMouseButtons.Destroy;
begin
  // Call inherited destructor.
  inherited Destroy;
end;

//***************************************************************************************
// NAME:           IsLeftButtonDown
// RETURN VALUE:   True if the button is currently down, False otherwise.
// DESCRIPTION:    Determines if the left mouse button is currently down.
//
//***************************************************************************************
function TMouseButtons.IsLeftButtonDown: Boolean;
begin
  Result := TMouseButtonsSingleton.Instance.LeftButtonDown;
end;

//***************************************************************************************
// NAME:           IsRightButtonDown
// RETURN VALUE:   True if the button is currently down, False otherwise.
// DESCRIPTION:    Determines if the right mouse button is currently down.
//
//***************************************************************************************
function TMouseButtons.IsRightButtonDown: Boolean;
begin
  Result := TMouseButtonsSingleton.Instance.RightButtonDown;
end;

//***************************************************************************************
// NAME:           IsMiddleButtonDown
// RETURN VALUE:   True if the button is currently down, False otherwise.
// DESCRIPTION:    Determines if the middle mouse button is currently down.
//
//***************************************************************************************
function TMouseButtons.IsMiddleButtonDown: Boolean;
begin
  Result := TMouseButtonsSingleton.Instance.MiddleButtonDown;
end;

//***************************************************************************************
// NAME:           IsAnyButtonDown
// RETURN VALUE:   True if a button is currently down, False otherwise.
// DESCRIPTION:    Determines if either the left, right or  middle mouse button is
//                 currently down.
//
//***************************************************************************************
function TMouseButtons.IsAnyButtonDown: Boolean;
begin
  Result := False;
  if (IsLeftButtonDown = True) or
     (IsRightButtonDown = True) or
     (IsMiddleButtonDown = True) then
  begin
    Result := True;
  end;
end;

//***************************************************************************************
// Initialization
//***************************************************************************************
initialization
  MouseButtonInfo := TMouseButtons.Create;

//***************************************************************************************
// Finalization
//***************************************************************************************
finalization
  FreeThenNil(MouseButtonInfo);

end.
//********************************** end of mousebuttons.pas ****************************

