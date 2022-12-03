unit HotAction;
//***************************************************************************************
//  Description: Hot action class.
//    File Name: hotaction.pas
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
  Classes, SysUtils, LazFileUtils, KeyBinding, MouseAndKeyInput, LCLIntf, LazUTF8;

//***************************************************************************************
// Type Definitions
//***************************************************************************************
type
  // All supported action types
  TActionType = (atSendKeys = 0, atLaunchApp);

  { THotAction }

  THotAction = class(TObject)
  private
    FText: string;
    FActionType: TActionType;
    FKeyBinding: TKeyBinding;
    function GetInfo: string;
    procedure SetText(AValue: string);
    function UTF8UppercaseFirstChar(s: string): string;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Execute;
    property Text: string read FText write SetText;
    property Info: string read GetInfo;
  end;

implementation

{ THotAction }

//***************************************************************************************
// NAME:           GetInfo
// RETURN VALUE:   Info string.
// DESCRIPTION:    Getter for the info string.
//
//***************************************************************************************
function THotAction.GetInfo: string;
var
  AppName: string;
begin
  // Only continue with a non empty text string.
  if FText <> '' then
  begin
    // Action type for launching an application?
    if FActionType = atLaunchApp then
    begin
      // Extract the application name.
      AppName := ExtractFileName(ExtractFileNameWithoutExt(FText));
      // Construct the info string.
      Result := 'Launch application: ' + UTF8UppercaseFirstChar(AppName);
    end
    // Action type for sending key strokes.
    else
    begin
      // Construct the info string.
      Result := 'Send keystrokes: ' + FText;
    end;
  end;
end;

//***************************************************************************************
// NAME:           SetText
// PARAMETER:      AValue Action text. Either a valid application executable or a valid
//                 key binding string. E.g.:
//                   'C:\Program Files\Mozilla Firefox\firefox.exe'
//                 or
//                   'Alt+Ctrl+Tab'
// DESCRIPTION:    Setter for the action text.
//
//***************************************************************************************
procedure THotAction.SetText(AValue: string);
begin
  // Is it an existing file?
  if FileExists(AValue) then
  begin
    FActionType := atLaunchApp;
  end
  // Must be an action type for sending key strokes.
  else
  begin
    FActionType := atSendKeys;
  end;
  // Store the field.
  FText := AValue;
end;

//***************************************************************************************
// NAME:           UTF8UppercaseFirstChar
// PARAMETER:      s String to work on.
// RETURN VALUE:   String with the first character in upper case.
// DESCRIPTION:    Utility function to make the first character of a string upper case.
//
//***************************************************************************************
function THotAction.UTF8UppercaseFirstChar(s: string): string;
var
  ch, rest: string;
begin
  ch := UTF8Copy(s, 1, 1);
  rest := Copy(s, Length(ch)+1, MaxInt);
  Result := UTF8Uppercase(ch) + rest;
end;

//***************************************************************************************
// NAME:           Create
// DESCRIPTION:    Object constructor.
//
//***************************************************************************************
constructor THotAction.Create;
begin
  // Call inherited constructor.
  inherited Create;
  // Initialize fields.
  FText := '';
  FActionType := atSendKeys;
  // Create the key binding object.
  FKeyBinding := TKeyBinding.Create;
end;

//***************************************************************************************
// NAME:           Destroy
// DESCRIPTION:    Object destructor.
//
//***************************************************************************************
destructor THotAction.Destroy;
begin
  // Release the key binding object.
  FKeyBinding.Free;
  // Call inherited destructor.
  inherited Destroy;
end;

//***************************************************************************************
// NAME:           Execute
// DESCRIPTION:    Performs the actual action.
//
//***************************************************************************************
procedure THotAction.Execute;
var
  Idx: Integer;
begin
  // Action type for launching an application?
  if FActionType = atLaunchApp then
  begin
    // Verify that the file exists. It could have been deleted since setting it.
    if FileExists(FText) then
    begin
      // Launch the application. Note that RunCommand and ExecuteProcess open in a modal
      // way. Meaning that you cannot close this application until the launch application
      // is first closed.
      OpenDocument(FText);
    end;
  end
  // Action type for sending key strokes.
  else
  begin
    // Configure key binding object.
    FKeyBinding.KeyBindingStr := FText;
    // Loop through the key codes to press them one-by-one.
    for Idx := 0 to (FKeyBinding.KeyBindingCodeCount - 1) do
    begin
      KeyInput.Down(FKeyBinding.KeyBindingCode[Idx]);
    end;
    // Loop through the key codes in reverse order to let them go again.
    for Idx := (FKeyBinding.KeyBindingCodeCount - 1) downto 0 do
    begin
      KeyInput.Up(FKeyBinding.KeyBindingCode[Idx]);
    end;
  end;
end;

end.
//********************************** end of hotaction.pas *******************************

