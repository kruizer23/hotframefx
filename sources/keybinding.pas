unit KeyBinding;
//***************************************************************************************
//  Description: Generic key binding class.
//    File Name: keybinding.pas
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
  Classes, SysUtils, LCLType;

//***************************************************************************************
// Type Definitions
//***************************************************************************************
type
  { TKeyBinding }

  TKeyBinding = class(TObject)
  private
    FKeyBindingStr: string;
    FKeyBindingCode: array[0..4] of Word;
    function KeyCodeToKeyStr(AValue: Word): string;
    function KeyStrToKeyCode(AValue: string): Word;
    function GetKeyBindingCode(Index: Integer): Word;
    function GetKeyBindingCodeCount: Integer;
    function GetKeyBindingStr: string;
    procedure SetKeyBindingCode(Index: Integer; AValue: Word);
    procedure SetKeyBindingStr(AValue: string);
  public
    constructor Create;
    destructor Destroy; override;
    property KeyBindingStr: string read GetKeyBindingStr write SetKeyBindingStr;
    property KeyBindingCode[Index: Integer]: Word read GetKeyBindingCode write SetKeyBindingCode;
    property KeyBindingCodeCount: Integer read GetKeyBindingCodeCount;
  end;

  TKeyMap = record
    KeyStr: string;
    KeyCode: Word;
  end;

//***************************************************************************************
// Constant data declarations
//***************************************************************************************
const
  NUM_SPECIAL_KEYS = 4;

const
  KeyMap: array[0..(59+NUM_SPECIAL_KEYS)] of TKeyMap =
  (
    (KeyStr: 'Tab'; KeyCode: VK_TAB),
    (KeyStr: 'Delete'; KeyCode: VK_DELETE),
    (KeyStr: 'Insert'; KeyCode: VK_INSERT),
    (KeyStr: 'Back'; KeyCode: VK_BACK),
    (KeyStr: 'End'; KeyCode: VK_END),
    (KeyStr: 'Home'; KeyCode: VK_HOME),
    (KeyStr: 'PgUp'; KeyCode: VK_PRIOR),
    (KeyStr: 'PgDown'; KeyCode: VK_NEXT),
    (KeyStr: 'Left'; KeyCode: VK_LEFT),
    (KeyStr: 'Right'; KeyCode: VK_RIGHT),
    (KeyStr: 'Up'; KeyCode: VK_UP),
    (KeyStr: 'Down'; KeyCode: VK_DOWN),
    (KeyStr: '0'; KeyCode: VK_0),
    (KeyStr: '1'; KeyCode: VK_1),
    (KeyStr: '2'; KeyCode: VK_2),
    (KeyStr: '3'; KeyCode: VK_3),
    (KeyStr: '4'; KeyCode: VK_4),
    (KeyStr: '5'; KeyCode: VK_5),
    (KeyStr: '6'; KeyCode: VK_6),
    (KeyStr: '7'; KeyCode: VK_7),
    (KeyStr: '8'; KeyCode: VK_8),
    (KeyStr: '9'; KeyCode: VK_9),
    (KeyStr: 'A'; KeyCode: VK_A),
    (KeyStr: 'B'; KeyCode: VK_B),
    (KeyStr: 'C'; KeyCode: VK_C),
    (KeyStr: 'D'; KeyCode: VK_D),
    (KeyStr: 'E'; KeyCode: VK_E),
    (KeyStr: 'F'; KeyCode: VK_F),
    (KeyStr: 'G'; KeyCode: VK_G),
    (KeyStr: 'H'; KeyCode: VK_H),
    (KeyStr: 'I'; KeyCode: VK_I),
    (KeyStr: 'J'; KeyCode: VK_J),
    (KeyStr: 'K'; KeyCode: VK_K),
    (KeyStr: 'L'; KeyCode: VK_L),
    (KeyStr: 'M'; KeyCode: VK_M),
    (KeyStr: 'N'; KeyCode: VK_N),
    (KeyStr: 'O'; KeyCode: VK_O),
    (KeyStr: 'P'; KeyCode: VK_P),
    (KeyStr: 'Q'; KeyCode: VK_Q),
    (KeyStr: 'R'; KeyCode: VK_R),
    (KeyStr: 'S'; KeyCode: VK_S),
    (KeyStr: 'T'; KeyCode: VK_T),
    (KeyStr: 'U'; KeyCode: VK_U),
    (KeyStr: 'V'; KeyCode: VK_V),
    (KeyStr: 'W'; KeyCode: VK_W),
    (KeyStr: 'X'; KeyCode: VK_X),
    (KeyStr: 'Y'; KeyCode: VK_Y),
    (KeyStr: 'Z'; KeyCode: VK_Z),
    (KeyStr: 'F1'; KeyCode: VK_F1),
    (KeyStr: 'F2'; KeyCode: VK_F2),
    (KeyStr: 'F3'; KeyCode: VK_F3),
    (KeyStr: 'F4'; KeyCode: VK_F4),
    (KeyStr: 'F5'; KeyCode: VK_F5),
    (KeyStr: 'F6'; KeyCode: VK_F6),
    (KeyStr: 'F7'; KeyCode: VK_F7),
    (KeyStr: 'F8'; KeyCode: VK_F8),
    (KeyStr: 'F9'; KeyCode: VK_F9),
    (KeyStr: 'F10'; KeyCode: VK_F10),
    (KeyStr: 'F11'; KeyCode: VK_F11),
    (KeyStr: 'F12'; KeyCode: VK_F12),
    // Start of special keys.
    (KeyStr: 'Shift'; KeyCode: VK_SHIFT),
    (KeyStr: 'Alt'; KeyCode: VK_MENU),
    (KeyStr: 'Ctrl'; KeyCode: VK_CONTROL),
    (KeyStr: 'Super'; KeyCode: VK_LWIN)
  );

implementation

{ TKeyBinding }

//***************************************************************************************
// NAME:           Create
// DESCRIPTION:    Object constructor. Calls TObjects's constructor and initializes
//                 the fields to their default values.
//
//***************************************************************************************
constructor TKeyBinding.Create;
var
  Idx: Integer;
begin
  // Call inherited constructor.
  inherited Create;
  // Initialize key codes of the bound keys.
  for Idx := 0 to (Length(FKeyBindingCode) - 1) do
  begin
    FKeyBindingCode[Idx] := VK_UNKNOWN;
  end;
  // Initialize the key binding string to an empty one.
  FKeyBindingStr := '';
end;

//***************************************************************************************
// NAME:           Destroy
// DESCRIPTION:    Object destructor. Calls TObjects's destructor
//
//***************************************************************************************
destructor TKeyBinding.Destroy;
begin
  // Call inherited destructor.
  inherited Destroy;
end;

//***************************************************************************************
// NAME:           KeyStrToKeyCode
// PARAMETER:      AValue Key string
// RETURN VALUE:   The key code of the key's string representation.
// DESCRIPTION:    Helper function to convert a key string representation to its key
//                 code.
//
//***************************************************************************************
function TKeyBinding.KeyStrToKeyCode(AValue: string): Word;
var
  Idx: Integer;
begin
  // Initialize the result to an invalid code.
  Result := VK_UNKNOWN;
  // Loop over all supported key codes.
  for Idx := 0 to (Length(KeyMap) - 1) do
  begin
    // Is this the key string to convert? Note that the comparising is case insensitive.
    if CompareText(KeyMap[Idx].KeyStr, AValue) = 0 then
    begin
      // Convert it to a key code
      Result := KeyMap[Idx].KeyCode;
      // All done so no need to continue the loop.
      Break;
    end;
  end;
end;

//***************************************************************************************
// NAME:           KeyCodeToKeyStr
// PARAMETER:      AValue Key code.
// RETURN VALUE:   The string representation of the key code.
// DESCRIPTION:    Helper function to convert a key code to its string representation.
//
//***************************************************************************************
function TKeyBinding.KeyCodeToKeyStr(AValue: Word): string;
var
  Idx: Integer;
begin
  // Initialize the result.
  Result := '';
  // Loop over all supported key codes.
  for Idx := 0 to (Length(KeyMap) - 1) do
  begin
    // Is this the key code to convert?
    if KeyMap[Idx].KeyCode = AValue then
    begin
      // Convert it to a string.
      Result := KeyMap[Idx].KeyStr;
      // All done so no need to continue the loop.
      Break;
    end;
  end;
end;

//***************************************************************************************
// NAME:           GetKeyBindingCode
// PARAMETER:      Index Index into the key binding code array.
// RETURN VALUE:   Key code.
// DESCRIPTION:    Getter for the key binding code stored at the specified index in the
//                 key binding code array.
//
//***************************************************************************************
function TKeyBinding.GetKeyBindingCode(Index: Integer): Word;
begin
  // Initialize the result.
  Result := 0;
  // Only read array element if the index is within its bounds.
  if Index < Length(FKeyBindingCode) then
  begin
    // Update the result.
    Result := FKeyBindingCode[Index];
  end;
end;

//***************************************************************************************
// NAME:           SetKeyBindingCode
// PARAMETER:      Index Index into the key binding code array.
//                 AValue: Key code to store.
// DESCRIPTION:    Setter for the key code at the specified index in the key binding
//                 code array.
//
//***************************************************************************************
procedure TKeyBinding.SetKeyBindingCode(Index: Integer; AValue: Word);
begin
  // Only write array element if the index is within its bounds.
  if Index < Length(FKeyBindingCode) then
  begin
    // Store the key code.
    FKeyBindingCode[Index] := AValue;
  end;
end;

//***************************************************************************************
// NAME:           GetKeyBindingCodeCount
// RETURN VALUE:   Number of key binding codes stored in the key binding code array.
// DESCRIPTION:    Getter for the number of key codes stored in the key binding code
//                 array. For example, if the key binding is for Alt+Ctrl+P, then this
//                 function would return 0.
//
//***************************************************************************************
function TKeyBinding.GetKeyBindingCodeCount: Integer;
var
  Idx: Integer;
begin
  // Initialize the index.
  Idx := 0;
  // Loop over the array as long as the key code is not VK_UNKNOWN.
  while FKeyBindingCode[Idx] <> VK_UNKNOWN do
  begin
    // Increment the indexer, which also serves as the count.
    Inc(Idx);
    // Stop the loop when the end of the array is reached.
    if Idx >= Length(FKeyBindingCode) then
    begin
      Break;
    end;
  end;
  // Set the result.
  Result := Idx;
end;

//***************************************************************************************
// NAME:           GetKeyBindingStr
// RETURN VALUE:   String representation of the key binding.
// DESCRIPTION:    Getter for the string representation of the key binding. For example,
//                 if the key binding is set to Alt+Ctrl+P, then the resulting string
//                 would be 'Alt+Ctrl+P'.
//
//***************************************************************************************
function TKeyBinding.GetKeyBindingStr: string;
var
  Idx: Integer;
  KeyStr: string;
begin
  // Initialize the result.
  Result := '';
  // Loop over all key codes currently stored in the key binding code array.
  for Idx := 0 to (Length(FKeyBindingCode) - 1) do
  begin
    // Is this still a valid key code?
    if FKeyBindingCode[Idx] <> VK_UNKNOWN then
    begin
      // Attempt to convert it to a string.
      KeyStr := KeyCodeToKeyStr(FKeyBindingCode[Idx]);
      // Conversion successful?
      if KeyStr <> '' then
      begin
        // Add a '+' character if the result sting is not empty.
        if Result <> '' then
        begin
          Result := Result + '+';
        end;
        // Add the string representation of the key code to the result string.
        Result := Result + KeyStr;
      end;
    end
    // No more valid key codes in the array so stop looping.
    else
    begin
      Break;
    end;
  end;
end;

//***************************************************************************************
// NAME:           SetKeyBindingStr
// PARAMETER:      AValue String representation of the key binding.
// DESCRIPTION:    Setter for the string representation of the key binding. For example,
//                 if the key binding is set to Alt+Ctrl+P, then following will be
//                 stored in the key binding code array:
//                   FKeyBindingCode[0] :=  VK_MENU
//                   FKeyBindingCode[1] :=  VK_CONTROL
//                   FKeyBindingCode[2] :=  VK_P
//                   FKeyBindingCode[3] :=  VK_UNKNOWN
//
//***************************************************************************************
procedure TKeyBinding.SetKeyBindingStr(AValue: string);
var
  Idx: Integer;
  KeyStrArray: TStringArray;
  KeyCodeIdx: Integer;
  KeyCode: Word;
begin
  // Clear the key codes of the bound keys because we are about to overwrite them.
  for Idx := 0 to (Length(FKeyBindingCode) - 1) do
  begin
    FKeyBindingCode[Idx] := VK_UNKNOWN;
  end;
  // Split the string using the '+' character as the delimited.
  KeyStrArray := AValue.Split('+');
  // Reset the key code indexer.
  KeyCodeIdx := 0;
  // Loop over all splits. Each split represents a key string representation.
  for Idx := 0 to (Length(KeyStrArray) - 1) do
  begin
    // Attempt to convert the key string representation to its key code.
    KeyCode := KeyStrToKeyCode(KeyStrArray[Idx]);
    // Check if a valid key code was found as only those can be added.
    if KeyCode <> VK_UNKNOWN then
    begin
      // Add the key code.
      FKeyBindingCode[KeyCodeIdx] := KeyCode;
      // Point to the next array entry.
      Inc(KeyCodeIdx);
    end
  end;
end;

end.
//********************************** end of keybinding.pas ******************************

