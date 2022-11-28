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
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, LCLType;

//***************************************************************************************
// Type Definitions
//***************************************************************************************
type
  TKeyMap = record
    KeyStr: string;
    KeyCode: Word;
  end;

  { TKeyBindingForm }

  TKeyBindingForm = class(TForm)
    BtnCancel: TButton;
    BtnOk: TButton;
    BtnGrab: TButton;
    ChbShift: TCheckBox;
    ChbAlt: TCheckBox;
    ChbCtrl: TCheckBox;
    ChbSuper: TCheckBox;
    CmbKey: TComboBox;
    GrbKey: TGroupBox;
    PnlButtons: TPanel;
    procedure FormCreate(Sender: TObject);
  private
    FKeyCodesBound: array[0..3] of Word;
    function GetKeyCode(Index: Integer): Word;
    procedure SetKeyCode(Index: Integer; AValue: Word);
    function GetKeyCodeCount: Integer;
  public
    property KeyCodes[Index: Integer]: Word read GetKeyCode write SetKeyCode;
    property KeyCodeCount: Integer read GetKeyCodeCount;
  end;


// IDEA ##Vg Look at how the Lazarus IDE itself does it. You can find stuff in:
//   C:\lazarus\components\ideintf\propedits.pp
// Look at class TCustomShortCutGrabBox. Just not that I want a checkbox for the
// Super key. It can also be less complex. I just want support for:
//   - Keys: a..z, 0..9, F1..F12, Tab
//   - Shifts: Shift, Ctrl, Alt, Super.
//
// TODO ##Vg I think I nee to refactor the KeyCodes / KeyCodesCounts property. Would be
// better is this is a separate object that then also have ToSt / FromStr methods.
// That way the key binding can be stored, constructed and passed around as a string.
// TKeyBinding sounds like an applicable name.


implementation

{$R *.lfm}

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
    (KeyStr: 'Backspace'; KeyCode: VK_BACK),
    (KeyStr: 'End'; KeyCode: VK_END),
    (KeyStr: 'Home'; KeyCode: VK_HOME),
    (KeyStr: 'Page Up'; KeyCode: VK_PRIOR),
    (KeyStr: 'Page Down'; KeyCode: VK_NEXT),
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

{ TKeyBindingForm }

//***************************************************************************************
// NAME:           FormCreate
// PARAMETER:      Sender Signal source.
// DESCRIPTION:    Called when the form is created
//
//***************************************************************************************
procedure TKeyBindingForm.FormCreate(Sender: TObject);
var
  Idx: Integer;
begin
  // Initialize key codes of the bound keys.
  for Idx := 0 to (Length(FKeyCodesBound) - 1) do
  begin
    FKeyCodesBound[Idx] := VK_UNKNOWN;
  end;
  // Populate the combo box with all keys, but excluding the special keys.
  for Idx := 0 to (Length(KeyMap) - NUM_SPECIAL_KEYS - 1) do
  begin
    CmbKey.Items.Add(KeyMap[Idx].KeyStr);
  end;
  CmbKey.ItemIndex := 0;
end;

//***************************************************************************************
// NAME:           GetKeyCode
// PARAMETER:      Index Index into the KeyCodes array.
// RETURN VALUE:   Key code.
// DESCRIPTION:    Getter for the key code stored at the specified index in the KeyCodes
//                 array.
//
//***************************************************************************************
function TKeyBindingForm.GetKeyCode(Index: Integer): Word;
begin
  // Initialize the result.
  Result := 0;
  // Only read array element if the index is within its bounds.
  if Index < Length(FKeyCodesBound) then
  begin
    // Update the result.
    Result := FKeyCodesBound[Index];
  end;
end;

//***************************************************************************************
// NAME:           SetKeyCode
// PARAMETER:      Index Index into the KeyCodes array.
//                 AValue: Key code to store.
// DESCRIPTION:    Setter for the key code at the specified index in the KeyCodes array.
//
//***************************************************************************************
procedure TKeyBindingForm.SetKeyCode(Index: Integer; AValue: Word);
begin
  // Only write array element if the index is within its bounds.
  if Index < Length(FKeyCodesBound) then
  begin
    // Store the key code.
    FKeyCodesBound[Index] := AValue;
  end;
end;

//***************************************************************************************
// NAME:           GetKeyCodeCount
// RETURN VALUE:   Number of key codes stored in the KeyCodes array.
// DESCRIPTION:    Getter for the number of key codes stored in the KeyCodes array. For
//                 example, if the key binding is for ALT+CTRL+P, then this function
//                 would return 0.
//
//***************************************************************************************
function TKeyBindingForm.GetKeyCodeCount: Integer;
var
  Idx: Integer;
begin
  // Initialize the index.
  Idx := 0;
  // Loop over the array as long as the key code is not VK_UNKNOWN.
  while FKeyCodesBound[Idx] <> VK_UNKNOWN do
  begin
    // Increment the indexer, which also serves as the count.
    Inc(Idx);
    // Stop the loop when the end of the array is reached.
    if Idx >= Length(FKeyCodesBound) then
    begin
      Break;
    end;
  end;
  // Set the result.
  Result := Idx;
end;

end.
//********************************** end of keybindingunit.pas **************************


