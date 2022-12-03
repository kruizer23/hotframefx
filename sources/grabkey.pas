unit GrabKey;
//***************************************************************************************
//  Description: Generic frame for selecting a keyboard key combination.
//    File Name: grabkey.pas
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
  Classes, SysUtils, Forms, Controls, Graphics, StdCtrls, LCLType;

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

  { TKeyBindingGrabForm }

  TKeyBindingGrabForm = class(TForm)
  private
    FLblInfo: TLabel;
    FKeyBinding: TKeyBinding;
    procedure FormKeyDown(Sender: TObject; var AKey: Word; AShift: TShiftState);
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    property KeyBinding: TKeyBinding read FKeyBinding;
  end;

  { TGrabKeyFrame }

  TGrabKeyFrame = class(TFrame)
    BtnGrab: TButton;
    ChbAlt: TCheckBox;
    ChbCtrl: TCheckBox;
    ChbShift: TCheckBox;
    ChbSuper: TCheckBox;
    CmbKey: TComboBox;
    procedure BtnGrabClick(Sender: TObject);
  private
    FKeyBinding: TKeyBinding;
    function GetKeyBinding: string;
    procedure SetKeyBinding(AValue: string);
    procedure UpdateUI;
    procedure UpdateFromUI;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    property KeyBinding: string read GetKeyBinding write SetKeyBinding;
  end;

implementation

{$R *.lfm}

//***************************************************************************************
// Type Definitions
//***************************************************************************************
type
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

{ TKeyBindingGrabForm }

//***************************************************************************************
// NAME:           Create
// PARAMETER:      TheOwner Owner of the form.
// DESCRIPTION:    Form constructor. Calls TForm's constructor and dynamically creates
//                 the form.
//
//***************************************************************************************
constructor TKeyBindingGrabForm.Create(TheOwner: TComponent);
begin
  // Call CreateNew insteaf of te inherited constructor, because this form does not have
  // a resource.
  CreateNew(TheOwner);
  // Initialize the form.
  Caption := 'Press a key...';
  Position := poOwnerFormCenter;
  KeyPreview := True;
  BorderStyle := bsDialog;
  ModalResult := mrCancel;
  Height := 70;
  Width := 240;
  // Create and add the info label.
  FLblInfo := TLabel.Create(Self);
  FLblInfo.Parent := Self;
  FLblInfo.Caption := 'You can press a key, e.g. Ctrl+P...';
  FLblInfo.Align := alClient;
  FLblInfo.Alignment := taCenter;
  FLblInfo.Layout :=tlCenter;
  // Construct the key binding object.
  FKeyBinding := TKeyBinding.Create;
  // Configure the key down event handler.
  OnKeyDown := @FormKeyDown;
end;

//***************************************************************************************
// NAME:           Destroy
// DESCRIPTION:    Form destructor. Calls TObjects's destructor
//
//***************************************************************************************
destructor TKeyBindingGrabForm.Destroy;
begin
  // Release the key binding object.
  FreeAndNil(FKeyBinding);
  // Call inherited destructor.
  inherited Destroy;
end;

//***************************************************************************************
// NAME:           FormKeyDown
// PARAMETER:      Sender Signal source.
//                 AKey Key code of the key that was pressed.
//                 AShift Set of possibly pressed special keys.
// DESCRIPTION:    Called when a key down event was detected on the form.
//
//***************************************************************************************
procedure TKeyBindingGrabForm.FormKeyDown(Sender: TObject; var AKey: Word;
  AShift: TShiftState);
var
  Idx: Integer;
  KeyCodeIdx: Integer;
begin
  // Check if the pressed key is one of the supported keys.
  for Idx := 0 to (Length(KeyMap) - NUM_SPECIAL_KEYS - 1) do
  begin
    // Is the key supported?
    if AKey = KeyMap[Idx].KeyCode then
    begin
      // Initialize the key code indexer.
      KeyCodeIdx := 0;
      // Process the special shift key.
      if ssShift in AShift then
      begin
        // Add the Shift key code.
        FKeyBinding.KeyBindingCode[KeyCodeIdx] := VK_SHIFT;
        Inc(KeyCodeIdx);
      end;
      // Process the special alt key.
      if ssAlt in AShift then
      begin
        // Add the Alt key code.
        FKeyBinding.KeyBindingCode[KeyCodeIdx] := VK_MENU;
        Inc(KeyCodeIdx);
      end;
      // Process the special control key.
      if ssCtrl in AShift then
      begin
        // Add the Ctrl key code.
        FKeyBinding.KeyBindingCode[KeyCodeIdx] := VK_CONTROL;
        Inc(KeyCodeIdx);
      end;
      // Process the special super key.
      if ssSuper in AShift then
      begin
        // Add the Supoer key code.
        FKeyBinding.KeyBindingCode[KeyCodeIdx] := VK_LWIN;
        Inc(KeyCodeIdx);
      end;
      // Finally add the key code of the actual key that got pressed.
      FKeyBinding.KeyBindingCode[KeyCodeIdx] := KeyMap[Idx].KeyCode;
      // Key grabbing completed successfully. Update the modal result for this.
      ModalResult := mrOK;
      // Stop the loop.
      Break;
    end;
  end;
end;

{ TGrabKeyFrame }

//***************************************************************************************
// NAME:           UpdateUI
// DESCRIPTION:    Updates the user interface for the currently configured key binding
//                 string.
//
//***************************************************************************************
procedure TGrabKeyFrame.UpdateUI;
var
  Idx: Integer;
  KeyCodeToMatch: Word;
begin
  // First reset the user interface.
  ChbShift.Checked := False;
  ChbAlt.Checked := False;
  ChbCtrl.Checked := False;
  ChbSuper.Checked := False;
  CmbKey.ItemIndex := 0;
  // Only need to continue if an actual key binding is currently configured.
  if FKeyBinding.KeyBindingCodeCount > 0 then
  begin
    // First loop through the key codes to determine which special keys it contains.
    for Idx := 0 to FKeyBinding.KeyBindingCodeCount - 1 do
    begin
      // Is this the shift key?
      if FKeyBinding.KeyBindingCode[Idx] = VK_SHIFT then
      begin
        ChbShift.Checked := True;
      end;
      // Is this the alt key?
      if FKeyBinding.KeyBindingCode[Idx] = VK_MENU then
      begin
        ChbAlt.Checked := True;
      end;
      // Is this the control key?
      if FKeyBinding.KeyBindingCode[Idx] = VK_CONTROL then
      begin
        ChbCtrl.Checked := True;
      end;
      // Is this the super key?
      if FKeyBinding.KeyBindingCode[Idx] = VK_LWIN then
      begin
        ChbSuper.Checked := True;
      end;
    end;
    // The last key code should be the one to show in the combo box. Loop through all
    // supported keys to try match the combo box item index.
    KeyCodeToMatch := FKeyBinding.KeyBindingCode[FKeyBinding.KeyBindingCodeCount - 1];
    for Idx := 0 to (Length(KeyMap) - NUM_SPECIAL_KEYS - 1) do
    begin
      // Is this the one to show?
      if KeyMap[Idx].KeyCode = KeyCodeToMatch then
      begin
        // Select this item in the combo box.
        CmbKey.ItemIndex := Idx;
        // Stop the loop.
        Break;
      end;
    end;
  end;
end;

//***************************************************************************************
// NAME:           BtnGrabClick
// PARAMETER:      Sender Signal source.
// DESCRIPTION:    Called when the button is clicked.
//
//***************************************************************************************
procedure TGrabKeyFrame.BtnGrabClick(Sender: TObject);
var
  GrabForm: TKeyBindingGrabForm;
begin
  GrabForm := TKeyBindingGrabForm.Create(Self);
  if GrabForm.ShowModal = mrOK then
  begin
    // Store the key binding that was grabbed.
    FKeyBinding.KeyBindingStr := GrabForm.KeyBinding.KeyBindingStr;
    // Update the user interface to show this one.
    UpdateUI;
  end;
  FreeAndNil(GrabForm);
end;

//***************************************************************************************
// NAME:           GetKeyBinding
// RETURN VALUE:   Key binding string.
// DESCRIPTION:    Getter for the string representation of the key binding.
//
//***************************************************************************************
function TGrabKeyFrame.GetKeyBinding: string;
begin
  // Update the key binding string based on what is currently selected on the user
  // interface.
  UpdateFromUI;
  // Obtain the current string representation of the key binding.
  Result := FKeyBinding.KeyBindingStr;
end;

//***************************************************************************************
// NAME:           SetKeyBinding
// PARAMETER:      Key binding string.
// DESCRIPTION:    Getter for the key binding from its string representation.
//
//***************************************************************************************
procedure TGrabKeyFrame.SetKeyBinding(AValue: string);
begin
  // Update the key binding.
  FKeyBinding.KeyBindingStr := AValue;
  // Update the user interface to make sure that its element represent the currently
  // configured key binding.
  UpdateUI;
end;

//***************************************************************************************
// NAME:           UpdateFromUI
// DESCRIPTION:    Updates the key binding based on what is currently selected on the
//                 user interface.
//
//***************************************************************************************
procedure TGrabKeyFrame.UpdateFromUI;
var
  Idx: Integer;
  KeyCodeIdx: Integer;
begin
  // Clear the key codes of the bound keys because we are about to overwrite them.
  for Idx := 0 to (FKeyBinding.KeyBindingCodeCount - 1) do
  begin
    FKeyBinding.KeyBindingCode[Idx] := VK_UNKNOWN;
  end;
  // Initialize the key code indexer.
  KeyCodeIdx := 0;
  // First process the special keys.
  if ChbShift.Checked then
  begin
    FKeyBinding.KeyBindingCode[KeyCodeIdx] := VK_SHIFT;
    // Increment the key code indexer.
    Inc(KeyCodeIdx);
  end;
  if ChbAlt.Checked then
  begin
    FKeyBinding.KeyBindingCode[KeyCodeIdx] := VK_MENU;
    // Increment the key code indexer.
    Inc(KeyCodeIdx);
  end;
  if ChbCtrl.Checked then
  begin
    FKeyBinding.KeyBindingCode[KeyCodeIdx] := VK_CONTROL;
    // Increment the key code indexer.
    Inc(KeyCodeIdx);
  end;
  if ChbSuper.Checked then
  begin
    FKeyBinding.KeyBindingCode[KeyCodeIdx] := VK_LWIN;
    // Increment the key code indexer.
    Inc(KeyCodeIdx);
  end;
  // Finally add the key from the combo box.
  FKeyBinding.KeyBindingCode[KeyCodeIdx] := KeyMap[CmbKey.ItemIndex].KeyCode;
end;

//***************************************************************************************
// NAME:           Create
// DESCRIPTION:    Frame constructor.
//
//***************************************************************************************
constructor TGrabKeyFrame.Create(TheOwner: TComponent);
var
  Idx: Integer;
begin
  // Call inherited constructor.
  inherited Create(TheOwner);
  // Create the key binding object.
  FKeyBinding := TKeyBinding.Create;
  // Populate the combo box with all keys, but excluding the special keys.
  for Idx := 0 to (Length(KeyMap) - NUM_SPECIAL_KEYS - 1) do
  begin
    CmbKey.Items.Add(KeyMap[Idx].KeyStr);
  end;
  CmbKey.ItemIndex := 0;
end;

//***************************************************************************************
// NAME:           Destroy
// DESCRIPTION:    Frame destructor.
//
//***************************************************************************************
destructor TGrabKeyFrame.Destroy;
begin
  // Release the key binding object.
  FKeyBinding.Free;
  // Call inherited destructor.
  inherited Destroy;
end;

end.
//********************************** end of grabkey.pas *********************************

