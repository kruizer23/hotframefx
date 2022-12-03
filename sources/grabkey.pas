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
  Classes, SysUtils, Forms, Controls, Graphics, StdCtrls, KeyBinding, LCLType;

//***************************************************************************************
// Type Definitions
//***************************************************************************************
type
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

