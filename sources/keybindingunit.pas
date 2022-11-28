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
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls;

//***************************************************************************************
// Type Definitions
//***************************************************************************************
type

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
  private

  public

  end;


implementation

{$R *.lfm}

{ TKeyBindingForm }

// TODO ##Vg Hmmm...special keys such as LWIN will make the form loose focus and do
// something else. How to override?
// IDEA ##Vg Look at how the Lazarus IDE itself does it. You can find stuff in:
//   C:\lazarus\components\ideintf\propedits.pp
// Look at class TCustomShortCutGrabBox. Just not that I want a checkbox for the
// Super key. It can also be less complex. I just want support for:
//   - Keys: a..z, 0..9, F1..F12, Tab
//   - Shifts: Shift, Ctrl, Alt, Super.
//MmoDebug.Lines.Add(IntToStr(Key));
// TODO ##Vg
//Key := 0;


end.
//********************************** end of keybindingunit.pas **************************


