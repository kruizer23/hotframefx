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
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

//***************************************************************************************
// Type Definitions
//***************************************************************************************
type

  { TKeyBindingForm }

  TKeyBindingForm = class(TForm)
    BtnCancel: TButton;
    BtnOk: TButton;
    MmoDebug: TMemo;
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
  private

  public

  end;


implementation

{$R *.lfm}

{ TKeyBindingForm }

//***************************************************************************************
// NAME:           FormKeyUp
// PARAMETER:      Sender Signal source.
//                 Key The key code of the regular key.
//                 Shift The key code of the special (shift, alt, ctrl, etc.) key.
// DESCRIPTION:    Called upon key up event.
//
//***************************************************************************************
procedure TKeyBindingForm.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  // Do not actually process the key.
  Key := 0;
end;

//***************************************************************************************
// NAME:           FormKeyDown
// PARAMETER:      Sender Signal source.
//                 Key The key code of the regular key.
//                 Shift The key code of the special (shift, alt, ctrl, etc.) key.
// DESCRIPTION:    Called upon key down event.
//
//***************************************************************************************
procedure TKeyBindingForm.FormKeyDown(Sender: TObject; var Key: Word;
                                      Shift: TShiftState);
begin
  // TODO ##Vg Hmmm...special keys such as LWIN will make the form lose focus and do
  // something else. How to override?
  MmoDebug.Lines.Add(IntToStr(Key));
  // TODO ##Vg
  Key := 0;
end;

end.
//********************************** end of keybindingunit.pas **************************


