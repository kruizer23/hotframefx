unit PickApp;
//***************************************************************************************
//  Description: Generic frame for selecting the executable of an application.
//    File Name: pickapp.pas
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
  Classes, SysUtils, Forms, Controls, EditBtn, FileUtil;

//***************************************************************************************
// Type Definitions
//***************************************************************************************
type

  { TPickAppFrame }

  TPickAppFrame = class(TFrame)
    FileNameEdit1: TFileNameEdit;
  private
    function GetFileName: string;
    function GetInitialDir: string;
    procedure SetFileName(AValue: string);
    procedure SetInitialDir(AValue: string);
  public
    constructor Create(TheOwner: TComponent); override;
    property FileName: string read GetFileName write SetFileName;
    property InitialDir: string read GetInitialDir write SetInitialDir;
  published
  end;

implementation

{$R *.lfm}

{ TPickAppFrame }

//***************************************************************************************
// NAME:           GetFileName
// RETURN VALUE:   Filename with full path.
// DESCRIPTION:    Getter for the filename.
//
//***************************************************************************************
function TPickAppFrame.GetFileName: string;
begin
  Result := FileNameEdit1.FileName;
end;

//***************************************************************************************
// NAME:           GetInitialDir
// RETURN VALUE:   Initial directory.
// DESCRIPTION:    Getter for the initial directory used by the file open dialog.
//
//***************************************************************************************
function TPickAppFrame.GetInitialDir: string;
begin
  Result := FileNameEdit1.InitialDir;
end;

//***************************************************************************************
// NAME:           SetFileName
// PARAMETER:      Filename with full path.
// DESCRIPTION:    Setter for the filename.
//
//***************************************************************************************
procedure TPickAppFrame.SetFileName(AValue: string);
begin
  // Only change the filename if it actually exists.
  if FileExists(AValue) then
  begin
    FileNameEdit1.FileName := AValue;
  end;
end;

//***************************************************************************************
// NAME:           SetInitialDir
// PARAMETER:      Initial directory.
// DESCRIPTION:    Setter for the initial directory used by the file open dialog.
//
//***************************************************************************************
procedure TPickAppFrame.SetInitialDir(AValue: string);
begin
  // Only change the directory if it actually exists.
  if DirectoryExists(AValue) then
  begin
    FileNameEdit1.InitialDir := AValue;
  end;
end;

//***************************************************************************************
// NAME:           Create
// DESCRIPTION:    Frame constructor.
//
//***************************************************************************************
constructor TPickAppFrame.Create(TheOwner: TComponent);
var
  ExeExt: string;
begin
  // Call inherited constructor.
  inherited Create(TheOwner);
  // Configure the filter based on the file extension of an executable.
  ExeExt := GetExeExt;
  if ExeExt <> '' then
  begin
    FileNameEdit1.Filter := 'Executables|*' + ExeExt + '|All files|*';
  end
  else
  begin
    FileNameEdit1.Filter := 'All files|*';
  end;
end;

end.
//********************************** end of pickapp.pas *********************************

