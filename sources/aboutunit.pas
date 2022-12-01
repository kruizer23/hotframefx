unit AboutUnit;
//***************************************************************************************
//  Description: About information dialog.
//    File Name: aboutunit.pas
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
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, LCLIntf, AppVersion;

//***************************************************************************************
// Type definitions
//***************************************************************************************
//---------------------------------- TAboutDialog ---------------------------------------
type
  { TAboutDialog }
  TAboutDialog = class(TForm)
    BtnClose: TButton;
    GrpBackground: TGroupBox;
    ImgLogo: TImage;
    LblWebsite: TLabel;
    LblCopyright: TLabel;
    LblBuildInfo1: TLabel;
    LblBuildInfo2: TLabel;
    LblVersion: TLabel;
    TgbLicense: TToggleBox;
    procedure FormCreate(Sender: TObject);
    procedure LblWebsiteClick(Sender: TObject);
    procedure LblWebsiteMouseEnter(Sender: TObject);
    procedure LblWebsiteMouseLeave(Sender: TObject);
    procedure TgbLicenseChange(Sender: TObject);
  private
    { private declarations }
    FMmoLicense: TMemo;
    procedure InitializeGui;
  public
    { public declarations }
  end;


implementation

{$R *.lfm}

{ TAboutDialog }

//***************************************************************************************
// NAME:           InitializeGui
// PARAMETER:      None.
// RETURN VALUE:   None.
// DESCRIPTION:    Initializes the user interface components of the dialog.
//
//***************************************************************************************
procedure TAboutDialog.InitializeGui;
var
  AppVersion: TAppVersion;
  AppVersionStr: string;
begin
  // Adjust the height of the form, such that the memo with license info is not visible.
  Height := 217;
  // Create the application version information object and add the version info to the
  // form's caption.
  AppVersion := TAppVersion.Create;
  AppVersionStr := 'version ' + AppVersion.Caption;
  FreeAndNil(AppVersion);
  // set the version label
  LblVersion.Caption := 'HotFrameFx' + ' ' + AppVersionStr;
  // set the copyright info
  LblCopyright.Caption := 'Copyright ' + '©' + ' 2022 by Frank Voorburg';
  // set the website
  LblWebsite.Caption := 'View on GitHub';
  // Dynamically create the memo for the license text, which is initially invisible.
  FMmoLicense := TMemo.Create(Self);
  FMmoLicense.Parent := Self;
  FMmoLicense.Visible := False;
  FMmoLicense.Left := GrpBackground.Left + 5;
  FMmoLicense.Width := GrpBackground.Width - 10;
  FMmoLicense.Height := GrpBackground.Height - LblBuildInfo1.Top - 23;
  FMmoLicense.Top := GrpBackground.Top + GrpBackground.Height - FMmoLicense.Height - 5;
  FMmoLicense.Lines.Clear;
  with FMmoLicense.Lines do
  begin
    Add('This program is free software: you can redistribute it and/or modify');
    Add('it under the terms of the GNU General Public License as published by');
    Add('the Free Software Foundation, either version 3 of the License, or');
    Add('(at your option) any later version.');
    Add('');
    Add('This program is distributed in the hope that it will be useful,');
    Add('but WITHOUT ANY WARRANTY; without even the implied warranty of');
    Add('MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the');
    Add('GNU General Public License for more details.');
    Add('');
    Add('You should have received a copy of the GNU General Public License');
    Add('along with this program.  If not, see <https://www.gnu.org/licenses/>.');
  end;
  FMmoLicense.ReadOnly := True;
  FMmoLicense.ScrollBars := ssAutoBoth;
end;

//***************************************************************************************
// NAME:           lblWebsiteClick
// PARAMETER:      Sender Source of the event.
// RETURN VALUE:   none
// DESCRIPTION:    Event handler that gets called when the component was clicked.
//
//***************************************************************************************
procedure TAboutDialog.LblWebsiteClick(Sender: TObject);
begin
  // Open the browser and visit HotFrameFx on GitHub.
  OpenURL('https://github.com/kruizer23/hotframefx');
end;

//***************************************************************************************
// NAME:           LblWebLinkMouseEnter
// PARAMETER:      Sender Source of the event.
// RETURN VALUE:   none
// DESCRIPTION:    Event handler that gets called when the mouse entered the rect of the
//                 component.
//
//***************************************************************************************
procedure TAboutDialog.LblWebsiteMouseEnter(Sender: TObject);
begin
  // Switch mouse cursor to hand to indicate that it is a hyperlink.
  LblWebsite.Cursor := crHandPoint;
end;

//***************************************************************************************
// NAME:           lblWebsiteMouseLeave
// PARAMETER:      Sender Source of the event.
// RETURN VALUE:   none
// DESCRIPTION:    Event handler that gets called when the mouse left the rect of the
//                 component.
//
//***************************************************************************************
procedure TAboutDialog.LblWebsiteMouseLeave(Sender: TObject);
begin
  // Switch back to the default mouse cursor.
  LblWebsite.Cursor := crDefault;
end;

//***************************************************************************************
// NAME:           TgbLicenseChange
// PARAMETER:      Sender Source of the event.
// RETURN VALUE:   none
// DESCRIPTION:    Event handler that gets called when the component was clicked.
//
//***************************************************************************************
procedure TAboutDialog.TgbLicenseChange(Sender: TObject);
begin
  // Did the toggle box get checked?
  if TgbLicense.Checked then
  begin
    // Move the license memo into view.
    FMmoLicense.Visible := True;
  end
  else
  begin
    // Move the license memo out of view.
    FMmoLicense.Visible := False;
  end;
end;

//***************************************************************************************
// NAME:           FormCreate
// PARAMETER:      Sender Signal source.
// RETURN VALUE:   None.
// DESCRIPTION:    Event handler for when the form is created.
//
//***************************************************************************************
procedure TAboutDialog.FormCreate(Sender: TObject);
begin
  // initialize the user interface components
  InitializeGui;
end;

end.
//********************************** end of aboutunit.pas *******************************

