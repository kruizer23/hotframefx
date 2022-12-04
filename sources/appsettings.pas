unit AppSettings;
//***************************************************************************************
//  Description: Application settings class.
//    File Name: appsettings.pas
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
  Classes, SysUtils, LazFileUtils, XMLConf, CornerEdge;

//***************************************************************************************
// Type Definitions
//***************************************************************************************
type

  { TAppSetings }

  TAppSetings = class(TObject)
  private
    FSettingsFile: string;
    FFirstRun: Boolean;
    FAutoStart: Boolean;
    FSensitivity: TSensitivity;
    FActionTopLeft: string;
    FActionTopRight: string;
    FActionBottomLeft: string;
    FActionBottomRight: string;
    FActionTop: string;
    FActionBottom: string;
    FActionLeft: string;
    FActionRight: string;
    procedure Defaults;
    procedure InitSettingsFile;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Load;
    procedure Save;
    property FirstRun: Boolean read FFirstRun write FFirstRun;
    property AutoStart: Boolean read FAutoStart write FAutoStart;
    property Sensitivity: TSensitivity read FSensitivity write FSensitivity;
    property ActionTopLeft: string read FActionTopLeft write FActionTopLeft;
    property ActionTopRight: string read FActionTopRight write FActionTopRight;
    property ActionBottomLeft: string read FActionBottomLeft write FActionBottomLeft;
    property ActionBottomRight: string read FActionBottomRight write FActionBottomRight;
    property ActionTop: string read FActionTop write FActionTop;
    property ActionBottom: string read FActionBottom write FActionBottom;
    property ActionLeft: string read FActionLeft write FActionLeft;
    property ActionRight: string read FActionRight write FActionRight;
  end;


implementation

{ TAppSetings }

//***************************************************************************************
// NAME:           Defaults
// DESCRIPTION:    Initialize all application settings to their default values.
//
//***************************************************************************************
procedure TAppSetings.Defaults;
begin
  FFirstRun := True;
  FAutoStart := False;
  FSensitivity := seMedium;
  FActionTopLeft := '';
  FActionTopRight := '';
  FActionBottomLeft := '';
  FActionBottomRight := '';
  FActionTop := '';
  FActionBottom := '';
  FActionLeft := '';
  FActionRight := '';
end;

//***************************************************************************************
// NAME:           InitSettingsFile
// DESCRIPTION:    Initializes the settings file that should be used for reading and
//                 writing the application settings.
//
//***************************************************************************************
procedure TAppSetings.InitSettingsFile;
var
 AppSettingsDir: string;
begin
  // Obtain the filename for the settings file.
  FSettingsFile := GetAppConfigFile(False, False);
  // Extract its directory.
  AppSettingsDir := ExtractFilePath(FSettingsFile);
  // Double check that the directory is actually there.
  if not DirectoryExists(AppSettingsDir) then
  begin
    // Force the directory creation.
    ForceDirectories(AppSettingsDir);
  end;
  // Now double-check that the directory is there and is writable
  if (not DirectoryExists(AppSettingsDir)) or
     (not DirectoryIsWritable(AppSettingsDir)) then
  begin
    // Set the filename to an invalid value to indicate that we cannot use it.
    FSettingsFile := '';
  end;
end;

//***************************************************************************************
// NAME:           Create
// DESCRIPTION:    Object constructor.
//
//***************************************************************************************
constructor TAppSetings.Create;
begin
  // Call inherited constructor.
  inherited Create;
  // Initialize settings to their defaults.
  Defaults;
  // Initialize the settings file.
  InitSettingsFile;
end;

//***************************************************************************************
// NAME:           Destroy
// DESCRIPTION:    Object destructor.
//
//***************************************************************************************
destructor TAppSetings.Destroy;
begin
  // Call inherited destructor.
  inherited Destroy;
end;

//***************************************************************************************
// NAME:           Load
// DESCRIPTION:    Loads the application settings to an XML file.
//
//***************************************************************************************
procedure TAppSetings.Load;
var
  XmlConfig: TXMLConfig;
  SensitivityVal: Integer;
begin
  // Only load if the settings file was properly configured.
  if FSettingsFile = '' then
    Exit;
  // Only load if the settings file actually exists.
  if not FileExists(FSettingsFile) then
    Exit;

  // Create and prepare the XML config object.
  XmlConfig := TXMLConfig.Create(nil);
  XmlConfig.StartEmpty := False;
  XmlConfig.Filename := FSettingsFile;
  // --------------- Generic configuration settings -------------------------------------
  XmlConfig.OpenKey('Generic');
  FFirstRun := XmlConfig.GetValue('FirstRun', True);
  FAutoStart := XmlConfig.GetValue('AutoStart', False);
  SensitivityVal := XmlConfig.GetValue('Sensitivity', Ord(seMedium));
  if (SensitivityVal < Ord(Low(TSensitivity))) or
     (SensitivityVal > Ord(High(TSensitivity))) then
    SensitivityVal := Ord(seMedium);
  FSensitivity := TSensitivity(SensitivityVal);
  XmlConfig.CloseKey;
  // --------------- Action configuration settings --------------------------------------
  XmlConfig.OpenKey('Actions');
  FActionTopLeft := String(XmlConfig.GetValue('ActionTopLeft', ''));
  FActionTopRight := String(XmlConfig.GetValue('ActionTopRight', ''));
  FActionBottomLeft := String(XmlConfig.GetValue('ActionBottomLeft', ''));
  FActionBottomRight := String(XmlConfig.GetValue('ActionBottomRight', ''));
  FActionTop := String(XmlConfig.GetValue('ActionTop', ''));
  FActionBottom := String(XmlConfig.GetValue('ActionBottom', ''));
  FActionLeft := String(XmlConfig.GetValue('ActionLeft', ''));
  FActionRight := String(XmlConfig.GetValue('ActionRight', ''));
  XmlConfig.CloseKey;
  // Release the XML config object.
  XmlConfig.Free;
end;

//***************************************************************************************
// NAME:           Save
// DESCRIPTION:    Stores the application settings to an XML file.
//
//***************************************************************************************
procedure TAppSetings.Save;
var
  XmlConfig: TXMLConfig;
begin
  // Only save if the settings file was properly configured.
  if FSettingsFile = '' then
    Exit;

  // Create and prepare the XML config object.
  XmlConfig := TXMLConfig.Create(nil);
  XmlConfig.StartEmpty := True;
  XmlConfig.Filename := FSettingsFile;
  // --------------- Generic configuration settings -------------------------------------
  XmlConfig.OpenKey('Generic');
  XmlConfig.SetValue('FirstRun', False); // If we're saving, then that was the first run.
  XmlConfig.SetValue('AutoStart', FAutoStart);
  XmlConfig.SetValue('Sensitivity', Ord(FSensitivity));
  XmlConfig.CloseKey;
  // --------------- Action configuration settings --------------------------------------
  XmlConfig.OpenKey('Actions');
  XmlConfig.SetValue('ActionTopLeft', WideString(FActionTopLeft));
  XmlConfig.SetValue('ActionTopRight', WideString(FActionTopRight));
  XmlConfig.SetValue('ActionBottomLeft', WideString(FActionBottomLeft));
  XmlConfig.SetValue('ActionBottomRight', WideString(FActionBottomRight));
  XmlConfig.SetValue('ActionTop', WideString(FActionTop));
  XmlConfig.SetValue('ActionBottom', WideString(FActionBottom));
  XmlConfig.SetValue('ActionLeft', WideString(FActionLeft));
  XmlConfig.SetValue('ActionRight', WideString(FActionRight));
  XmlConfig.CloseKey;
  // Write the contents and release the XML config object.
  XmlConfig.Flush;
  XmlConfig.Free;
end;

end.
//********************************** end of appsettings.pas *****************************
