unit AppVersion;
//***************************************************************************************
//  Description: Access to application version information.
//    File Name: appversion.pas
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
  Classes, SysUtils, resource, versiontypes, versionresource, LCLVersion;

//***************************************************************************************
// Type Definitions
//***************************************************************************************
type

  { TAppVersion }

  TAppVersion = class (TObject)
  private
    FMajor: Word;
    FMinor: Word;
    FPatch: Word;
    function GetText: string;
  public
    constructor Create;
    destructor Destroy; override;
    property Major: Word read FMajor;
    property Minor: Word read FMinor;
    property Patch: Word read FPatch;
    property Text: string read GetText;
  end;

implementation

{ TAppVersion }

//***************************************************************************************
// NAME:           Create
// DESCRIPTION:    Object constructor. Calls TObjects's constructor and initializes
//                 the fields to their default values.  Note that this assumes that
//                 version information is enabled and set through Lazarus Project ->
//                 Project Options -> Version Info.
//
//***************************************************************************************
constructor TAppVersion.Create;
var
  VersionResource: TVersionResource;
  ResourceStream: TResourceStream;
begin
  // Call inherited constructor.
  inherited Create;
  // Initialize fields.
  FMajor := 0;
  FMinor := 0;
  FPatch := 0;
  // Construct version resource object.
  VersionResource := TVersionResource.Create;
  // Only continue if resource information is actually available.
  if FindResource(HINSTANCE, PChar(1), PChar(RT_VERSION)) <> 0 then
  begin
    // Create resource stream.
    ResourceStream := TResourceStream.CreateFromID(HINSTANCE, 1, PChar(RT_VERSION));
    try
      // Set stream to extract the version resource data from.
      VersionResource.SetCustomRawDataStream(ResourceStream);
      // Access version number fields. Note that accessing a property also results in
      // all version resource info to be loaded from the stream.
      FMajor := VersionResource.FixedInfo.FileVersion[0];
      FMinor := VersionResource.FixedInfo.FileVersion[1];
      FPatch := VersionResource.FixedInfo.FileVersion[2];
      // Clear the stream
      VersionResource.SetCustomRawDataStream(nil);
    finally
      FreeAndNil(ResourceStream);
    end;
  end;
  // Release the version resource object.
  FreeAndNil(VersionResource);
end;

//***************************************************************************************
// NAME:           Destroy
// DESCRIPTION:    Object destructor. Calls TObjects's destructor
//
//***************************************************************************************
destructor TAppVersion.Destroy;
begin
  inherited Destroy;
end;

//***************************************************************************************
// NAME:           GetCaption
// RETURN VALUE:   Version information string;
// DESCRIPTION:    Getter for the application's version information in its string
//                 representation.
//
//***************************************************************************************
function TAppVersion.GetText: string;
begin
  // Construct version information string as 'x.y.z'.
  Result := IntToStr(FMajor) + '.' + IntToStr(FMinor) + '.' + IntToStr(FPatch);
end;

end.
//********************************** end of appversion.pas ******************************

