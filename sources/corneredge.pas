unit CornerEdge;
//***************************************************************************************
//  Description: Hot corner and edge detection unit.
//    File Name: corneredge.pas
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
  Classes, SysUtils, Forms, ExtCtrls, Controls;

//***************************************************************************************
// Type Definitions
//***************************************************************************************
type
  // Set that lists all supported screen corners.
  TCorner = (coTopLeft = 0, coTopRight, coBottomLeft, coBottomRight);

  // Set that lists all supported screen edges.
  TEdge = (edLeft = 0, edRight, edTop, edBottom);

  // Enumeration will all supported sensitivity levels.
  TSensitivity = (seHigh = 0, seMedium, seLow);

  // Event handler for a hot corner.
  THotCornerEvent = procedure(Sender: TObject; Corner: TCorner) of object;

  // Event handler for a hot edge.
  THotEdgeEvent = procedure(Sender: TObject; Edge: TEdge) of object;

  // Hot corner and edge detection class.
  TCornerEdge = class(TObject)
  const
    UPDATE_INTERVAL_MS = 80;
    HOT_BORDER_HALF_LEN = 1;
    WARM_BORDER_HALF_LEN = 5;
  type
    TDetectState = (dsIdle, dsWaiting, dsTriggered);
  private
    FSensitivity: TSensitivity;
    FOnHotCorner: THotCornerEvent;
    FOnHotEdge: THotEdgeEvent;
    FUpdateTimer: TTimer;
    FCornerDetectState: TDetectState;
    FEdgeDetectState: TDetectState;
    FCorner: TCorner;
    FEdge: TEdge;
    FCornerWaitCount: Integer;
    FEdgeWaitCount: Integer;
    FHotCorners: array[Ord(coTopLeft)..Ord(coBottomRight)] of TRect;
    FWarmCorners: array[Ord(coTopLeft)..Ord(coBottomRight)] of TRect;
    procedure OnUpdateTimer(Sender: TObject);
    procedure UpdateCorners;
  public
    constructor Create;
    destructor Destroy; override;
    property Sensitivity: TSensitivity read FSensitivity write FSensitivity;
    property OnHotCorner: THotCornerEvent read FOnHotCorner write FOnHotCorner;
    property OnHotEdge: THotEdgeEvent read FOnHotEdge write FOnHotEdge;
  end;

implementation
//***************************************************************************************
// NAME:           Create
// DESCRIPTION:    Object constructor. Calls TObjects's constructor and initializes
//                 the fields to their default values.
//
//***************************************************************************************
constructor TCornerEdge.Create;
var
  idx: integer;
begin
  // Call inherited constructor.
  inherited Create;
  // Initialize fields.
  FSensitivity := seMedium;
  FOnHotCorner := nil;
  FOnHotEdge := nil;
  FCornerDetectState := dsIdle;
  FEdgeDetectState := dsIdle;
  FCornerWaitCount := 0;
  FEdgeWaitCount := 0;
  // Initialize all corner rects to be of zero size.
  for idx := Ord(coTopLeft) to Ord(coBottomRight) do
  begin
    FHotCorners[idx] := Rect(0, 0, 0, 0);
    FWarmCorners[idx] := Rect(0, 0, 0, 0);
  end;
  // Set the locations of all hot corner rects.
  FHotCorners[Ord(coTopLeft)].SetLocation(0, 0);
  FHotCorners[Ord(coTopRight)].SetLocation(Screen.PrimaryMonitor.BoundsRect.Width, 0);
  FHotCorners[Ord(coBottomLeft)].SetLocation(0, Screen.PrimaryMonitor.BoundsRect.Height);
  FHotCorners[Ord(coBottomRight)].SetLocation(Screen.PrimaryMonitor.BoundsRect.Width,
                                              Screen.PrimaryMonitor.BoundsRect.Height);
  // Move all warm corner rects to the hot corner rects and inflat all rects.
  for idx := Ord(coTopLeft) to Ord(coBottomRight) do
  begin
    FWarmCorners[idx].Location := FHotCorners[idx].Location;
    FHotCorners[idx].Inflate(HOT_BORDER_HALF_LEN, HOT_BORDER_HALF_LEN);
    FWarmCorners[idx].Inflate(WARM_BORDER_HALF_LEN, WARM_BORDER_HALF_LEN);
  end;
  // Construct, configure and start the update timer.
  FUpdateTimer := TTimer.Create(nil);
  FUpdateTimer.Interval := UPDATE_INTERVAL_MS;
  FUpdateTimer.OnTimer := @OnUpdateTimer;
  FUpdateTimer.Enabled := True;
end;

//***************************************************************************************
// NAME:           Destroy
// DESCRIPTION:    Object destructor. Calls TObjects's destructor
//
//***************************************************************************************
destructor TCornerEdge.Destroy;
begin
  // Stop and release the update timer.
  FUpdateTimer.Enabled := False;
  FUpdateTimer.Free;
  // Call inherited destructor.
  inherited Destroy;
end;

//***************************************************************************************
// NAME:           UpdateCorners
// DESCRIPTION:    Updates the corner state machine to detect hot corners. Should be
//                 called at fixed timer intervals.
//
//***************************************************************************************
procedure TCornerEdge.UpdateCorners;
var
  idx: integer;
begin
  // ----------------------------- IDLE -------------------------------------------------
  if FCornerDetectState = dsIdle then
  begin
    // Is the cursor in one of the hot corner rectangles?
    for idx := Ord(coTopLeft) to Ord(coBottomRight) do
    begin
      if FHotCorners[idx].Contains(Mouse.CursorPos) then
      begin
        // Store the corner that is now warm.
        FCorner := TCorner(idx);
        // Transition to the waiting state.
        FCornerDetectState := dsWaiting;
        // Stop the loop.
        Break;
      end;
    end;

    // Are we transitioning to the waiting state?
    if FCornerDetectState = dsWaiting then
    begin
      // Initialize the wait counter, based on the selected sensitivity.
      FCornerWaitCount := Ord(FSensitivity);
      // No need to wait (typically on high sensitivity)?
      if FCornerWaitCount = 0 then
      begin
        // Corner is now hot. Call the event handler.
        if Assigned(FOnHotCorner) then
        begin
          FOnHotCorner(Self, FCorner);
        end;
        // Transition to the triggered state instead.
        FCornerDetectState := dsTriggered;
      end;
    end;
  end
  // ----------------------------- WAITING ----------------------------------------------
  else if FCornerDetectState = dsWaiting then
  begin
    // Check if the cursor moved out of the warm corner.
    if not FWarmCorners[Ord(FCorner)].Contains(Mouse.CursorPos) then
    begin
      // Transition back to the idle state.
      FCornerDetectState := dsIdle;
    end;

    // Still in the waiting state?
    if FCornerDetectState = dsWaiting then
    begin
      // Decrement the wait counter.
      Dec(FCornerWaitCount);
      // Done waiting?
      if FCornerWaitCount = 0 then
      begin
        // Corner is now hot. Call the event handler.
        if Assigned(FOnHotCorner) then
        begin
          FOnHotCorner(Self, FCorner);
        end;
        // Transition to the triggered state.
        FCornerDetectState := dsTriggered;
      end;
    end;
  end
  // ----------------------------- TRIGGERED --------------------------------------------
  else if FCornerDetectState = dsTriggered then
  begin
    // Check if the cursor moved out of the corner.
    if not FWarmCorners[Ord(FCorner)].Contains(Mouse.CursorPos) then
    begin
      // Transition back to the idle state.
      FCornerDetectState := dsIdle;
    end;
  end;
end;

//***************************************************************************************
// NAME:           OnUpdateTimer
// PARAMETER:      Sender Source of the event.
// DESCRIPTION:    Event handler that gets called each time the update timer elapsed.
//
//***************************************************************************************
procedure TCornerEdge.OnUpdateTimer(Sender: TObject);
begin
  // Update the state machine to detect hot corners.
  UpdateCorners;
end;

end.
//********************************** end of corneredge.pas ******************************

