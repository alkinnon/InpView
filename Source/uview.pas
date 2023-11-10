{
file: uView.pas

Summary:
iView (inputView) form displays in set position when activated, then fades away.
}

unit uView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls,lcltype, Menus, Windows;

type

  { TiView }

  TiView = class(TForm)
    lblBigText: TLabel;
    lbLog: TListBox;
    procedure FormActivate(Sender: TObject);
    procedure updatePosition;
    procedure FormCreate(Sender: TObject);

    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FormMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private


  public
    ViewMode:Integer;

  end;

var
  iView: TiView;
  MouseIsDown: boolean;
  PX, PY: integer;


implementation

{$R *.lfm}


{ TiView }

procedure TiView.FormMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
    if Button = mbLeft then
  begin
    MouseIsDown := True;
    PX := X;
    PY := Y;
  end;
end;

procedure TiView.FormMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
    if MouseIsDown then begin
    SetBounds(Left + (X - PX), Top + (Y - PY), Width, Height);
  end
end;

procedure TiView.FormMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
    MouseIsDown:=False;
    updatePosition;
end;

procedure TiView.UpdatePosition;
var i:Integer;
    r:Trect;
    p:tpoint;
begin
  // rgViewMode index 0 is snap to bottom of current monitor
  if ViewMode=0 then begin
         with iView do
         begin
           // create a point p as the center of ivew form
           p:=TPoint.create(left+width Div 2, top+height div 2);
           for i:=0 to screen.MonitorCount-1 do
           begin
                r:=screen.Monitors[i].WorkareaRect;//.BoundsRect;
                if PtInRect(r,p) = true then
                begin // position inside monitor rect
                  width:=(r.Width Div 4)*3;
                  Left:=r.Left+((r.Width-width) div 2);
                  // top or bottom of the screen.
                  if p.Y < (r.Height div 2) then
                     Top := r.Top + GetSystemMetrics(SM_CYCAPTION)
                  else
                     Top:=r.bottom-GetSystemMetrics(SM_CYCAPTION)-height;
                end; // eo if PtInRect(r,p) = true
           end; // eo for i:=0 to screen.MonitorCount-1
         end; // eo with iView do
  end; // eo if ViewMode = 0
end;



procedure TiView.FormCreate(Sender: TObject);
begin
  Caption:='';
  BorderIcons:=[];
  BorderStyle:=bsSizeable;
end;

procedure TiView.FormActivate(Sender: TObject);
begin
  SetWindowLong(Handle, GWL_STYLE, GetWindowLong(Handle, GWL_STYLE) and not(WS_DLGFRAME));
  MoveWindow(Handle, Left, Top, Width-1, Height, True);
end;


end.

