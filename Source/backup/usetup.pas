{
File: usetup.pas
Summary:
Main form used to manage setings and update the input view (uview.pas)

}

unit usetup;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, ComCtrls, ActnList, Menus, XMLPropStorage, Buttons, Windows,
  LazUTF8, RegExpr, vinfo, uAbout, uview, LCLIntf;

type
  StatusType = Integer;

  MouseLLHookStruct = record
      pt          : TPoint;
      mouseData   : cardinal;
      flags       : cardinal;
      time        : cardinal;
      dwExtraInfo : cardinal;
    end;
  { TSetup }

  TSetup = class(TForm)
    aAbout: TAction;
    aClr: TAction;
    aClearHistory: TAction;
    aInvertCol: TAction;
    aFont: TAction;
    aQuit: TAction;
    aSetup: TAction;
    Acts: TActionList;
    cbFade: TCheckBox;
    cbTransparent: TCheckBox;
    clrDlg: TColorDialog;
    FontDlg: TFontDialog;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    ImageList1: TImageList;
    Label1: TLabel;
    Label2: TLabel;
    lblFadeRate: TLabel;
    lblAlpha: TLabel;
    lblDelay: TLabel;
    MenuItem1: TMenuItem;
    MenuItem3: TMenuItem;
    PopupMenu1: TPopupMenu;
    rgViewMode: TRadioGroup;
    sbInvertClr: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    SpeedButton4: TSpeedButton;
    tbDelay: TTrackBar;
    tbOpacity: TTrackBar;
    tbFadeRate: TTrackBar;
    tmrStartUp: TTimer;
    tmrUpdate: TTimer;
    tmrFade: TTimer;
    TrayIcon1: TTrayIcon;
    XMLPropStorage1: TXMLPropStorage;
    procedure aAboutExecute(Sender: TObject);
    procedure aClrExecute(Sender: TObject);
    procedure aFontExecute(Sender: TObject);
    procedure aInvertColExecute(Sender: TObject);
    procedure aQuitExecute(Sender: TObject);
    procedure aSetupExecute(Sender: TObject);
    procedure cbTransparentChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure cbLockedChange(Sender: TObject);
    procedure Label2Click(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure rgViewModeClick(Sender: TObject);
    procedure tbDelayChange(Sender: TObject);
    procedure tbFadeRateChange(Sender: TObject);
    procedure tmrFadeTimer(Sender: TObject);
    procedure tmrStartUpTimer(Sender: TObject);
    procedure tmrUpdateTimer(Sender: TObject);
    procedure tbOpacityChange(Sender: TObject);
    procedure KeyListAdd(s:String; allowDuplicates:Boolean);
    procedure SetViewAlphablendValue;
    procedure SetViewAlphablendEnabled;
    procedure ResetViewAlphablendValue;


  private

    function IsStarted: Boolean;
    function TranslateVirtualKey(VirtualKey: integer): AnsiString;
    procedure applySettings;
  public
  end;

var
  Setup: TSetup;
  progDir:String;
  vString:String;// version string
  mHook : cardinal;
  TextString:String;
  lastX, lastY:Integer;
  lmbDown, rmbDown, mmbDown:Boolean;
  llKeyboardHook: HHOOK = 0;
  AltDown, ShiftDown, CtrlDown: Boolean;
  KeyBoardState: TKeyboardState;
  KeyBoardLayOut: HKL;
  KeyListLock:Boolean;

  LastTick: Word;

  // for form dragging
  MouseIsDown: boolean;
  PX, PY: integer;

implementation
{$R *.lfm}

function GetTickCount : DWORD;
 {On Windows, this is number of milliseconds since Windows was
   started. On non-Windows platforms, LCL returns number of
   milliseconds since Dec. 30, 1899, wrapped by size of DWORD.
   This value can overflow LongInt variable when checks turned on,
   so "wrap" value here so it fits within LongInt.
  Also, since same thing could happen with Windows that has been
   running for at least approx. 25 days, override it too.
   https://forum.lazarus.freepascal.org/index.php?topic=6099.0

   we use it to time input inactivity
   }
begin
{$IFDEF MSWINDOWS}
  Result := Windows.GetTickCount mod High(LongInt);
{$ELSE}
  Result := LclIntf.GetTickCount mod High(LongInt);
{$ENDIF}
end;


function ToUnicodeEx(wVirtKey, wScanCode: UINT; lpKeyState: PByte;
   pwszBuff: PWideChar; cchBuff: Integer; wFlags: UINT; dwhkl: HKL): Integer; stdcall; external 'user32.dll';

const
  //LLKHF_ALTDOWN = KF_ALTDOWN shr 8;
  WH_KEYBOARD_LL  = 13;


type
  PKBDLLHOOKSTRUCT = ^TKBDLLHOOKSTRUCT;
  TKBDLLHOOKSTRUCT = packed record
    vkCode: DWORD;
    scanCode: DWORD;
    flags: DWORD;
    time: DWORD;
    dwExtraInfo: DWORD;
  end;

{ TSetup }

 procedure createTextString;
 var i:Integer;
     s:String;
 begin
   // build the human readable key press string
   s:='';
   for i:=0 to iview.lbLog.Items.Count-1 do begin
       s+=iview.lbLog.Items.Strings[i];
       if (i<iview.lbLog.Items.Count-1) and (iview.lbLog.Items.Strings[i].Length>1) then s+=' + ';
   end;
   iview.lblBigText.caption:=s;

   // ensure the opacity is reset
   if setup.cbTransparent.checked=true then begin
      setup.ResetViewAlphablendValue();
   end;
 end;

procedure TSetup.KeyListAdd(s:String; allowDuplicates:Boolean);
var i, r:Integer ;
    thisKey:String;
begin
  lastTick:=GetTickCount;
  if KeyListLock then exit;
  KeyListLock:=true;
  if allowDuplicates=true then begin
    iview.lbLog.Items.Add(s);
  end else begin
     r:=-1;
     for i:=0 to iview.lbLog.Items.Count-1 do begin
         thisKey:=iview.lbLog.Items.Strings[i];
         if thisKey=s then  begin
           r:=i;break;
         end;
     end;
     if r=-1 then begin
       iview.lbLog.Items.Add(s);
     end;
  end;
  KeyListLock:=false;
end;

 procedure ClearKeyList;
 begin
    if KeyListLock then exit;
    KeyListLock:=true;
    iview.lbLog.Items.Clear;
    KeyListLock:=false;
 end;

 function isAlphaNumeric(s:String):Boolean;
 var  re:TRegExpr;
 begin
    re := TRegExpr.Create('\w');
    if re.Exec(s) then result:=true else result:=false;
 end;

function GetCharFromVirtualKey(Key: Word): string;
var
    keyboardState: TKeyboardState;
    asciiResult: Integer;
begin
    // Initialize keyboardState to zero
    ZeroMemory(@keyboardState, SizeOf(TKeyboardState));
    Result := ''; // Initialize the result variable

    GetKeyboardState(keyboardState) ;
    SetLength(Result, 2) ;
    asciiResult := ToAscii(key, MapVirtualKey(key, 0), keyboardState, @Result[1], 0) ;
    case asciiResult of
      0: Result := '';
      1: SetLength(Result, 1) ;
      2:;
      else
        Result := '';
    end;
end;

function getKeyChar (VirtualKey:Integer):AnsiString;
var
  keyboardState: TKeyboardState;
  UnicodeResult: AnsiString = '';
begin
  // Initialize keyboardState to zero
  ZeroMemory(@keyboardState, SizeOf(TKeyboardState));

  GetKeyboardState(keyboardState);
  SetLength(UnicodeResult, 2);

  if ToUnicodeEx(VirtualKey, MapVirtualKey(VirtualKey, 0), @keyboardState, @UnicodeResult[1], 2, 0, GetKeyboardLayout(0)) > 0 then
  begin
    result := UnicodeResult[1];
  end
  else
  begin
    // If ToUnicodeEx fails, use a default representation
    result := chr(VirtualKey);
  end;
end;

function LowLevelKeyboardHook(nCode: Integer; wParam: WPARAM; lParam: LPARAM): HRESULT; stdcall;
var
  pkbhs: PKBDLLHOOKSTRUCT;
  VirtualKey: integer;
  s: AnsiString;
begin
  //pkbhs := PKBDLLHOOKSTRUCT(Pointer(lParam));
  pkbhs := PKBDLLHOOKSTRUCT(IntPtr(lParam));

  if nCode = HC_ACTION then
  begin
    VirtualKey := pkbhs^.vkCode;

    //Alt key state
    if ((VirtualKey = VK_LMENU) or (VirtualKey = VK_RMENU)) and (not AltDown) then
    begin
       s:='[Alt]'; Setup.KeyListAdd(s,false);
       AltDown:=true;
       if (VirtualKey = VK_LMENU) then VirtualKey:=VirtualKey -VK_LMENU;
       if (VirtualKey = VK_RMENU) then VirtualKey:=VirtualKey -VK_RMENU;
    end;
    if (wParam = WM_KEYUP) and ((VirtualKey = VK_LMENU) or (VirtualKey = VK_RMENU)) and (AltDown) then
    begin
       AltDown:=false;
    end;

    // Ctrl
    if ((VirtualKey = VK_LCONTROL) or (VirtualKey = VK_RCONTROL)) and (not CtrlDown) then begin
       s:='[Ctrl]'; Setup.KeyListAdd(s,false);
       CtrlDown := True;
       if (VirtualKey = VK_LCONTROL) then VirtualKey:=VirtualKey -VK_LCONTROL;
       if (VirtualKey = VK_RCONTROL) then VirtualKey:=VirtualKey -VK_RCONTROL;
    end;

    if (wParam = WM_KEYUP) and ((VirtualKey = VK_LCONTROL) or (VirtualKey = VK_RCONTROL)) and (ctrlDown) then
    begin
       CtrlDown := False;
    end;

    //Shift key
    if ((VirtualKey = VK_LSHIFT) or (VirtualKey = VK_RSHIFT)) and not (shiftDown) then
    begin
       s:='[Shift]'; Setup.KeyListAdd(s,false);
       ShiftDown:=true;
    end;

    if (wParam = WM_KEYUP) and ((VirtualKey = VK_LSHIFT) or (VirtualKey = VK_RSHIFT)) and (shiftDown) then
    begin
       ShiftDown:=false;
    end;

    //Other Virtual Keys
    if (wParam = WM_KEYDOWN) and
          ((VirtualKey <> VK_LMENU) and (VirtualKey <> VK_RMENU)) and  //not Alt
           (VirtualKey <> VK_LSHIFT) and (VirtualKey <> VK_RSHIFT) and // not Shift
            (VirtualKey <> VK_LCONTROL) and (VirtualKey <> VK_RCONTROL) then //not Ctrl
    begin
      s:= Setup.TranslateVirtualKey(VirtualKey);

      if (s='') then
      begin
        //s := chr(VirtualKey); // if its a character, then return the character. Fails to return non alpha-numeric symbols such as -=_+{}[]<>?:@~#';/.,'
        s:= getKeyChar(VirtualKey);
      end;
      // if s is a single character, only use it if the keylist has kjuhkjh
      if (s.Length = 1) and (iview.lbLog.Items.Count>0) then
      begin
           if (iview.lbLog.Items[iview.lbLog.items.Count-1].Length > 1) then Setup.KeyListAdd(s,true);
      end else
      begin
           Setup.KeyListAdd(s,true);
      end;
    end;
  end;
  createTextString;
  Result := CallNextHookEx(llKeyboardHook, nCode, wParam, lParam);
end;

function TSetup.IsStarted: Boolean;
begin
  Result := (llKeyboardHook <> 0)
end;

function TSetup.TranslateVirtualKey(VirtualKey: integer): AnsiString;
begin
  Result := '';
  {$Region 'Translate VirtualKey'}
  case VirtualKey of
    VK_RETURN:   Result := sLineBreak;
    VK_TAB:      Result := '     ';
    VK_BACK:     Result := '[BackSpace]';
    VK_SHIFT:    Result := '[Shift]';
    VK_CONTROL:  Result := '[Ctrl]';
    VK_MENU:     Result := '[Alt]';
    VK_ESCAPE:   Result := '[Esc]';
    VK_PAUSE: 	 Result := '[Pause]';
    VK_CAPITAL:  Result := '[Caps Lock]';
    VK_PRIOR: 	 Result := '[Page Up]';
    VK_NEXT: 	 Result := '[Page Down]';
    VK_END: 	 Result := '[End]';
    VK_HOME: 	 Result := '[Home]';
    VK_LEFT:     Result := '[Left Arrow]';
    VK_UP: 	 Result := '[Up Arrow]';
    VK_RIGHT: 	 Result := '[Right Arrow]';
    VK_DOWN: 	 Result := '[Down Arrow]';
    VK_SELECT: 	 Result := '[Select]';
    VK_PRINT: 	 Result := '[Print Screen]';
    VK_EXECUTE:  Result := '[Execute]';
    VK_SNAPSHOT: Result := '[Print]';
    VK_INSERT: 	 Result := '[Ins]';
    VK_DELETE:   Result := '[Del]';
    VK_HELP: 	 Result := '[Help]';
    VK_F1:   	 Result := '[F1]';
    VK_F2: 	 Result := '[F2]';
    VK_F3: 	 Result := '[F3]';
    VK_F4: 	 Result := '[F4]';
    VK_F5: 	 Result := '[F5]';
    VK_F6: 	 Result := '[F6]';
    VK_F7: 	 Result := '[F7]';
    VK_F8: 	 Result := '[F8]';
    VK_F9: 	 Result := '[F9]';
    VK_F10: 	 Result := '[F10]';
    VK_F11: 	 Result := '[F11]';
    VK_F12: 	 Result := '[F12]';
    VK_NUMPAD0:  Result := '0';
    VK_NUMPAD1:  Result := '1';
    VK_NUMPAD2:  Result := '2';
    VK_NUMPAD3:  Result := '3';
    VK_NUMPAD4:  Result := '4';
    VK_NUMPAD5:  Result := '5';
    VK_NUMPAD6:  Result := '6';
    VK_NUMPAD7:  Result := '7';
    VK_NUMPAD8:  Result := '8';
    VK_NUMPAD9:  Result := '9';
    VK_SEPARATOR:Result := '+';
    VK_SUBTRACT: Result := '-';
    VK_DECIMAL:  Result := '.';
    VK_DIVIDE:   Result := '/';
    VK_NUMLOCK:  Result := '[Num Lock]';
    VK_SCROLL: 	 Result := '[Scroll Lock]';
    VK_PLAY:     Result := '[Play]';
    VK_ZOOM:     Result := '[Zoom]';
    VK_LWIN,
    VK_RWIN:     Result := '[Win Key]';
    VK_APPS:     Result := '[Menu]';
   else result:='';
   end;
   {$EndRegion}
end;


function LowLevelMouseHookProc(nCode: integer; wParam: WPARAM; lParam : LPARAM): LRESULT; stdcall;
var
  info : ^MouseLLHookStruct absolute lParam;
  s:String;
begin
  s:='';
  result := CallNextHookEx(mHook, nCode, wParam, lParam);
  with info^ do begin
     if (wParam = wm_lbuttondown) and (not lmbDown) then begin
        s:='(LMB)'; Setup.KeyListAdd(s,false);
        lmbDown:=True;
     end;
     if (wParam = wm_lbuttonup) and (lmbDown) then begin
        lmbDown:=false;
     end;
    case wParam of
      wm_mbuttondown : begin s:='(MMB)'; Setup.KeyListAdd(s,false); mmbDown:=true; end;
      wm_mbuttonup   : begin mmbDown:=false; end;
      wm_rbuttondown : begin s:='(RMB)';Setup.KeyListAdd(s,false); rmbDown:=true; end;
      wm_rbuttonup   : begin rmbDown:=true; end;
      wm_mousewheel  : begin
        if smallInt(mouseData shr 16) > 0
        then begin s:='(Wheel Up)'; Setup.KeyListAdd(s,false); end
        else begin s:='(Wheel Down)'; Setup.KeyListAdd(s,false); end;
      end;
    end;

    if (lmbDown=true) and ((pt.x<lastX-5) or (pt.x>lastX+5) or (pt.y<lastY-5) or (pt.y>lastY+5)) then begin
          s:='(Drag)';
          Setup.KeyListAdd(s,false);
    end;
    lastx:=pt.x;
    lasty:=pt.y;
  end;
  if s<>'' then createTextString;
end;

{ TSetup }


function FormExists(apForm: TForm): boolean;
var
  i: Word;
begin
  Result := False;
  for i := 0 to Screen.FormCount - 1 do
    if Screen.Forms[i] = apForm then
    begin
      Result := True;
      Break;
    end;
end;

// ----------------------------------------------------------- View form modifier methods
procedure TSetup.SetViewAlphablendValue;
var f:double;
begin
  f:= (100 / 255) * tbOpacity.Position;
  lblAlpha.Caption:=floattostrf(f,fffixed,2,0)  +'%';
  ResetViewAlphablendValue;
end;

 procedure TSetup.ResetViewAlphablendValue;
 begin
   if isStarted=false then exit;// prevent runtime errors modifying non-existant objects
   if cbTransparent.checked=true then begin
     iview.AlphaBlend:=true;
     iview.AlphaBlendValue:=255-tbOpacity.position;
   end else begin
     iview.AlphaBlend:=false;
     iview.AlphaBlendValue:=255;
   end;
 end;

 procedure TSetup.SetViewAlphablendEnabled;
 begin
   if isStarted=false then exit;
   iview.AlphaBlend:=cbTransparent.checked;
 end;

procedure TSetup.cbTransparentChange(Sender: TObject);
begin
     SetViewAlphablendEnabled;
end;

procedure TSetup.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  Setup.Hide;
end;

procedure TSetup.aQuitExecute(Sender: TObject);
begin
  application.Terminate;
end;

procedure TSetup.aSetupExecute(Sender: TObject);
begin
  Setup.Show;
end;

procedure TSetup.aFontExecute(Sender: TObject);
begin
  FontDlg.Font:=iview.lblBigText.Font;
  if FontDlg.Execute then
  begin
      iview.lblBigText.Font:=FontDlg.Font;
  end;
end;

procedure TSetup.aAboutExecute(Sender: TObject);
begin
  fAbout.Show;
end;

procedure TSetup.aClrExecute(Sender: TObject);
begin
  clrDlg.Color:=iview.lblBigText.Color;
  if clrDlg.Execute then begin
     iview.lblBigText.Color:=clrDlg.Color;
  end;
end;

procedure TSetup.aInvertColExecute(Sender: TObject);
begin
  if Setup.tag=1 then begin
     iview.lblBigText.Color:=clWhite;
     iview.lblBigText.Font.Color:=clBlack;
     Setup.tag:=0;
  end else begin
    iview.lblBigText.Color:=clBlack;
    iview.lblBigText.Font.Color:=clWhite;
    Setup.tag:=1;
  end;
end;

procedure TSetup.FormCreate(Sender: TObject);

var
  Info: TVersionInfo;

begin
  // program information
  progDir:= ExtractFilePath(ParamStr(0));
  Info := TVersionInfo.Create;
  Info.Load(HINSTANCE);
  vString := 'InpView Version '
             + IntToStr(Info.FixedInfo.FileVersion[0])
             + '.' + IntToStr(Info.FixedInfo.FileVersion[1])
             + '.' + IntToStr(Info.FixedInfo.FileVersion[2])
             + '.' + IntToStr(Info.FixedInfo.FileVersion[3]);
  Info.Free;
  //uvkeys.populate;
end;

procedure TSetup.FormDestroy(Sender: TObject);
begin
  UnhookWindowsHookEx(llKeyboardHook);
  UnhookWindowsHookEx(mHook);
end;

procedure TSetup.FormResize(Sender: TObject);
begin
  Setup.Update;
end;

procedure TSetup.cbLockedChange(Sender: TObject);
begin
  applySettings
end;

procedure TSetup.Label2Click(Sender: TObject);
begin
     OpenDocument(TLabel(Sender).Caption);
end;

procedure TSetup.MenuItem1Click(Sender: TObject);
begin
  Setup.Show;

end;

procedure TSetup.applySettings;
begin
  if FormExists(iview) then begin
    // cbLocked
    iview.BorderStyle:=bsNone;
    iview.ViewMode:=rgViewMode.ItemIndex;
    iview.updatePosition;
  end;// eo if form exists
end;

procedure TSetup.rgViewModeClick(Sender: TObject);
begin
  applySettings;
end;

procedure TSetup.tbDelayChange(Sender: TObject);
begin
  tmrUpdate.Interval:=(tbDelay.Position*1000) div 2;
  lblDelay.Caption:=inttostr(tbDelay.Position)+' secs';
end;

procedure TSetup.tbFadeRateChange(Sender: TObject);
begin
  tmrFade.Interval:=tbFadeRate.Position;
  lblFadeRate.Caption:=inttostr(tmrFade.Interval)+' ms';
end;

procedure TSetup.tmrFadeTimer(Sender: TObject);
begin
     if iview.AlphaBlendValue>0 then
        iview.AlphaBlendValue:=iview.AlphaBlendValue-tmrFade.tag
     else
     begin
       tmrFade.Enabled:=false;
       iview.lblBigText.Caption:='';
     end;
end;

procedure TSetup.tmrStartUpTimer(Sender: TObject);
const
  WH_MOUSE_LL = 14;
begin
  // set up the application if our iview form has loaded
  // set up the iview
  if FormExists(iview) then begin
     applySettings;
     iview.Show;
     iview.lblBigText.caption:=vString;
     tmrStartup.Enabled:=false;
     // start the keyboard and mouse hooks
     llKeyboardHook := SetWindowsHookEx(WH_KEYBOARD_LL, @LowLevelKeyboardHook, HInstance, 0);
     mHook := SetWindowsHookEx(WH_MOUSE_LL, @LowLevelMouseHookProc, hInstance, 0);
     tmrFade.Enabled:=true;
  end;
end;



procedure TSetup.tmrUpdateTimer(Sender: TObject);
var CurrTick,TickDiff:Word;
begin
    if KeyListLock then exit;
    CurrTick:=GetTickCount;
    TickDiff:=CurrTick-LastTick;
    if (TickDiff>tmrUpdate.Interval*2) and (not CtrlDown) and (not AltDown) and (not ShiftDown) then
    begin
      // idle interval passed so update the history
      //iview.lblBigText.Caption:='';
      ClearKeyList;
    end else
    begin
      // if fade is enabled then ensure the fade timer is on
      if cbFade.Checked=true then tmrFade.enabled:=true else
      begin
        tmrFade.enabled:=false;
      end;
    end;
end;

procedure TSetup.tbOpacityChange(Sender: TObject);
begin
  SetViewAlphaBlendValue;
end;

end.

