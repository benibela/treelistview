(***
  This unit contains the class TSearchBar which implements a Mozilla like
  search bar.

  @author Benito van der Zander (http://www.benibela.de)
*)
unit findControl;

{$ifdef fpc}
{$mode delphi}
{$endif}
{$H+}

interface

uses
  Classes, SysUtils, StdCtrls, ExtCtrls, Controls,Buttons,Graphics
  {$ifdef lcl},LCLType,LCLIntf{$else},windows,messages{$endif};

{$IFNDEF LCL}
const
  WM_USER_VISIBLE_CHANGED = WM_USER+1150;
{$ENDIF}
type
TFindState = set of (fsFound, fsLoopAround);


TSearchBarSubControl = (fscCloseButton, //**< This is a close button at the left side
                        fscCaption, //**< This is a label next to the close button and before the search edit
                        fscSelectLocation, //**< This is a combobox next to the search edit
                        fscSearchForward, //**< This is a forward search button next to the location combobox
                        fscSearchBackwards, //**< This is a backward search button next to the forward one
                        fscHighlight, //**< This is a highlight all button (which changes its down state)
                        fscStatus); //**< This is a label showing the search result state
TSearchBarSubControls = set of TSearchBarSubControl;
//**This event is called when something should be searched
//**@param incremental This is true when the new search continues an old one, e.g. it is called during typing for every pressed key
//**@param backwards This is true when the search should be occur backwards, e.g. the user press shift+enter or the backward search button
TSearchEvent = procedure (sender: TObject;  incremental,backwards: boolean) of object;

{ TFindControl }
                                                   //todo: highlight shortcut, dblclick, doku, delphi
{ TSearchBar }

{** @abstract This class implements a Mozilla like search bar
  You can use it this way:
  @longCode(#
    SearchBar:=TSearchBar.create(self);  //replace self with the control which should contain the search bar
    SearchBar.Parent:=self;              //the search bar is automatically placeted at the bottom (align)
    SearchBar.OnSearch:=SearchBarSearch; //this must be an event handler which is than called whenever something should be searched
  #)
}
TSearchBar = class (TPanel)
private
  FFindState: TFindState;
  FFoundColor: TColor;
  FHighlightChanged: TNotifyEvent;
  FHighlighting: boolean;
  FIgnoreNextKey:longint;
  FLoopAroundState: string;
  FNotFoundColor: TColor;
  FNotFoundState: string;
  FOldHeight: longint;
  FCaption: string;
  FOnClose: TNotifyEvent;
  FOnSearch: TSearchEvent;
  FOnShow: TNotifyEvent;
  FsearchBackwardText: string;
  FSearchForwardText: string;
  FHighlightText:string;
  FSubComponents: TSearchBarSubControls;
  function GetHighlighting: boolean;
  function GetSearchLocation: longint;
  function GetSearchLocations: TStrings;
  function GetSearchText: string;
  procedure SetCaption(const AValue: string);
  procedure SetFindState(const AValue: TFindState);
  procedure SetHighlightText(const AValue: string);
  procedure SetsearchBackwardText(const AValue: string);
  procedure SetSearchForwardText(const AValue: string);
  procedure SetSearchLocation(const AValue: longint);
  procedure SetSubComponents(const AValue: TSearchBarSubControls);

  procedure closeBtnClick(Sender: TObject);
  procedure searchButtonClick(Sender: TObject);
  procedure searchEdtKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  procedure searchEdtKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
  procedure highlightClick(Sender: TObject);
  procedure highlightBtnMouseUp(Sender: TObject; {%H-}Button: TMouseButton; {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: Integer);
protected
  closeBtn, searchForwardBtn, searchBackwardBtn, highlightBtn: TSpeedButton;
  captionLbl,statusLabel: TLabel;
  locationsCmb: TComboBox;
  searchEdt: TEdit;
  visibleSubControls: array of tcontrol;
  procedure updateComponents;
  procedure moveComponents;
  {$ifdef lcl}
  procedure DoOnResize; override;
 // procedure ResizeDelayedAutoSizeChildren; override;
  procedure SetVisible(Value: Boolean); override;
  {$else}
  procedure VisibleChanging; override;
  procedure VisibleChangedMessage(var msg: TMsg); message WM_USER_VISIBLE_CHANGED;
  {$endif}
  procedure DoSearch(incremental, backwards: boolean);
public
  property SearchText: string read GetSearchText; //**<This is the text to search, entered by the user
  property SearchLocation: longint read GetSearchLocation write SetSearchLocation;  //**< Currently selected combobox item
  property SearchLocations: TStrings read GetSearchLocations; //**<ComboBox-items
  property Highlighting: boolean read GetHighlighting; //**<State of the highlight all button
  property FindState: TFindState read FFindState write SetFindState; //**< Set this to the result of the search operation
  procedure SetFocus;override;//**< focuses the search text edit
  constructor Create(TheOwner: TComponent);override;
  destructor destroy();override;
published
  property OnSearch: TSearchEvent read FOnSearch write FOnSearch; //**<This is called when the text should be searched, e.g. when the user clicks the buttons or types in the edit control
  property OnClose: TNotifyEvent read FOnClose write FOnClose; //**< This is called when the search bar is closed (close button, or escape key)
  property OnShow: TNotifyEvent read FOnShow write FOnShow; //**< This is called when the search bar is opened (setvisible/show)
  property OnHighlightChanged: TNotifyEvent read FHighlightChanged write FHighlightChanged;//**< Called when the highlight button is pressed
  property OnKeyDown; //**< Typical OnKeyDown-event. Setting key:=0, will prevent the default handling

  property SubComponents: TSearchBarSubControls read FSubComponents write SetSubComponents; //**< This is a set specifies which sub components should be created @br The default is [fscCloseButton, fscCaption, fscSearchForward, fscSearchBackwards, fscStatus] @seealso TSearchBarSubControl
  property Caption: string read FCaption write SetCaption; //**< @noautolink Caption of the search bar (needs fscCaption)
  property SearchForwardText: string read FSearchForwardText write SetSearchForwardText; //**< @noautolink Caption of the button for forward search (needs fscSearchForward)
  property SearchBackwardText: string read FsearchBackwardText write SetsearchBackwardText; //**< @noautolink Caption of the backward button (needs fscSearchBackwards)
  property HighlightText: string read FHighlightText write SetHighlightText; //**< @noautolink Caption of the highlight all button (needs fscHighlight)
  property FoundColor: TColor read FFoundColor write FFoundColor; //**< Background-color of the edit control when the text is found
  property NotFoundColor: TColor read FNotFoundColor write FNotFoundColor; //**< Background-color of the edit control when the text is not found
  property NotFoundState: string read FNotFoundState write FNotFoundState; //**< Text to display when the text is not found (needs fscStatus)
  property LoopAroundState: string read FLoopAroundState write FLoopAroundState; //**< Text to display when the text has only been found after looping around (needs fscStatus)
end;

procedure Register;

implementation
uses math
     {$ifdef lcl},LResources{$endif} //for icon
     ;

{ TSearchBar }

procedure TSearchBar.closeBtnClick(Sender: TObject);
begin
  Visible:=false;
  if assigned(OnClose) then OnClose(self);
end;

procedure TSearchBar.searchButtonClick(Sender: TObject);
begin
  DoSearch(false,sender=searchBackwardBtn);
end;

procedure TSearchBar.searchEdtKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
 var oldKey: word;
begin
  oldKey:=key;
  if assigned(OnKeyDown) then begin
    OnKeyDown(self,key,shift); //pass key down event
    if (Key=0) and not (key in [VK_SHIFT,VK_CONTROL,VK_MENU]) then
      FIgnoreNextKey:=oldkey;
  end;
  if key = 13 then begin
    key:=0;
    DoSearch(false,ssShift in shift);
  end;
end;

procedure TSearchBar.searchEdtKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if key=FIgnoreNextKey then begin
    FIgnoreNextKey:=0;
    exit;
  end;
  case key of
    0:  ;
    13: begin
      key:=0;
      //DoSearch(false,ssShift in shift);
    end;
    vk_escape: closeBtn.Click;
    //VK_DOWN,VK_UP,VK_NEXT,VK_PRIOR:; //see key down
    VK_SHIFT,VK_CONTROL,VK_MENU: ;
    else begin
      if assigned(OnKeyDown) then
       OnKeyDown(self,key,shift); //pass key events
      DoSearch(true,false);
    end;
  end;
end;

function TSearchBar.GetSearchText: string;
begin
  if assigned(searchEdt) then result:=searchEdt.Text
  else result:='';
end;

procedure TSearchBar.highlightClick(Sender: TObject);
begin
  if GetHighlighting = FHighlighting then exit;
  if assigned(FHighlightChanged) then
    FHighlightChanged(self);
  FHighlighting:=GetHighlighting;
end;

procedure TSearchBar.highlightBtnMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if GetHighlighting = FHighlighting then exit;
  if assigned(FHighlightChanged) then
    FHighlightChanged(self);
  FHighlighting:=GetHighlighting;
end;

function TSearchBar.GetSearchLocation: longint;
begin
  if locationsCmb=nil then result:=-1
  else result:=locationsCmb.ItemIndex;
end;


function TSearchBar.GetHighlighting: boolean;
begin
  Result:=(highlightBtn<>nil) and (highlightBtn.Down);
end;

function TSearchBar.GetSearchLocations: TStrings;
begin
  if locationsCmb=nil then result:=nil
  else result:=locationsCmb.Items;
end;

procedure TSearchBar.SetCaption(const AValue: string);
begin
  if FCaption=AValue then exit;
  FCaption:=AValue;
end;

procedure TSearchBar.SetFindState(const AValue: TFindState);
begin
  FFindState:=AValue;
  if assigned(searchEdt) then
    if searchEdt.Text='' then searchEdt.Color:=clWindow
    else if fsFound in AValue then searchEdt.Color:=FFoundColor
    else searchEdt.Color:=FNotFoundColor;
  if assigned(statusLabel) then begin
    if not (fsFound in AValue) then statusLabel.Caption:=FNotFoundState
    else if (fsLoopAround in AValue) then statusLabel.Caption:=FLoopAroundState
    else statusLabel.Caption:='';
  end;
end;

procedure TSearchBar.SetHighlightText(const AValue: string);
begin
  if FHighlightText=AValue then exit;
  FHighlightText:=AValue;
  updateComponents;
end;

procedure TSearchBar.SetsearchBackwardText(const AValue: string);
begin
  if FsearchBackwardText=AValue then exit;
  FsearchBackwardText:=AValue;
  updateComponents;
end;

procedure TSearchBar.SetSearchForwardText(const AValue: string);
begin
  if FSearchForwardText=AValue then exit;
  FSearchForwardText:=AValue;
  updateComponents;
end;

procedure TSearchBar.SetSearchLocation(const AValue: longint);
begin
  if locationsCmb<>nil then locationsCmb.ItemIndex:=AValue;
end;

procedure TSearchBar.SetSubComponents(const AValue: TSearchBarSubControls);
begin
  if FSubComponents=AValue then exit;
  FSubComponents:=AValue;
  updateComponents;
end;
{$ifndef lcl}
type TControlCracker = class(TControl)
public
  property caption;
end;
{$endif}

procedure TSearchBar.updateComponents;
var tempBitmap: graphics.TBitmap;
    controlIndex: integer = 0;
  //returns if the control is new created
  function setControl(show: boolean; var control: TControl; controlClass: TControlClass; cap:string; wid: longint=-1): boolean;
  begin
    result:=false;
    if show then begin
      if control = nil then begin
        control:=controlClass.Create(Self);
        control.parent:=Self;
        result:=true;
      end;
      control.Visible:=true;
      {$ifndef lcl}TControlCracker{$endif}(control).Caption:=cap;
      if tempBitmap.Canvas.TextWidth(cap) > wid-5 then
         wid:=tempBitmap.Canvas.TextWidth(cap)+5;
      if wid<>-1 then control.Width:=max(control.Height, wid);
      visibleSubControls[controlIndex] := control;
      inc(controlIndex);
    end else if control<>nil then begin
      control.free;
      control:=nil;
    end;
  end;
begin
  tempBitmap:=graphics.TBitmap.Create;
  tempBitmap.Width:=8;
  tempBitmap.Height:=8;
  tempBitmap.Canvas.Font:=font;
  tempBitmap.Canvas.TextOut(0,0,'load font'); //yes this is really needed, otherwise the lcl chrashes
  SetLength(visibleSubControls, ord(high(TSearchBarSubControls)) - ord(low(TSearchBarSubControls)) + 1 + 1);
  try
    if setControl(fscCloseButton in SubComponents,tcontrol(closeBtn),TSpeedButton, 'X',20) then
      closeBtn.OnClick:=closeBtnClick;
    SetControl(fscCaption in SubComponents,tcontrol(captionLbl),TLabel, Caption);
    if SetControl(true,tcontrol(searchEdt),TEdit, '',150) then begin
      searchEdt.OnKeyDown:=searchEdtKeyDown;
      searchEdt.OnKeyUp:=searchEdtKeyUp;
    end;
    if SetControl(fscSelectLocation in SubComponents,tcontrol(locationsCmb),TComboBox, '',100) then
      locationsCmb.Style:=csDropDownList;
    if SetControl(fscSearchForward in SubComponents,tcontrol(searchForwardBtn),TSpeedButton, SearchForwardText,75) then
      searchForwardBtn.OnClick:=searchButtonClick;
    if setControl(fscSearchBackwards in SubComponents,tcontrol(searchBackwardBtn),TSpeedButton,searchBackwardText,75) then
      searchBackwardBtn.OnClick:=searchButtonClick;
    if setControl(fscHighlight in SubComponents,tcontrol(highlightBtn),TSpeedButton,HighlightText,75) then begin
      highlightBtn.OnClick:=highlightClick;
      highlightBtn.OnMouseUp:=highlightBtnMouseUp;
      highlightBtn.AllowAllUp:=true;
      highlightBtn.GroupIndex:=1;
    end;

    setControl(fscStatus in SubComponents,tcontrol(statusLabel),TLabel,'');
    SetLength(visibleSubControls, controlIndex);
    moveComponents;
  finally
    tempBitmap.free;
  end;
end;

procedure TSearchBar.moveComponents;
var i,maxHeight:longint;
  totalWidth, editWidth: Integer;
const HSPACING:longint = 3;
      minEditWidth = 150;
      maxEditWidth = 450;
begin
  maxHeight:=0;
  for i:=0 to ControlCount -1 do
    if Controls[i].Height>maxHeight then
       maxHeight:=Controls[i].Height;
  for i:=0 to ControlCount -1 do begin
    if Controls[i] is TSpeedButton then
       Controls[i].Height:=maxHeight;
    Controls[i].Top:=(Height-Controls[i].Height) div 2;
  end;

  totalWidth := 0;
  for i := 0 to high(visibleSubControls) do begin
    if visibleSubControls[i] <> searchEdt then
      totalWidth := totalWidth + visibleSubControls[i].Width + HSPACING;
  end;

  if assigned(searchEdt) then begin
    editWidth := ClientWidth - totalWidth;
    if editWidth < minEditWidth then editWidth := minEditWidth
    else if editWidth > maxEditWidth then editWidth := maxEditWidth;
    searchEdt.Width := editWidth;
  end;

  totalWidth := HSPACING;
  for i := 0 to high(visibleSubControls) do begin
    visibleSubControls[i].Left := totalWidth;
    totalWidth := totalWidth + visibleSubControls[i].Width + HSPACING;
  end;
end;

{$ifdef lcl}
procedure TSearchBar.DoOnResize;
begin
  //if height<>FOldHeight then
    moveComponents;
  FOldHeight:=Height;
  inherited DoOnResize;
end;

{procedure TSearchBar.ResizeDelayedAutoSizeChildren;
begin
  inherited ResizeDelayedAutoSizeChildren;
  //this is the first time the size of the caption label is known!
  updateComponents;
end;}

procedure TSearchBar.SetVisible(Value: Boolean);
var changed: boolean;
begin
  changed:=Visible<>value;
  inherited SetVisible(Value);
  if changed then
    if not Visible and assigned(OnClose) then OnClose(self)
    else if  Visible and assigned(OnShow) then OnShow(self);
end;

{$else}
procedure TSearchBar.VisibleChanging;
begin
  moveComponents;
  FOldHeight:=Height;
  postMessage(handle,WM_USER_VISIBLE_CHANGED,0,0);

  inherited VisibleChanging;
end;

procedure TSearchBar.VisibleChangedMessage(var msg: TMsg);
begin
  if Visible and assigned(OnClose) then OnShow(self)
  else if  not Visible and assigned(OnShow) then OnClose(self);
end;
{$endif}

procedure TSearchBar.DoSearch(incremental, backwards: boolean);
begin
  if assigned(OnSearch) then OnSearch(self, incremental, backwards);
end;


procedure TSearchBar.setFocus;
begin
  inherited setFocus;
  if assigned(searchEdt) then searchEdt.SetFocus;
end;


constructor TSearchBar.Create(TheOwner: TComponent);
var
  tempBitmap: graphics.TBitmap;
begin
  inherited Create(TheOwner);
  Align:=alBottom;
  FsearchBackwardText:='&Previous';
  FSearchForwardText:='&Next';
  FHighlightText:='&Highlight all';
  FCaption:='Find:';
  FNotFoundState:='Not found!';
  FLoopAroundState:='Loop around';
  FSubComponents:=[];
  SubComponents:=[fscCloseButton, fscCaption, fscSearchForward, fscSearchBackwards, fscStatus];
  //size which seem to work well with the default font size
  tempBitmap:=graphics.TBitmap.Create;
  tempBitmap.Width:=8;
  tempBitmap.Height:=8;
  tempBitmap.Canvas.Font:=font;
  tempBitmap.Canvas.TextOut(0,0,'load font'); //yes this is really needed, otherwise the lcl chrashes
  Height:=2*tempBitmap.Canvas.TextHeight('^MH,')+3;
  tempBitmap.free;
  FHighlighting:=false;
  FFoundColor:=$77DD77;
  FNotFoundColor:=rgb($DD,$77,$77);
  {$ifdef delphi}
    {$ifndef VER100}{$ifndef VER120}{$ifndef VER130}{$ifndef VER140} //delphi 7 or higher
    ControlStyle := controlstyle  - [csparentbackground];
    {$endif}{$endif}{$endif}{$endif}
  {$endif}

end;

destructor TSearchBar.destroy();
begin
  closeBtn.free;
  searchForwardBtn.free;
  searchBackwardBtn.free;
  highlightBtn.free;
  captionLbl.free;
  statusLabel.free;
  locationsCmb.free;
  searchEdt.free;
  inherited destroy();
end;

procedure Register;
begin
  RegisterComponents('BeniBela', [TSearchBar]);
end;

{$ifdef lcl}
initialization
{$I findControl.lrs}
{$endif}

end.


