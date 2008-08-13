unit findControl;

{$ifdef fpc}
{$mode delphi}
{$endif}
{$H+}

interface

uses
  Classes, SysUtils, StdCtrls, ExtCtrls, Controls,Buttons,Graphics
  {$ifdef lcl},LCLType,LCLIntf{$else},windows{$endif};

type
TFindState = set of (fsFound, fsLoopAround);
TSearchBarSubControl = (fscCloseButton, fscCaption, fscSelectLocation, fscSearchForward, fscSearchBackwards, fscHighlight, fscStatus);
TSearchBarSubControls = set of TSearchBarSubControl;
TSearchEvent = procedure (sender: TObject;  incremental,backwards: boolean) of object;

{ TFindControl }
                                                   //todo: highlight shortcut, dblclick, doku, delphi
{ TSearchBar }

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
  procedure highlightBtnMouseUp(Sender: TObject; Button: TMouseButton;
    Shift: TShiftState; X, Y: Integer);
protected
  closeBtn, searchForwardBtn, searchBackwardBtn, highlightBtn: TSpeedButton;
  captionLbl,statusLabel: TLabel;
  locationsCmb: TComboBox;
  searchEdt: TEdit;
  procedure updateComponents;
  procedure moveComponents;
  {$ifdef lcl}procedure DoOnResize; override;{$endif}
  procedure DoSearch(incremental, backwards: boolean);
  {$ifdef lcl}procedure ResizeDelayedAutoSizeChildren; override;{$endif}
public
  property SearchText: string read GetSearchText;
  property SearchLocation: longint read GetSearchLocation write SetSearchLocation ;
  property SearchLocations: TStrings read GetSearchLocations;
  property Highlighting: boolean read GetHighlighting ;
  property FindState: TFindState read FFindState write SetFindState;
  procedure setFocus;override;
  constructor create(TheOwner: TComponent);override;
published
  property OnSearch: TSearchEvent read FOnSearch write FOnSearch;
  property OnClose: TNotifyEvent read FOnClose write FOnClose;
  property OnHighlightChanged: TNotifyEvent read FHighlightChanged write FHighlightChanged;
  property OnKeyDown;

  property SubComponents: TSearchBarSubControls read FSubComponents write SetSubComponents;
  property Caption: string read FCaption write SetCaption;
  property SearchForwardText: string read FSearchForwardText write SetSearchForwardText;
  property SearchBackwardText: string read FsearchBackwardText write SetsearchBackwardText;
  property HighlightText: string read FHighlightText write SetHighlightText;
  property FoundColor: TColor read FFoundColor write FFoundColor;
  property NotFoundColor: TColor read FNotFoundColor write FNotFoundColor;
  property NotFoundState: string read FNotFoundState write FNotFoundState;
  property LoopAroundState: string read FLoopAroundState write FLoopAroundState;
end;

implementation

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
      if assigned(OnKeyUp) then
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
  if assigned(statusLabel) then
    if not (fsFound in AValue) then statusLabel.Caption:=FNotFoundState
    else if (fsLoopAround in AValue) then statusLabel.Caption:=FLoopAroundState
    else statusLabel.Caption:='';
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

type TControlCracker = class(TControl)
public
  property caption;
end;

procedure TSearchBar.updateComponents;
const HSPACING:longint = 3;
var cx:longint;
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
      TControlCracker(control).Caption:=cap;
      control.Left:=cx;
      if wid<>-1 then control.Width:=wid;
      cx:=cx+control.Width+HSPACING;
    end else begin
      control.free;
      control:=nil;
    end;
  end;
begin
  cx:=HSPACING;
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
  moveComponents;
end;

procedure TSearchBar.moveComponents;
var i:longint;
begin
  if searchEdt=nil then exit;
  for i:=0 to ControlCount -1 do begin
    if not (Controls[i] is TLabel) then
       Controls[i].Height:=searchEdt.Height;
    Controls[i].Top:=(Height-Controls[i].Height) div 2;
  end;
end;

{$ifdef lcl}
procedure TSearchBar.DoOnResize;
begin
  if height<>FOldHeight then moveComponents;
  FOldHeight:=Height;
  inherited DoOnResize;
end;
{$endif}

procedure TSearchBar.DoSearch(incremental, backwards: boolean);
begin
  if assigned(OnSearch) then OnSearch(self, incremental, backwards);
end;

{$ifdef lcl}
procedure TSearchBar.ResizeDelayedAutoSizeChildren;
begin
  inherited ResizeDelayedAutoSizeChildren;
  //this is the first time the size of the caption label is known!
  updateComponents;
end;
{$endif}

procedure TSearchBar.setFocus;
begin
  inherited setFocus;
  if assigned(searchEdt) then searchEdt.SetFocus;
end;


constructor TSearchBar.create(TheOwner: TComponent);
begin
  inherited create(TheOwner);
  Align:=alBottom;
  FsearchBackwardText:='&Previous';
  FSearchForwardText:='&Next';
  FHighlightText:='&Highlight all';
  FCaption:='Find:';
  FNotFoundState:='Not found!';
  FLoopAroundState:='Loop around';
  FSubComponents:=[];
  SubComponents:=[fscCloseButton, fscCaption, fscSearchForward, fscSearchBackwards, fscStatus];
  Height:=30;
  FHighlighting:=false;
  FFoundColor:=$77DD77;
  FNotFoundColor:=rgb($DD,$77,$77);
end;

end.

