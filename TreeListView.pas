{*******************************************************************************
                                TreeListView
  Diese Komponente ist eine Mischung zwischen der "record"-Ansicht von TListView
  und der Baumansicht von TTreeView.
  Im wesentlichen  wird ein Baum angezeigt, wobei für  jede Zeile  im Baum,  in
  Spalten  zusätzliche Informationen angezeigt werden können. Jeder Eintrag in
  den einzelnen Spalten kann unabhängig von den anderen formatiert werden.

  Das Copyright liegt alleinig bei Benito van der Zander, alias BeniBela
                                   benibela@aol.com
                                   www.benibela.de

  Die Komponente steht unter der:
  This component is licenced under the terms of the:
     Creative Commons Attribution-NonCommercial-ShareAlike 2.0 Licence
     
  Zusammenfassung:
  Summary:
    - Sie müssen den Urheber angeben
    - Sie dürfen die Komponente nicht für kommerzielle Zwecke benutzen
    - Die Komponente darf nur unter denselben Lizenzbedingungen veröffentlicht 
      und verändert werden.
    - You must give the original author credit.
    - You may not use this work for commercial purposes.
    - If you alter, transform, or build upon this work, you may distribute the 
      resulting work only under a licence identical to this one.
*******************************************************************************}

unit TreeListView;
{$mode delphi}
interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,comctrls,stdctrls,math;

type
  {$TYPEINFO ON}
  //Forward
  TTreeListRecordItemText=class;
  TTreeListRecordItem=class;
  TTreeListItem=class;
  TW32TreeListView = class;

  TImageTyp=(itNone,itListIndex,itBitmap);
  TObjectList=class(TList)
    public
      //Events
      OnChanging:TNotifyEvent;
      OnChange:TNotifyEvent;

      procedure BeginEdit;
      procedure EndEdit;

      function AddObject(Item: TObject): Integer;
      procedure InsertObject(Index: Integer; Item: TObject);
      function RemoveObject(Item: TObject): Integer;

      procedure Clear; override;
      procedure Delete(Index: Integer);
      procedure Exchange(Index1, Index2: Integer);
      function Expand: TList;
      procedure Move(CurIndex, NewIndex: Integer);
      procedure Pack;
      procedure Sort(Compare: TListSortCompare);

      procedure FreeObjects;

      procedure Assign(list:TList);

      constructor create;
      destructor destroy;override;
    protected
      freeing:boolean;
      editing:boolean;

      //Eventauslöser
      procedure DoChanging;virtual;
      procedure DoChange;virtual;

      function Add(Item: Pointer): Integer;
      procedure Insert(Index: Integer; Item: Pointer);
      function Remove(Item: TObject): Integer;
  end;

  TRealItemCounting=set of (ricCountCollapsedsubItems{,ricCountExpandItems,ricCountEndNodes});

  { TTreeListItems }

  TTreeListItems=class(TObjectList)
  private
    procedure Put(Index: Integer; const AValue: TTreeListItem);
  protected
    F_Parent:TtreeListItem;
    F_TreeListView:tw32TreeListView;
    constructor create(parent:TTreeListItem;const TreeListView:Tw32treelistview);
    function Get(Index: Integer): TTreeListItem; inline;
  public
    function AddItem:TTreelistItem;
    function AddItemWithCaption(caption:string):TTreelistItem;
    function AddChildItem(Parent:TTreeListItem):TTreelistItem;
    function AddChildItemWithCaption(Parent:TTreeListItem;caption:string):TTreelistItem;
    function GetRealItemCount(const countTyp:TRealItemCounting ) :integer;
    function GetItemWithRealIndex(index:integer):TTreeListItem;
    function RealIndexOf(const item:ttreeListItem;const countTyp:TRealItemCounting):integer;
    function FindItemWithCaption(caption: string): TTreeListItem;
    property Items[Index: Integer]: TTreeListItem read Get write Put; default;
  end;
  TRecordItemTextList=class(TObjectList)
    function AddTextItem(text:string):TTreeListRecordItemText;
  end;

  { TRecordItemList }

  TRecordItemList=class(TObjectList)
    function Add:TTreeListRecordItem;
    function AddWithText(s: string):TTreeListRecordItem;
    procedure AddItem(recordItem: TTreeListRecordItem);
  end;

  TTreeListRecordItemText=class(TPersistent)
    protected
      F_Text:string;

      F_HotTrackFont:TFont;
      F_HotTrack:boolean;
      F_ParentHotTrack:boolean;
      F_ParentHotTrackFont:boolean;

      procedure SetHotTrackFont(const value:TFont);
    public
      constructor create;
      destructor destroy;override;
    published
      property Text:string read F_text write F_text;
      property HotTrackFont:TFont read F_HotTrackFont write SetHotTrackFont;
      property ParentHotTrackFont:boolean read F_ParentHotTrackFont write F_ParentHotTrackFont;
      property HotTrack:boolean read F_HotTrack write F_HotTrack ;
      property ParentHotTrack:boolean read F_ParentHotTrack write F_ParentHotTrack ;
  end;

  TTreeListRecordItem=class(TPersistent)
    protected
      F_TextItems:TRecordItemTextList;

      procedure SetTextItems(value:TRecordItemTextList);

      procedure SetSimpleText(caption:string);
      function GetSimpleText:string;
    public
      procedure PaintTo(const listView:TW32TreeListView;x:integer;const y,xColumn:integer;const parentItem:TTreeListItem);

      function GetRecordItemTextAtPos(const listView:TW32TreeListView;const TestX:integer):TTreeListRecordItemText;

      constructor create;
      constructor createWithText(caption:string);
      destructor destroy;override;
    published
      property TextItems:TRecordItemTextList read F_textitems write setTextItems;
      property SimpleText:string read GetSimpleText write setSimpleText;
  end;

  TTreeListItem=class(TPersistent)
    protected
      F_IndexOrBitmap:cardinal;
      F_ImageTyp:TImageTyp;
      F_SubItems:TTreeListItems;
      F_RecordItems:TRecordItemList;
      F_caption:string;
      F_Expanded:boolean;

      F_Indent:integer; //Gibt an, wieoft, das Item eingerückt wurde, diese Eigenschaft wird von PaintTo und GetItemAtPosWithIndentSet gesetzt, und muss *nicht* stimmmen

      F_Parent:Ttreelistitem;
      F_TreeListView:TW32TreeListView;

  //   F_ParentFont:boolean;
  //    F_Font:TFont;

      procedure DoChange;
      procedure DoChanging;

      procedure SetSubItems(const value:TtreeListItems);
      procedure SetRecordItems(const value:TRecordItemList);
      procedure SetExpand(const expanded:boolean);
    public
      Tag:integer;

      //Create
      constructor create(const parent:TTreeListItem;const TreeListView:Tw32treelistview);
      constructor createWithCaption(const parent:TTreeListItem;const TreeListView:Tw32treelistview;const ACaption:string);

      function GetItemAtPos(const listView:TW32TreeListView;const TestY:integer;var startY:integer):TTreeListItem;
      function GetItemAtPosWithIndentSet(const listView:TW32TreeListView;const TestY,Indent:integer;var startY:integer):TTreeListItem;
      function GetRecordItemAtPos(const listView:TW32TreeListView;const TestX:integer):TTreeListRecordItem;
      function GetRecordItemTextAtPos(const listView:TW32TreeListView;const TestX:integer):TTreeListRecordItemText;

      procedure Expand;
      procedure Collapse;


      function GetNextItemIgnoringChildren:TTreeListItem;
      //function GetNextItem:TTreeListItem; deprecated use nextvisibleitem
      //function GetPrevItem:TTreeListItem;
      function GetLastSubSubItem:TTreeListItem; //Gibt das unterster Item, des untersten Items, des untersten Items, des untersten Items, des untersten Items, des untersten Items..., des untersten SubItems zuück.
      function GetNextVisibleItem(Delta:longint=1):TTreeListItem;
      function GetPrevVisibleItem(Delta:longint=1):TTreeListItem;


      property Parent:TTreeListItem read F_parent;
      property TreeListView:Tw32TreeListView read F_TreeListview;

      procedure PaintTo(const listView:TW32TreeListView;const x:integer;var y:integer;const xColumn:integer;const last:boolean);

      //Destroy
      destructor destroy;override;

      property Indent:integer read F_Indent;
      property Expanded:boolean read F_expanded write SetExpand;
    published
      property RecordItems:TRecordItemList read F_RecordItems write SetRecordItems;
      property SubItems:TTreeListItems read F_SubItems write SetSubItems;
      property IndexOrBitmap:cardinal read F_indexOrBitmap write F_indexOrBitmap;
      property Caption:string read F_Caption write F_Caption;
      property ImageTyp:TImageTyp read F_ImageTyp write F_ImageTyp;
//      property Font:TFont read F_Font write SetFont
  end;

  TTreeListInternOptions=set of (tlioDisablePainting, tlioDeleting, tlioUpdating);
  TExpandMode=(emExpandByClick,emExpandByDoubleClick,emExpandNot);
  TLineMode=(lmNone,lmSolid,lmDot);
  TCustomDrawEventTyp=(cdevtPrePaint,cdevtPostPaint);
  TCustomBackgroundDrawEvent=procedure (sender:TObject;eventTyp_cdet:TCustomDrawEventTyp;var defaultDraw:Boolean) of object;
  TCustomItemDrawEvent=procedure (sender:TObject;eventTyp_cdet:TCustomDrawEventTyp;item:TTreeListItem;xpos,ypos,xColumn:integer;lastItem:boolean;var defaultDraw:Boolean) of object;
  TCustomRecordItemDrawEvent=procedure (sender:TObject;eventTyp_cdet:TCustomDrawEventTyp;parentItem:TTreeListItem;RecordItem:TTreeListRecordItem;xpos,ypos,xColumn:integer;var defaultDraw:Boolean) of object;
  TItemEvent=procedure (sender:TObject;item:TTreeListItem) of object;
  TRecordItemTextEvent=procedure (sender:TObject;parentItem:TTreeListItem;item:TTreeListRecordItemText) of object;

  { TW32TreeListView }

  TW32TreeListView = class(TCustomControl)
    { Private-Deklarationen}
  protected
    { Protected-Deklarationen}
    InternOptions_tlio:TTreeListInternOptions;

    StartX:integer;
    LastItems:array[0..31] of boolean; //Gibt an, ob unter dem entsprechenden Item eine Linie gezeichnet werden soll, damit ist es nicht möglich mehr als 32 RecordItems zu machen.

    F_HotTrackFont:TFont;
    F_HotTrackSubTextItems:boolean;
    F_Items:TTreeListItems;
    F_Header:THeaderControl;
    F_VScroll:TScrollBar;
    F_HScroll:TScrollBar;
    F_RowHeight:integer;
    F_ImageList:TImageList;

    F_SelectedFont:TFont;
    F_SelectHotTrackFont:TFont;
    F_SelectBackColor:TColor;

    F_ButtonColor:TColor;
    F_BgColor:TColor;

    F_Striped:boolean;
    F_StripedOddColor:TColor;
    F_StripedEvenColor:TColor;

    F_ExpandMode:TExpandMode;

    F_HorizontalLines:TLineMode;
    F_HorizontalLineColor:TColor;
    F_VerticalLines:TLineMode;
    F_VerticalLineColor:TColor;
    F_RootLines:TLineMode;
    F_RootLineColor:TColor;

    //Headerevents
    {$ifdef FPC}
    F_HeaderSectionResize:TCustomSectionNotifyEvent;
    F_HeaderSectionTrack:TCustomSectionTrackEvent;
    {$else}
    F_HeaderSectionResize:TSectionNotifyEvent;
    F_HeaderSectionTrack:TSectionTrackEvent;
    {$endif}
    //Events

    //Scrollbarevents
    F_VScrollBarChange:TNotifyEvent;
    F_HScrollBarChange:TNotifyEvent;

    //CustomDrawEvents
    F_CustomBgDraw:TCustomBackgroundDrawEvent;
    F_CustomItemDraw:TCustomItemDrawEvent;
    F_CustomRecordItemDraw:TCustomRecordItemDrawEvent;

    //Inputevents
    F_ClickAtItem:TItemEvent;
//    F_ItemCollapsed:TItemEvent;
 //   F_ItemExpanded:TItemEvent;
    F_ClickAtRecordItemText:TRecordItemTextEvent;
    F_OnSelect:TItemEvent;

    PaintEvenItem:boolean;

    //Ereignissausösungen
    function DoCustomBackgroundDrawEvent (eventTyp_cdet:TCustomDrawEventTyp):boolean;
    function DoCustomItemDrawEvent(eventTyp_cdet:TCustomDrawEventTyp;item:TTreeListItem;xpos,ypos,xColumn:integer;lastItem:boolean):boolean;
    function DoCustomRecordItemDrawEvent(eventTyp_cdet:TCustomDrawEventTyp;parentItem:TTreeListItem;RecordItem:TTreeListRecordItem;xpos,ypos,xColumn:integer):boolean;

    procedure DoSelect;virtual;

    //Kommunikationsroutinen (Set- und Getfunktionen)
    procedure SetItems(const value:TTreeListItems);

    procedure SetTopPos(const i:integer);
    function GetTopPos:integer;

    procedure SetHeaderSections(const value:THeaderSections);
    function GetHeaderSections(): THeaderSections;

    procedure setImageList(const images:TImageList);

    procedure SetRowHeight(const newHeight:integer);

    procedure SetHotTrackSubTextItems(const value:boolean);

    procedure DrawAlignDotLine(x,y:integer;const x2,y2:integer;const color:TColor);

    //Interne Kommunikation mit Unterkomponenten
    procedure _GeneralEvent(Sender: TObject);
    {$ifndef fpc}
    procedure _HeaderSectionTrack( HeaderControl: THeaderControl;  Section: THeaderSection;  Width: Integer;  State: TSectionTrackState);
    procedure _HeaderSectionResize( HeaderControl: THeaderControl;  Section: THeaderSection);
    {$else}
    procedure _HeaderSectionTrack( HeaderControl: TCustomHeaderControl;  Section: THeaderSection;  Width: Integer;  State: TSectionTrackState);
    procedure _HeaderSectionResize( HeaderControl: TCustomHeaderControl;  Section: THeaderSection);
    {$endif}
    procedure _HScrollChange(Sender: TObject);
    procedure _VScrollChange(Sender: TObject);

    procedure UpdateScrollBarPos;
  public
    { Public-Deklarationen}
    selected:TTreeListItem;
    hotTrackedTextItem:TTreeListRecordItemText;

    procedure UpdateScrollSize;

    //Create
    constructor create(aowner:TComponent);override;

    function GetItemAtPos(const y:integer):TTreeListItem;
    function GetItemAtPosWithIndentSet(const y:integer):TTreeListItem;
    function GetRecordItemAtPos(const x,y:integer):TTreeListRecordItem;
    function GetRecordItemTextAtPos(const x,y:integer):TTreeListRecordItemText;

    procedure SetHotTrackFont(const value:TFont);
    procedure SetSelectedFont(const value:TFont);
    procedure SetSelectHotTrackFont(const value:TFont);

    //Items
    procedure BeginUpdate;
    procedure EndUpdate;
    function VisibleRowCount:longint;

    //Messages
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure WndProc(var message:TMessage);override;

    //Ausgaberoutinen
    procedure Paint;override;

    //Destroy
    destructor destroy;override;

    property TopPos:integer read GetTopPos write SetTopPos;
    property Canvas;
  published
    { Published-Deklarationen }

    {-------------------------------START Ereignisse---------------------------}

    //Scrollbarereignisse
    property OnVScrollBarChange:TNotifyEvent read F_VScrollBarChange write F_VScrollBarChange;
    property OnHScrollBarChange:TNotifyEvent read F_HScrollBarChange write F_HScrollBarChange;

    //CustomDrawEvents
    property OnCustomBgDraw:TCustomBackgroundDrawEvent read F_CustomBgDraw write F_CustomBgDraw;
    property OnCustomItemDraw:TCustomItemDrawEvent read F_CustomItemDraw write F_CustomItemDraw;
    property OnCustomRecordItemDraw:TCustomRecordItemDrawEvent read F_CustomRecordItemDraw write F_CustomRecordItemDraw;

    //Inputevents
    property OnClickAtRecordItemText:TRecordItemTextEvent read F_ClickAtRecordItemText write F_ClickAtRecordItemText;
    property OnClickAtItem:TItemEvent read F_ClickAtItem write F_ClickAtItem;
    property OnSelect:TItemEvent read F_OnSelect write F_OnSelect;
//   property OnItemCollapsed:TItemEvent read F_ItemCollapsed write F_ItemCollapsed;
  //  property OnItemExpanded:TItemEvent read F_ItemExpanded write F_ItemExpanded;

    //Header-Ereignisse
    {$ifdef FPC}
    property OnHeaderSectionResize:TCustomSectionNotifyEvent read F_HeaderSectionResize write F_HeaderSectionResize;
    property OnHeaderSectionTrack:TCustomSectionTrackEvent read F_HeaderSectionTrack write F_HeaderSectionTrack;
    {$else}
    property OnHeaderSectionResize:TSectionNotifyEvent read F_HeaderSectionResize write F_HeaderSectionResize;
    property OnHeaderSectionTrack:TSectionTrackEvent read F_HeaderSectionTrack write F_HeaderSectionTrack;
    {$endif}


    //Freigeben von TWinControlereignissen
    property OnDockDrop;
	   property OnDockOver ;
    property OnEnter     ;
    property OnExit       ;
    property OnGetSiteInfo ;
    property OnKeyDown      ;
    property OnKeyPress      ;
    property OnKeyUp          ;
    property OnMouseWheel      ;
    property OnMouseWheelDown   ;
    property OnMouseWheelUp      ;
    property OnUnDock             ;

    //Freigeben von TControl-Ereignissen
    {$ifndef fpc}property OnCanResize;{$endif}
    property OnClick                ;
    property OnConstrainedResize     ;
    property OnDblClick               ;
    property OnDragDrop                 ;
    property OnDragOver                  ;
    property OnEndDock                    ;
    property OnEndDrag                     ;
    property OnMouseDown                    ;
    property OnMouseMove                     ;
    property OnMouseUp                        ;
    property OnResize                          ;
    property OnStartDock                        ;
    property OnStartDrag                         ;

{--------------------------------ENDE Ereignisss---------------------------}

    //Header-Eigenschaften
    property HeaderSections:THeaderSections read GetHeaderSections write SetHeaderSections;

    property RowHeight:integer read F_RowHeight write SetRowHeight;

    property Images:TImageList read F_ImageList write setImageList;

    property HorizontalLineMode:TLineMode read F_HorizontalLines write F_HorizontalLines;
    property HorizontalLineColor:TColor read F_HorizontalLineColor write F_HorizontalLineColor;
    property VerticalLineMode:TLineMode read F_VerticalLines write F_VerticalLines;
    property VerticalLineColor:TColor read F_VerticalLineColor write F_VerticalLineColor;
    property RootLineMode:TLineMode read F_RootLines write F_RootLines;
    property RootLineColor:TColor read F_RootLineColor write F_RootLineColor;

    property ExpandMode:TExpandMode read F_ExpandMode write F_ExpandMode;
    property HotTrackSubTextItems:Boolean read F_HotTrackSubTextItems write SetHotTrackSubTextItems;

    property HotTrackFont:TFont read F_HotTrackFont write SetHotTrackFont;
    property Font;
    property SelectedFont:TFont read F_SelectedFont write SetSelectedFont;
    property SelectHotTrackFont:TFont read F_SelectHotTrackFont write SetSelectHotTrackFont;

    property Striped:boolean read F_Striped write F_Striped;
    property StripedOddColor:TColor read F_StripedOddColor write F_StripedOddColor;
    property StripedEvenColor:TColor read F_StripedEvenColor write F_StripedEvenColor;

    property SelectBackColor:TColor read F_SelectBackColor write F_SelectBackColor;
    property ButtonColor:TColor read F_ButtonColor write F_ButtonColor;
    property BackGroundColor:TColor read F_BgColor write F_BgColor;

    property Items:TTreeListItems read F_Items write SetItems;

    property BorderWidth;
    {$ifndef fpc}property BevelWidth;
    property BevelEdges;
    property BevelInner default bvLowered;
    property BevelOuter default bvLowered;
    property BevelKind default bkTile; {$endif}
    property TabStop default true;
    property TabOrder;

    //Freigeben von TControl
    property Align;
    property Anchors;
    property Constraints;
    property DragCursor;
    property DragKind   ;
    property DragMode    ;
    property Hint         ;
    property ShowHint      ;

  end;
  TTreeListView = TW32TreeListView;

procedure Register;

implementation

{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{
################################################################################
================================================================================
////////////////////////////////////////////////////////////////////////////////
--------------------------------------------------------------------------------
................................TObjectList.....................................
--------------------------------------------------------------------------------
////////////////////////////////////////////////////////////////////////////////
================================================================================
################################################################################
}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}

constructor TObjectList.create;
begin
  inherited;
  editing:=false;
  freeing:=false;
end;

procedure TObjectList.BeginEdit;
begin
  editing:=true;
  DoChanging;
end;

procedure TObjectList.EndEdit;
begin
  DoChange;
  editing:=false;
end;

//Neu mit Objekten
function TObjectList.AddObject(Item: TObject): Integer;
begin
  result:=add(item);
end;

procedure TObjectList.InsertObject(Index: Integer; Item: TObject);
begin
  Insert(index,item);
end;

function TObjectList.RemoveObject(Item: TObject): Integer;
begin
  result:=remove(item);
end;

//Eventbearbeitung
procedure TObjectList.DoChanging;
begin
  if Assigned(OnChanging) then OnChanging(self);
end;

procedure TObjectList.DoChange;
begin
  if Assigned(OnChange) then OnChange(self);
end;

//Eventauslöser
function TObjectList.Add(Item: Pointer): Integer;
begin
  if not editing then
    DoChanging;
  result:=inherited add(item);
  if not editing then
    DoChange;
end;
procedure TObjectList.Clear;
var i:longint;
begin
  if not editing then
    DoChanging;
  for i:=0 to Count-1 do
    TObject(Items[i]).free;
  inherited;
  count:=0;
  if not editing then
    DoChange;
end;
procedure TObjectList.Delete(Index: Integer);
begin
  if not editing then
    DoChanging;
  inherited;
  if not editing then
    DoChange;
end;
procedure TObjectList.Exchange(Index1, Index2: Integer);
begin
  if not editing then
    DoChanging;
  inherited;
  if not editing then
    DoChange;
end;
function TObjectList.Expand: TList;
begin
  if not editing then
    DoChanging;
  result:=inherited expand;
  if not editing then
    DoChange;
end;
procedure TObjectList.Insert(Index: Integer; Item: Pointer);
begin
  if not editing then
    DoChanging;
  inherited;
  if not editing then
    DoChange;
end;
procedure TObjectList.Move(CurIndex, NewIndex: Integer);
begin
  if not editing then
    DoChanging;
  inherited;
  if not editing then
    DoChange;
end;
procedure TObjectList.Pack;
begin
  if not editing then
    DoChanging;
  inherited;
  if not editing then
    DoChange;
end;
function TObjectList.Remove(Item: TObject): Integer;
begin
  if not editing then
    DoChanging;
  item.free;
  result:=inherited remove(item);
  if not editing then
    DoChange;
end;
procedure TObjectList.Sort(Compare: TListSortCompare);
begin
  if not editing then
    DoChanging;
  inherited;
  if not editing then
    DoChange;
end;
procedure TObjectList.Assign(list:TList);
var i:integer;
begin
  Count:=list.Count;
  for i:=0 to Count-1 do
    Items[i]:=list.Items[i];
end;
procedure TObjectList.FreeObjects;
var i:integer;
begin
  freeing:=true;
//  DoChanging;
  for i:=0 to Count-1 do begin
    TObject(Items[i]).free;
    Items[i]:=nil;
  end;
  count:=0;
//  DoChange;
  freeing:=false;
end;
destructor TObjectList.destroy;
begin
  FreeObjects;
  inherited;
end;


{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{
################################################################################
================================================================================
////////////////////////////////////////////////////////////////////////////////
--------------------------------------------------------------------------------
...............................tTreeListItems...................................
--------------------------------------------------------------------------------
////////////////////////////////////////////////////////////////////////////////
================================================================================
################################################################################
}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}

procedure TTreeListItems.Put(Index: Integer; const AValue: TTreeListItem);
begin
  inherited put(Index, AValue);
end;

function TTreeListItems.Get(Index: Integer): TTreeListItem; inline;
begin
  result:=TTreeListItem(inherited get(index));
end;

constructor TTreeListItems.create(parent:TTreeListItem;const TreeListView:Tw32treelistview);
begin
  inherited create;
  F_Parent:=parent;
  F_TreeListView:=TreeListView;
end;


function TTreeListItems.AddItem:TTreelistItem;
begin
  Result:=TTreeListItem.Create(F_Parent,F_TreeListView);
  Result.RecordItems.OnChanging:=OnChanging;
  Result.RecordItems.OnChange:=OnChange;
  Result.SubItems.OnChanging:=OnChanging;
  Result.SubItems.OnChange:=OnChange;
  add(result);
end;

function TTreeListItems.AddItemWithCaption(caption:string):TTreelistItem;
begin
  Result:=TTreeListItem.CreateWithCaption(F_Parent,F_TreeListView, caption);
  Result.RecordItems.OnChanging:=OnChanging;
  Result.RecordItems.OnChange:=OnChange;
  Result.SubItems.OnChanging:=OnChanging;
  Result.SubItems.OnChange:=OnChange;
  add(result);
end;

function TTreeListItems.AddChildItem(Parent:TTreeListItem):TTreelistItem;
begin
  if Parent=nil then exit(AddItem)
  else exit(Parent.SubItems.AddItem);
end;

function TTreeListItems.AddChildItemWithCaption(Parent:TTreeListItem;caption:string):TTreelistItem;
begin
  result:=AddChildItem(Parent);
  result.Caption:=caption;
end;

function tTreeListItems.GetRealItemCount(const countTyp:TRealItemCounting ) :integer;
var i:integer;
begin
  result:=1;
  if freeing then exit;
  for i:=0 to Count-1 do
    if ((TObject(items[i]) as TTreeListItem).expanded) or (ricCountCollapsedSubItems in countTyp) then
      result:= Result+TTreeListItem(items[i]).SubItems.GetRealItemCount(countTyp)
     else
      inc(result);
end;

function tTreeListItems.RealIndexOf(const item:ttreeListItem;const countTyp:TRealItemCounting):integer;
var pos:integer;
    gefunden:boolean;
  procedure RealIndexOfRek(const newself:TTreeListItem); //Durchsucht rekursiv alle SubListen der SubItems
  var i:integer;
  begin
    for i:=0 to newself.SubItems.Count-1 do begin
      inc(pos);
      if newself.SubItems[i]=item then begin
        gefunden:=true;
        break;
      end;
      if (TObject(newself.SubItems[i]) as TTreeListItem).Expanded or (ricCountCollapsedSubItems in counttyp) then
        RealIndexOfRek(newself.SubItems[i]);
      if gefunden then break;
    end;
  end;
var i:integer;
begin
  pos:=-1;
  gefunden:=false;
    for i:=0 to Count-1 do begin
      inc(pos);
      if Items[i]=item then begin
        gefunden:=true;
        break;
      end;
      if (TObject(Items[i]) as TTreeListItem).Expanded or (ricCountCollapsedSubItems in counttyp) then
        RealIndexOfRek(Items[i]);
      if gefunden then break;
    end;
  if gefunden then Result:=pos
  else Result:=-1;
end;

function TTreeListItems.FindItemWithCaption(caption: string): TTreeListItem;
var i:longint;
begin
  result:=nil;
  for i:=0 to count-1 do
    if Items[i].Caption=caption then exit(Items[i])
    else begin
      result:=Items[i].SubItems.FindItemWithCaption(caption);
      if result<>nil then exit;
    end;
end;

function tTreeListItems.GetItemWithRealIndex(index:integer):TTreeListItem;
  function GetItemWithRealIndexRek(const nself:TTreeListItems;var index:integer):TTreeListItem;
  var i:integer;
  begin
    result:=nil;
    for i:=0 to nself.count-1 do begin
      dec(index);
      if index=0 then begin result:=nself[i];exit;end
      else result:=GetItemWithRealIndexRek(TTreeListItem(nself[i]).SubItems,index);
    end;
  end;
var i:integer;
begin
  if index=-1 then begin
    result:=nil;
    exit;
  end;
  i:=index+1;
  result:=GetItemWithRealIndexRek(self,i);
end;
{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{
################################################################################
================================================================================
////////////////////////////////////////////////////////////////////////////////
--------------------------------------------------------------------------------
..............................TRecordItemTextList...............................
--------------------------------------------------------------------------------
////////////////////////////////////////////////////////////////////////////////
================================================================================
################################################################################
}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}
function TRecordItemTextList.AddTextItem(text:string):TTreeListRecordItemText;
var textitem:TTreeListRecordItemText;
begin
  textitem:=TTreeListRecordItemText.Create;
  textitem.Text:=text;
  result:=textitem;
  AddObject(textitem);

end;
{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{
################################################################################
================================================================================
////////////////////////////////////////////////////////////////////////////////
--------------------------------------------------------------------------------
................................TRecordItemList....................................
--------------------------------------------------------------------------------
////////////////////////////////////////////////////////////////////////////////
================================================================================
################################################################################
}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}

function TRecordItemList.Add:TTreeListRecordItem;
begin
  Result:=TTreeListRecordItem.create;
  inherited add(result);
end;

procedure TRecordItemList.AddItem(recordItem: TTreeListRecordItem);
begin
  inherited add(recordItem);
end;

function TRecordItemList.AddWithText(s: string): TTreeListRecordItem;
begin
  result:=Add;
  result.SimpleText:=s;
end;

{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{
################################################################################
================================================================================
////////////////////////////////////////////////////////////////////////////////
--------------------------------------------------------------------------------
..........................TTreeListRecordItemText...............................
--------------------------------------------------------------------------------
////////////////////////////////////////////////////////////////////////////////
================================================================================
################################################################################
}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}

constructor TTreeListRecordItemText.create;
begin
  inherited;
  F_HotTrackFont:=TFont.create;
  F_HotTrackFont.Style:=[fsBold,fsUnderline];
  F_HotTrackFont.Color:=clRed;

  F_ParentHotTrack:=false;
  F_HotTrack:=true;
  F_ParentHotTrack:=true;
end;
procedure TTreeListRecordItemText.SetHotTrackFont(const value:TFont);
begin
  F_HotTrackFont.Assign(value);
end;
destructor TTreeListRecordItemText.destroy;
begin
  F_HotTrackFont.Free;
  inherited;
end;

{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{
################################################################################
================================================================================
////////////////////////////////////////////////////////////////////////////////
--------------------------------------------------------------------------------
............................TTreeListRecordItem.................................
--------------------------------------------------------------------------------
////////////////////////////////////////////////////////////////////////////////
================================================================================
################################################################################
}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}


procedure TTreeListRecordItem.SetTextItems(value:TRecordItemTextList);
begin
  F_TextItems.Assign(value);
end;

procedure TTreeListRecordItem.SetSimpleText(caption:string);
begin
  if F_TextItems.Count=0 then
    F_TextItems.AddTextItem(caption)
   else
    (TObject(F_TextItems[0]) as TTreeListRecordItemText).Text:=caption;
end;
function TTreeListRecordItem.GetSimpleText:string;
begin
  if F_TextItems.Count=0 then
    result:=''
   else
    result:=(TObject(F_TextItems[0]) as TTreeListRecordItemText).Text;
end;

procedure TTreeListRecordItem.PaintTo(const listView:TW32TreeListView;x:integer;const y,xColumn:integer;const parentItem:TTreeListItem);
var i:integer;
    ausgabeRect:TRect;
begin
  ausgabeRect.Left:=x;
  ausgabeRect.Right:=x+listView.F_Header.Sections[xColumn+1].Width-2{+listView.StartX};
  ausgabeRect.Top:=y;
  ausgabeRect.Bottom:=y+listView.RowHeight;
  listView.Canvas.brush.Style:=bsClear;
  for i:=0 to TextItems.Count-1 do begin
    if listView.hotTrackedTextItem=(TObject(TextItems[i]) as TTreeListRecordItemText) then begin
      if TTreeListRecordItemText(TextItems[i]).ParentHotTrackFont then begin
        if listView.selected=parentItem then
          listView.Canvas.Font.Assign(listView.SelectHotTrackFont)
         else
          listView.Canvas.Font.Assign(listView.HotTrackFont);
      end else
        listView.Canvas.Font.Assign(TTreeListRecordItemText(TextItems[i]).F_HotTrackFont)

//      listView.Canvas.Font.Assign(TTreeListRecordItemText(TextItems[i]).F_HotTrackFont)
    end else {if listView.Canvas.Font.Handle<>listView.Font.Handle then}
      if listView.selected=parentItem  then
        listView.Canvas.Font.Assign(listView.SelectedFont)
       else
        listView.Canvas.Font.Assign(listView.Font);
    listView.Canvas.TextRect(ausgabeRect,x+3,y,TTreeListRecordItemText(TextItems[i]).Text);
    x:=x+listView.Canvas.TextWidth( TTreeListRecordItemText(TextItems[i]).Text);
  end;
end;

function TTreeListRecordItem.GetRecordItemTextAtPos(const listView:TW32TreeListView;const TestX:integer):TTreeListRecordItemText;
var i,w,xpos:integer;
begin
  Result:=nil;
  xPos:=0;
  for i:=0 to TextItems.Count-1 do begin
    w:=listView.Canvas.TextWidth((TObject(TextItems[i]) as TTreeListRecordItemText).Text);
    if (TestX>xPos) and (TestX<xPos+w) then Result:=TTreeListRecordItemText(TextItems[i]);
    xPos:=xPos+w;
  end;
end;

constructor TTreeListRecordItem.create;
begin
  inherited;
  F_TextItems:=TRecordItemTextList.create;
end;

constructor TTreeListRecordItem.createWithText(caption:string);
begin
  create;
  SetSimpleText(caption);
end;

destructor TTreeListRecordItem.destroy;
begin
  F_TextItems.Free;
  inherited;
end;

{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{
################################################################################
================================================================================
////////////////////////////////////////////////////////////////////////////////
--------------------------------------------------------------------------------
...............................TTreeListItem....................................
--------------------------------------------------------------------------------
////////////////////////////////////////////////////////////////////////////////
================================================================================
################################################################################
}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}

//Create
constructor TTreeListItem.create(const Parent:TTreeListItem;const TreeListView:TW32TreeListView);
begin
  createWithCaption(Parent,TreeListView,'');
end;

constructor TTreeListItem.createWithCaption(const Parent:TTreeListItem;const TreeListView:TW32TreeListView;const ACaption:string);
begin
  F_Parent:=parent;
  F_caption:=ACaption;
  F_RecordItems:=TRecordItemList.create;
  F_SubItems:=TTreeListItems.create(self,TreeListView);
  F_Indent:=-1;
  F_TreeListView:=TreeListView;
  Expanded:=true;
end;

procedure TTreeListItem.DoChange;
begin
  RecordItems.DoChange;
end;
procedure TTreeListItem.DoChanging;
begin
  RecordItems.DoChanging;
end;

//Set-Funktion
procedure TTreeListItem.SetRecordItems(const value:TRecordItemList);
begin
  F_RecordItems.Assign(value);
end;

procedure TTreeListItem.SetExpand(const expanded:boolean);
begin
  if expanded then expand else Collapse;
end;

procedure TTreeListItem.Expand;
begin
  DoChanging;
  F_Expanded:=true;
//  if assigned(F_OnExpanded) then F_OnExpanded(self);
  DoChange;
end;
procedure TTreeListItem.Collapse;
begin
  DoChanging;
  F_Expanded:=false;
//  if assigned(F_OnCollapsed) then F_OnCollapsed(self);
  DoChange;
end;


procedure TTreeListItem.SetSubItems(const value: TtreeListItems);
begin
  F_SubItems.Assign(value);
end;


//Hinweis: starty wird nicht verändert, wenn das Item an der gesuchten Stelle ist
function TTreeListItem.GetItemAtPos(const listView:TW32TreeListView;const TestY:integer;var startY:integer):TTreeListItem;
var i:integer;
begin
  result:=nil;
  if (TestY>startY)and(TestY<startY+listView.RowHeight) then begin
    result:=self;
    exit;
  end;
  startY:=startY+listView.RowHeight;
  if Expanded then
    for i:=0 to SubItems.Count-1 do begin
      result:=(TObject(SubItems[i]) as TTreeListItem).GetItemAtPos(listView,TestY,startY);
      if result<>nil then exit;
    end ;
end;

function TTreeListItem.GetItemAtPosWithIndentSet(const listView:TW32TreeListView;const TestY,Indent:integer;var startY:integer):TTreeListItem;
var i:integer;
begin
  F_Indent:=Indent;
  result:=nil;
  if (TestY>startY)and(TestY<startY+listView.RowHeight) then begin
    result:=self;
    exit;
  end;
  startY:=startY+listView.RowHeight;
  for i:=0 to SubItems.Count-1 do begin
    result:=(TObject(SubItems[i]) as TTreeListItem).GetItemAtPosWithIndentSet(listView,TestY,indent + 1,startY);
    if result<>nil then exit;
  end;
end;

function TTreeListItem.GetRecordItemAtPos(const listView:TW32TreeListView;const TestX:integer):TTreeListRecordItem;
var i,x:integer;
begin
  Result:=nil;
  x:=listView.F_Header.Sections[0].Width;
  for i:=0 to min(listView.F_Header.Sections.Count-2,RecordItems.Count-1) do begin
    if TestX<x then exit;
    if (x+listView.F_Header.Sections[i+1].Width>TestX) then
      result:=RecordItems[i];
    x:=listView.F_Header.Sections[i+1].Width;
  end;
end;

function TTreeListItem.GetRecordItemTextAtPos(const listView:TW32TreeListView;const TestX:integer):TTreeListRecordItemText;
var i,x:integer;
begin
  Result:=nil;
  x:=listView.F_Header.Sections[0].Width;
  for i:=0 to min(listView.F_Header.Sections.Count-2,RecordItems.Count-1) do begin
    if x>TestX then exit;
    if (x+listView.F_Header.Sections[i+1].Width>TestX) then begin
      result:=(TObject(RecordItems[i]) as TTreeListRecordItem).GetRecordItemTextAtPos(listView,TestX-x);
    end;
    x:=x+listView.F_Header.Sections[i+1].Width;
  end;
end;

//Gibt das nächste Item zurück, dass auf der gleichen Ebene ist, gibt es kein solches, wird rekursiv das nächste Item zurückgegeben, dass auf einer kleinere Ebene hat
function TTreeListItem.GetNextItemIgnoringChildren: TTreeListItem;
var temp:integer;
begin
  if (parent=nil) then begin
    temp:=TreeListView.Items.IndexOf(self);
    if temp<TreeListView.Items.Count-1 then Result:=TreeListView.Items[temp+1]
    else Result:=self;
  end else begin
    temp:=Parent.SubItems.IndexOf(self);
    if temp<Parent.SubItems.Count-1 then Result:=parent.SubItems[temp+1]
    else result:=parent.GetNextItemIgnoringChildren;
  end;
end;

{function TTreeListItem.GetNextItem:TTreeListItem;
var temp:integer;
begin
  result:=self;
  if (SubItems.Count>0)and(expanded) then Result:=SubItems[0]
  else if Self.Parent=nil then begin
    temp:=Self.TreeListView.Items.IndexOf(self);
    if temp<Self.TreeListView.Items.Count-1 then Result:=Self.TreeListView.Items[temp+1];
  end else Result:=GetNextItemIgnoringChildren;
end;

function TTreeListItem.GetPrevItem:TTreeListItem;
var temp:integer;
begin
  result:=self;
  if self.Parent=nil then begin
    temp:=Self.TreeListView.Items.IndexOf(self);
    if temp>0 then Result:=(TObject(Self.TreeListView.items[temp-1]) as TTreeListItem).GetLastSubSubItem;
  end else begin
    temp:=Self.Parent.SubItems.IndexOf(self);
    if temp=0 then result:=self.parent
    else result:=(TObject(self.parent.SubItems[temp-1])as TTreeListItem).GetLastSubSubItem;
  end;
end;}

function TTreeListItem.GetLastSubSubItem:TTreeListItem;
begin
  result:=self;
  while (result.SubItems.Count<>0)and(result.Expanded) do
    Result:=Result.SubItems[Result.SubItems.count-1];
end;

function TTreeListItem.GetNextVisibleItem(Delta: longint=1): TTreeListItem;
var next: TTreeListItem;
begin
  if Delta<0 then exit(GetPrevVisibleItem(-Delta));
  Result:=self;
  while delta>0 do begin
    if (result.SubItems.Count>0) and (result.Expanded) then
      result:=result.SubItems[0]
    else begin
      next:=result.GetNextItemIgnoringChildren;
      if next=result then exit;
    end;
    result:=next;
    delta-=1;
  end;
end;

function TTreeListItem.GetPrevVisibleItem(Delta: longint): TTreeListItem;
var temp:longint;
begin
  if Delta<0 then exit(GetNextVisibleItem(-Delta));
  Result:=self;
  while delta>0 do begin
    if result.Parent=nil then begin
      temp:=result.TreeListView.Items.IndexOf(result);
      if temp>0 then Result:=result.TreeListView.Items[temp-1].GetLastSubSubItem;
    end else begin
      temp:=Result.Parent.SubItems.IndexOf(Result);
      if temp=0 then result:=result.parent
      else result:=result.parent.SubItems[temp-1].GetLastSubSubItem;
    end;
    Delta-=1;
  end
end;

procedure TTreeListItem.PaintTo(const listView:TW32TreeListView;const x:integer;var y:integer;const xColumn:integer;const last:boolean);
var i,x2,yold,tempX:integer;
    defaultDraw:boolean;
    rec:Trect;
begin
  listView.LastItems[xColumn]:=not last;

  if y+listView.RowHeight>listView.ClientHeight-listView.F_HScroll.Height then exit;
  listView.PaintEvenItem:=not listView.PaintEvenItem;
  if not Expanded then
    if (SubItems.GetRealItemCount([ricCountCollapsedSubItems]) and $1=0) then
      listView.PaintEvenItem:=not listView.PaintEvenItem;
  defaultDraw:=listView.DoCustomItemDrawEvent(cdevtPrePaint,self,x,y,xColumn,last);
  F_Indent:=xColumn;
  yold:=y;
  if defaultDraw then begin
    if y>listView.F_Header.Height then begin
      if listView.Striped then begin
        if listView.PaintEvenItem then listView.canvas.Brush.Color:=listView.StripedEvenColor
        else listView.canvas.Brush.Color:=listView.StripedOddColor;
        listView.Canvas.FillRect(rect(0,y,listView.clientWidth-listview.f_vscroll.width,y+listView.rowHeight));
      end;
      rec.Left:=x;
      rec.top:=y;
      rec.right:=listView.F_Header.Sections[0].Width-14+listView.StartX;
      rec.Bottom:=y+listView.RowHeight;
      case listView.RootLineMode of
        lmDot:  listView.DrawAlignDotLine(x-5,y+listView.RowHeight div 2,min(x,rec.right),y + listView.RowHeight div 2,listView.F_RootLineColor);
        lmSolid: with listView.canvas do begin
                   pen.color:=listView.F_RootLineColor;
                   pen.Style:=psSolid;
                   MoveTo(x-5,y+listView.RowHeight div 2);
                   LineTo(min(x,rec.right),y + listView.RowHeight div 2);
                 end;
      end;
      if listView.selected=self then begin
        listView.Canvas.pen.Style:=psClear;
        listView.Canvas.Brush.color:=listView.SelectBackColor;
        listView.Canvas.Brush.style:=bsSolid;
        listView.Canvas.font.Assign(listView.selectedfont);
       // listView.Canvas.font.Color:=clHighlightText;
        listView.Canvas.Rectangle(0,y,listView.ClientWidth-listView.F_VScroll.Width,y+listView.RowHeight);
       end else begin
        listView.Canvas.pen.Style:=psClear;
        listView.Canvas.brush.Style:=bsClear;
        listView.Canvas.font.Assign(listView.font);
      end;
      if rec.right>x-5 then begin
        case listView.RootLineMode of
          lmDot:  listView.DrawAlignDotLine(x-5,y,x-5,y+listView.RowHeight div 2,listView.F_RootLineColor);
          lmSolid: with listView.canvas do begin
                     MoveTo(x-5,y);
                     LineTo(x-5,y+listView.RowHeight div 2);
                   end;
        end;
        if (ImageTyp<>itNone) then begin
          case ImageTyp of
            itListIndex: if listView.Images<>nil then begin
                           listView.Images.draw(listView.Canvas,x,y,IndexOrBitmap);
                           listView.Canvas.brush.Style:=bsClear;
                           listView.Canvas.TextRect(rec,x+listView.Images.Width+3,y,Caption);
                         end;
            itBitmap:    if (TObject(IndexOrBitmap)<>nil)and(TObject(IndexOrBitmap)is TBitmap) then begin
                           listView.Canvas.Draw(x,y,TBitmap(IndexOrBitmap));
                           listView.Canvas.brush.Style:=bsClear;
                           listView.Canvas.TextRect(rec,x+TBitmap(IndexOrBitmap).Width+3,y,Caption);
                         end;
          end;
        end else begin
          listView.Canvas.brush.Style:=bsClear;
          listView.Canvas.TextRect(rec,x+2,y,Caption);
        end;
      end;
  //    selected:=listView.selected = self;
      x2:=-listView.F_HScroll.Position;
      for i:=0 to min(listView.F_Header.Sections.Count-2,RecordItems.Count-1) do begin
        x2:=x2+listView.F_Header.Sections[i].Width;
//      listView.Canvas.TextOut(x2,y,(TObject(RecordRecordItems[i]) as TTreeListRecordItem).Caption);
        if listView.DoCustomRecordItemDrawEvent(cdevtPrePaint,self,(TObject(RecordItems[i]) as TTreeListRecordItem),x2,y,i) then
          TTreeListRecordItem(RecordItems[i]).PaintTo(listView,x2,y,i,self);
        if not listView.DoCustomRecordItemDrawEvent(cdevtPostPaint,self,TTreeListRecordItem(RecordItems[i]),x2,y,i) then break;
      end;

    end;
  tempX:=listView.StartX-5;
  if listView.RootLineMode <> lmNone then
    for i:=0 to xColumn do begin
      if tempx>rec.right then break;
      if listView.LastItems[i] then
        if listView.RootLineMode=lmDot then
          listView.DrawAlignDotLine(tempX,y,tempX,y+listView.RowHeight-1,listView.F_RootLineColor)
         else with listView.canvas do begin
           pen.color:=listView.F_RootLineColor;
           pen.Style:=psSolid;
           MoveTo(tempX,y);
           LineTo(tempX,y+listView.RowHeight);
         end;
      tempX:=tempX+15;
    end;
  end;
  y:=y+listView.RowHeight;
  if defaultDraw then
    case listView.HorizontalLineMode of
      lmSolid: with listView.canvas do begin
                 pen.color:=listView.F_HorizontalLineColor;
                 pen.Style:=psSolid;
                 MoveTo(0,y-1);
                 LineTo(ListView.ClientWidth-ListView.F_VScroll.Width,y-1);
               end;
      lmDot:   listView.DrawAlignDotLine(0,y-1,ListView.ClientWidth-ListView.F_VScroll.Width,y-1,listView.F_HorizontalLineColor);
    end;
  if (SubItems.Count>0) then begin
    if defaultDraw then
      if (yold>listView.F_Header.Height)and(rec.right>x-5) then begin
        listView.Canvas.pen.Style:=psSolid;
        listView.Canvas.pen.Color:=clBlack;
        listView.Canvas.brush.Style:=bsSolid;
        listView.Canvas.brush.Color:=listView.ButtonColor;
        listView.canvas.Rectangle(x-10,yold+listView.RowHeight div 2-5,x-1,yold+listView.RowHeight div 2+4);
        listView.canvas.moveTo(x-8,yold+listView.RowHeight div 2-1);
        listView.canvas.LineTo(x-3,yold+listView.RowHeight div 2-1);
        if not Expanded then begin
          listView.canvas.moveTo(x-6,yold+listView.RowHeight div 2-3);
          listView.canvas.LineTo(x-6,yold+listView.RowHeight div 2+2);
        end;
      end;

    if (Expanded)and(listView.DoCustomItemDrawEvent(cdevtPostPaint,Self,x,y,xColumn,last)) then
      for i:=0 to SubItems.Count-1 do
        (TObject(SubItems[i]) as TTreeListItem).PaintTo(listView,x+15,y,xColumn+1,i=SubItems.Count-1);
  end;
end;

//Destroy
destructor TTreeListItem.destroy;
begin
  F_RecordItems.free;
  F_SubItems.free;
  inherited;
end;


{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{
################################################################################
================================================================================
////////////////////////////////////////////////////////////////////////////////
--------------------------------------------------------------------------------
..............................TW32TreeListView..................................
--------------------------------------------------------------------------------
////////////////////////////////////////////////////////////////////////////////
================================================================================
################################################################################
}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}

//Create
constructor TW32TreeListView.create(aowner:TComponent);
begin
  inherited;
  F_Items:=TTreeListItems.Create(nil,self);
  F_Items.OnChange:=_GeneralEvent;
  F_Items.freeing:=false;

  //Fonts
  F_HotTrackFont:=TFont.create;
  F_HotTrackFont.Color:=clBlue;
  F_HotTrackFont.Style:=[fsBold,fsUnderline];

  F_SelectHotTrackFont:=TFont.create;
  F_SelectHotTrackFont.Color:=clHighlightText;
  F_SelectHotTrackFont.Style:=[fsBold,fsUnderline];

  F_SelectedFont:=TFont.create;
  F_SelectedFont.Color:=clHighlightText;
  F_SelectedFont.Style:=[];

  Font.Color:=clWindowText;
  Font.Style:=[];

  //Backcolors
  F_SelectBackColor:=clHighlight;
  F_BgColor:=clWindow;
  F_ButtonColor:=clWindow;

  F_StripedOddColor:=clWindow;
  F_StripedEvenColor:= $00E0FFFF;
  F_Striped:=true;

  //Linien
  F_RootLines:=lmDot;
  F_RootLineColor:=clWindowFrame;
  F_HorizontalLines:=lmNone;
  F_HorizontalLineColor:=clWindowFrame;
  F_VerticalLines:=lmDot;
  F_VerticalLineColor:=clWindowFrame;

  InternOptions_tlio:=[];

  width:=200;
  Height:=200;

  {$ifndef fpc}
  BevelInner:=bvLowered;
  BevelOuter:=bvLowered;
  BevelKind:=bkTile;
  {$endif}
  TabStop:=true;

  //Headercontrol initialisieren
  F_Header:=THeaderControl.create(self);
  F_Header.Align:=alNone;
  F_Header.parent:=Self;
  F_Header.Visible:=true;
  with F_Header.Sections.Add do begin
    Caption:='';
  end;
  F_Header.Left:=0;
  F_Header.Width:=10000;
  {$ifdef fpc}
  if font.Height=0 then
    F_Header.Height:=13+2*GetSystemMetrics(SM_CYEDGE)
   else
    F_Header.Height:=abs(font.height)+2*GetSystemMetrics(SM_CYEDGE);
  {$endif}
  F_Header.OnSectionTrack:=_HeaderSectionTrack;
  F_Header.OnSectionResize:=_HeaderSectionResize;

  //Scrollbar initialisieren
  F_VScroll:=TScrollbar.create(self);
  F_VScroll.parent:=self;
  F_VScroll.Enabled:=false;
  F_VScroll.Visible:=true;
  F_VScroll.Kind:=sbVertical;
  F_VScroll.OnChange:=_VScrollChange;
 // F_VScroll.Ctl3D


  //Scrollbar initialisieren
  F_HScroll:=TScrollbar.create(self);
  F_HScroll.parent:=self;
  F_HScroll.Enabled:=false;
  F_HScroll.Visible:=true;
  F_HScroll.Kind:=sbHorizontal;
  F_HScroll.Left:=0;
  F_HScroll.OnChange:=_HScrollChange;

  RowHeight:=F_Header.Height-2*GetSystemMetrics(SM_CYEDGE);
end;


function TW32TreeListView.DoCustomBackgroundDrawEvent (eventTyp_cdet:TCustomDrawEventTyp):boolean;
begin
  Result:=true;
  if assigned(F_CustomBgDraw) then F_CustomBgDraw(self,eventTyp_cdet,result);
end;
function TW32TreeListView.DoCustomItemDrawEvent(eventTyp_cdet:TCustomDrawEventTyp;item:TTreeListItem;xpos,ypos,xColumn:integer;lastItem:boolean):boolean;
begin
  Result:=true;
  if assigned(F_CustomItemDraw) then F_CustomItemDraw(self,eventTyp_cdet,item,xpos,ypos,xColumn,lastItem,result);
end;
function TW32TreeListView.DoCustomRecordItemDrawEvent(eventTyp_cdet:TCustomDrawEventTyp;parentItem:TTreeListItem;RecordItem:TTreeListRecordItem;xpos,ypos,xColumn:integer):boolean;
begin
  Result:=true;
  if assigned(F_CustomRecordItemDraw) then F_CustomRecordItemDraw(self,eventTyp_cdet,parentItem,recordItem,xpos,ypos,xColumn,result);
end;

procedure TW32TreeListView.DoSelect;
begin
  if assigned(F_OnSelect) then F_OnSelect(self,selected);
end;

//Kommunikationsroutinen (Set- und Getfunktionen)
procedure TW32TreeListView.SetItems(const value:TTreeListItems);
begin
  F_Items.Assign(value);
end;

procedure TW32TreeListView.SetTopPos(const i:integer);
begin
end;

function TW32TreeListView.GetTopPos:integer;
begin
  result:=2+F_Header.Height-F_VScroll.Position*RowHeight;
end;

procedure TW32TreeListView.SetHeaderSections(const value:THeaderSections);
begin
  F_Header.Sections.Assign(value);
end;
function TW32TreeListView.GetHeaderSections():THeaderSections;
begin
  Result:=F_Header.Sections;
end;

function TW32TreeListView.GetItemAtPos(const y:integer):TTreeListItem;
var i,startY:integer;
begin
  startY:=TopPos;
  result:=nil;
  for i:=0 to Items.Count-1 do begin
    result:=(TObject(Items[i]) as TTreeListItem).GetItemAtPos(self,y,startY);
    if result<>nil then exit;
  end;
end;

function TW32TreeListView.GetItemAtPosWithIndentSet(const y:integer):TTreeListItem;
var i,startY:integer;
begin
  startY:=TopPos;
  result:=nil;
  for i:=0 to Items.Count-1 do begin
    result:=(TObject(Items[i]) as TTreeListItem).GetItemAtPosWithIndentSet(self,y,0,startY);
    if result<>nil then exit;
  end;
end;

function TW32TreeListView.GetRecordItemAtPos(const x,y:integer):TTreeListRecordItem;
var item:TTreeListItem;
begin
  result:=nil;
  item:=getItemAtPos(y);
  if item<>nil then
   result:= item.GetRecordItemAtPos(self,x);

end;
function TW32TreeListView.GetRecordItemTextAtPos(const x,y:integer):TTreeListRecordItemText;
var item:TTreeListItem;
begin
  result:=nil;
  item:=getItemAtPos(y);
  if item<>nil then
   result:= item.GetRecordItemTextAtPos(self,x);
end;

procedure TW32TreeListView.SetHotTrackFont(const value:TFont);
begin
  F_HotTrackFont.Assign(value);
end;
procedure TW32TreeListView.SetSelectedFont(const value:TFont);
begin
  F_SelectedFont.Assign(value);
end;
procedure TW32TreeListView.SetSelectHotTrackFont(const value:TFont);
begin
  F_SelectHotTrackFont.Assign(value);
end;

procedure TW32TreeListView.BeginUpdate;
begin
  include(InternOptions_tlio,tlioUpdating)
end;

procedure TW32TreeListView.EndUpdate;
begin
  exclude(InternOptions_tlio,tlioUpdating);
  paint;
end;

function TW32TreeListView.VisibleRowCount:longint;
begin
  if RowHeight=0 then exit(0);
  result:=(ClientHeight-F_Header.Height) div RowHeight;
end;

procedure TW32TreeListView.setImageList(const images:TImageList);
begin
  F_ImageList:=images;
end;

procedure TW32TreeListView.SetRowHeight(const newHeight:integer);
begin
  if newHeight and $1=$1 then F_RowHeight:=newHeight+1
  else F_RowHeight:=newHeight;
end;

procedure TW32TreeListView.SetHotTrackSubTextItems(const value:boolean);
begin
//  if value then hotTrackedTextItem:=
//  else hotTrackedTextItem:=nil;
  F_HotTrackSubTextItems:=value;
end;

procedure TW32TreeListView.DrawAlignDotLine(x,y:integer;const x2,y2:integer;const color:TColor);
var dc:HDC;
     F_HeaderHeight:integer;
begin
  dc:=Canvas.Handle;
  F_HeaderHeight:=F_Header.Height;
  if y2<F_HeaderHeight then exit;
  if y<F_HeaderHeight then y:=F_HeaderHeight;
  if x=x2 then begin
    while (y<=y2) do begin
      SetPixel(dc,x,y,color);
      inc(y,2);
    end;
  end else begin
    while (x<=x2) do begin
      SetPixel(dc,x,y,color);
      inc(x,2);
    end;
  end;
end;

//Interne Kommunikation mit Unterkomponenten
procedure TW32TreeListView._GeneralEvent(Sender: TObject);
begin
  if not HandleAllocated then exit;
  UpdateScrollBarPos;
  UpdateScrollSize;
  paint;
end;
{$ifndef fpc}
procedure TW32TreeListView._HeaderSectionTrack(HeaderControl: THeaderControl; Section: THeaderSection; Width: Integer; State: TSectionTrackState);
{$else}
procedure TW32TreeListView._HeaderSectionTrack(HeaderControl: TCustomHeaderControl; Section: THeaderSection; Width: Integer; State: TSectionTrackState);
{$endif}
begin
  UpdateScrollSize;
  if assigned(F_HeaderSectionTrack) then F_HeaderSectionTrack(HeaderControl,Section,Width,State);
  paint;
end;
{$ifndef fpc}
procedure TW32TreeListView._HeaderSectionResize(HeaderControl: THeaderControl; Section: THeaderSection);
{$else}
procedure TW32TreeListView._HeaderSectionResize(HeaderControl: TCustomHeaderControl; Section: THeaderSection);
{$endif}
begin
  UpdateScrollSize;
  if assigned(F_HeaderSectionResize) then F_HeaderSectionResize(HeaderControl,Section);
  paint;
end;
procedure TW32TreeListView._HScrollChange(Sender: TObject);
begin
  UpdateScrollBarPos;
//  UpdateScrollSize;
  if assigned(F_HScrollBarChange) then F_VScrollBarChange(F_HScroll);
  paint;
end;

procedure TW32TreeListView._VScrollChange(Sender: TObject);
begin
  UpdateScrollBarPos;
//  UpdateScrollSize;
  if assigned(F_VScrollBarChange) then F_VScrollBarChange(F_VScroll);
  paint;
end;

procedure TW32TreeListView.UpdateScrollBarPos;
begin
  if (tlioDeleting in InternOptions_tlio) or (tlioUpdating in InternOptions_tlio) then exit;
  F_Header.left:=-F_HScroll.Position;

  F_VScroll.Left:=ClientWidth-F_VScroll.Width;
  F_VScroll.Top:=F_Header.Height;
  F_VScroll.Height:=ClientHeight-F_VScroll.Top-F_HScroll.Height;

  F_HScroll.Top:=ClientHeight-F_HScroll.Height;
  F_HScroll.Width:=ClientWidth-F_VScroll.Width;
end;

procedure TW32TreeListView.UpdateScrollSize;
var i,j:integer;
begin
  if (tlioDeleting in InternOptions_tlio) or (tlioUpdating in InternOptions_tlio) then exit;
  i:=Items.GetRealItemCount([])-(Height-F_Header.Height-F_HScroll.Height) div RowHeight; //Anzahl der nicht anzeigbare Items
  if i-1>F_VScroll.Min then begin
    F_VScroll.Enabled:=false;
    F_VScroll.Enabled:=true;
    F_VScroll.Max:=i-1;
  end else begin
    F_VScroll.Enabled:=true;
    F_VScroll.Enabled:=false;
  end;
  //F_VScroll.PageSize:=(ClientHeight-F_HScroll.Height*3-F_Header.Height) div (F_VScroll.Max - F_VScroll.Min + 1);
  F_VScroll.PageSize:=50;

  j:=0;
  for i:=0 to F_Header.Sections.Count-1 do begin
    j:=j+F_Header.Sections[i].Width;
  end;
  i:=j-width+F_VScroll.Width;
  if i>0 then F_HScroll.max:=i
  else begin
    F_HScroll.max:=0;
    F_HScroll.Enabled:=true;
    F_HScroll.Enabled:=false;
  end;
end;

//Messages
procedure TW32TreeListView.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if (AComponent=F_ImageList) and (Operation=opRemove) then
    F_ImageList:=nil;
  inherited;
end;

procedure TW32TreeListView.WndProc(var message:TMessage);
///var TreeListRecordText:TTreeListRecordItemText;
var temp:integer;
    tempRecordItemText:TTreeListRecordItemText;
begin

  case message.Msg of
    WM_GETDLGCODE:  message.Result:=DLGC_WANTARROWS or DLGC_WANTCHARS;
    WM_MOUSEMOVE: begin
                    inherited;
                    {TreeListRecordText}hotTrackedTextItem:=GetRecordItemTextAtPos(TWMMouseMove(message).XPos,TWMMouseMove(message).YPos);
                    if hotTrackedTextItem<>nil then
                      if not ((hotTrackedTextItem.ParentHotTrack and HotTrackSubTextItems) or (not hotTrackedTextItem.ParentHotTrack and hotTrackedTextItem.HotTrack)) then
                        hotTrackedTextItem:=nil;
                    if hotTrackedTextItem<>nil then
                      Cursor:=crHandPoint
                     else
                      Cursor:=crDefault; 
                    paint;
                  end;
    WM_LBUTTONDOWN: begin
                      SetFocus;
                      selected:=GetItemAtPos(TWMLBUTTONDOWN(message).YPos);

                      if (selected<>nil) and (TWMLBUTTONDOWN(message).XPos<selected.F_Indent*15-1+StartX) and (TWMLBUTTONDOWN(message).XPos>selected.F_Indent*15-10+StartX) then
                        selected.Expanded:=not selected.Expanded; 
                      Paint;
                      if (selected<>nil)and(assigned (F_ClickAtItem)) then F_ClickAtItem(self,selected);
                      if (selected<>nil)and(assigned (F_ClickAtRecordItemText)) then begin
                        tempRecordItemText:=selected.GetRecordItemTextAtPos(self,TWMLBUTTONDOWN(message).XPos);
                        if tempRecordItemText<>nil then
                          F_ClickAtRecordItemText(self,selected,tempRecordItemText);
                      end;
                      DoSelect;
                      inherited;
                    end;
    WM_KEYDOWN: begin
                  case TWMKeyDown(message).CharCode of
                    VK_UP:   begin
                               if selected=nil then selected:=Items[0]
                               else selected:=selected.GetPrevVisibleItem;
                               DoSelect;
                             end;

                    VK_DOWN: begin
                               if selected<>nil then selected:=selected.GetNextVisibleItem()
                               else if items.count>0 then selected:=Items[items.count-1];
                               DoSelect;
                             end;

                    VK_HOME: if items.count>0 then begin
                               selected:=Items[0];
                               DoSelect;
                             end;

                    VK_END: if items.count>0 then begin
                               selected:=(TObject(Items[items.count-1])as TTreeListItem).GetLastSubSubItem;
                               DoSelect;
                             end;

                    VK_PRIOR: if items.count>0 then begin
                      selected:=selected.GetPrevVisibleItem(VisibleRowCount);
                      
                    end;

                    VK_RIGHT: begin
                                if selected<>nil then begin
                                  if not selected.Expanded then selected.Expand;
                                  if selected.SubItems.Count>0 then selected:=selected.SubItems[0];
                                  DoSelect;
                                end;
                              end;

                    VK_LEFT: begin
                               if selected<>nil then begin
                                 if (selected.Expanded) and (selected.SubItems.Count>0) then selected.Collapse
                                 else if selected.Parent<>nil then selected:=selected.Parent;
                                 DoSelect;
                               end;
                             end;

                    VK_BACK: if selected.Parent<>nil then begin
                               selected:=selected.Parent;
                               DoSelect;
                             end;
                  end;
                  temp:=(Items.RealIndexOf(selected,[]))*RowHeight+TopPos+F_Header.Height;
                  if temp-rowHeight<F_Header.Height then
                    F_VScroll.Position:=min(F_VScroll.Position-1,F_VScroll.Position+(temp-F_Header.Height-rowHeight-3) div RowHeight);
                  if temp>ClientHeight-F_HScroll.Height then
                    F_VScroll.Position:=max(F_VScroll.Position+1,F_VScroll.Position+((temp-ClientHeight+F_HScroll.Height+F_Header.Height) div RowHeight));
                  paint;
                end;
    WM_SETFOCUS:    begin
                      Paint;
                      inherited;
                    end;
    WM_KILLFOCUS:   begin
                      paint;
                      inherited;
                    end;
    WM_SIZE:        begin
                      UpdateScrollBarPos;
                      UpdateScrollSize;
                      paint;
                      inherited;
                    end;
    WM_PAINT:       begin
                      if tlioDeleting in InternOptions_tlio then begin
                        inherited;
                        exit;
                      end;
                      include(InternOptions_tlio,tlioDisablePainting);
                      try
                        inherited;
                      finally
                        exclude(InternOptions_tlio,tlioDisablePainting);
                      end;
                      paint;
                    end;
    WM_ERASEBKGND: message.Result:=1;
    else inherited;
  end;
end;

//Ausgaberoutinen
procedure TW32TreeListView.Paint;
var i,ypos,xpos:integer;
    doubleBuffer:Tbitmap;
    oldHandle:Thandle;
    defaultDraw:boolean;
begin
  if (tlioUpdating in InternOptions_tlio) or (tlioDisablePainting in InternOptions_tlio) or (tlioDeleting in InternOptions_tlio) or (f_items=nil)or (f_items.freeing) then exit;
  //F_VScroll.Repaint;
  //F_HScroll.Repaint;
  PaintEvenItem:=true;
  canvas.Lock;
  try
    doubleBuffer:=TBitmap.create;
    doubleBuffer.Width:=Width;
    doubleBuffer.Height:=Height;
    doubleBuffer.Canvas.Lock;
    oldHandle:=Canvas.Handle;
    try
      with Canvas do begin
        //Background
        defaultDraw:=DoCustomBackgroundDrawEvent(cdevtPrePaint);
        if defaultDraw then begin
          pen.Style:=psClear;
          brush.Color:=clBtnFace;
          brush.Style:=bsSolid;
          Rectangle(ClientWidth-F_VScroll.Width,ClientHeight-F_HScroll.Height,clientWidth+1,clientHeight+1);

          Handle:=doubleBuffer.Canvas.Handle; //Canvas verbiegen, mit z.B.: Canvas.LineTo wird nun in den Buffer gezeichnet

          pen.Style:=psClear;
          brush.Style:=bsSolid;
          brush.color:=F_BgColor;

//        Rectangle(handle,0,F_Header.Height,ClientWidth-F_VScroll.Width,ClientHeight-F_HScroll.Height);
          FillRect(rect(0,F_Header.Height,ClientWidth-F_VScroll.Width,ClientHeight-F_HScroll.Height));


        end;
        //Items
        ypos:=TopPos;
        xpos:=13-F_HScroll.Position;
        StartX:=xpos;
        for i:=0 to Items.count-1 do begin
          (TObject(Items[i]) as TTreeListItem).PaintTo(self,xpos,ypos,0,i = Items.count-1);
  //        if i=Items.count-2 then
//           DrawAlignDotLine(xpos-5,max(TopPos,f_header.height),xpos-5,ypos+RowHeight div 2,clBlack);
        end;
        //Lines
        if defaultDraw then begin
          xpos:=-F_HScroll.Position;
          for i:=0 to F_Header.Sections.Count-1 do begin
            inc(xpos,F_Header.Sections[i].Width);
            case VerticalLineMode of
              lmSolid: begin
                         pen.color:=VerticalLineColor;
                         pen.Style:=psSolid;
                         MoveTo(xpos,F_Header.Height);
                         LineTo(xpos,ClientHeight-F_HScroll.Height);
                       end;
              lmDot:   DrawAlignDotLine(xpos,F_Header.Height,xpos,ClientHeight-F_HScroll.Height,VerticalLineColor);
            end;
          end;
        end;
        DoCustomBackgroundDrawEvent(cdevtPostPaint);
        Handle:=oldHandle; //Handle zurückbiegen, nun wird wieder ins Steuerelement gezeichnez
        CopyRect(rect(0,F_Header.Height,ClientWidth-F_VScroll.Width,ClientHeight-F_HScroll.Height),doubleBuffer.canvas,rect(0,F_Header.Height,ClientWidth-F_VScroll.Width,ClientHeight-F_HScroll.Height)); //DoubleBuffer ausgeben
      end;
    finally
      canvas.Handle:=oldHandle;
      doubleBuffer.Canvas.Unlock;
      doubleBuffer.free;
    end;
  finally
    canvas.Unlock;
  end;
end;

//Destroy
destructor TW32TreeListView.destroy;
begin
  Include(InternOptions_tlio,tlioDeleting);
  F_HotTrackFont.free;
  F_SelectHotTrackFont.free;
  F_SelectedFont.free;

  F_HScroll.free;
  F_VScroll.free;
  F_Header.free;
  F_Items.free;
  inherited;
end;
procedure Register;
begin
  RegisterComponents('BeniBela', [TW32TreeListView]);
end;

end.







