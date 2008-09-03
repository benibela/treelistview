{**
  This unit contains the TTreeListView which is a combination of a TreeView and a
  ListView. This means that you can organize the items in a tree and show
  additional information in columns.

  $Revision$
  @lastmod $Date$
  @author Benito van der Zander (http://www.benibela.de)
  @author Thanks to: Bruce Christensen
}

unit TreeListView;
{$ifdef fpc}
  {$mode delphi}
{$endif}
{$ifdef clr}
  {$UNSAFECODE ON}
{$endif}

{$ifdef lcl}
  {$define allowHeaderDragging}  //this needs at least lazarus 9.25 Beta(SVN)
  {$define allowHeaderVisible}   //this needs the patch from bugs.freepascal.org/view.php?id=11727 (in 9.25)
{$endif}
interface

uses
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs,comctrls,stdctrls,Menus,math,
  findControl
  {$ifdef clr},types{$endif}
  {$ifdef lcl},LCLType,LCLIntf, LMessages{$else},windows,messages{$endif};

type
  {$TYPEINFO ON}
  //Forward
  TTreeListRecordItem=class;
  TTreeListItem=class;
  TTreeListView = class;

  TImageTyp=(itNone,itListIndex,itBitmap);
  {** @abstract This is a list storing TObjects
      The list supports change notifications and automatically deletes the items}
  TObjectList= class(TList)
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

      constructor Create;
      destructor Destroy;override;
    protected
      freeing:boolean;
      editing:boolean;

      //Eventauslöser
      procedure DoChanging;virtual;
      procedure DoChange;virtual;

      function Add(Item: TObject): Integer;
      procedure Insert(Index: Integer; Item: TObject);
      function Remove(Item: TObject): Integer;
  end;

  //**This specifies if invisible/collapsed items are counted
  TRealItemCounting=set of (ricCountCollapsedsubItems{,ricCountExpandItems,ricCountEndNodes});

  { TTreeListItems }
  //**This is a typical compare function
  TTreeListItemCompare = function (i1, i2: TTreeListItem): longint of object;
  {** @abstract This is the list used for storing (sub)-items
  }
  TTreeListItems=class(TObjectList)
  private
    procedure Put(Index: Integer; const AValue: TTreeListItem);
  protected
    F_Parent:TtreeListItem;
    F_TreeListView:TTreeListView;
    function Get(Index: Integer): TTreeListItem; {$ifdef fpc}inline;{$endif}
    procedure Sort(CompareFunc: TTreeListItemCompare);
  public
    constructor create(parent:TTreeListItem;const TreeListView:TTreeListView);

    //**This adds an item with the given caption to this list
    function Add(caption:string=''):TTreelistItem;overload;
    //**This adds an item with the given parent and caption @br
    //**If parent is @nil the item is added to this list, otherwise to the list parent.subitems
    function Add(Parent:TTreeListItem;caption:string=''):TTreelistItem;overload;

    //**This counts all (direct and indirect) children of this item
    function GetRealItemCount(const countTyp:TRealItemCounting ) :integer;
    //**This retrieve the real index of the given item @br
    //**The real index tells you how many items (including all children) are between item and self @br
    //**This also finds indirect children
    function RealIndexOf(const item:ttreeListItem;const countTyp:TRealItemCounting):integer;
    //**This get the item which are certain real index @seealso RealIndexOf
    function GetItemWithRealIndex(index:integer):TTreeListItem;

    //**This searches recursive for a item with the given caption
    //**@return the first matching item or nil if nothing is found
    function FindItemWithText(caption: string): TTreeListItem;
    //**This searches recursive for a item with text in the column pos (if pos = 0 this is the same as FindItemWithText)
    //**@return the first matching item or nil if nothing is found
    function FindItemWithRecordText(pos: longint; text: string):TTreeListItem;

    //** access to the direct sub items
    property Items[Index: Integer]: TTreeListItem read Get write Put; default;
  end;

  { TRecordItemList }
  {** @abstract This list stores the items in the detail columns }
  TRecordItemList=class(TObjectList)
  private
    Owner: TTreeListItem;
    function Get(Index: Integer): TTreeListRecordItem;
    procedure Put(Index: Integer; const AValue: TTreeListRecordItem);
  public
    property Items[Index: Integer]: TTreeListRecordItem read Get write Put; default;
    function Add:TTreeListRecordItem;overload;
    function Add(s: string):TTreeListRecordItem;overload;
    procedure AddItem(recordItem: TTreeListRecordItem);
  end;


  { TTreeListRecordItem }
  {** @abstract This is a item shown in the detail columns}
  TTreeListRecordItem=class(TPersistent)
    protected
      F_Parent: TTreeListItem;
      F_Text:string;
      F_Index:Longint;
      {F_HotTrackFont:TFont;
      F_HotTrack:boolean;
      F_ParentHotTrack:boolean;
      F_ParentHotTrackFont:boolean; }
      procedure SetText(caption:string);
    public
      //** This selects the font used to draw this item (useful when owner drawing)
      procedure selectFont(can: TCanvas);
      //procedure PaintTo(const listView:TTreeListView;x:integer;const y,xColumn:integer;const parentItem:TTreeListItem);

      //** This returns the size of this item
      function GetNecessaryWidth(listView:TTreeListView=nil): longint;

      constructor Create(aparent:TTreeListItem);overload;                 //**<Creates an item
      constructor Create(aparent:TTreeListItem;caption:string);overload;  //**<Creates an item with given caption
      destructor Destroy;override;
    published
      property Text:string read F_Text write setText;
  end;

  {** @abstract This is an item which can contain subitems and items in the detail columns }
  TTreeListItem=class(TPersistent)
  private
    function GetText: string;
    procedure SetText(const AValue: string);
    protected
      F_ImageIndex:longint;
      F_ImageBitmap:graphics.TBitmap;
      F_SubItems:TTreeListItems;
      F_RecordItems:TRecordItemList;
      F_Expanded,F_Selected,F_MouseSelected:boolean;

      F_Indent:integer; //Gibt an, wieoft, das Item eingerückt wurde, diese Eigenschaft wird von PaintTo und GetItemAtPosWithIndentSet gesetzt, und muss *nicht* stimmmen

      F_Parent:Ttreelistitem;
      F_TreeListView:TTreeListView;

  //   F_ParentFont:boolean;
  //    F_Font:TFont;
      function GetExtraTextIndentation(column: longint): longint; //mainly tree width
      function GetExtendingButtonPos: longint;

      procedure DoChange;
      procedure DoChanging;

      procedure SetSelected(newSelected: boolean);

      procedure SetSubItems(const value:TtreeListItems);
      procedure SetRecordItems(const value:TRecordItemList);
      procedure SetExpand(const expanded:boolean);

      function GetRecordItemsText(i: Integer): string;
      procedure SetRecordItemsText(i: Integer; const AValue: string);
    public
      Tag:longint; //**< This value can be used to store arbitrary integer values

      //**This creates an item with given parent and caption in the given TreeListView
      constructor Create(const parent:TTreeListItem;const TreeListView:TTreeListView;const ACaption:string='');overload;

      //**This returns the size of the displayed item @br
      //** @return if column = -1 the size of the whole line is returned, otherwise the size of the given column
      function getBounds(column: longint):TRect; //-1 => whole line
      //**This returns the maximal size of the displayed text @br This is like getBounds but subtracts indentation and padding
      function getMaxTextBounds(column: longint):TRect; //-1 => whole line
      //**This returns the item in the given TreeListView at the position TestY which is a sub item (or/of) self @br
      //**startY returns the top position of the found item @seealso TTreeListView.GetItemAtPos
      function GetItemAtPos(const listView:TTreeListView;const TestY:integer;var startY:integer):TTreeListItem;
      //**This returns the record item at the given position @seealso TTreeListView.GetRecordItemAtPos
      function GetRecordItemAtPos(const listView:TTreeListView;const TestX:integer):TTreeListRecordItem;

      //**This returns the width of the largest record item in the column id of any sub item
      function GetMaxColumnWidth(const id:longint): longint;

      //**This expands this item, to show all subitems
      procedure Expand;
      //**This collapses this item, to hide all subitems
      procedure Collapse;


      function GetNextItemIgnoringChildren:TTreeListItem; //**<Returns the next item which is no sub item of this @br Notice that this runs in O(m), so don't use it in a loop
      function GetLastVisibleSubSubItem:TTreeListItem; //**<Returns the latest visible item which is an (indirect) children of this
      function GetLastSubSubItem:TTreeListItem; //**<Returns the latest item which is an (indirect) children of this
      //If the item doesn't exists the current item is returned!
      function GetNextVisibleItem(Delta:longint=1):TTreeListItem;//**<Returns the next visible item, or the Delta-th next item. Is Delta < 0 this is like a call to GetPrevVisibleItem @br Notice that this runs in O(m), so don't use it in a loop. If the item doesn't exists the current item is returned!
      function GetPrevVisibleItem(Delta:longint=1):TTreeListItem;//**<Returns the previous visible item, or the Delta-th previous item. Is Delta < 0 this is like a call to GetNextVisibleItem @br Notice that this runs in O(m), so don't use it in a loop. If the item doesn't exists the current item is returned!
      function GetNextItem():TTreeListItem;//**< Returns the next item @br Notice that this runs in O(m), so don't use it in a loop. If the item doesn't exists the current item is returned!
      function GetPrevItem():TTreeListItem;//**< Returns the previous item  @br Notice that this runs in O(m), so don't use it in a loop. If the item doesn't exists the current item is returned!
      function GetParentInList(List: TTreeListItems=nil):TTreeListItem;//**< Returns the parent which is in the given list, or nil. @br If List = nil then it takes TreeListView.Items @br If self is in the list it returns self


      property Parent:TTreeListItem read F_parent; //**< This is a parent of this item @br This is @nil if the item is in @noAutoLink TreeListView.Items
      property TreeListView:TTreeListView read F_TreeListview; //**< This is the TreeListView showing this item
      function ParentItems: TTreeListItems; //**< This returns the list containing the item @br It is either @noAutoLink TreeListView.Items or @noAutoLink Parent.SubItems
      
      //**This draws the item @br Don't call it direct
      //**@param listView TreeListView
      //**@param y position
      //**@param xColumn indentation/level of this item, this value will be stored
      //**@param last specifies if this the last item with this indentation
      procedure PaintTo(const listView:TTreeListView;var y:integer;const xColumn:integer;const last:boolean);

      //**Destroy
      destructor Destroy;override;

      function SeemsSelected:boolean;//**< Returns if the items is drawn selected @br When the user selects new items there new selection state can be previewed
      property Indent:integer read F_Indent;//**< Level of indentation @br This value is not guaranteed to be correct
      property Expanded:boolean read F_expanded write SetExpand; //**< Specifies if the sub items are currently visible

      property RecordItemsText[i: Integer]:string read GetRecordItemsText write SetRecordItemsText; //**< Sets the value of the given column @br Notice that this array is 0-based and RecordItemsText[0] is always the same as Text @br Getting a not existing item will give you '', setting will create it
    published
      property RecordItems:TRecordItemList read F_RecordItems write SetRecordItems; //**< Items in the columns @br Normally you can use RecordItemsText for easier access
      property SubItems:TTreeListItems read F_SubItems write SetSubItems; //**< Indented child items
      property ImageIndex:longint read F_ImageIndex write F_ImageIndex; //**< If this is > -1 then the image of the TreeListView.Images will be painted before this item @br This property is ignored if ImageBitmap <> @nil or TreeListView.Images = @nil
      property ImageBitmap:graphics.TBitmap read F_ImageBitmap  write F_ImageBitmap; //**< Bitmap which should be drawn before the item
      property Text:string read GetText write SetText; //**< Text in the first column of this item @br This is always equal to RecordItemsText[0]
      property Selected: boolean read F_Selected write SetSelected; //**< Controls if this item is selected or not
  end;

  TTreeListInternOptions=set of (tlioDisablePainting, tlioDeleting, tlioUpdating);
  TExpandMode=(emExpandByClick,emExpandByDoubleClick,emExpandNot);
  TLineMode=(lmNone,lmSolid,lmDot);
  TExpandItemEvent = procedure (Sender: TObject; Item: TTreeListItem);
  TCustomDrawEventTyp=(cdetPrePaint,cdetPostPaint);
  TCustomBackgroundDrawEvent=procedure (sender:TObject;eventTyp_cdet:TCustomDrawEventTyp;var defaultDraw:Boolean) of object;
  TCustomItemDrawEvent=procedure (sender:TObject;eventTyp_cdet:TCustomDrawEventTyp;item:TTreeListItem;xpos,ypos,xColumn:integer;lastItem:boolean;var defaultDraw:Boolean) of object;
  TCustomRecordItemDrawEvent=procedure (sender:TObject;eventTyp_cdet:TCustomDrawEventTyp;parentItem:TTreeListItem;RecordItem:TTreeListRecordItem;xpos,ypos,xColumn:integer;var defaultDraw:Boolean) of object;
  TItemEvent=procedure (sender:TObject;item:TTreeListItem) of object;
  TRecordItemEvent=procedure (sender:TObject;parentItem:TTreeListItem;item:TTreeListRecordItem) of object;
  TCompareTreeListItemsEvent=procedure (sender: TObject; item1, item2: TTreeListItem; var result: longint)of object;
  TUserSortItemsEvent=procedure (sender: TObject; var sortColumn: longint; var invertSorting: boolean) of object;

  { TTreeListView }
  {$ifndef fpc}
  TEventHeaderControl=THeaderControl;
  {$else}
  TEventHeaderControl=TCustomHeaderControl;
  {$endif}

  {** @abstract This is the main TreeListView-class
      You should never need to @noAutoLink create an object of a different class of this file @br
      Example (use in FormCreate):
  @longCode(#
         List:=TTreeListView.create(self);
         List.Parent:=self;
         List.Align:=alClient;
         List.Columns.Clear;
         List.Columns.Add.Text:='A';
         List.Columns.Add.Text:='B';
         List.BeginUpdate;
         List.Items.Add('Item').SubItems.Add('Child').RecordItemsText[1]:='Property';
         List.EndUpdate;
      #)
  }
  TTreeListView = class(TCustomControl)
  protected
    { Protected-Deklarationen}
    InternOptions_tlio:TTreeListInternOptions;
    RedrawBlock: longint;
    doubleBuffer:graphics.TBitmap;

    StartX:integer;
    LastItems:array[0..31] of boolean; //Gibt an, ob unter dem entsprechenden Item eine Linie gezeichnet werden soll, damit ist es nicht möglich mehr als 32 RecordItems zu machen.

    F_Sorted: boolean;
    F_SortColumn: longint;
    F_SortColumnInverted: boolean;
    F_OnCompareItems: TCompareTreeListItemsEvent;
    F_OnUserSortItems: TUserSortItemsEvent;

    F_Items:TTreeListItems;
    F_Header:THeaderControl;
    F_HeaderColumnPopupMenu: TPopupMenu;
    F_VScroll:TScrollBar; //vertikale, rechte scrollbar
    F_HScroll:TScrollBar; //horizontale, untere scrollbar
    F_RowHeight:integer;
    F_ImageList:TImageList;

    F_ExpandMode:TExpandMode;
    F_RightMouseSelects: boolean;
    F_Tooltips: boolean;

    //Selection
    F_MultiSelect:boolean;
    F_SelCount: longint;
    F_Focused: TTreeListItem;
    F_BaseSelect: TTreeListItem; //last item selected without shift


    //appearance
    F_TreeSectionPos: TRect;
    
    F_HotTrackFont:TFont;
    F_HotTrackSubTextItems:boolean;

    F_SelectedFont:TFont;
    F_SelectedHotTrackFont:TFont;
    F_SelectBackColor:TColor;

    F_ButtonColor:TColor;
    F_BgColor:TColor;

    F_Striped:boolean;
    F_StripedOddColor:TColor;
    F_StripedEvenColor:TColor;
    F_StripInvisibleItems: boolean;

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
    F_MouseWheelDelta: longint;

    //CustomDrawEvents
    F_CustomBgDraw:TCustomBackgroundDrawEvent;
    F_CustomItemDraw:TCustomItemDrawEvent;
    F_CustomRecordItemDraw:TCustomRecordItemDrawEvent;

    //Inputevents
    F_RealClickPos, F_RealMousePos: TPoint;
    F_ClickedItem: TTreeListItem;
    F_MouseSelecting: (msNone,msLeft,msRight);
    F_ClickAtItem:TItemEvent;
    F_ItemCollapsed:TItemEvent;
    F_ItemExpanded:TItemEvent;
    F_ClickAtRecordItem:TRecordItemEvent;
    F_OnSelect:TItemEvent;
    F_OnItemExpanded: TItemEvent;
    F_OnItemCollapsed: TItemEvent;
    F_OnItemsSorted: TNotifyEvent;

    PaintEvenItem:boolean;

    //Search
    f_searchMarkItem: TTreeListItem;
    f_searchMarkCol,f_searchMarkStart,f_searchMarkLen: longint;
    f_searchMarkVisible,f_searchActivated:boolean;
    f_colorSearchMark: tcolor;
    f_colorSearchMarkField: tcolor;
    F_SearchBar: TSearchBar;
    F_HighlightAll: boolean;
    procedure SearchBarSearch(sender: TObject; incremental, backwards: boolean);
    procedure SearchBarClose(Sender: TObject);
    procedure SearchBarKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure F_SearchBarHighlightChanged(Sender: TObject);

    //Ereignissausösungen
    function DoCustomBackgroundDrawEvent (eventTyp_cdet:TCustomDrawEventTyp):boolean;
    function DoCustomItemDrawEvent(eventTyp_cdet:TCustomDrawEventTyp;item:TTreeListItem;xpos,ypos,xColumn:integer;lastItem:boolean):boolean;
    function DoCustomRecordItemDrawEvent(eventTyp_cdet:TCustomDrawEventTyp;parentItem:TTreeListItem;RecordItem:TTreeListRecordItem;xpos,ypos,xColumn:integer):boolean;

    procedure removeSelection(list: TTreeListItems);
    procedure removeMouseSelection(list: TTreeListItems);
    procedure setMouseSelection(list: TTreeListItems);
    procedure SetMultiSelect(value: boolean);
    procedure DoSelect(item: TTreeListItem);virtual;
    procedure selectRange(a,b: TTreeListItem;mouseSelect:boolean=false);

    //Kommunikationsroutinen (Set- und Getfunktionen)
    procedure SetItems(const value:TTreeListItems);
    procedure SetFocused(const AValue: TTreeListItem);
    procedure SetSelected(const AValue: TTreeListItem);

    function GetColumnsDragable: boolean;
    procedure SetColumnsDragable(const AValue: boolean);

    function GetTopPos:integer;

    procedure SetSortColumn(const AValue: longint);
    procedure SetSorted(const AValue: boolean);

    procedure SetColumns(const value:THeaderSections);
    function GetColumns: THeaderSections;

    procedure setImageList(const images:TImageList);

    procedure SetRowHeight(const newHeight:integer);

    procedure SetHotTrackSubTextItems(const value:boolean);

    procedure SetHotTrackFont(const value:TFont); //< Set the font used to draw a hottracked item
    procedure SetSelectedFont(const value:TFont); //< Set the font used to draw a selected item
    procedure SetSelectedHotTrackFont(const value:TFont);//< Set the font used to draw a selected and hottracked item

    //Sonstiges
    function RealControlHeight(c: Twincontrol): longint;
    function RealClientHeight: longint;
    procedure DrawAlignDotLine(x,y:integer;const x2,y2:integer;const color:TColor);
    procedure drawTextRect(s:string;extraIndentation:longint;align:TAlignment; const rec: TRect; searchDraw: boolean=false);
    function CompareItems(i1, i2: TTreeListItem): longint;

    procedure BeginMultipleUpdate;
    procedure EndMultipleUpdate;

    //Interne Kommunikation mit Unterkomponenten
    procedure _GeneralEvent(Sender: TObject);
    procedure _HeaderSectionTrack( HeaderControl: TEventHeaderControl;  Section: THeaderSection;  Width: Integer;  State: TSectionTrackState);
    procedure _HeaderSectionResize( HeaderControl: TEventHeaderControl;  Section: THeaderSection);
    procedure _HeaderSectionClick( HeaderControl: TEventHeaderControl;  Section: THeaderSection);
    procedure _HeaderSectionDblClick( HeaderControl: TEventHeaderControl;  Section: THeaderSection);
    procedure _HeaderSectionEndDrag(Sender: TObject);
    {$ifdef allowHeaderVisible}procedure ColumnPopupMenuClick(Sender: TObject);{$endif}
    procedure _HScrollChange(Sender: TObject);
    procedure _VScrollChange(Sender: TObject);

    procedure UpdateScrollBarPos;
  public
    { Public-Deklarationen}
    hotTrackedRecordItem:TTreeListRecordItem; //**<Item currently touched by the mouse
    property selCount: longint read F_SelCount; //**<Count of selected items
    property multiSelect: boolean read F_MultiSelect write SetMultiSelect; //**<Specifies if multiple items can be selected
    {$warnings off}
    property focused:TTreeListItem read F_Focused write SetFocused; //**<Currently focused item, it is not necessarily selected. @br Setting this property will not select the new item
    {$warnings on}
    property Selected:TTreeListItem read F_Focused write SetSelected; //**<This is the same as Focused, but setting this property will select the item and deselect every other one
    property SortColumn: longint read F_SortColumn write SetSortColumn; //**<Column currently used for sorting

    procedure UpdateScrollSize; //**<@deprecated Recalculates the necessary scrollbar properties @br Normally you don't need to call this

    //**Create
    constructor Create(aowner:TComponent);override;

    function GetItemAtPos(const y:integer):TTreeListItem;//**<Returns the item at position y
    function GetRecordItemAtPos(const x,y:integer):TTreeListRecordItem;//**<Returns the record item at position x,y @br Notice that it doesn't check for visibility, e.g you can use negative coordinates or find items hidden by the scrollbars


    //Items
    procedure BeginUpdate; //**< Notifies the control that the @noAutoLink items are changed, so it will not redrawn itself. @br Never forget to use this, otherwise it will be very slow.
    procedure EndUpdate; //**< Stops the redraw block and redraws everything
    function VisibleRowCount:longint; //**< Count of visible lines
    procedure sort; //**< Sorts the items according to the current sorting options
    procedure ensureVisibility(item: TTreeListItem;column: longint=-1); //**< Makes item visible, this includes scrolling and expanding of items @br If column is not -1 it scroll horizontally to the beginning/ending of this column if it isn't visible

    //**This searches the text searchFor in searchFields.
    //**@param searchFields Bit-wise combination of column numbers. Use @code(1 shl i) for column i. (you can only use the first 32 columns)
    //**@param backward  Specifies search direction
    //**@param extendSelection set this to true for an incremental search
    function search(searchFor: string; searchFields:cardinal; backward: boolean=false;extendSelection: boolean=false): TFindState;

    //Header

    function ColumnFromOriginalIndex(index: longint):  THeaderSection; //**< If the columns can be dragged this will return them in the old order
    procedure CreateUserColumnVisibilityPopupMenu(); //**< Creates a popupmenu to hide/show columns @br You need to call this method after every removing/creating of columns, because the TreeListView doesn't known when the columns are changed, since you have direct access to the headercontrol sections.@br This needs FPC
    function serializeColumnWidths: string; //**<save the widths of the columns in a string @seealso deserializeColumnWidths
    function serializeColumnOrder: string; //**<save the order of the columns in a string (needs FPC) @seealso deserializeColumnOrder
    function serializeColumnVisibility: string; //**<save the visibility of the columns in a string (needs FPC) @seealso deserializeColumnVisibility
    procedure deserializeColumnWidths(s: string); //**<load the widths of the columns from a string @seealso serializeColumnWidths
    procedure deserializeColumnOrder(s: string);  //**<load the order of the columns from a string (needs FPC) @seealso serializeColumnOrder
    procedure deserializeColumnVisibility(s: string); //**<load the visibility of the columns from a string (needs FPC) @seealso serializeColumnVisibility
    procedure CreateSearchBar(); //**< Creates a FireFox like SearchBar
    property SearchBar: TSearchBar read F_SearchBar; //**< This a FireFox like search bar, call createSearchBar before using (this property)

    //Messages
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    procedure WndProc(var message:{$IFDEF LCL}TLMessage{$else}TMessage{$endif});override;

    //this draws all elements
    procedure internPaint;
    procedure Paint;override;

    //Destroy
    destructor Destroy;override;

    property TopPos:integer read GetTopPos; //**< V-Scrollposition calculated in pixels (=position of Items[0])

  published
    { Published-Deklarationen }
    {-------------------------------START Ereignisse---------------------------}

    //Header-Eigenschaften
    property Columns:THeaderSections read GetColumns write SetColumns; //**< All columns
    property ColumnsDragable: boolean read GetColumnsDragable write SetColumnsDragable; //**< Determines if the user can move the columns around (needs FPC)

    property RowHeight:integer read F_RowHeight write SetRowHeight; //**< Height of a row

    property Images:TImageList read F_ImageList write setImageList; //**< ImageList used to get the images for items using the TTreeListView.ImageIndex property

    property HorizontalLineMode:TLineMode read F_HorizontalLines write F_HorizontalLines; //**< Determines  how/if lines are drawn between the items
    property HorizontalLineColor:TColor read F_HorizontalLineColor write F_HorizontalLineColor;
    property VerticalLineMode:TLineMode read F_VerticalLines write F_VerticalLines; //**< Determines  how/if lines are drawn between the columns
    property VerticalLineColor:TColor read F_VerticalLineColor write F_VerticalLineColor;
    property RootLineMode:TLineMode read F_RootLines write F_RootLines; //**< Determines  how/if lines are drawn to connect the tree items
    property RootLineColor:TColor read F_RootLineColor write F_RootLineColor;
    property ColorSearchMark: tcolor read F_ColorSearchMark write F_ColorSearchMark;
    property ColorSearchMarkField: tcolor read F_ColorSearchMarkField write F_ColorSearchMarkField;

    property RightMouseSelects: boolean read F_RightMouseSelects write F_RightMouseSelects; //**< Determines if you can select items using the right mouse button
    property ToolTips: boolean read F_Tooltips write F_Tooltips; //**<Specifies if tooltips are shown when item text is longer than the column
    property ExpandMode:TExpandMode read F_ExpandMode write F_ExpandMode; //**< Determines  how/if the user is allowed to collapse/expand items
    property HotTrackSubTextItems:Boolean read F_HotTrackSubTextItems write SetHotTrackSubTextItems; //**< Determines if the record items are hot tracked

    property HotTrackFont:TFont read F_HotTrackFont write SetHotTrackFont;
    property Font;
    property SelectedFont:TFont read F_SelectedFont write SetSelectedFont;
    property SelectedHotTrackFont:TFont read F_SelectedHotTrackFont write SetSelectedHotTrackFont;

    property Striped:boolean read F_Striped write F_Striped; //**< Determines if the item background is drawn alternating
    property StripedOddColor:TColor read F_StripedOddColor write F_StripedOddColor;
    property StripedEvenColor:TColor read F_StripedEvenColor write F_StripedEvenColor;
    property StripInvisibleItems: boolean read F_StripInvisibleItems write F_StripInvisibleItems; //**< Controls if invisible items are counted when the control determines if a item is odd or even

    property SelectBackColor:TColor read F_SelectBackColor write F_SelectBackColor;
    property ButtonColor:TColor read F_ButtonColor write F_ButtonColor; //**< Color of the expand/collaps button
    property BackGroundColor:TColor read F_BgColor write F_BgColor;

    property Items:TTreeListItems read F_Items write SetItems; //**< All the items, use items.add to create new ones


    property Sorted: boolean read F_Sorted write SetSorted; //**< Controls of the items should be @noAutoLink sorted @br Notice that the items are not always automatically sorted (e.g. new inserted items are not)

    //Sortierungsereignisse
    property OnCompareItems: TCompareTreeListItemsEvent read F_OnCompareItems write F_OnCompareItems; //**< Event which is called when two items are compared during sorting @br The default sorting tries first to compare the text as numbers than as text, every level is @noAutoLink sorted on its own, parents are not changed
    property OnUserSortItemsEvent: TUserSortItemsEvent read F_OnUserSortItems write F_OnUserSortItems; //**< Called when the user clicks on the header to resort the items
    property OnItemsSortedEvent: TNotifyEvent read F_OnItemsSorted write F_OnItemsSorted; //**< Called after the items have been @noAutoLink sorted

    //Scrollbarereignisse
    property OnVScrollBarChange:TNotifyEvent read F_VScrollBarChange write F_VScrollBarChange;
    property OnHScrollBarChange:TNotifyEvent read F_HScrollBarChange write F_HScrollBarChange;

    //CustomDrawEvents
    property OnCustomBgDraw:TCustomBackgroundDrawEvent read F_CustomBgDraw write F_CustomBgDraw; //**< This is called before/after the items are drawn
    property OnCustomItemDraw:TCustomItemDrawEvent read F_CustomItemDraw write F_CustomItemDraw; //**< This is called before/after an item is drawn @seealso TTreeListItem.PaintTo
    property OnCustomRecordItemDraw:TCustomRecordItemDrawEvent read F_CustomRecordItemDraw write F_CustomRecordItemDraw; //**< This is called before/after any record items is drawn

    //Inputevents
    property OnClickAtRecordItem:TRecordItemEvent read F_ClickAtRecordItem write F_ClickAtRecordItem; //**< Called when the user clicks on a record item
    property OnClickAtItem:TItemEvent read F_ClickAtItem write F_ClickAtItem; //**< Called when the user clicks on an item (if you need the column use OnClickAtRecordItem)
    property OnSelect:TItemEvent read F_OnSelect write F_OnSelect; //**< Called when an item is selected or deselected
    property OnItemCollapsed:TItemEvent read F_OnItemCollapsed write F_OnItemCollapsed; //**< Called when an item is collapsed
    property OnItemExpanded:TItemEvent read F_OnItemExpanded write F_OnItemExpanded;  //**< Called when an item is expanded

    //Header-Ereignisse
    {$ifdef FPC}
    property OnHeaderSectionResize:TCustomSectionNotifyEvent read F_HeaderSectionResize write F_HeaderSectionResize;
    property OnHeaderSectionTrack:TCustomSectionTrackEvent read F_HeaderSectionTrack write F_HeaderSectionTrack;
    {$else}
    property OnHeaderSectionResize:TSectionNotifyEvent read F_HeaderSectionResize write F_HeaderSectionResize;
    property OnHeaderSectionTrack:TSectionTrackEvent read F_HeaderSectionTrack write F_HeaderSectionTrack;
    {$endif}

    //Freigeben (parent properties)

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
    property Hint         ; //**< You can't use this if tooltips is true
    property ShowHint      ;

    //Freigeben von TWinControlereignissen
    property OnDockDrop;
    property OnDockOver;
    property OnEnter;
    property OnExit;
    property OnGetSiteInfo;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnUnDock;

    //Freigeben von TControl-Ereignissen
    {$ifndef fpc}property OnCanResize;{$endif}
    property OnClick;
    property OnConstrainedResize;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
  end;

procedure Register;

implementation

const HeaderItemDistance=2; //Distance between Header and first drawn item
      LEFT_TEXT_PADDING=3;
      LINE_DISTANCE=15;
      EXPANDING_BUTTON_WIDTH=9;
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

constructor TObjectList.Create;
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
function TObjectList.Add(Item: TObject): Integer;
begin
  if not editing then
    DoChanging;
  result:=inherited add(item);
  if not editing then
    DoChange;
end;
procedure TObjectList.Clear;
begin
  if not editing then
    DoChanging;
  FreeObjects;
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
procedure TObjectList.Insert(Index: Integer; Item: TObject);
begin
  if not editing then
    DoChanging;
  inherited insert(index,item);
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
destructor TObjectList.Destroy;
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

function TTreeListItems.Get(Index: Integer): TTreeListItem; {$ifdef fpc}inline;{$endif}
begin
  result:=TTreeListItem(inherited get(index));
end;

//***find?
procedure TTreeListItems.Sort(CompareFunc: TTreeListItemCompare);
var temp: array of TTreeListItem;

  procedure mergeSort(f,t:longint);
  var {$ifndef fpc}i,{$endif}a,b,d,p: longint;
  begin
    if f>=t then exit;
    d:=(f+t) div 2;
    mergeSort(f,d);
    mergeSort(d+1,t);
    {$ifdef fpc}
    system.move(List^[f],temp[f],(t-f+1)*sizeof(TTreeListItem));
    {$else}
    for i := f to t  do
      temp[i]:=TTreeListItem(List[i]);
    {$endif}
    p:=f;
    a:=f;
    b:=d+1;
    while (a<=d) and (b<=t) do begin
      if CompareFunc(temp[a],temp[b])<=0 then begin
        List[p]:=temp[a];
        inc(a);
      end else begin
        List[p]:=temp[b];
        inc(b);
      end;
      inc(p);
    end;
    if a<=d then begin
      f:=a;
      t:=d;
    end else if b<=t then f:=b
    else exit;
    {$ifdef fpc}
      system.move(temp[f],List[p],(t-f+1)*sizeof(TTreeListItem));
    {$else}
      for i := f to t  do
        List[i+p-f]:=temp[i];
    {$endif}
  end;
var i:longint;
begin
  //sort children
  for i:=0 to count-1 do
    Items[i].SubItems.Sort(CompareFunc);
  SetLength(temp,count);
  mergeSort(0,count-1);
end;

constructor TTreeListItems.Create(parent:TTreeListItem;const TreeListView:TTreeListView);
begin
  inherited Create;
  F_Parent:=parent;
  F_TreeListView:=TreeListView;
end;

function TTreeListItems.Add(caption:string):TTreelistItem;
begin
  Result:=TTreeListItem.Create(F_Parent,F_TreeListView, caption);
  Result.RecordItems.OnChanging:=OnChanging;
  Result.RecordItems.OnChange:=OnChange;
  Result.SubItems.OnChanging:=OnChanging;
  Result.SubItems.OnChange:=OnChange;
  if F_Parent<>nil then result.f_Indent:=F_Parent.Indent+1;
  inherited add(result);
end;

function TTreeListItems.Add(Parent:TTreeListItem;caption:string):TTreelistItem;
begin
  if Parent=nil then result:=Add(caption)
  else result:=Parent.SubItems.Add(caption);
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

function TTreeListItems.FindItemWithText(caption: string): TTreeListItem;
var i:longint;
begin
  result:=nil;
  for i:=0 to count-1 do
    if Items[i].Text=caption then begin
      result:=Items[i];
      exit;
    end else begin
      result:=Items[i].SubItems.FindItemWithText(caption);
      if result<>nil then exit;
    end;
end;

function TTreeListItems.FindItemWithRecordText(pos: longint; text: string
  ): TTreeListItem;
var i:longint;
begin
  result:=nil;
  for i:=0 to count-1 do
    if Items[i].RecordItemsText[pos]=text then begin
      result:=Items[i];
      exit;
    end else begin
      result:=Items[i].SubItems.FindItemWithRecordText(pos,text);
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
................................TRecordItemList....................................
--------------------------------------------------------------------------------
////////////////////////////////////////////////////////////////////////////////
================================================================================
################################################################################
}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}

function TRecordItemList.Get(Index: Integer): TTreeListRecordItem;
begin
  result:=TTreeListRecordItem(inherited Get(Index));
end;

procedure TRecordItemList.Put(Index: Integer;
  const AValue: TTreeListRecordItem);
begin
  inherited Put(Index, AValue);
end;

function TRecordItemList.Add:TTreeListRecordItem;
begin
  Result:=TTreeListRecordItem.Create(owner);
  result.F_Index:=Count;
  inherited add(result);
end;

procedure TRecordItemList.AddItem(recordItem: TTreeListRecordItem);
begin
  inherited add(recordItem);
end;

function TRecordItemList.Add(s: string): TTreeListRecordItem;
begin
  result:=Add;
  result.Text:=s;
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


procedure TTreeListRecordItem.SetText(caption: string);
begin
  F_Text:=caption;
  F_Parent.DoChange;
end;

procedure TTreeListRecordItem.selectFont(can: TCanvas);
begin
  if F_Parent.TreeListView.hotTrackedRecordItem = self then begin
    if F_Parent.SeemsSelected then
     can.Font.Assign(F_Parent.TreeListView.SelectedHotTrackFont)
    else
     can.Font.Assign(F_Parent.TreeListView.HotTrackFont);
   end else if F_Parent.SeemsSelected then
    can.Font.Assign(F_Parent.TreeListView.SelectedFont)
   else
    can.Font.Assign(F_Parent.TreeListView.Font);
end;
     (*
procedure TTreeListRecordItem.PaintTo(const listView:TTreeListView;x:integer;const y,xColumn:integer;const parentItem:TTreeListItem);
var i:integer;
    ausgabeRect:TRect;
    textStyle: TTextStyle;
begin
  ausgabeRect.Left:=x+3;
  ausgabeRect.Right:=x+listView.F_Header.Sections[xColumn+1].Width-2{+listView.StartX};
  ausgabeRect.Top:=y;
  ausgabeRect.Bottom:=y+listView.RowHeight;
  listView.Canvas.brush.Style:=bsClear;

  DrawText(listView.Canvas.Handle,pchar(text),length(text),ausgabeRect,DT_LEFT or DT_NOPREFIX or DT_SINGLELINE or DT_VCENTER or DT_WORD_ELLIPSIS);
end;*)

function TTreeListRecordItem.GetNecessaryWidth(listView:TTreeListView=nil): longint;
begin
  if listView=nil then listView:=F_Parent.F_TreeListView;
  selectFont(listView.Canvas);
  Result:=listView.Canvas.TextWidth(Text);
end;

constructor TTreeListRecordItem.Create(aparent:TTreeListItem);
begin
  inherited Create;
  F_Parent:=aparent;
end;

constructor TTreeListRecordItem.Create(aparent:TTreeListItem; caption: string);
begin
  inherited Create;
  F_Parent:=aparent;
  SetText(caption);
end;

destructor TTreeListRecordItem.Destroy;
begin
  inherited Destroy;
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
constructor TTreeListItem.Create(const Parent:TTreeListItem;const TreeListView:TTreeListView;const ACaption:string);
begin
  inherited Create;
  F_Parent:=parent;
  F_RecordItems:=TRecordItemList.Create;
  F_RecordItems.Owner:=self;
  F_RecordItems.Add(ACaption);
  F_SubItems:=TTreeListItems.Create(self,TreeListView);
  F_Indent:=-1;
  F_TreeListView:=TreeListView;
  f_Selected:=false;
  Expanded:=true;
end;

function TTreeListItem.getBounds(column: longint): TRect;
begin                                               //todo: correct indentation handling
  result.Top:=(F_TreeListView.Items.RealIndexOf(self,[]))*F_TreeListView.RowHeight+F_TreeListView.TopPos;
  result.bottom:=Result.top+F_TreeListView.RowHeight;
  if column=-1 then begin
    result.Left:=0;
    Result.right:=F_TreeListView.F_Header.Width;
  end else if column<TreeListView.Columns.Count then begin
    Result.left:=TreeListView.ColumnFromOriginalIndex(column).Left-F_TreeListView.F_HScroll.Position;
    Result.right:=result.left+TreeListView.ColumnFromOriginalIndex(column).Width;
  end else begin
    Result.Left:=0;
    Result.Right:=0;
  end;
end;

function TTreeListItem.getMaxTextBounds(column: longint): TRect;
begin
  Result:=getBounds(column);
  if (column>0) and (column<TreeListView.Columns.Count) then begin
    case TreeListView.Columns[column].Alignment of
      taLeftJustify: Result.Left:=result.Left+LEFT_TEXT_PADDING;
      taRightJustify: Result.Right:=result.Right-LEFT_TEXT_PADDING;
    end;
  end else if column=0 then
    Result.Left:=Result.Left+LEFT_TEXT_PADDING+GetExtraTextIndentation(column);
  if result.Left>result.right then Result.Left:=Result.Right;
end;

function TTreeListItem.GetRecordItemsText(i: Integer): string;
begin
  if i<RecordItems.Count then result:=RecordItems[i].Text
  else result:='';
end;

procedure TTreeListItem.SetRecordItemsText(i: Integer; const AValue: string);
var j:longint;
begin
  if i<RecordItems.Count then RecordItems[i].Text:=AValue
  else begin
    for j:=RecordItems.Count to i-1 do RecordItems.Add('');
    RecordItems.Add(AValue);
  end;
end;

function TTreeListItem.GetText: string;
begin
  if RecordItems.Count>0 then result:=RecordItems[0].Text
  else result:='';
end;

procedure TTreeListItem.SetText(const AValue: string);
begin
  if RecordItems.Count>0 then RecordItems[0].Text:=AValue
  else RecordItems.Add(AValue);
end;
function TTreeListItem.GetExtraTextIndentation(column: longint): longint; //mainly tree width
begin
  result:=0;
  if column<>0 then exit;
  result:=F_Indent*LINE_DISTANCE+TreeListView.startx;;
  if ImageBitmap<>nil then Result:=Result+ImageBitmap.width+LEFT_TEXT_PADDING
  else if (ImageIndex>-1) and (TreeListView.Images<>nil) then Result:=Result+TreeListView.Images.Width+LEFT_TEXT_PADDING;
end;

function TTreeListItem.GetExtendingButtonPos: longint;
begin
  result:=TreeListView.F_TreeSectionPos.left+(F_Indent+1)*LINE_DISTANCE-10;
end;

procedure TTreeListItem.DoChange;
begin
  RecordItems.DoChange;
end;
procedure TTreeListItem.DoChanging;
begin
  RecordItems.DoChanging;
end;

procedure TTreeListItem.SetSelected(newSelected: boolean);
begin
  if Selected = newSelected then exit;
  F_Selected:=newSelected;
  if F_Selected then begin
    if not TreeListView.multiSelect then TreeListView.focused:=self;
    TreeListView.F_SelCount:=TreeListView.F_SelCount+1;
  end else TreeListView.F_SelCount:=TreeListView.F_SelCount-1;
  TreeListView.DoSelect(self);
  DoChange;
end;

function TTreeListItem.SeemsSelected: boolean;
begin
  if TreeListView.F_MouseSelecting<>msNone then result:=Selected xor F_MouseSelected
  else result:=Selected;
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
  if F_Expanded then exit;
  DoChanging;
  F_Expanded:=true;
  if assigned(TreeListView) and assigned(TreeListView.F_OnItemExpanded) then
    TreeListView.F_OnItemExpanded(TreeListView,self);
  DoChange;
end;
procedure TTreeListItem.Collapse;
begin
  if not F_Expanded then exit;
  DoChanging;
  F_Expanded:=false;
  if assigned(TreeListView) and assigned(TreeListView.F_OnItemCollapsed) then
    TreeListView.F_OnItemCollapsed(TreeListView,self);
  DoChange;
end;


procedure TTreeListItem.SetSubItems(const value: TtreeListItems);
begin
  F_SubItems.Assign(value);
end;


//Hinweis: starty wird nicht verändert, wenn das Item an der gesuchten Stelle ist
function TTreeListItem.GetItemAtPos(const listView:TTreeListView;const TestY:integer;var startY:integer):TTreeListItem;
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

function TTreeListItem.GetRecordItemAtPos(const listView:TTreeListView;const TestX:integer):TTreeListRecordItem;
var i,x:integer;
begin
  Result:=nil;
  x:=0;
  for i:=0 to min(listView.F_Header.Sections.Count-1,RecordItems.Count-1) do begin
    if TestX<x then exit;
    if (x+listView.F_Header.Sections[i].Width>TestX) then begin
      {$ifdef fpc}
        x:=listView.F_Header.Sections[i].OriginalIndex;
      {$else}
        x:=i;
      {$endif}
      if (x>=0)and(x<RecordItems.Count) then  result:=RecordItems[x]
      else Result:=nil;
      exit;
    end;
    x:=x+listView.F_Header.Sections[i].Width;
  end;
end;

function TTreeListItem.GetMaxColumnWidth(const id: longint): longint;
var i,temp:longint;
begin
  if id=0 then result:=RecordItems[0].GetNecessaryWidth()+LINE_DISTANCE*F_Indent+13+LEFT_TEXT_PADDING
  else if id<RecordItems.Count then result:=RecordItems[id].GetNecessaryWidth()
  else result:=0;
  for i:=0 to SubItems.Count-1 do begin
    temp:=SubItems[i].GetMaxColumnWidth(id);
    if temp>result then result:=temp;
  end;
end;

//Gibt das nächste Item zurück, dass auf der gleichen Ebene ist, gibt es kein solches, wird rekursiv das nächste Item zurückgegeben, dass auf einer kleinere Ebene hat
function TTreeListItem.GetNextItemIgnoringChildren: TTreeListItem;
var temp:integer;
begin
  if (parent=nil) then begin
    temp:=TreeListView.Items.IndexOf(self);
    if temp<TreeListView.Items.Count-1 then Result:=TreeListView.Items[temp+1]
    else Result:=nil;
  end else begin
    temp:=Parent.SubItems.IndexOf(self);
    if temp<Parent.SubItems.Count-1 then Result:=parent.SubItems[temp+1]
    else result:=parent.GetNextItemIgnoringChildren;
  end;
end;


function TTreeListItem.GetLastVisibleSubSubItem:TTreeListItem;
begin
  result:=self;
  while (result.SubItems.Count<>0)and(result.Expanded) do
    Result:=Result.SubItems[Result.SubItems.count-1];
end;

function TTreeListItem.GetLastSubSubItem: TTreeListItem;
begin
  result:=self;
  while (result.SubItems.Count<>0) do
    Result:=Result.SubItems[Result.SubItems.count-1];
end;

function TTreeListItem.GetNextVisibleItem(Delta: longint=1): TTreeListItem;
var next: TTreeListItem;
begin
  if Delta<0 then begin
    result:=GetPrevVisibleItem(-Delta);
    exit;
  end;
  Result:=self;
  while delta>0 do begin
    if (result.SubItems.Count>0) and (result.Expanded) then
      next:=result.SubItems[0]
    else begin
      next:=result.GetNextItemIgnoringChildren;
      if next=nil then next:=result;
      if next=result then exit;
    end;
    result:=next;
    dec(delta,1);
  end;
end;

function TTreeListItem.GetPrevVisibleItem(Delta: longint): TTreeListItem;
var temp:longint;
begin
  if Delta<0 then begin
    result:=GetNextVisibleItem(-Delta);
    exit;
  end;
  Result:=self;
  while delta>0 do begin
    if result.Parent=nil then begin
      temp:=result.TreeListView.Items.IndexOf(result);
      if temp>0 then Result:=result.TreeListView.Items[temp-1].GetLastVisibleSubSubItem;
    end else begin
      temp:=Result.Parent.SubItems.IndexOf(Result);
      if temp=0 then result:=result.parent
      else result:=result.parent.SubItems[temp-1].GetLastVisibleSubSubItem;
    end;
    dec(Delta);
  end
end;

function TTreeListItem.GetNextItem(): TTreeListItem;
begin
  if SubItems.Count>0 then result:=SubItems[0]
  else result:=GetNextItemIgnoringChildren;
  if result=nil then result:=self;
end;

function TTreeListItem.GetPrevItem(): TTreeListItem;
var temp:longint;
begin
  result:=self;
  if result.Parent=nil then begin
    temp:=result.TreeListView.Items.IndexOf(result);
    if temp>0 then Result:=result.TreeListView.Items[temp-1].GetLastSubSubItem;
  end else begin
    temp:=Result.Parent.SubItems.IndexOf(Result);
    if temp=0 then result:=result.parent
    else result:=result.parent.SubItems[temp-1].GetLastSubSubItem;
  end;
  if result=nil then result:=self;
end;

function TTreeListItem.GetParentInList(List: TTreeListItems
  ): TTreeListItem;
begin
  if list=nil then list:=TreeListView.Items;
  if ParentItems = list then result:=self
  else if Parent = nil then result:=nil
  else result:=parent.GetParentInList(list);
end;

function TTreeListItem.ParentItems: TTreeListItems;
begin
  if Parent = nil then result:=TreeListView.Items
  else result:=Parent.SubItems;
end;

procedure TTreeListItem.PaintTo(const listView:TTreeListView;var y:integer;const xColumn:integer;const last:boolean);
var i,yold,recordId:integer;
    defaultDraw:boolean;
    rec:Trect;
  procedure drawTreeColumnText;
  var textStartX: longint;
  begin
    textStartX:=xColumn*LINE_DISTANCE+listview.startx;
    if ImageBitmap<>nil then begin
      listView.Canvas.Draw(textStartX+rec.left,rec.top,ImageBitmap);
      TreeListView.drawTextRect(Text,textStartX+ImageBitmap.Width+LEFT_TEXT_PADDING,taLeftJustify, rec);
    end else if (TreeListView.Images<>nil) and (ImageIndex>-1) then begin
      listView.Images.draw(listView.Canvas,textStartX+rec.left,rec.Top,ImageIndex);
      TreeListView.drawTextRect(Text,textStartX+listView.Images.Width+LEFT_TEXT_PADDING,taLeftJustify,rec);
    end else
      TreeListView.drawTextRect(Text,textStartX,taLeftJustify,rec);
  end;
  procedure drawTreeColumn;
  var i,tempX:longint;
      tempColor:TColor;
  begin
    tempX:=rec.left;
    if listView.RootLineMode <> lmNone then begin
      //draw vertical item lines (not ending items)
      tempX:=tempX+LINE_DISTANCE div 2+1;
      for i:=0 to xColumn do begin
        if tempx>rec.right then break;
        if listView.LastItems[i] then
          if listView.RootLineMode=lmDot then
            listView.DrawAlignDotLine(tempX,yold,tempX,y-1,listView.F_RootLineColor)
           else with listView.canvas do begin
             pen.color:=listView.F_RootLineColor;
             pen.Style:=psSolid;
             MoveTo(tempX,yold);
             LineTo(tempX,y);
           end;
        tempX:=tempX+LINE_DISTANCE;
      end;

      tempX:=tempX-LINE_DISTANCE div 2-3;

      //draw vertical item lines (ending items)
      case listView.RootLineMode of
        lmDot:  begin
          listView.DrawAlignDotLine(tempX-5,yold+listView.RowHeight div 2,min(tempX,rec.right),yold + listView.RowHeight div 2,listView.F_RootLineColor);
          listView.DrawAlignDotLine(tempX-5,yold,tempX-5,yold+listView.RowHeight div 2,listView.F_RootLineColor);
        end;
        lmSolid: with listView.canvas do begin
           pen.color:=listView.F_RootLineColor;
           pen.Style:=psSolid;
           MoveTo(tempX-5,yold+listView.RowHeight div 2);
           LineTo(min(tempX,rec.right),yold + listView.RowHeight div 2);
             MoveTo(tempX-5,yold);
             LineTo(tempX-5,yold+listView.RowHeight div 2);
         end;
      end;
    end;

    if not defaultDraw then exit;

    if (yold>listView.F_Header.Height)
       and(rec.right-rec.left>EXPANDING_BUTTON_WIDTH)
       and(SubItems.Count>0)  then begin
      tempX:=GetExtendingButtonPos;
      tempColor:=listView.Canvas.brush.Color;
      listView.Canvas.pen.Style:=psSolid;
      listView.Canvas.pen.Color:=clBlack;
      listView.Canvas.brush.Style:=bsSolid;
      listView.Canvas.brush.Color:=listView.ButtonColor;
      //draw expanding/de button
      listView.canvas.Rectangle(tempX,yold+listView.RowHeight div 2-5,tempX+9,yold+listView.RowHeight div 2+4);
      listView.canvas.moveTo(tempX+2,yold+listView.RowHeight div 2-1); //draw ---
      listView.canvas.LineTo(tempX+7,yold+listView.RowHeight div 2-1);
      if not Expanded then begin                                   //draw  |   also -|-
        listView.canvas.moveTo(tempX+4,yold+listView.RowHeight div 2-3);
        listView.canvas.LineTo(tempX+4,yold+listView.RowHeight div 2+2);
      end;
      listView.Canvas.brush.Color:=tempColor;
    end;
  end;
begin
  listView.LastItems[xColumn]:=not last;
  if y>listView.ClientHeight-listView.F_HScroll.Height then exit;
  listView.PaintEvenItem:=not listView.PaintEvenItem;
  if listView.StripInvisibleItems and not Expanded then
    if (SubItems.GetRealItemCount([ricCountCollapsedSubItems]) and $1=0) then
      listView.PaintEvenItem:=not listView.PaintEvenItem;
  F_Indent:=xColumn;
  //item bg color/font
  if SeemsSelected then begin
    listView.Canvas.Brush.color:=listView.SelectBackColor;
    listView.Canvas.Brush.style:=bsSolid;
  end else if listView.Striped then begin
    listView.canvas.Brush.Style:=bsSolid;
    if listView.PaintEvenItem then listView.canvas.Brush.Color:=listView.StripedEvenColor
    else listView.canvas.Brush.Color:=listView.StripedOddColor;
  end else listView.Canvas.Brush.style:=bsClear;
  defaultDraw:=listView.DoCustomItemDrawEvent(cdetPrePaint,self,0,y,xColumn,last); //event handling
  yold:=y;
  y:=y+listView.RowHeight;
  if defaultDraw then
    if y>listView.F_Header.Height then begin
      //clearing
      listView.Canvas.FillRect(rect(0,yold,listView.ClientWidth-listview.f_vscroll.width,y));

      rec.top:=yold;
      rec.Bottom:=y;

      //draw text
      for i:=0 to listView.F_Header.Sections.Count-1 do begin
        {$IFDEF allowHeaderDragging}
          recordId:=listView.F_Header.Sections[i].OriginalIndex;
        {$ELSE}
          recordId:=i;
        {$ENDIF}
        if recordId>RecordItems.Count-1 then continue;
        rec.left:=listView.F_Header.Sections[i].Left-listView.F_HScroll.Position;
        rec.right:=rec.left+listView.F_Header.Sections[i].Width;
        RecordItems[recordId].selectFont(listView.Canvas);
        if listView.DoCustomRecordItemDrawEvent(cdetPrePaint,self,RecordItems[recordId],rec.left,rec.top,recordId) then begin
          if recordId=0 then begin
            drawTreeColumnText;
            drawTreeColumn;
          end else
            TreeListView.drawTextRect(RecordItems[recordId].Text,0,listView.F_Header.Sections[i].Alignment,rec);
          if not listView.DoCustomRecordItemDrawEvent(cdetPostPaint,self,RecordItems[recordId],rec.left,rec.top,recordId) then
            break;
        end;
      end;

      //draw focus rect
      if listView.focused = self then
        DrawFocusRect( listView.Canvas.Handle,rect(0,yold,listView.ClientWidth-listView.F_VScroll.Width,y));

      //draw horizontal separation lines
      case listView.HorizontalLineMode of
        lmSolid: with listView.canvas do begin
                   pen.color:=listView.F_HorizontalLineColor;
                   pen.Style:=psSolid;
                   MoveTo(0,y-1);
                   LineTo(ListView.ClientWidth-ListView.F_VScroll.Width,y-1);
                 end;
        lmDot:   listView.DrawAlignDotLine(0,y-1,ListView.ClientWidth-ListView.F_VScroll.Width,y-1,listView.F_HorizontalLineColor);
      end;
    end;

  //draw sub items
  if (SubItems.Count=0) then
    listView.DoCustomItemDrawEvent(cdetPostPaint,self,0,yold,xColumn,last) //event handling
   else
    if (Expanded)and(listView.DoCustomItemDrawEvent(cdetPostPaint,Self,0,yold,xColumn,last)) then
      for i:=0 to SubItems.Count-1 do
        (TObject(SubItems[i]) as TTreeListItem).PaintTo(listView,y,xColumn+1,i=SubItems.Count-1);

end;

//Destroy
destructor TTreeListItem.Destroy;
begin
  if self=TreeListView.focused then TreeListView.focused:=nil;
  if Selected then dec(TreeListView.f_selCount);
  F_RecordItems.free;
  F_SubItems.free;
  inherited;
end;


{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{
################################################################################
================================================================================
////////////////////////////////////////////////////////////////////////////////
--------------------------------------------------------------------------------
..............................TTreeListView..................................
--------------------------------------------------------------------------------
////////////////////////////////////////////////////////////////////////////////
================================================================================
################################################################################
}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}

//Create
constructor TTreeListView.Create(aowner:TComponent);
begin
  inherited;
  F_Items:=TTreeListItems.Create(nil,self);
  F_Items.OnChange:=_GeneralEvent;
  F_Items.freeing:=false;

  ToolTips:=true;
  RightMouseSelects:=true;
  doubleBuffer:=graphics.TBitmap.Create;

  //Fonts
  F_HotTrackFont:=TFont.Create;
  F_HotTrackFont.Color:=clBlue;
  F_HotTrackFont.Style:=[fsUnderline];

  F_SelectedHotTrackFont:=TFont.Create;
  F_SelectedHotTrackFont.Color:=clHighlightText;
  F_SelectedHotTrackFont.Style:=[fsBold,fsUnderline];

  F_SelectedFont:=TFont.Create;
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

  colorSearchMark:=clAqua;
  colorSearchMarkField:=(clBlue + clAqua) div 2;
  f_searchMarkItem:=nil;
  f_searchMarkVisible:=false;

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
  F_Header:=THeaderControl.Create(self);
  F_Header.Align:=alNone;
  F_Header.parent:=Self;
  F_Header.Visible:=true;
  with F_Header.Sections.Add do begin
    Caption:='';
    Width:=1000;
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
  F_Header.OnSectionClick:=_HeaderSectionClick;
  {$ifdef allowHeaderDragging}F_Header.OnSectionSeparatorDblClick:=_HeaderSectionDblClick();
  F_Header.OnSectionEndDrag:=_HeaderSectionEndDrag;{$endif}
  {$ifdef lcl}F_Header.Cursor:=crArrow;{$endif}


  //Scrollbar initialisieren
  F_VScroll:=TScrollbar.Create(self);
  F_VScroll.parent:=self;
  F_VScroll.Enabled:=false;
  F_VScroll.Visible:=true;
  F_VScroll.Kind:=sbVertical;
  F_VScroll.OnChange:=_VScrollChange;
  F_VScroll.TabStop:=false;
  {$ifdef lcl}F_VScroll.Cursor:=crArrow;{$endif}


  //Scrollbar initialisieren
  F_HScroll:=TScrollbar.Create(self);
  F_HScroll.parent:=self;
  F_HScroll.Enabled:=false;
  F_HScroll.Visible:=true;
  F_HScroll.Kind:=sbHorizontal;
  F_HScroll.Left:=0;
  F_HScroll.SmallChange:=5;
  F_HScroll.OnChange:=_HScrollChange;
  F_HScroll.TabStop:=false;
  {$ifdef lcl}F_HScroll.Cursor:=crArrow;{$endif}

  RowHeight:=F_Header.Height-2*GetSystemMetrics(SM_CYEDGE);

end;

procedure TTreeListView.SetFocused(const AValue: TTreeListItem);
begin
  if AValue=F_Focused then exit;
  if tlioDeleting in InternOptions_tlio then exit;
  F_Focused:=AValue;
  DoSelect(F_Focused);
  if focused<>nil then
    ensureVisibility(focused);
  f_searchMarkVisible:=false;
  internPaint;
  {*RowHeight+TopPos+F_Header.Height;
  if temp-rowHeight<F_Header.Height then
    F_VScroll.Position:=min(F_VScroll.Position-1,F_VScroll.Position+(temp-F_Header.Height-rowHeight-3) div RowHeight);
  if temp>ClientHeight-F_HScroll.Height then
    F_VScroll.Position:=max(F_VScroll.Position+1,F_VScroll.Position+((temp-ClientHeight+F_HScroll.Height+F_Header.Height) div RowHeight));}
end;

procedure TTreeListView.SetSelected(const AValue: TTreeListItem);
begin
  if AValue = Selected then exit;
  BeginMultipleUpdate;
  removeSelection(items);
  if Avalue<>nil then
    AValue.Selected:=true;
  SetFocused(AValue);
  EndMultipleUpdate;
end;

procedure TTreeListView.SetSortColumn(const AValue: longint);
begin
  if SortColumn=AValue then exit;
  F_SortColumn:=AValue;
  F_SortColumnInverted:=false;
  if F_Sorted then sort;
end;

procedure TTreeListView.SetSorted(const AValue: boolean);
begin
  if F_Sorted=AValue then exit;
  F_Sorted:=AValue;
  if F_Sorted then sort;
end;

function TTreeListView.GetColumnsDragable: boolean;
begin
  Result:={$ifdef allowHeaderDragging}F_Header.DragReorder{$else}false{$endif};
end;

procedure TTreeListView.SetColumnsDragable(const AValue: boolean);
begin
  {$ifdef allowHeaderDragging}F_Header.DragReorder:=AValue;{$endif}
end;

{$ifdef allowHeaderVisible}
procedure TTreeListView.ColumnPopupMenuClick(Sender: TObject);
var mi: TMenuItem;
begin
  if not (sender is tmenuitem) then exit;
  mi:=TMenuItem(sender);
  if (mi.tag<0) or (mi.tag>=Columns.Count) then exit;
  mi.Checked:=not mi.Checked;
  Columns[mi.Tag].visible:=mi.Checked;
  internPaint;
end;
{$endif}

procedure TTreeListView.SearchBarKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var temp:{$ifdef lcl}TLMKeyDown{$else}TWMKeyDown{$endif};
begin
  case key of
    VK_DOWN,VK_UP,VK_NEXT,VK_PRIOR: begin
      temp.msg:={$ifdef lcl}LM_KEYDOWN{$else}WM_KEYDOWN{$endif};
      temp.CharCode:=key;
      temp.KeyData:=0;
      WndProc({$ifdef lcl}TLMessage{$else}TMessage{$endif}(temp));
      key:=0;
    end;
  end;
end;

procedure TTreeListView.F_SearchBarHighlightChanged(Sender: TObject);
begin
  F_HighlightAll:=F_SearchBar.Highlighting;
  internPaint;
end;

procedure TTreeListView.SearchBarClose(Sender: TObject);
begin
  UpdateScrollBarPos;
  SetFocus;
end;

procedure TTreeListView.SearchBarSearch(sender: TObject; incremental,
  backwards: boolean);
begin
  if sender<>F_SearchBar then exit;
  if F_SearchBar.SearchLocation<0 then F_SearchBar.SearchLocation:=0;
  SearchBar.FindState:= search(F_SearchBar.SearchText,
                               cardinal(F_SearchBar.SearchLocations.Objects[F_SearchBar.SearchLocation]),
                               backwards,incremental);
end;

function TTreeListView.DoCustomBackgroundDrawEvent (eventTyp_cdet:TCustomDrawEventTyp):boolean;
begin
  Result:=true;
  if assigned(F_CustomBgDraw) then F_CustomBgDraw(self,eventTyp_cdet,result);
end;
function TTreeListView.DoCustomItemDrawEvent(eventTyp_cdet:TCustomDrawEventTyp;item:TTreeListItem;xpos,ypos,xColumn:integer;lastItem:boolean):boolean;
begin
  Result:=true;
  if assigned(F_CustomItemDraw) then F_CustomItemDraw(self,eventTyp_cdet,item,xpos,ypos,xColumn,lastItem,result);
end;
function TTreeListView.DoCustomRecordItemDrawEvent(eventTyp_cdet:TCustomDrawEventTyp;parentItem:TTreeListItem;RecordItem:TTreeListRecordItem;xpos,ypos,xColumn:integer):boolean;
begin
  Result:=true;
  if assigned(F_CustomRecordItemDraw) then F_CustomRecordItemDraw(self,eventTyp_cdet,parentItem,recordItem,xpos,ypos,xColumn,result);
end;

procedure TTreeListView.removeSelection(list: TTreeListItems);
var i:longint;
begin
  BeginMultipleUpdate;
  for i:=0 to list.count-1 do begin
    list[i].Selected:=false;
    if list[i].SubItems.count>0 then removeSelection(list[i].SubItems);
  end;
  EndMultipleUpdate;
end;

procedure TTreeListView.removeMouseSelection(list: TTreeListItems);
var i:longint;
begin
  for i:=0 to list.count-1 do begin
    list[i].F_MouseSelected:=false;
    removeMouseSelection(list[i].SubItems);
  end;
end;

procedure TTreeListView.setMouseSelection(list: TTreeListItems);
var i:longint;
begin
  BeginMultipleUpdate;
  for i:=0 to list.count-1 do begin
    list[i].Selected:=list[i].Selected xor list[i].F_MouseSelected;
    list[i].F_MouseSelected:=false;
    setMouseSelection(list[i].SubItems);
  end;
  EndMultipleUpdate;
end;

procedure TTreeListView.SetMultiSelect(value: boolean);
begin
  F_MultiSelect:=value;
  if not F_MultiSelect then Selected:=focused;
end;

procedure TTreeListView.DoSelect(item: TTreeListItem);
begin
  if assigned(F_OnSelect) then
    F_OnSelect(self,item);
end;

//Kommunikationsroutinen (Set- und Getfunktionen)
procedure TTreeListView.SetItems(const value:TTreeListItems);
begin
  F_Items.Assign(value);
end;

function TTreeListView.GetTopPos:integer;
begin
  result:=HeaderItemDistance+F_Header.Height-F_VScroll.Position*RowHeight;
end;

procedure TTreeListView.SetColumns(const value:THeaderSections);
begin
  F_Header.Sections.Assign(value);
end;
function TTreeListView.GetColumns:THeaderSections;
begin
  Result:=F_Header.Sections;
end;

function TTreeListView.GetItemAtPos(const y:integer):TTreeListItem;
var i,startY:integer;
begin
  startY:=TopPos;
  result:=nil;
  for i:=0 to Items.Count-1 do begin
    result:=(TObject(Items[i]) as TTreeListItem).GetItemAtPos(self,y,startY);
    if result<>nil then exit;
  end;
end;

function TTreeListView.GetRecordItemAtPos(const x,y:integer):TTreeListRecordItem;
var item:TTreeListItem;
begin
  result:=nil;
  item:=getItemAtPos(y);
  if item<>nil then
   result:= item.GetRecordItemAtPos(self,x);

end;


procedure TTreeListView.SetHotTrackFont(const value:TFont);
begin
  F_HotTrackFont.Assign(value);
end;
procedure TTreeListView.SetSelectedFont(const value:TFont);
begin
  F_SelectedFont.Assign(value);
end;
procedure TTreeListView.SetSelectedHotTrackFont(const value:TFont);
begin
  F_SelectedHotTrackFont.Assign(value);
end;

procedure TTreeListView.BeginUpdate;
begin
  include(InternOptions_tlio,tlioUpdating)
end;

procedure TTreeListView.EndUpdate;
begin
  exclude(InternOptions_tlio,tlioUpdating);
  _GeneralEvent(self);
  if F_Sorted then sort;
end;

function TTreeListView.VisibleRowCount:longint;
begin
  if RowHeight=0 then result:=0
  else  result:=RealClientHeight div RowHeight;
end;

procedure TTreeListView.sort;
begin
  Items.Sort(CompareItems);
  if assigned(F_OnItemsSorted) then F_OnItemsSorted(self);
  internPaint;
end;

procedure TTreeListView.ensureVisibility(item: TTreeListItem;column: longint);
var rindex:longint;
    temp: TTreeListItem;
    rl,np: longint;
begin
  temp:=item.Parent;
  while temp<>nil do begin
    if not temp.Expanded then temp.Expand;
    temp:=temp.Parent;
  end;
  rindex:=Items.RealIndexOf(item,[]);
  if rindex<F_VScroll.Position then F_VScroll.Position:=rindex
  else if rindex>F_VScroll.Position+VisibleRowCount-1 then F_VScroll.Position:=rindex-VisibleRowCount+1;
  if column<>-1 then begin
    rl:=ColumnFromOriginalIndex(column).Left;
    np:=F_HScroll.Position;
    if rl+ ColumnFromOriginalIndex(column).Width - np>ClientWidth then
      np:=rl+ ColumnFromOriginalIndex(column).Width - ClientWidth; //move right
    if rl-np<0 then np:=rl; //move left (can revert right moving)
    F_HScroll.Position:=np;
  end;
end;

function TTreeListView.search(searchFor: string; searchFields: cardinal;
  backward: boolean; extendSelection: boolean): TFindState;
  function checkStr(item:TTreeListItem; col: integer):boolean;
  var stp:integer;
      currentText:string;
  begin
    result:=false;
    currentText:=item.RecordItemsText[col];
    stp:=pos(searchFor,lowercase(currentText));
    if stp>0 then begin
      if f_searchMarkVisible and extendSelection and (item=f_searchMarkItem) and
         ((col<f_searchMarkCol)or((col=f_searchMarkCol)and(stp<f_searchMarkStart))) then
           exit; //don't find something before the current selection

      if ColumnFromOriginalIndex(col)=nil then exit; //can't show it

      if (F_HighlightAll) or (f_searchMarkVisible and ((item<>f_searchMarkItem)or(f_searchMarkCol<>col)or(f_searchMarkStart<>stp))) then
        internPaint;

      f_searchMarkItem:=item;
      f_searchMarkCol:=col;
      f_searchMarkStart:=stp;
      f_searchMarkLen:=length(searchFor);
      f_searchMarkVisible:=true;
      if selCount<=1 then selected:=item;
      ensureVisibility(item,col);
      Application.ProcessMessages; //ensureVisibility changes the scroll pos, but this change is send as a message


      if col=0 then
        drawTextRect(item.RecordItemsText[f_searchMarkCol], item.GetExtraTextIndentation(col),
                     taLeftJustify,item.getBounds(f_searchMarkCol), true)
      else
        drawTextRect(item.RecordItemsText[f_searchMarkCol], 0,
                     ColumnFromOriginalIndex(col).Alignment,item.getBounds(f_searchMarkCol), true);
      result:=true;
    end;
  end;

  function checkItem(item:TTreeListItem): boolean;
  var i:longint;
  begin
    Result:=true;
    for i:=0 to Columns.count-1 do
      if (1 shl {$ifdef allowHeaderDragging}Columns[i].OriginalIndex{$else}i{$endif}) and searchFields <> 0 then
        if checkStr(item,{$ifdef allowHeaderDragging}Columns[i].OriginalIndex{$else}i{$endif}) then exit;
    Result:=false;
  end;

  function searchAllItems(item: TTreeListItem):boolean;
  var i:longint;
  begin
    result:=checkItem(item);
    if result then exit;
    for i:=0 to item.SubItems.Count-1 do begin
      result:=searchAllItems(item.SubItems[i]);
      if result then exit;
    end;
  end;

  function searchRemainingForward(items: TTreeListItems):TFindState; //would searchAllItems
  var i:longint;
  begin
    Result:=[fsLoopAround];
    for i:=0 to items.Count-1 do begin
      if checkItem(items[i]) then begin
        Result:=[fsFound, fsLoopAround];
        exit;
      end;
      result:=searchRemainingForward(items[i].SubItems);
      if fsFound in Result then exit;
    end;
  end;

  function searchBackwards(items: TTreeListItems):boolean; //would searchAllItems
  var i:longint;
  begin
    Result:=false;
    for i:=items.Count-1 downto 0 do begin
      result:=searchBackwards(items[i].SubItems);
      if Result then exit;
      if checkItem(items[i]) then begin
        Result:=true;
        exit;
      end;
    end;
  end;

var startItem,curItem: TTreeListItem;
    myStack: array of record
      list: TTreeListItems;
      index: longint;
    end;
    stackSize,lastStackSize:longint;

begin
 if  f_searchActivated then exit; //search calls Application.ProcessMessages, which could call search
 f_searchActivated:=true;
 try
  result:=[];
  if f_searchMarkVisible and (f_searchMarkItem<>nil) then startItem:=f_searchMarkItem
  else if F_Focused<>nil then startItem:=F_Focused
  else if Items.count>0 then startItem:=items[0]
  else exit;

  if not extendSelection then
    if not backward then begin
      if startItem<>Items[items.count-1].GetLastSubSubItem then startItem:=startItem.GetNextItem()
      else startItem:=Items[0];
    end else begin
      if startItem<>items[0] then startItem:=startItem.GetPrevItem()
      else startItem:=Items[items.count-1];
    end;

  searchFor:=LowerCase(searchFor);


  SetLength(myStack,8);
  stackSize:=0;
  curItem:=startItem;
  while curItem <> nil do begin
    stackSize:=stackSize+1;
    if stackSize>length(myStack) then SetLength(myStack,length(myStack)*2);
    move(myStack[0],myStack[1],(stackSize-1)*sizeof(myStack[0]));
    myStack[0].list:=curItem.ParentItems;
    myStack[0].index:=curItem.ParentItems.IndexOf(curItem);
    curItem:=curItem.Parent;
  end;


  if not backward then begin
    while stackSize>0 do begin //go to the end of the list
      if searchAllItems(myStack[stackSize-1].list[myStack[stackSize-1].index]) then begin
        Result:=[fsFound];
        exit;
      end;
      myStack[stackSize-1].index:=myStack[stackSize-1].index+1;
      while myStack[stackSize-1].index>=myStack[stackSize-1].list.count do begin
        dec(stackSize);
        if stackSize=0 then break;
        myStack[stackSize-1].index:=myStack[stackSize-1].index+1;
      end;
    end;
    result:=searchRemainingForward(Items);
  end else begin
    lastStackSize:=stackSize+1;
    while stackSize>0 do begin //go to the end of the list
      if ((stackSize=lastStackSize) and
           searchBackwards(myStack[stackSize-1].list[myStack[stackSize-1].index].SubItems))
         or checkItem(myStack[stackSize-1].list[myStack[stackSize-1].index]) then begin
        Result:=[fsFound];
        exit;
      end;
      lastStackSize:=stackSize;
      myStack[stackSize-1].index:=myStack[stackSize-1].index-1;
      if myStack[stackSize-1].index<0 then begin
        dec(stackSize);
        if stackSize=0 then break;
      end;
    end;
    if searchBackwards(Items) then Result:=[fsFound, fsLoopAround]
    else result:=[fsLoopAround];
  end;

  if fsFound in result then exit;
  if f_searchMarkVisible or F_HighlightAll then begin
    internPaint;
    f_searchMarkVisible:=false;
  end;
 finally //
   f_searchActivated:=false;
 end;
end;

function TTreeListView.ColumnFromOriginalIndex(index: longint): THeaderSection;
{$IFDEF allowHeaderDragging}var i:longint;{$endif}
begin
  result:=nil;
  {$IFDEF allowHeaderDragging}
    for i:=0 to F_Header.Sections.count-1 do
      if F_Header.Sections[i].OriginalIndex = index then
        exit(F_Header.Sections[i]);
  {$ELSE}
    if index <Columns.count then
      result:=Columns[index];
  {$ENDIF}
end;

procedure TTreeListView.CreateUserColumnVisibilityPopupMenu();
{$ifdef allowHeaderVisible}
var mi: TMenuItem;
    i:longint;
{$endif}
begin
  {$ifdef allowHeaderVisible}
  if F_HeaderColumnPopupMenu=nil then
    F_HeaderColumnPopupMenu:=TPopupMenu.Create(self);
  if F_Header.PopupMenu<>F_HeaderColumnPopupMenu then F_Header.PopupMenu:=F_HeaderColumnPopupMenu;
  F_HeaderColumnPopupMenu.Items.Clear;
  for i:=0 to Columns.count-1 do begin
    mi:=TMenuItem.Create(F_HeaderColumnPopupMenu);
    mi.OnClick:=ColumnPopupMenuClick;
    mi.Caption:=Columns[i].Text;
    mi.Checked:=Columns[i].visible;
    mi.tag:=i;
    F_HeaderColumnPopupMenu.Items.Add(mi);
  end;
  {$endif}
end;

function TTreeListView.serializeColumnWidths: string;
var i:longint;
    {$ifdef allowHeaderVisible}
    vis:boolean;
    {$endif}
    sec:THeaderSection;
begin
  Result:='';
  for i:=0 to F_Header.Sections.Count-1 do begin
    sec:=ColumnFromOriginalIndex(i);
    {$ifdef allowHeaderVisible}
      vis:=sec.Visible;
      if not vis then
        sec.Visible :=true;
    {$endif}
    result:=result+IntToStr(sec.Width)+',';
    {$ifdef allowHeaderVisible}
    if not vis then sec.Visible :=false;
    {$endif}
  end;
end;

function TTreeListView.serializeColumnOrder: string;
{$IFDEF allowHeaderDragging}
var i:longint;
{$endif}
begin
{$ifdef allowHeaderDragging}
  result:='';
  for i:=0 to F_Header.Sections.Count-1 do
    result:=result+ IntToStr(ColumnFromOriginalIndex(i).Index)+',';
{$endif}
end;

function TTreeListView.serializeColumnVisibility: string;
var i:longint;
begin
  result:='';
  for i:=0 to F_Header.Sections.count-1 do
    {$ifdef fpc}
    if not ColumnFromOriginalIndex(i).Visible then
      result+='-'
     else
     {$endif}result:=result+'+';
end;

procedure TTreeListView.deserializeColumnWidths(s: string);
var i:longint;
    sep: longint;
begin
  i:=0;
  while i<F_Header.Sections.Count do begin
    sep:=pos(',',s);
    if (sep=0) then break;
    ColumnFromOriginalIndex(i).Width:=StrToInt(trim(copy(s,1,sep-1)));
    delete(s,1,sep);
    inc(i);
  end;
end;

procedure TTreeListView.deserializeColumnOrder(s: string);
{$ifdef allowHeaderDragging}
var i:longint;
    sep: longint;
    tempOrder: array[0..20] of longint;
{$endif}
begin
{$ifdef allowHeaderDragging}
  //setting random index can changes other ones, therefore they will be set ascending
  //setlength(tempOrder,F_Header.Sections.Count);
  for i:=0 to high(tempOrder) do
    tempOrder[i]:=i;
  i:=0;
  while i<F_Header.Sections.Count do begin
    sep:=pos(',',s);
    if (sep=0) then break;
    tempOrder[i]:=StrToInt(trim(copy(s,1,sep-1)));
    delete(s,1,sep);
    inc(i);
  end;
  for i:=0 to F_Header.Sections.count-1 do
    ColumnFromOriginalIndex(tempOrder[i]).Index:=i;
{$endif}
end;

procedure TTreeListView.deserializeColumnVisibility(s: string);
{$ifdef allowHeaderVisible}
var i:longint;
{$endif}
begin
{$ifdef allowHeaderVisible}
  for i:=0 to min(length(s)-1,F_Header.Sections.Count-1) do
    ColumnFromOriginalIndex(i).Visible:=s[i+1]='+';
{$endif}
end;

procedure TTreeListView.CreateSearchBar();
var i:longint;
begin
  if F_SearchBar = nil then begin
    F_SearchBar:=TSearchBar.Create(self);
    F_SearchBar.Parent:=self;
    F_SearchBar.OnSearch:=SearchBarSearch;
    F_SearchBar.OnKeyDown:=SearchBarKeyDown;
    F_SearchBar.OnHighlightChanged:=F_SearchBarHighlightChanged;
    F_SearchBar.OnClose:=SearchBarClose;
  end;
  F_SearchBar.SubComponents:=[fscCloseButton, fscCaption, fscSelectLocation,
                              fscSearchForward, fscSearchBackwards, fscHighlight, fscStatus];
  F_SearchBar.SearchLocations.Clear;
  F_SearchBar.SearchLocations.AddObject('all',tobject(-1));
  for i:=0 to Columns.Count-1 do
    F_SearchBar.SearchLocations.AddObject(ColumnFromOriginalIndex(i).Text,tobject(1 shl i));
  F_SearchBar.SearchLocation:=0;
  {$ifdef lcl}F_SearchBar.Cursor:=crArrow;
  for i:=0 to F_SearchBar.ControlCount-1 do
    if F_SearchBar.Controls[i] is tedit then
      F_SearchBar.Controls[i].Cursor:=crIBeam;{$endif}

  UpdateScrollBarPos;
 // SetWindowLong(handle,GWL_STYLE,GetWindowLong(handle,GWL_STYLE) or WS_CLIPCHILDREN);
end;

procedure TTreeListView.setImageList(const images:TImageList);
begin
  F_ImageList:=images;
end;

procedure TTreeListView.SetRowHeight(const newHeight:integer);
begin
  if newHeight and $1=$1 then F_RowHeight:=newHeight+1
  else F_RowHeight:=newHeight;
end;

procedure TTreeListView.SetHotTrackSubTextItems(const value:boolean);
begin
  F_HotTrackSubTextItems:=value;
  if not value then hotTrackedRecordItem:=nil;
end;

function TTreeListView.RealControlHeight(c: Twincontrol): longint;
var r:TRect;
begin
  GetWindowRect(c.Handle,r);
  result:=r.bottom-r.top;
end;

function TTreeListView.RealClientHeight: longint;
begin
  result:=ClientHeight-RealControlHeight(F_Header)-HeaderItemDistance;
  if F_HScroll.Visible then result:=result-RealControlHeight(F_HScroll);
  if F_SearchBar <>nil then if F_SearchBar.Visible then result:=result-RealControlHeight(F_SearchBar);
end;

procedure TTreeListView.DrawAlignDotLine(x,y:integer;const x2,y2:integer;const color:TColor);
var F_HeaderHeight:integer;
begin
  F_HeaderHeight:=F_Header.Height;
  if y2<F_HeaderHeight then exit;
  if y<F_HeaderHeight then y:=F_HeaderHeight;
  {$R-}
  if x=x2 then begin
    while (y<=y2) do begin
      canvas.Pixels[x,y]:=color;
      inc(y,2);
    end;
  end else begin
    while (x<=x2) do begin
      canvas.Pixels[x,y]:=color;
      inc(x,2);
    end;
  end;
end;

procedure TTreeListView.drawTextRect(s: string; extraIndentation: longint;
  align:TAlignment;const rec: TRect; searchDraw: boolean);

var temp: TRect;
    flags: longint;

 procedure drawTextDef(s: string); {$ifdef fpc}inline;{$endif}
 begin
    DrawText(Canvas.Handle,{$IFNDEF CLR}pchar{$endif}(s),length(s), temp,flags);
 end;

var
    searched: string;
    stpos,i: longint;
    highlightText: boolean;
    parts: array of string;
    textStart: longint;
begin
  temp:=rec;
  temp.left:=temp.left+extraindentation;
  highlightText:=searchDraw or F_HighlightAll;
  stpos:=0;
  if highlightText then
    if F_SearchBar = nil then highlightText := false
    else begin
      searched:=lowercase(F_SearchBar.SearchText);
      stpos:=pos(searched,lowercase(s));
      highlightText:=stpos>0;
    end;
  if canvas.TextWidth(s)+LEFT_TEXT_PADDING>=rec.Right-rec.Left then
    align:=taLeftJustify; //justified because draw text always clips on the right side (TODO:??implement own clipping)
  flags:=DT_NOPREFIX or DT_SINGLELINE or DT_VCENTER or DT_END_ELLIPSIS;
  case align of
    taRightJustify:
      temp.Right:=temp.right-LEFT_TEXT_PADDING;
    taLeftJustify:
      temp.Left:=temp.left+LEFT_TEXT_PADDING;
  end;
  if not highlightText then begin
    case align of
      taCenter: flags:=flags or DT_CENTER;
      taRightJustify: flags:=flags or DT_RIGHT;
      else flags:=flags or DT_LEFT;
    end;
    drawTextDef(s);
  end else begin
     case align of
       taCenter: textStart:=(temp.right+temp.left-canvas.TextWidth(s)) div 2;
       taRightJustify: textStart:=temp.Right-Canvas.TextWidth(s);
       else textStart:=temp.Left;
     end;

     //split
     SetLength(parts,0);
     repeat
       SetLength(parts,length(parts)+2);
       parts[high(parts)-1]:=copy(s,1,stpos-1);
       parts[high(parts)]:=copy(s,stpos,length(searched));
       delete(s,1,stpos+Length(searched)-1);
       stpos:=pos(searched,lowercase(s));
     until (stpos<=0) or searchDraw;  //search marks only one
     setlength(parts,length(parts)+1);
     parts[high(parts)]:=s;

     //draw background
     canvas.brush.style:=bsSolid;
     temp.right:=textStart;
     for i:=0 to high(parts) do begin
       temp.left:=temp.right;
       temp.right:=temp.left+canvas.TextWidth(parts[i]);
       if temp.right>rec.right then temp.Right:=rec.right
       else if temp.left>rec.right then break;
       if i and 1 = 0 then begin
         if searchDraw then begin //highlight whole column by search
           canvas.brush.color:=ColorSearchMarkField;
           canvas.FillRect(temp);
         end;
       end else begin
         canvas.brush.color:=colorSearchMark;
         canvas.FillRect(temp);
       end;
     end;

     //draw text
     temp.left:=textStart;
     temp.right:=rec.right;
     canvas.brush.Style:=bsClear;
     for i:=0 to high(parts) do begin
       if temp.left>=rec.right then exit;
       drawTextDef(parts[i]);
       temp.left:=temp.left+canvas.TextWidth(parts[i]);
     end;
   end;

end;

function TTreeListView.CompareItems(i1, i2: TTreeListItem): longint;
var t1,t2:string;
    v1, v2,error: longint;
begin
  if assigned(F_OnCompareItems) then begin
    result:=0;
    F_OnCompareItems(self,i1,i2,result);
   end else begin
    t1:=i1.RecordItemsText[F_SortColumn];
    t2:=i2.RecordItemsText[F_SortColumn];

    val(t1,v1,error);
    if error=0 then begin
      val(t2,v2,error);
      if error=0 then begin
        if v1<v2 then result:=-1
        else if v1>v2 then result:=1
        else result:=0;
      end;
    end;
    if error<>0 then
      result:=CompareText(t1,t2);
  end;
  if F_SortColumnInverted then result:=-result;
end;

procedure TTreeListView.BeginMultipleUpdate;
begin
  inc(RedrawBlock);
end;

procedure TTreeListView.EndMultipleUpdate;
begin
  dec(RedrawBlock);
  if RedrawBlock=0 then _GeneralEvent(Self);
end;

//Interne Kommunikation mit Unterkomponenten
procedure TTreeListView._GeneralEvent(Sender: TObject);
begin
  if not HandleAllocated then exit;
  UpdateScrollBarPos;
  UpdateScrollSize;
  internPaint;
end;

procedure TTreeListView._HeaderSectionTrack(HeaderControl: TEventHeaderControl; Section: THeaderSection; Width: Integer; State: TSectionTrackState);
begin
  UpdateScrollSize;
  if assigned(F_HeaderSectionTrack) then F_HeaderSectionTrack(HeaderControl,Section,Width,State);
  internPaint;
end;
procedure TTreeListView._HeaderSectionResize(HeaderControl: TEventHeaderControl; Section: THeaderSection);
begin
  UpdateScrollSize;
  if assigned(F_HeaderSectionResize) then F_HeaderSectionResize(HeaderControl,Section);
  internPaint;
end;

procedure TTreeListView._HeaderSectionClick(HeaderControl: TEventHeaderControl; Section: THeaderSection);
var NewSortColumn,i:Longint;
    cursor: TPoint;
begin
  if not F_Sorted then exit;
  if Section=nil then begin
    GetCursorPos(cursor);
    cursor:=HeaderControl.ScreenToClient(cursor);
    for i := 0 to HeaderControl.Sections.Count - 1 do
      if (HeaderControl.Sections[i].Left<cursor.x)and
        (HeaderControl.Sections[i].right>cursor.x) then begin
          section:=HeaderControl.Sections[i];
          break;
        end;
    if Section=nil  then exit;
  end;
  {$IFDEF allowHeaderDragging}
  NewSortColumn:=Section.OriginalIndex;
  {$ELSE}
  NewSortColumn:=Section.Index;
  {$ENDIF}
  if F_SortColumn=NewSortColumn then F_SortColumnInverted:=not F_SortColumnInverted
  else begin
    F_SortColumn:=NewSortColumn;
    F_SortColumnInverted:=false;
  end;
  if assigned(F_OnUserSortItems) then F_OnUserSortItems(self,F_SortColumn,F_SortColumnInverted);
  sort;
end;

procedure TTreeListView._HeaderSectionDblClick(
  HeaderControl: TEventHeaderControl; Section: THeaderSection);
var i,w,maxw:longint;
begin
  maxw:=0;
  for i:=0 to items.count-1 do begin
    {$ifdef fpc}
    w:=Items[i].GetMaxColumnWidth(section.OriginalIndex);
    {$else}
    w:=Items[i].GetMaxColumnWidth(section.index);
    {$endif}
    if w>maxw then maxw:=w;
  end;
  if maxw+5>section.Width then section.width:=maxw+5
  else if Section.width+10>maxw+5 then section.width:=maxw+5;
end;

procedure TTreeListView._HeaderSectionEndDrag(Sender: TObject);
begin
  _GeneralEvent(Sender);
end;

procedure TTreeListView._HScrollChange(Sender: TObject);
begin
  hotTrackedRecordItem:=nil;
  UpdateScrollBarPos;
//  UpdateScrollSize;
  if assigned(F_HScrollBarChange) then F_VScrollBarChange(F_HScroll);
  internPaint;
end;

procedure TTreeListView._VScrollChange(Sender: TObject);
begin
  UpdateScrollBarPos;
  hotTrackedRecordItem:=nil;
//  UpdateScrollSize;
  if assigned(F_VScrollBarChange) then F_VScrollBarChange(F_VScroll);
  internPaint;
end;

procedure TTreeListView.UpdateScrollBarPos;
var newvalue: longint;
begin
  if (tlioDeleting in InternOptions_tlio) or
     (tlioUpdating in InternOptions_tlio) or
     (RedrawBlock>0) then exit;

  F_Header.left:=-F_HScroll.Position;

  F_VScroll.Left:=ClientWidth-F_VScroll.Width;
  F_VScroll.Top:=F_Header.Height;
  newvalue:=ClientHeight-F_VScroll.Top-F_HScroll.Height;
  if F_SearchBar<>nil then if F_SearchBar.Visible then
    newvalue:=newvalue - F_SearchBar.Height;
  F_VScroll.Height:=newvalue;

  F_HScroll.Width:=ClientWidth-F_VScroll.Width;
  newvalue:=ClientHeight-F_HScroll.Height;
  if F_SearchBar<>nil then if F_SearchBar.Visible then
    newvalue:=newvalue - F_SearchBar.Height;
  F_HScroll.Top:=newvalue;
end;

procedure TTreeListView.selectRange(a, b: TTreeListItem;mouseSelect:boolean=false);
var meetA, meetB: boolean;
  procedure setSelection(list: TTreeListItems);
  var i:longint;
  begin
    for i:=0 to list.count-1 do begin
      if meetB then begin
        if mouseSelect then list[i].F_MouseSelected:=false
        else list[i].Selected:=false
      end else if meetA or (list[i]=a) then begin
        meetA:=true;
        if mouseSelect then list[i].F_MouseSelected:=true
        else list[i].Selected:=true;
        if list[i]=b then meetB:=true;
      end else
        if mouseSelect then list[i].F_MouseSelected:=false
        else list[i].Selected:=false;

      setSelection(list[i].SubItems);
    end;
  end;

var temp: TTreeListItem;
begin
  if items.count=0 then exit;
  if a=nil then a:=items[0];
  if b=nil then b:=items[items.count-1];
  if items.RealIndexOf(a,[ricCountCollapsedsubItems])>items.RealIndexOf(b,[ricCountCollapsedsubItems]) then begin
    temp:=a;
    a:=b;
    b:=temp;
  end;
  BeginMultipleUpdate;
  meetA:=false;
  meetB:=false;
  try
    setSelection(Items);
  finally
    EndMultipleUpdate;
  end;
end;

procedure TTreeListView.UpdateScrollSize;
var i,j:integer;
begin
  if (tlioDeleting in InternOptions_tlio) or
     (tlioUpdating in InternOptions_tlio) or
     (RedrawBlock>0) then exit;

  i:=Items.GetRealItemCount([])-VisibleRowCount; //Anzahl der nicht anzeigbaren Items
  if i-1>F_VScroll.Min then begin
    //F_VScroll.Enabled:=false;
    F_VScroll.Enabled:=true;
    F_VScroll.Max:=i{$ifdef lcl}-1{$else}+VisibleRowCount{$endif};
    F_VScroll.PageSize:=VisibleRowCount;
    F_VScroll.LargeChange:=VisibleRowCount;
  end else if F_VScroll.Enabled then begin
    F_VScroll.Enabled:=true; //disabling doesn't always work direct??
    F_VScroll.Position:=0;
    F_VScroll.Enabled:=false;
  end;
  //F_VScroll.PageSize:=(ClientHeight-F_HScroll.Height*3-F_Header.Height) div (F_VScroll.Max - F_VScroll.Min + 1);

  j:=0;
  for i:=0 to F_Header.Sections.Count-1 do begin
    j:=j+F_Header.Sections[i].Width;
  end;
  i:=j-width+F_VScroll.Width;
  if i>=0 then begin
    F_HScroll.Enabled:=true;
    F_HScroll.max:=i;
    F_HScroll.PageSize:=F_HScroll.width;
    F_HScroll.LargeChange:=F_HScroll.width;
  end else if F_HScroll.Enabled then begin
    F_HScroll.Enabled:=true; //s.a.
    F_HScroll.Position:=0;
    F_HScroll.Enabled:=false;
  end;
end;

//Messages
procedure TTreeListView.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if (AComponent=F_ImageList) and (Operation=opRemove) then
    F_ImageList:=nil;
  inherited;
end;

procedure TTreeListView.WndProc(var message:{$IFDEF LCL}TLMessage{$else}TMessage{$endif});
  {$ifndef LCL}
    const LM_GETDLGCODE = WM_GETDLGCODE;
          LM_MOUSEWHEEL = WM_MOUSEWHEEL;
          LM_MOUSEMOVE = WM_MOUSEMOVE;
          LM_LBUTTONDOWN = WM_LBUTTONDOWN;
          LM_RBUTTONDOWN = WM_RBUTTONDOWN;
          LM_LBUTTONUP = WM_LBUTTONUP;
          LM_RBUTTONUP = WM_RBUTTONUP;
          LM_KEYDOWN = WM_KEYDOWN;
          LM_KEYUP = WM_KEYUP;
          LM_SETFOCUS = WM_SETFOCUS;
          LM_KILLFOCUS = WM_KILLFOCUS;
          LM_SIZE = WM_SIZE;
          LM_PAINT = WM_PAINT;
          LM_ERASEBKGND = WM_ERASEBKGND;
    type TLMMouseMove = TWMMouseMove;
         TLMLBUTTONDOWN = TWMLBUTTONDOWN;
         TLMLButtonUp = TLMLBUTTONDOWN;
         TLMKeyDown = TWMKeyDown;
         TLMKeyUp = TWMKeyUp;
  {$endif}

var tempRecordItem:TTreeListRecordItem;
    itemAtPos,nextToFocus: TTreeListItem;
    shiftState: TShiftState;
    cursorPos: tpoint;
begin
  nextToFocus:=nil;
  case message.Msg of
    LM_GETDLGCODE:  message.Result:=DLGC_WANTARROWS or DLGC_WANTCHARS;
    LM_MOUSEWHEEL: if F_VScroll.Visible and F_VScroll.Enabled then begin
      {$ifdef lcl}
      F_MouseWheelDelta:=F_MouseWheelDelta+(PLMMouseEvent(@message))^.WheelDelta;
      {$else}
      F_MouseWheelDelta:=F_MouseWheelDelta+TWMMouseWheel(message).WheelDelta;
      {$endif}
      if (F_MouseWheelDelta<=-120) or (F_MouseWheelDelta>=120) then begin
        F_VScroll.Position:=F_VScroll.Position-F_MouseWheelDelta div 120;
        F_MouseWheelDelta:=0;
      end;
    end;
    LM_MOUSEMOVE: begin
      inherited;
      shiftState:=KeyDataToShiftState(TLMMouseMove(message).Keys);
      if (F_ClickedItem<>nil) and (multiSelect) and   // \/ left or right, not both
          ((MK_LBUTTON and message.wParam <> 0)<>(MK_RBUTTON and message.wParam <> 0))
          and ((MK_LBUTTON and message.wParam <> 0) or
               (RightMouseSelects and (MK_RBUTTON and message.wParam <> 0))) then begin
        F_RealMousePos:=point(TLMMouseMove(message).XPos+F_HScroll.Position,
                              TLMMouseMove(message).YPos+F_VScroll.Position);
        if F_MouseSelecting=msNone then
          if sqr(F_RealClickPos.x-F_RealMousePos.x)+ sqr(F_RealClickPos.y-F_RealMousePos.y)>100 then
            if MK_RBUTTON and message.wParam <> 0 then F_MouseSelecting:=msRight
            else F_MouseSelecting:=msLeft;
        if F_MouseSelecting<>msNone then begin
          itemAtPos:=GetItemAtPos(TLMMouseMove(message).YPos);
          if F_ClickedItem.Selected then begin
            F_ClickedItem.F_MouseSelected:=true;
            F_ClickedItem.Selected:=false;
          end;
          if itemAtPos<>nil then begin
            selectRange(F_ClickedItem,itemAtPos,true);
            ensureVisibility(itemAtPos);
          end;
        end;
      end;


      if (TLMMouseMove(message).XPos<F_VScroll.Left) and (TLMMouseMove(message).YPos>F_Header.Height)
         and (TLMMouseMove(message).YPos<F_HScroll.Top) then
        tempRecordItem:=GetRecordItemAtPos(TLMMouseMove(message).XPos,TLMMouseMove(message).YPos)
       else
        tempRecordItem:=nil;
      if tempRecordItem<>nil then begin
        if ToolTips then
          if (ColumnFromOriginalIndex(tempRecordItem.F_Index)<>nil) and
            (tempRecordItem.GetNecessaryWidth(self)+10>ColumnFromOriginalIndex(tempRecordItem.F_Index).Width) then begin
            hint:=tempRecordItem.Text;
            ShowHint:=true;
          end else ShowHint:=false;
      end;
      IF HotTrackSubTextItems THEN
        if tempRecordItem<>hotTrackedRecordItem then begin
          hotTrackedRecordItem:=tempRecordItem;
          if hotTrackedRecordItem<>nil then Cursor:=crHandPoint else Cursor:=crDefault;
          internPaint;
        end;
    end;
    LM_LBUTTONDOWN,LM_RBUTTONDOWN: begin
      SetFocus;
      if TLMLBUTTONDOWN(message).YPos<F_HScroll.top then begin
        shiftState:=KeyDataToShiftState(TLMLBUTTONDOWN(message).Keys);
        itemAtPos:=GetItemAtPos(TLMLBUTTONDOWN(message).YPos);
        if message.msg=LM_LBUTTONDOWN then begin
          if (itemAtPos<>nil) and
             (TLMLBUTTONDOWN(message).XPos<itemAtPos.GetExtendingButtonPos+9) and
             (TLMLBUTTONDOWN(message).XPos>itemAtPos.GetExtendingButtonPos) then begin
            itemAtPos.Expanded:=not itemAtPos.Expanded;
            if itemAtPos=focused then internPaint;
          end;
        end;
        if (message.msg=LM_LBUTTONDOWN) or RightMouseSelects then begin
          F_ClickedItem:=itemAtPos;
          F_RealClickPos:=point(TLMLBUTTONDOWN(message).XPos+F_HScroll.Position,TLMLBUTTONDOWN(message).YPos+F_VScroll.Position);
          nextToFocus:=itemAtPos;
          if multiSelect and (nextToFocus<>nil) and (ssCtrl in shiftState) then
            nextToFocus.Selected:=not nextToFocus.Selected;
          if F_MouseSelecting<>msNone then begin
            F_MouseSelecting:=msNone;
            setMouseSelection(Items);
          end;
        end;
      end;
      inherited;
    end;
    LM_LBUTTONUP,LM_RBUTTONUP: begin
      if F_MouseSelecting<>msNone then begin
        F_ClickedItem:=nil;
        F_MouseSelecting:=msNone;
        setMouseSelection(Items);
        internPaint;
      end;
      if message.msg=LM_LBUTTONDOWN then begin
        if F_ClickedItem = GetItemAtPos(TLMLBUTTONDOWN(message).YPos) then begin
          if assigned(OnClickAtItem) then OnClickAtItem(self,F_ClickedItem);
          if assigned(OnClickAtRecordItem) then begin
            tempRecordItem:=F_ClickedItem.GetRecordItemAtPos(self,TLMLButtonUp(message).XPos);
            if tempRecordItem<>nil then OnClickAtRecordItem(self,F_ClickedItem,tempRecordItem);
          end;
        end;
      end else if message.msg=LM_RBUTTONUP then begin
        GetCursorPos(cursorPos);
        if assigned(PopupMenu) then PopupMenu.PopUp(cursorPos.x,cursorPos.Y);
      end;
      inherited;
    end;
    LM_KEYDOWN: begin
      shiftState:=KeyDataToShiftState(TLMKeyDown(message).KeyData);
      case TLMKeyDown(message).CharCode of
        VK_UP:
          if focused=nil then nextToFocus:=Items[0]
          else nextToFocus:=focused.GetPrevVisibleItem;
        VK_DOWN:
          if focused<>nil then nextToFocus:=focused.GetNextVisibleItem()
          else if items.count>0 then nextToFocus:=Items[0];//items.count-1];

        VK_HOME:
          if items.count>0 then nextToFocus:=Items[0];

        VK_END:
          if items.count>0 then nextToFocus:=Items[items.count-1].GetLastVisibleSubSubItem;

        VK_PRIOR:
          if focused<>nil then nextToFocus:=focused.GetPrevVisibleItem(VisibleRowCount)
          else if items.count>0 then nextToFocus:=Items[0];

        VK_NEXT:
          if focused<>nil then nextToFocus:=focused.GetNextVisibleItem(VisibleRowCount)
          else if items.count>0 then nextToFocus:=Items[items.count-1];

        VK_RIGHT:
          if focused<>nil then begin
            if not focused.Expanded then focused.Expand;
            if focused.SubItems.Count>0 then nextToFocus:=focused.SubItems[0]
            else nextToFocus:=focused;
          end;

        VK_LEFT:
          if focused<>nil then begin
            if (focused.Expanded) and (focused.SubItems.Count>0) then focused.Collapse
            else if focused.Parent<>nil then nextToFocus:=focused.Parent;
            if nextToFocus=nil then nextToFocus:=focused;
          end;

        VK_BACK:
          if (focused<>nil) and (focused.Parent<>nil) then nextToFocus:=focused.Parent;

        VK_SPACE:
          if (focused <> nil) and (ssCtrl in shiftState) then begin
            if ssShift in shiftState then Selected:=focused
            else focused.Selected:=not focused.Selected;
            F_BaseSelect:=focused;
          end;
        else inherited;
      end;
    end;
    LM_SETFOCUS:    begin
                      internPaint;
                      inherited;
                    end;
    LM_KILLFOCUS:   begin
                      internPaint;
                      inherited;
                    end;
    LM_SIZE:        begin
                      UpdateScrollBarPos;
                      UpdateScrollSize;
                      internPaint;
                      inherited;
                    end;
    LM_ERASEBKGND: message.Result:=1;
    LM_KEYUP:
      if (TLMKeyUp(message).CharCode = VK_APPS) and assigned(PopupMenu) then begin
        GetCursorPos(cursorPos);
        PopupMenu.PopUp(cursorPos.X,cursorPos.y);
      end else if (TLMKeyUp(message).CharCode = ord('F')) and (F_SearchBar<>nil) then begin
        shiftState:=KeyDataToShiftState(TLMKeyDown(message).KeyData);
        if ssCtrl in shiftState then begin
          F_SearchBar.Visible:=true;
          F_SearchBar.SetFocus;
        end else inherited;
      end else inherited;
    else inherited;
  end;
  if nextToFocus<>nil then begin //select or focus
    if not multiSelect or (shiftState=[]) then begin
      Selected:=nextToFocus;
      F_BaseSelect:=Selected;
    end else if shiftState=[ssCtrl] then focused:=nextToFocus
    else if ssShift in shiftState then begin
      F_Focused:=nextToFocus;
      SelectRange(F_BaseSelect,nextToFocus);
      ensureVisibility(nextToFocus);
    end;
  end;
    
end;

procedure TTreeListView.internPaint;
  function srect(x1,y1,x2,y2:longint):trect;
  begin
    if y1<y2 then begin
      result.top:=y1;
      result.Bottom:=y2;
    end else begin
      result.Bottom:=y1;
      result.top:=y2;
    end;
    if x1<x2 then begin
      result.Left:=x1;
      result.Right:=x2;
    end else begin
      result.Right:=x1;
      result.Left:=x2;
    end;
  end;
var i,ypos,xpos:integer;
 //   doubleBuffer:Tbitmap;
    oldHandle:Thandle;
    defaultDraw:boolean;
    outRect: Trect;
    newWidth, newHeight: longint;
begin
  if (tlioUpdating in InternOptions_tlio) or
     (tlioDisablePainting in InternOptions_tlio) or
     (tlioDeleting in InternOptions_tlio) or
     (f_items=nil) or (f_items.freeing) or
     (RedrawBlock>0) then exit;

  {$IFDEF allowHeaderDragging}
    for i:=0 to F_Header.Sections.Count-1 do
      if F_Header.Sections[i].OriginalIndex=0 then begin
        F_TreeSectionPos:=rect(F_Header.Sections[i].Left-F_HScroll.Position,0,F_Header.Sections[i].Right-F_HScroll.Position,0);
        break;
      end;
  {$ELSE}
    F_TreeSectionPos:=rect(F_Header.Sections[0].Left-F_HScroll.Position,0,F_Header.Sections[0].Right-F_HScroll.Position,0);
  {$ENDIF}
  PaintEvenItem:=true;
  newWidth:=width +128 - Width mod 128;//don't change the size for small control size changes
  newHeight:=height +128 - height mod 128;
  {$ifdef lcl}
  doubleBuffer.SetSize(newwidth,newheight);
  {$else}
  doubleBuffer.width:=newwidth;
  doubleBuffer.height:=newheight;
  {$endif}
  //if Canvas.Handle=0 then exit;
  //canvas.Lock;
  try
    //doubleBuffer.Canvas.Lock;
    oldHandle:=Canvas.Handle;
    try
      with Canvas do begin
        //Background
        defaultDraw:=DoCustomBackgroundDrawEvent(cdetPrePaint);
        if defaultDraw then begin
          pen.Style:=psClear;
          brush.Style:=bsSolid;
          brush.Color:=clBtnFace;
          outRect:=rect(F_HScroll.left+F_HScroll.Width,F_VScroll.top+F_VScroll.Height,ClientWidth,ClientHeight);
          if (F_SearchBar<>nil) and (f_searchbar.Visible) then dec(outRect.Bottom, F_SearchBar.Height);
          FillRect(outRect);

          Handle:=doubleBuffer.Canvas.Handle; //Canvas verbiegen, mit z.B.: Canvas.LineTo wird nun in den Buffer gezeichnet

          pen.Style:=psClear;
          brush.Style:=bsSolid;
          brush.color:=F_BgColor;

//        Rectangle(handle,0,F_Header.Height,ClientWidth-F_VScroll.Width,ClientHeight-F_HScroll.Height);
          FillRect(rect(0,F_Header.Height,ClientWidth-F_VScroll.Width,ClientHeight-F_HScroll.Height));
        end;

        //Items
        ypos:=TopPos;
        if RootLineMode <> lmNone then StartX:=13
        else StartX:=3;

        for i:=0 to Items.count-1 do
          Items[i].PaintTo(self,ypos,0,i = Items.count-1);

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
        if F_MouseSelecting<>msNone then
          DrawFocusRect(srect(F_RealClickPos.x-F_HScroll.Position,F_RealClickPos.y-F_VScroll.Position,
                              F_RealMousePos.x-F_HScroll.Position,F_RealMousePos.y-F_VScroll.Position));
        DoCustomBackgroundDrawEvent(cdetPostPaint);
        Handle:=oldHandle; //Handle zurückbiegen, nun wird wieder ins Steuerelement gezeichnez
        outRect:=rect(0,F_Header.Height,F_VScroll.Left {$ifndef lcl}-1{$endif},F_HScroll.top{$ifndef lcl}-1{$endif});
        CopyRect(outRect,doubleBuffer.canvas,outRect); //DoubleBuffer ausgeben
      end;

    finally
      canvas.Handle:=oldHandle;
      //doubleBuffer.Canvas.Unlock;
      {doubleBuffer.free;}
    end;
  finally
    //canvas.Unlock;
  end;
end;

//Ausgaberoutinen
procedure TTreeListView.Paint;
begin
  internPaint;
  if F_SearchBar<>nil then
    F_SearchBar.Invalidate
end;

//Destroy
destructor TTreeListView.Destroy;
begin
  Include(InternOptions_tlio,tlioDeleting);
  F_HotTrackFont.free;
  F_SelectedHotTrackFont.free;
  F_SelectedFont.free;

  F_HScroll.free;
  F_VScroll.free;
  if F_HeaderColumnPopupMenu<>nil then F_HeaderColumnPopupMenu.free;
  F_Header.free;
  F_Items.free;
  DoubleBuffer.Free;

  inherited;
end;
procedure Register;
begin
  RegisterComponents('BeniBela', [TTreeListView]);
end;

end.







