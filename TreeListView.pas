{**
  This unit contains the TTreeListView which is a combination of a TreeView and a
  ListView. This means that you can organize the items in a tree and show
  additional information in columns.

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
  {$define allowHeaderDragging}  //header section can only be dragged by the user if this is defined,
                                 //it needs at least lazarus 9.26
  {$define allowHeaderVisible}   //the visibility of header section can only be changed by the user if this
                                 //is defined; it needs at least lazarus 9.27 (SVN, r16817)
{$endif}
{$ifdef lcl}
  {$define openOwnPopupMenu}     //disable if you get 2 popupmenus (it is necessary in some lcl versions)
  {$define useRealClipping}        //old Lazarus/fpc don't support clipping (should work with (Lazarus  >= 19742 (0.9.27) and fpc >2.3.1) or (Lazarus > 20731 (0.9.27)))
{$endif}

interface

uses
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs,comctrls,stdctrls,ExtCtrls, Menus,math,
  findControl
  {$ifdef lclqt}, qtwidgets{$endif}
  {$ifdef clr},types{$endif}
  {$ifdef lcl},LCLType,LCLIntf, LMessages{$else},windows,messages{$endif};

type
  {$TYPEINFO ON}
  //Forward
  TObjectList=class;
  TTreeListRecordItem=class;
  TTreeListItem=class;
  TTreeListView = class;

  TListEventTyp=(levBeginEdit,levEndEdit,levAdd,levInsert,levClear,levDelete,levExchange,levMove,levSort,levAssign);
  TListEvent = procedure (list: TObjectList; typ: TListEventTyp) of object;
  {** @abstract This is a list storing TObjects for the TreeListView
      The list supports change notifications and automatically deletes the items}
  TObjectList= class(TList)
    public
      procedure BeginEdit;
      procedure EndEdit;

      function AddObject(Item: TObject): Integer;
      procedure InsertObject(Index: Integer; Item: TObject);
      function RemoveObject(Item: TObject): Integer;

      procedure Clear; override;
      procedure Delete(Index: Integer);
      procedure Exchange(Index1, Index2: Integer);
      procedure Move(CurIndex, NewIndex: Integer);
      procedure Sort(Compare: TListSortCompare);

      procedure Assign(list:TList);

      constructor Create(listEvent: TListEvent);
      destructor Destroy;override;
    protected
      onListEvent: TListEvent;

      procedure FreeObjects;

      function Add(Item: TObject): Integer;
      procedure Insert(Index: Integer; Item: TObject);
      function Remove(Item: TObject): Integer;
  end;

  //**This specifies if invisible/collapsed items are counted
  TRealItemCounting=set of (ricCountCollapsedsubItems{,ricCountExpandItems,ricCountEndNodes});

  { TTreeListItems }
  {$ifdef fpc}pbool=pboolean;{$endif}
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
    function Get(Index: Integer): TTreeListItem;
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
    function GetItemWithRealIndex(index:integer;const countTyp:TRealItemCounting):TTreeListItem;

    //**This searches recursive for a item with the given caption
    //**@return the first matching item or nil if nothing is found
    function FindItemWithText(caption: string): TTreeListItem;
    //**This searches recursive for a item with text in the column pos (if pos = 0 this is the same as FindItemWithText)
    //**@return the first matching item or nil if nothing is found
    function FindItemWithRecordText(text: string;pos: longint=0):TTreeListItem;
    //**This searches the text searchFor in searchFields.
    //**@param searchFields Bit-wise combination of column numbers. Use @code(1 shl i) for column i. (you can only use the first 32 columns)
    //**@param backward  Specifies search direction
    function find(searchFor: string; searchFields:cardinal; backward: boolean=false; loopAround: PBOOL=nil; startItem: TTreeListItem=nil; startColumn: longint=0; startTextPosition:longint=0): TTreeListRecordItem;overload;

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
    private
    protected
      F_Parent: TTreeListItem;
      F_Text:string;
      {F_HotTrackFont:TFont;
      F_HotTrack:boolean;
      F_ParentHotTrack:boolean;
      F_ParentHotTrackFont:boolean; }
      procedure SetText(caption:string);
      function getIndex(): longint;
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
      property Index: longint read getIndex;
      property Parent:TTreeListItem read F_Parent;
  end;

  //**stores the parent items of an item, don't use the members
  TItemHierarchyStack=record
    size:longint;
    stack:array of record
      list: TTreeListItems;
      index: longint;
    end;
  end;

  //**can be used to store abitrary 64 bit data in every item (will be removed as soon as the generics are really usable)
  TItemDataRec = packed record //inspired by int64rec
     case integer of
       0 : (i64: int64);
       1 : (lo32, hi32 : Cardinal);
       2 : (Words : Array[0..3] of Word);
       3 : (Bytes : Array[0..7] of Byte);
       4 : (p : pointer);
       5 : (obj : TObject);
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

      procedure SetSelected(newSelected: boolean);
      procedure SetMouseSelected(newSelected: boolean);
      procedure SetSelections(realSelected, mouseSelection:boolean);

      procedure SetSubItems(const value:TtreeListItems);
      procedure SetRecordItems(const value:TRecordItemList);
      procedure SetExpand(const expanded:boolean);

      function GetRecordItemsText(i: Integer): string;
      procedure SetRecordItemsText(i: Integer; const AValue: string);

      procedure SheduleInternRepaint();
    public
      data:TItemDataRec; //**< This value can be used to store arbitrary integer values

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
      function GetRecordItemAtPos(const listView:TTreeListView;TestX:integer):TTreeListRecordItem;

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

      procedure GetParentHierarchyStack(out stack:TItemHierarchyStack); //**returns a stack you can use to enumerate all item iterative
      function GetNextFromHierarchyStack(var stack: TItemHierarchyStack; const mustBeVisible: boolean=false): TTreeListItem;//<** get the (visible) next item, using a hierarchy stack

      property Parent:TTreeListItem read F_parent; //**< This is a parent of this item @br This is @nil if the item is in @noAutoLink TreeListView.Items
      property TreeListView:TTreeListView read F_TreeListview; //**< This is the TreeListView showing this item
      function ParentItems: TTreeListItems; //**< This returns the list containing the item @br It is either @noAutoLink TreeListView.Items or @noAutoLink Parent.SubItems

      //**This draws the item @br Don't call it direct
      //**@param hierarchyStack list of parents
      procedure Paint(const hierarchyStack: TItemHierarchyStack);

      //**Destroy
      destructor Destroy;override;

      property Indent:integer read F_Indent;//**< Level of indentation @br This value is not guaranteed to be correct (but it is during paint events)
      function SeemsSelected:boolean;//**< Returns if the items is drawn selected @br When the user selects new items there new selection state can be previewed
      property Expanded:boolean read F_expanded write SetExpand; //**< Specifies if the sub items are currently visible

      property MouseSelected: boolean read F_MouseSelected write SetMouseSelected; //**< Controls if this item is selected or not
      property RecordItemsText[i: Integer]:string read GetRecordItemsText write SetRecordItemsText; //**< Sets the value of the given column @br Notice that this array is 0-based and RecordItemsText[0] is always the same as Text @br Getting a not existing item will give you '', setting will create it
    published
      property RecordItems:TRecordItemList read F_RecordItems write SetRecordItems; //**< Items in the columns @br Normally you can use RecordItemsText for easier access
      property SubItems:TTreeListItems read F_SubItems write SetSubItems; //**< Indented child items
      property ImageIndex:longint read F_ImageIndex write F_ImageIndex; //**< If this is > -1 then the image of the TreeListView.Images will be painted before this item @br This property is ignored if ImageBitmap <> @nil or TreeListView.Images = @nil @br Use ImageBitmap if you are in doubt (a image list may be better with regards to caching issues, but clipping is slower)
      property ImageBitmap:graphics.TBitmap read F_ImageBitmap  write F_ImageBitmap; //**< Bitmap which should be drawn before the item @br This image is not freed when the item is destroyed, so you can use the same bitmap for multiple items
      property Text:string read GetText write SetText; //**< Text in the first column of this item @br This is always equal to RecordItemsText[0]
      property Selected: boolean read F_Selected write SetSelected; //**< Controls if this item is selected or not
  end;

  //**General appearance/behaviour options
  TTreeListViewOption = ( tlvoMultiSelect, //**<Specifies if multiple items can be selected
                          tlvoToolTips, //**<Specifies if tooltips are shown when item text is longer than the column
                          tlvoRightMouseSelects, //**< Determines if you can select items using the right mouse button
                          tlvoHotTrackRecordTextItems, //**< Determines if the record items are hot tracked
                          tlvoStriped, //**< Determines if the item background is drawn alternating
                          tlvoStripInvisibleItems, //**< Controls if invisible items are counted when the control determines if a item is odd or even (if on items are striped, if off positions are striped)
                          tlvoColumnsDragable, //**< Controls if the columns of the header control can be reordered (needs FPC)
                          tlvoSorted, //**< Controls of the items should be @noAutoLink sorted @br Notice that items are only sorted in endUpdate (and the first time tlvoSorted is set), so new inserted or changed items are not automatically sorted until you call endUpdate
                          tlvoAlwaysFullRepaint, //** repaints everything even after a small change
                          tlvoDragScrolling //** The list can be scrolled by dragging an item up and down (like standard Android lists)
                        );
  TTreeListViewOptions=set of TTreeListViewOption;

  TTreeListInternOptions=set of (tlioDeleting, tlioUpdating); //**<@exclude
  TExpandMode=(emExpandByClick,emExpandByDoubleClick,emExpandNot);
  TLineMode=(lmNone,lmSolid,lmDot);
  TExpandItemEvent = procedure (Sender: TObject; Item: TTreeListItem);
  TCustomDrawEventTyp=(cdetPrePaint,cdetPostPaint);
  TCustomBackgroundDrawEvent=procedure (sender:TObject;eventTyp_cdet:TCustomDrawEventTyp;var defaultDraw:Boolean) of object;
  TCustomItemDrawEvent=procedure (sender:TObject;eventTyp_cdet:TCustomDrawEventTyp;item:TTreeListItem;var defaultDraw:Boolean) of object;
  TCustomRecordItemDrawEvent=procedure (sender:TObject;eventTyp_cdet:TCustomDrawEventTyp;recordItem:TTreeListRecordItem;var defaultDraw:Boolean) of object;
  TCustomRecordItemPositioningEvent = procedure (sender:TObject; visualColumnIndex: integer; recordItem:TTreeListRecordItem; var aposition: TRect) of object;
  TItemEvent=procedure (sender:TObject;item:TTreeListItem) of object;
  TRecordItemEvent=procedure (sender:TObject;recorditem:TTreeListRecordItem) of object;
  TCompareTreeListItemsEvent=procedure (sender: TObject; item1, item2: TTreeListItem; var result: longint)of object;
  TUserSortItemsEvent=procedure (sender: TObject; var sortColumn: longint; var invertSorting: boolean) of object;

  { TTreeListView }
  {$ifndef fpc}
  TEventHeaderControl=THeaderControl; //**<@exclude
  {$else}
  TEventHeaderControl=TCustomHeaderControl; //**<@exclude
  {$endif}

  {** @abstract This is the main TreeListView-class and the only class you need to @noAutoLink create yourself. @br
      Simple Example (use in FormCreate), which creates two @noAutoLink(items) one called 'Item' and one called
      'Child' where latter shows the value 'Property' in the second column (the tree with the
      names will be in the first column):
  @longCode(#
         //Standard component creation
         List:=TTreeListView.create(self);
         List.Parent:=self;
         List.Align:=alClient;
         //Create Columns
         List.Columns.Clear;
         List.Columns.Add.Text:='A';
         List.Columns.Add.Text:='B';
         //Create Items
         List.BeginUpdate;
         List.Items.Add('Item').SubItems.Add('Child').RecordItemsText[1]:='Property';
         List.EndUpdate;
      #)

      @br@br
      Generally, the treelistview shows its @noAutoLink(items) in a 2d record layout, with
      the vertical @noAutoLink(items) of a treeview and the horizontal @noAutoLink(items) of a listview. @br
      Former are just called "@noAutoLink(items)", latter are called "record @noAutoLink(items)"; and each
      record item is associated to a normal item.

      @br@br
      Creating normal @noAutoLink(items)
      @br@br
      The simplest way to @noAutoLink(create) a new item is by calling the method @code(treelistview.Items.Add('text')).@br
      This will add a new item with text "text" to the first level of the tree.@br
      If you want to @noAutoLink(create) an item on a deeper level of the tree i.e. as sub item to a given parent item, you can call
      either @code(treelistview.Items.Add(parent, 'text')) or @code(parent.subitems.add('text');)@br
      If you're going to add several @noAutoLink(items), you should call @code(treelistview.BeginUpdate) and @code(treelistview.EndUpdate).


      @br@br
      Creating record @noAutoLink(items)
      @br@br
      To add a record item to a given @code(item) e.g. to set its text in column @code(i), you can just call @code(item.RecordItemsText[i]:='text';)



  }
  TTreeListView = class(TCustomControl)
  private
    F_HeaderVisible: boolean;
    F_ScrollStyle: TScrollStyle;
    function GetTopItemVisualIndex: integer;
    procedure SetBgColor(const AValue: TColor);
    procedure SetButtonColor(const AValue: TColor);
    procedure SetColorSearchMark(const AValue: tcolor);
    procedure SetColorSearchMarkField(const AValue: tcolor);
    procedure SetExpandMode(const AValue: TExpandMode);
    procedure SetHeaderVisible(AValue: boolean);
    procedure SetHorizontalLineColor(const AValue: TColor);
    procedure SetHorizontalLines(const AValue: TLineMode);
    procedure SetRootLineColor(const AValue: TColor);
    procedure SetRootLines(const AValue: TLineMode);
    procedure SetScrollStyle(AValue: TScrollStyle);
    procedure SetSelectBackColor(const AValue: TColor);
    procedure SetStripedEvenColor(const AValue: TColor);
    procedure SetStripedOddColor(const AValue: TColor);
    procedure SetVerticalLineColor(const AValue: TColor);
    procedure SetVerticalLines(const AValue: TLineMode);
  protected
    { Protected-Deklarationen}
    InternOptions_tlio:TTreeListInternOptions;
    doubleBuffer:graphics.TBitmap;
    F_LastPaintedWidth, F_LastPaintedHeight: integer;

    f_RedrawBlock: longint;
    f_invalidatedItems: TList;
    f_invalidateAll: boolean;
    f_bufferComplete: boolean;//This is true if the buffer contains the recent state of all items (= the last drawing was called with f_invalidateAll)

    TreeColumnIndentation:integer;
    F_TopItem: TTreeListItem;
    F_TopItemEven: boolean;

    F_SortColumn: longint;
    F_SortColumnInverted: boolean;
    F_OnCompareItems: TCompareTreeListItemsEvent;
    F_OnUserSortItems: TUserSortItemsEvent;

    F_Items:TTreeListItems;
    F_Header:THeaderControl;
    F_HeaderColumnPopupMenu: TPopupMenu;
    F_VScroll:TScrollBar; //vertikale, rechte scrollbar, in listeinträgen
    F_HScroll:TScrollBar; //horizontale, untere scrollbar, in pixeln
    F_RowHeight:integer;
    F_ImageList:TImageList;

    F_ExpandMode:TExpandMode;


    //Selection
    F_SelCount: longint;
    F_Focused: TTreeListItem;
    F_BaseSelect: TTreeListItem; //last item selected without shift


    //appearance
    F_Options: TTreeListViewOptions;
    F_TreeSectionPos: TRect;

    F_HotTrackFont:TFont;


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

    //Events

    //Headerevents
    {$ifdef FPC}
    F_HeaderSectionResize:TCustomSectionNotifyEvent;
    F_HeaderSectionTrack:TCustomSectionTrackEvent;
    {$else}
    F_HeaderSectionResize:TSectionNotifyEvent;
    F_HeaderSectionTrack:TSectionTrackEvent;
    {$endif}

    //Scrollbarevents
    F_VScrollBarChange:TNotifyEvent;
    F_HScrollBarChange:TNotifyEvent;
    F_MouseWheelDelta: longint;

    //CustomDrawEvents
    F_CustomBgDraw:TCustomBackgroundDrawEvent;
    F_CustomItemDraw:TCustomItemDrawEvent;
    F_CustomRecordItemDraw:TCustomRecordItemDrawEvent;
    F_CustomRecordItemPositioningEvent: TCustomRecordItemPositioningEvent;
      //details
    F_DrawingEvenItem: boolean;
    F_DrawingYPos: longint;
    F_DrawingRecordItemRect: TRect;
    F_SheduledRepaint: DWord;
    F_SheduledHScroll: DWord;

    //Inputevents
    F_RealClickPos, F_RealMousePos: TPoint;
    F_ScrollClickPos: integer; //v_scroll.position, when the mouse button was pressed
    F_LastMouseMove: cardinal;
    F_ClickedItem: TTreeListItem;
    F_MouseSelecting: (msNone,msLeft,msRight);
    F_MouseSelectingFocusRectDraw: boolean;
    F_MouseSelectingFocusRect: TRect;
    F_ClickAtItem:TItemEvent;
    F_ItemCollapsed:TItemEvent;
    F_ItemExpanded:TItemEvent;
    F_ClickAtRecordItem:TRecordItemEvent;
    F_OnSelect:TItemEvent;
    F_OnItemExpanded: TItemEvent;
    F_OnItemCollapsed: TItemEvent;
    F_OnItemsSorted: TNotifyEvent;

    //Search
    f_searchMarkItem: TTreeListItem;
    f_searchMarkCol,f_searchMarkStart,f_searchMarkLen: longint;
    f_searchMarkVisible,f_searchActivated:boolean;
    f_colorSearchMark: tcolor;
    f_colorSearchMarkField: tcolor;
    F_SearchBar: TSearchBar;
    F_NewSearchBarFindState: TFindState;
    F_HighlightAll: boolean;
    procedure SearchBarSearch(sender: TObject; incremental, backwards: boolean);
    procedure SearchBarClose(Sender: TObject);
    procedure SearchBarShow(Sender: TObject);
    procedure SearchBarKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure F_SearchBarHighlightChanged(Sender: TObject);

    //Ereignissausösungen
    function DoCustomBackgroundDrawEvent (eventTyp_cdet:TCustomDrawEventTyp):boolean;
    function DoCustomItemDrawEvent(const eventTyp_cdet:TCustomDrawEventTyp;const item:TTreeListItem):boolean;
    function DoCustomRecordItemDrawEvent(const eventTyp_cdet:TCustomDrawEventTyp;const RecordItem:TTreeListRecordItem;const outrec: TRect):boolean;

    procedure removeSelection(list: TTreeListItems);
    procedure removeMouseSelection(list: TTreeListItems);
    procedure setMouseSelection(list: TTreeListItems);
    procedure DoSelect(item: TTreeListItem);virtual;
    procedure selectRange(a,b: TTreeListItem;mouseSelect:boolean=false);

    //Kommunikationsroutinen (Set- und Getfunktionen)
    procedure SetOptions(const AValue: TTreeListViewOptions);
    procedure SetOption(const Option: TTreeListViewOption; const active:boolean);

    procedure SetItems(const value:TTreeListItems);
    procedure SetFocused(const AValue: TTreeListItem);
    procedure SetSelected(const AValue: TTreeListItem);

    procedure setTopItem(item: TTreeListItem);
    function GetTopItem:TTreeListItem;
    function GetTopItemEven: boolean;
    function GetTopPos:integer;

    procedure SetSortColumn(const AValue: longint);

    procedure SetColumns(const value:THeaderSections);
    function GetColumns: THeaderSections;

    procedure setImageList(const images:TImageList);

    procedure SetRowHeight(const newHeight:integer);

    procedure SetHotTrackFont(const value:TFont); //< Set the font used to draw a hottracked item
    procedure SetSelectedFont(const value:TFont); //< Set the font used to draw a selected item
    procedure SetSelectedHotTrackFont(const value:TFont);//< Set the font used to draw a selected and hottracked item

    //Sonstiges
    function RealControlHeight(c: Twincontrol): longint;
    function RealBaseClientWidth: longint;
    function RealBaseClientHeight: longint;
    function RealClientHeight: longint;
    procedure DrawAlignDotLine(x,y:integer;const x2,y2:integer;const color:TColor);
    procedure drawTextRect(s:string;extraIndentation:longint;align:TAlignment; const rec: TRect; searchDraw: boolean);
    function CompareItems(i1, i2: TTreeListItem): longint;

    procedure BeginMultipleUpdate;
    procedure EndMultipleUpdate;

    //Interne Kommunikation mit Unterkomponenten
    procedure updateAll();
    procedure _SubItemListEvent(list: TObjectList; typ: TListEventTyp);
    procedure _RecordItemListEvent(list: TObjectList; typ: TListEventTyp);
    procedure _HeaderSectionTrack( HeaderControl: TEventHeaderControl;  Section: THeaderSection;  Width: Integer;  State: TSectionTrackState);
    procedure _HeaderSectionResize( HeaderControl: TEventHeaderControl;  Section: THeaderSection);
    procedure _HeaderSectionClick( HeaderControl: TEventHeaderControl;  Section: THeaderSection);
    procedure _HeaderSectionDblClick( HeaderControl: TEventHeaderControl;  Section: THeaderSection);
    procedure _HeaderSectionEndDrag(Sender: TObject);
    {$ifdef allowHeaderVisible}procedure ColumnPopupMenuClick(Sender: TObject);{$endif}
    procedure _HScrollChange(Sender: TObject);
    procedure _VScrollChange(Sender: TObject);

    procedure UpdateScrollBarPos; virtual;
  protected
    {$ifdef android}
    F_PostMessage: TLMessage;
    F_PostMessageTimer: TTimer;
    procedure PostMessageTimerTimer(Sender: TObject);
    {$endif}
    procedure internPostMessage(Msg: Cardinal; WParam: WParam); //**< Wrapper around PostMessage. PM does not seem to work on Android
  public
    { Public-Deklarationen}
    hotTrackedRecordItem:TTreeListRecordItem; //**<Item currently touched by the mouse
    property selCount: longint read F_SelCount; //**<Count of selected items
    {$warnings off}
    property focused:TTreeListItem read F_Focused write SetFocused; //**<Currently focused item, it is not necessarily selected. @br Setting this property will not select the new item
    {$warnings on}
    property Selected:TTreeListItem read F_Focused write SetSelected; //**<This is the same as Focused, but setting this property will select the item and deselect every other one
    property SortColumn: longint read F_SortColumn write SetSortColumn; //**<Column currently used for sorting

    procedure UpdateScrollSizeH; //**<@deprecated Recalculates the necessary scrollbar properties @br Normally you don't need to call this
    procedure UpdateScrollSizeV; //**<@deprecated Recalculates the necessary scrollbar properties @br Normally you don't need to call this
    procedure UpdateScrollSize; //**<@deprecated Recalculates the necessary scrollbar properties @br Normally you don't need to call this

    //**Create
    constructor Create(aowner:TComponent);override;
    procedure loaded;override;

    function GetItemAtPos(const y:integer):TTreeListItem;//**<Returns the item at position y
    function GetRecordItemAtPos(const x,y:integer):TTreeListRecordItem;//**<Returns the record item at position x,y @br Notice that it doesn't check for visibility, e.g you can use negative coordinates or find items hidden by the scrollbars


    //Items
    procedure BeginUpdate; //**< Notifies the control that the @noAutoLink items are changed, so it will not redrawn itself. @br Never forget to use this, otherwise it will be very slow.
    procedure EndUpdate; //**< Stops the redraw block and redraws everything
    function VisibleRowCount:longint; //**< Count of visible lines
    procedure sort; //**< Sorts the items according to the current sorting options
    procedure ensureVisibility(item: TTreeListItem;column: longint=-1); //**< Makes item visible, this includes scrolling and expanding of items @br If column is not -1 it scroll horizontally to the beginning/ending of this column if it isn't visible


    //**This searches the text searchFor in searchFields and marks it.
    //**@param searchFields Bit-wise combination of column numbers. Use @code(1 shl i) for column i. (you can only use the first 32 columns)
    //**@param backward  Specifies search direction
    //**@param extendSelection set this to true for an incremental search
    function search(searchFor: string; searchFields:cardinal; backward: boolean=false;extendSelection: boolean=false): TFindState;overload;

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

    //elements drawing
    //**This method will soon repaint all items.
    //**This means as soon as the calling functions finished (= returns to the application message loop) and all pending messages are processed, internRepaint will be called
    //**@seealso(internPaint) @seealso(sheduleInternRepaint) @seealso(invalidateAll) @seealso(invalidateItem)
    procedure sheduleInternRepaint();
    //**This method forces a redraw of all items (=invalidateAll and internPaint)
    //**@seealso(internPaint) @seealso(sheduleInternRepaint) @seealso(invalidateAll) @seealso(invalidateItem)
    procedure internRepaint();
    //**This invalidates the passed item, so that it will be redrawn if the next painting occurs
    //**@seealso(internPaint) @seealso(sheduleInternRepaint) @seealso(internRepaint) @seealso(invalidateAll)
    procedure invalidateItem(item: TTreeListItem=nil);
    //**This invalidates all items, so the whole control will be redrawn if next painting occurs
    //**@seealso(internPaint) @seealso(sheduleInternRepaint) @seealso(internRepaint) @seealso(invalidateItem)
    procedure invalidateAll();
    //**This method draws all changed (=invalidated) items in the double buffer. There should never be a reason to call this
    //**@seealso(internPaint) @seealso(sheduleInternRepaint) @seealso(internRepaint) @seealso(invalidateAll) @seealso(invalidateItem)
    procedure internDraw();
    //**This method will paint all changed items on the screen. This means it will call internDraw to draw all changed item in the double buffer and then copy the changed areas of the double buffer on the screen
    //**@seealso(sheduleInternRepaint) @seealso(internRepaint) @seealso(invalidateAll) @seealso(invalidateItem)
    procedure internPaint(calledFromPaintingEvent: boolean=false);//shows the changes
    //**This is the normal Delphi/Lazarus painting routine called when a paint message is received. You should call it seldom.
    //**@seealso(sheduleInternRepaint) @seealso(internRepaint) @seealso(invalidateAll) @seealso(invalidateItem)
    procedure Paint;override;

    //Destroy
    destructor Destroy;override;

    property TopPos:integer read GetTopPos; //**< V-Scrollposition calculated in pixels (=position of Items[0])
    property TopItem: TTreeListItem read GetTopItem write SetTopItem; //**< Visible item with the least real index (can be nil)
    property TopItemIsEvenItem: boolean read GetTopItemEven; //**< Is the top item even (used for striping)
    property TopItemVisualIndex: integer read GetTopItemVisualIndex; //**< Index of the top item, if all visible items were in a single list
    property DrawingEvenItem: boolean read F_DrawingEvenItem; //**< Is the currently drawn item even (only valid during custom draw events, having this as property prevents parameter cluttering)
    property DrawingYPos: longint read F_DrawingYPos; //**< Y-Position of the currently drawn item (only valid during custom draw events, having this as property prevents parameter cluttering)
    property DrawingRecordItemRect: TRect read F_DrawingRecordItemRect; //**< boundaries of the currently drawn record item (only valid during custom draw events, having this as property prevents parameter cluttering)
  published
    { Published-Deklarationen }
    {-------------------------------START Ereignisse---------------------------}
    //**General appearance/behaviour options @seealso TTreeListViewOption
    property Options: TTreeListViewOptions read F_Options write SetOptions;

    //Header-Eigenschaften
    property Columns:THeaderSections read GetColumns write SetColumns; //**< All columns

    property RowHeight:integer read F_RowHeight write SetRowHeight; //**< Height of a row

    property Images:TImageList read F_ImageList write setImageList; //**< ImageList used to get the images for items using the TTreeListView.ImageIndex property

    property HorizontalLineMode:TLineMode read F_HorizontalLines write SetHorizontalLines; //**< Determines  how/if lines are drawn between the items
    property HorizontalLineColor:TColor read F_HorizontalLineColor write SetHorizontalLineColor;
    property VerticalLineMode:TLineMode read F_VerticalLines write SetVerticalLines; //**< Determines  how/if lines are drawn between the columns
    property VerticalLineColor:TColor read F_VerticalLineColor write SetVerticalLineColor;
    property RootLineMode:TLineMode read F_RootLines write SetRootLines; //**< Determines  how/if lines are drawn to connect the tree items
    property RootLineColor:TColor read F_RootLineColor write SetRootLineColor;
    property ColorSearchMark: tcolor read F_ColorSearchMark write SetColorSearchMark;
    property ColorSearchMarkField: tcolor read F_ColorSearchMarkField write SetColorSearchMarkField;

    property ExpandMode:TExpandMode read F_ExpandMode write SetExpandMode; //**< Determines  how/if the user is allowed to collapse/expand items

    property HotTrackFont:TFont read F_HotTrackFont write SetHotTrackFont;
    property Font;
    property SelectedFont:TFont read F_SelectedFont write SetSelectedFont;
    property SelectedHotTrackFont:TFont read F_SelectedHotTrackFont write SetSelectedHotTrackFont;

    property StripedOddColor:TColor read F_StripedOddColor write SetStripedOddColor;
    property StripedEvenColor:TColor read F_StripedEvenColor write SetStripedEvenColor;

    property SelectBackColor:TColor read F_SelectBackColor write SetSelectBackColor;
    property ButtonColor:TColor read F_ButtonColor write SetButtonColor; //**< Color of the expand/collaps button
    property BackGroundColor:TColor read F_BgColor write SetBgColor;

    property Items:TTreeListItems read F_Items write SetItems; //**< All the items, use items.add to @noAutoLink create new ones

    property Scrollbars: TScrollStyle read F_ScrollStyle write SetScrollStyle;
    property HeaderVisible: boolean read F_HeaderVisible write SetHeaderVisible;

    //Sortierungsereignisse
    property OnCompareItems: TCompareTreeListItemsEvent read F_OnCompareItems write F_OnCompareItems; //**< Event which is called when two items are compared during sorting @br The default sorting is case-insensitive lexicographical on text and numerical on number string parts, every level is @noAutoLink sorted on its own, parents are not changed
    property OnUserSortItemsEvent: TUserSortItemsEvent read F_OnUserSortItems write F_OnUserSortItems; //**< Called when the user clicks on the header to resort the items
    property OnItemsSortedEvent: TNotifyEvent read F_OnItemsSorted write F_OnItemsSorted; //**< Called after the items have been @noAutoLink sorted

    //Scrollbarereignisse
    property OnVScrollBarChange:TNotifyEvent read F_VScrollBarChange write F_VScrollBarChange;
    property OnHScrollBarChange:TNotifyEvent read F_HScrollBarChange write F_HScrollBarChange;

    //CustomDrawEvents
    property OnCustomBgDraw:TCustomBackgroundDrawEvent read F_CustomBgDraw write F_CustomBgDraw; //**< This is called before/after the items are drawn
    property OnCustomItemDraw:TCustomItemDrawEvent read F_CustomItemDraw write F_CustomItemDraw; //**< This is called before/after an item is drawn @seealso TTreeListItem.PaintTo, TTreeListView.DrawingEvenItem, TTreeListView.DrawingYPos, TTreeListView.DrawingRecordItemRect
    property OnCustomRecordItemDraw:TCustomRecordItemDrawEvent read F_CustomRecordItemDraw write F_CustomRecordItemDraw; //**< This is called before/after any record items is drawn

    property OnCustomRecordItemPositioning: TCustomRecordItemPositioningEvent read F_CustomRecordItemPositioningEvent write F_CustomRecordItemPositioningEvent; //**< This is called when the position of a record item is calculated

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
    {$ifndef fpc}property BorderWidth; //if you want a border in Lazarus put it in a panel
    property BevelWidth;
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
    property PopupMenu;
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

{$ifdef lcl}
uses LResources; //for icon
{$endif}

const HeaderItemDistance=2; //Distance between Header and first drawn item
      LEFT_TEXT_PADDING=3;
      LINE_DISTANCE=15;
      {$IFNDEF lcl}
      LM_USER=WM_USER;
      {$ENDIF}
      LM_USER_SHEDULED_EVENT = LM_USER+1125;
      EVENT_REPAINT = 1;
      EVENT_MOUSE_SCROLL = 2;
      EVENT_HSCROLL = 3;

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

constructor TObjectList.Create(listEvent: TListEvent);
begin
  inherited Create;
  self.onListEvent:=listEvent;
end;

procedure TObjectList.BeginEdit;
begin
  if assigned(onListEvent) then onListEvent(self,levBeginEdit);
end;

procedure TObjectList.EndEdit;
begin
  if assigned(onListEvent) then onListEvent(self,levEndEdit);
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

//Eventauslöser
function TObjectList.Add(Item: TObject): Integer;
begin
  result:=inherited add(item);
  if assigned(onListEvent) then onListEvent(self,levAdd);
end;
procedure TObjectList.Clear;
begin
  FreeObjects;
  count:=0;
  if assigned(onListEvent) then onListEvent(self,levClear);
end;
procedure TObjectList.Delete(Index: Integer);
begin
  inherited;
  if assigned(onListEvent) then onListEvent(self,levDelete);
end;
procedure TObjectList.Exchange(Index1, Index2: Integer);
begin
  inherited;
  if assigned(onListEvent) then onListEvent(self,levExchange);
end;
procedure TObjectList.Insert(Index: Integer; Item: TObject);
begin
  inherited insert(index,item);
  if assigned(onListEvent) then onListEvent(self,levInsert);
end;
procedure TObjectList.Move(CurIndex, NewIndex: Integer);
begin
  inherited;
  if assigned(onListEvent) then onListEvent(self,levMove);
end;
function TObjectList.Remove(Item: TObject): Integer;
begin
  item.free;
  result:=inherited remove(item);
  if assigned(onListEvent) then onListEvent(self,levDelete);
end;
procedure TObjectList.Sort(Compare: TListSortCompare);
begin
  inherited;
  if assigned(onListEvent) then onListEvent(self,levSort);
end;
procedure TObjectList.Assign(list:TList);
var i:integer;
begin
  Count:=list.Count;
  for i:=0 to Count-1 do
    Items[i]:=list.Items[i];
  if assigned(onListEvent) then onListEvent(self,levAssign);
end;
procedure TObjectList.FreeObjects;
var i:integer;
begin
  for i:=0 to Count-1 do begin
    TObject(Items[i]).free;
    Items[i]:=nil;
  end;
  count:=0;
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

function TTreeListItems.Get(Index: Integer): TTreeListItem;
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
  if assigned(onListEvent) then onListEvent(self,levSort);
end;

constructor TTreeListItems.Create(parent:TTreeListItem;const TreeListView:TTreeListView);
begin
  inherited Create(TreeListView._SubItemListEvent);
  F_Parent:=parent;
  F_TreeListView:=TreeListView;
end;

function TTreeListItems.Add(caption:string):TTreelistItem;
begin
  Result:=TTreeListItem.Create(F_Parent,F_TreeListView, caption);
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

function TTreeListItems.FindItemWithRecordText(text: string;pos: longint=0
  ): TTreeListItem;
var i:longint;
begin
  result:=nil;
  for i:=0 to count-1 do
    if Items[i].RecordItemsText[pos]=text then begin
      result:=Items[i];
      exit;
    end else begin
      result:=Items[i].SubItems.FindItemWithRecordText(text,pos);
      if result<>nil then exit;
    end;
end;

function TTreeListItems.find(searchFor: string; searchFields: cardinal;
  backward: boolean; loopAround: PBOOL; startItem: TTreeListItem;
  startColumn: longint; startTextPosition: longint): TTreeListRecordItem;
  function checkStr(item:TTreeListItem; col: integer):TTreeListRecordItem;
  var stp:integer;
      currentText:string;
  begin
    result:=nil;
    currentText:=item.RecordItemsText[col];
    stp:=pos(searchFor,lowercase(currentText));
    if stp>0 then begin
      if (item=startItem) and
         ((col<startColumn)or((col=startColumn)and(stp<startTextPosition))) then
           exit; //don't find something before the current selection

      result:=item.RecordItems[col];
    end;
  end;

  function checkItem(item:TTreeListItem): TTreeListRecordItem;
  var i:longint;
  begin
    Result:=nil;
    for i:=0 to F_TreeListView.Columns.count-1 do //process in dragged order
      if {$ifdef allowHeaderDragging}F_TreeListView.Columns[i].OriginalIndex{$else}i{$endif}<32 then
        if (1 shl {$ifdef allowHeaderDragging}F_TreeListView.Columns[i].OriginalIndex{$else}i{$endif}) and searchFields <> 0 then begin
          result:=checkStr(item,{$ifdef allowHeaderDragging}F_TreeListView.Columns[i].OriginalIndex{$else}i{$endif});
          if result<>nil then exit;
        end;
  end;
                                           {
  function searchAllItems(item: TTreeListItem):TTreeListRecordItem;
  var i:longint;
  begin
    result:=checkItem(item);
    if result<>nil then exit;
    for i:=0 to item.SubItems.Count-1 do begin
      result:=searchAllItems(item.SubItems[i]);
      if result<>nil then exit;
    end;
  end;                                    }

  function searchRemainingForward(items: TTreeListItems):TTreeListRecordItem;
  var i:longint;
  begin
    Result:=nil;
    for i:=0 to items.Count-1 do begin
      result:=checkItem(items[i]);
      if result<>nil then exit;
      result:=searchRemainingForward(items[i].SubItems);
      if Result<>nil then exit;
    end;
  end;

  function searchBackwards(items: TTreeListItems):TTreeListRecordItem;
  var i:longint;
  begin
    Result:=nil;
    for i:=items.Count-1 downto 0 do begin
      result:=searchBackwards(items[i].SubItems);
      if Result<>nil then exit;
      result:=checkItem(items[i]);
      if result<>nil then exit;
    end;
  end;

var curItem: TTreeListItem;
    stack: TItemHierarchyStack;
    lastStackSize:longint;

begin
  Result:=nil;
  if count=0 then exit;
  if loopAround<>nil then loopAround^:=false;
  if searchFor='' then exit; //stop here, or it may crash later
  if startItem=nil then startItem:=Items[0];
  searchFor:=LowerCase(searchFor);

  curItem:=startItem;
  curItem.GetParentHierarchyStack(stack);
  if not backward then begin
    repeat
      result:=checkItem(curItem);
      if result<>nil then exit;
      curItem:=curItem.GetNextFromHierarchyStack(stack);
    until curItem=nil;
   {while stack.size>0 do begin
      result:=searchAllItems(stack.stack[stack.size-1].list[stack.stack[stack.size-1].index]);
      if result<>nil then exit;
      stack.stack[stack.size-1].index:=stack.stack[stack.size-1].index+1;
      while stack.stack[stack.size-1].index>=stack.stack[stack.size-1].list.count do begin
        dec(stack.size);
        if stack.size=0 then break;
        stack.stack[stack.size-1].index:=stack.stack[stack.size-1].index+1;
      end;
    end;                                   }
    result:=searchRemainingForward(self);
    if (result <>nil) and (loopAround<>nil) then loopAround^:=true;
  end else begin
    laststacksize:=stack.size+1;
    while stack.size>0 do begin
      if stack.size=laststacksize then begin
         result:=searchBackwards(stack.stack[stack.size-1].list[stack.stack[stack.size-1].index].SubItems);
         if result<>nil then exit;
      end;
      result:=checkItem(stack.stack[stack.size-1].list[stack.stack[stack.size-1].index]);
      if result<>nil then exit;
      laststacksize:=stack.size;
      stack.stack[stack.size-1].index:=stack.stack[stack.size-1].index-1;
      if stack.stack[stack.size-1].index<0 then begin
        dec(stack.size);
        if stack.size=0 then break;
      end;
    end;
    result:=searchBackwards(self);
    if (result<>nil) and (loopAround<>nil) then loopAround^:=true;
  end;
end;

function tTreeListItems.GetItemWithRealIndex(index:integer;const countTyp:TRealItemCounting):TTreeListItem;
  function GetItemWithRealIndexRek(const nself:TTreeListItems;var index:integer):TTreeListItem;
  var i:integer;
  begin
    result:=nil;
    for i:=0 to nself.count-1 do begin
      dec(index);
      if index<=0 then begin result:=nself[i];exit;end
      else begin
        result:=GetItemWithRealIndexRek(nself[i].SubItems,index);
        if index<=0 then exit;
      end;
    end;
  end;
  function GetItemWithRealIndexRekVisible(const nself:TTreeListItems;var index:integer):TTreeListItem;
  var i:integer;
  begin
    result:=nil;
    for i:=0 to nself.count-1 do begin
      dec(index);
      if index=0 then begin result:=nself[i];exit;end
      else if nself[i].Expanded then begin
        result:=GetItemWithRealIndexRekVisible(nself[i].SubItems,index);
        if index<=0 then exit;
      end;
    end;
  end;
var i:integer;
begin
  if index<0 then begin
    result:=nil;
    exit;
  end;
  i:=index+1;
  if ricCountCollapsedsubItems in countTyp then result:=GetItemWithRealIndexRek(self,i)
  else result:=GetItemWithRealIndexRekVisible(self,i);
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

function TTreeListRecordItem.getIndex(): longint;
begin
  result:=F_Parent.RecordItems.IndexOf(self);
end;

procedure TTreeListRecordItem.SetText(caption: string);
begin
  F_Text:=caption;
  if F_Parent <> nil then
    F_Parent.SheduleInternRepaint();
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
constructor TTreeListItem.Create(const parent: TTreeListItem; const TreeListView: TTreeListView; const ACaption: string);
begin
  assert(TreeListView<>nil);
  inherited Create;
  F_Parent:=parent;
  if Parent=nil then F_Indent:=0
  else F_Indent:=Parent.F_Indent+1;
  F_TreeListView:=TreeListView;
  f_Selected:=false;
  F_RecordItems:=TRecordItemList.Create(TreeListView._RecordItemListEvent);
  F_RecordItems.Owner:=self;
  F_RecordItems.Add(ACaption);
  F_SubItems:=TTreeListItems.Create(self,TreeListView);
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
    SheduleInternRepaint();
  end;
end;

procedure TTreeListItem.SheduleInternRepaint;
begin
  if F_TreeListView <> nil then begin
    F_TreeListView.invalidateItem(Self);
    F_TreeListView.sheduleInternRepaint();
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
  result:=F_Indent*LINE_DISTANCE+TreeListView.TreeColumnIndentation;
  if ImageBitmap<>nil then Result:=Result+ImageBitmap.width+LEFT_TEXT_PADDING
  else if (ImageIndex>-1) and (TreeListView.Images<>nil) then Result:=Result+TreeListView.Images.Width+LEFT_TEXT_PADDING;
end;

function TTreeListItem.GetExtendingButtonPos: longint;
begin
  result:=TreeListView.F_TreeSectionPos.left+(F_Indent+1)*LINE_DISTANCE-10;
end;

procedure TTreeListItem.SetMouseSelected(newSelected: boolean);
begin
  if F_MouseSelected = newSelected then exit;
  F_MouseSelected:=newSelected;
  TreeListView.invalidateItem(self);
end;

procedure TTreeListItem.SetSelected(newSelected: boolean);
begin
  if Selected = newSelected then exit;
  F_Selected:=newSelected;
  if F_Selected then begin
    if not (tlvoMultiSelect in TreeListView.F_Options) then TreeListView.focused:=self;
    TreeListView.F_SelCount:=TreeListView.F_SelCount+1;
  end else TreeListView.F_SelCount:=TreeListView.F_SelCount-1;
  TreeListView.DoSelect(self);
  TreeListView.invalidateItem(self);
  TreeListView.internPaint;
end;

procedure TTreeListItem.SetSelections(realSelected, mouseSelection:boolean);
begin
  if realSelected xor mouseSelection <> Selected xor F_MouseSelected then
    TreeListView.invalidateItem(self);
  F_Selected:=realSelected;
  F_MouseSelected:=mouseSelection;
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
  F_Expanded:=true;
  TreeListView.internRepaint();
  if assigned(TreeListView.F_OnItemExpanded) then
    TreeListView.F_OnItemExpanded(TreeListView,self);

end;
procedure TTreeListItem.Collapse;
begin
  if not F_Expanded then exit;
  F_Expanded:=false;
  TreeListView.internRepaint();
  if assigned(TreeListView.F_OnItemCollapsed) then
    TreeListView.F_OnItemCollapsed(TreeListView,self);
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

function TTreeListItem.GetRecordItemAtPos(const listView:TTreeListView;TestX:integer):TTreeListRecordItem;
var i,x:integer;
begin
  Result:=nil;
  x:=0;
  TestX+=listView.F_HScroll.Position;
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

procedure TTreeListItem.GetParentHierarchyStack(out stack:TItemHierarchyStack);
var curItem:TTreeListItem;
begin
  //TODO: consider using a linked list
  SetLength(stack.stack,8);
  stack.size:=0;
  curItem:=self;
  while curItem <> nil do begin
    stack.size:=stack.size+1;
    if stack.size>length(stack.stack) then SetLength(stack.stack,length(stack.stack)*2);
    system.move(stack.stack[0],stack.stack[1],(stack.size-1)*sizeof(stack.stack[0]));
    stack.stack[0].list:=curItem.ParentItems;
    stack.stack[0].index:=curItem.ParentItems.IndexOf(curItem);
    curItem:=curItem.Parent;
  end;
end;
function TTreeListItem.GetNextFromHierarchyStack(var stack: TItemHierarchyStack; const mustBeVisible: boolean): TTreeListItem;
begin
  assert(stack.size>0);
  assert(self=stack.stack[stack.size-1].list[stack.stack[stack.size-1].index]);
  if (self.SubItems.Count>0) and (Expanded or not mustBeVisible) then begin
    if length(stack.stack)<=stack.size then setlength(stack.stack,length(stack.stack)*2);
    stack.stack[stack.size].list:=F_SubItems;
    stack.stack[stack.size].index:=0;
    inc(stack.size);
    result:=SubItems[0];
  end else begin
    stack.stack[stack.size-1].index:=stack.stack[stack.size-1].index+1;
    while stack.stack[stack.size-1].index>=stack.stack[stack.size-1].list.count do begin
      dec(stack.size);
      if stack.size=0 then begin
        result:=nil;
        exit;
      end;
      stack.stack[stack.size-1].index:=stack.stack[stack.size-1].index+1;
    end;
    result:=stack.stack[stack.size-1].list[stack.stack[stack.size-1].index];
  end;
end;

function TTreeListItem.ParentItems: TTreeListItems;
begin
  if Parent = nil then result:=TreeListView.Items
  else result:=Parent.SubItems;
end;

procedure TTreeListItem.Paint(const hierarchyStack: TItemHierarchyStack);
var i,ynew,yold,recordId:integer;
    defaultDraw:boolean;
    rec:Trect;
  //draw the text and images in the tree column
  procedure drawTreeColumnText;
  var textStartX,imageX: longint;
      drawSearchMark: boolean;
      {$ifdef useRealClipping}
      oldClipping:boolean;
      oldClippingRect:TRect;
      {$ELSE}
      tempBitmap: graphics.TBitmap;
      {$ENDIF}
  begin
    textStartX:=F_Indent*LINE_DISTANCE+F_TreeListView.TreeColumnIndentation;
    drawSearchMark:=TreeListView.f_searchMarkVisible and (self=TreeListView.f_searchMarkItem) and (TreeListView.f_searchMarkCol=0);
    imageX:=textStartX+rec.left+LEFT_TEXT_PADDING;
    if ImageBitmap<>nil then begin
      //draw image from bitmap
      if imageX+ImageBitmap.Width<=rec.Right then
        F_TreeListView.Canvas.Draw(imageX,rec.top,ImageBitmap)
       else if imageX<rec.right then //clip horizontal if the column is too small (don't clip vertically, since the row height is fixed and the images can be fit to it)
        F_TreeListView.Canvas.CopyRect(rect(imageX,rec.top,rec.Right,rec.top+ImageBitmap.Height),ImageBitmap.Canvas,rect(0,0,rec.right-imageX,ImageBitmap.Height));
      F_TreeListView.drawTextRect(Text,textStartX+ImageBitmap.Width+LEFT_TEXT_PADDING,taLeftJustify, rec, drawSearchMark);
    end else if (F_TreeListView.Images<>nil) and (ImageIndex>-1) then begin
      //draw image from imagelist
      if imageX+F_TreeListView.Images.Width<=rec.Right then
         F_TreeListView.Images.draw(F_TreeListView.Canvas,imageX,rec.Top,ImageIndex) //just draw fully
       else if imageX<rec.right then begin
         {$ifdef useRealClipping}
         oldClippingRect:=F_TreeListView.Canvas.ClipRect;
         oldClipping:=F_TreeListView.Canvas.Clipping;
         F_TreeListView.Canvas.Clipping:=true;
         F_TreeListView.Canvas.ClipRect:=rect(rec.left,0,rec.Right, rec.top+F_TreeListView.Images.Height); //only horizontal clipping
         F_TreeListView.Images.draw(F_TreeListView.Canvas,imageX,rec.Top,ImageIndex);
         F_TreeListView.Canvas.ClipRect:=oldClippingRect;
         F_TreeListView.Canvas.Clipping:=oldClipping;
         {$else}
         tempBitmap:=graphics.TBitmap.create;
         try
           F_TreeListView.Images.GetBitmap(ImageIndex,tempBitmap);
           F_TreeListView.Canvas.CopyRect(rect(imageX,rec.top,rec.Right,rec.top+tempBitmap.Height),tempBitmap.Canvas,rect(0,0,rec.right-imageX,tempBitmap.Height));
         finally
           tempBitmap.free;
         end;
         {$endif}
       end;
      TreeListView.drawTextRect(Text,textStartX+F_TreeListView.Images.Width+LEFT_TEXT_PADDING,taLeftJustify,rec,drawSearchMark);
    end else
      //draw text only
      TreeListView.drawTextRect(Text,textStartX,taLeftJustify,rec,drawSearchMark);
  end;
  //draw the tree lines and collapse/expand buttons
  procedure drawTreeColumn;
  var i,tempX:longint;
      tempColor:TColor;
  begin
    tempX:=rec.left;
    if F_TreeListView.RootLineMode <> lmNone then begin
      tempX:=tempX+LINE_DISTANCE div 2+1;
      //draw vertical item lines (not ending items)
      for i:=0 to hierarchyStack.size-1 do begin
        if tempx>rec.right then break;
        if hierarchyStack.stack[i].index<>hierarchyStack.stack[i].list.Count-1 then //last item on this level
          if F_TreeListView.RootLineMode=lmDot then
            F_TreeListView.DrawAlignDotLine(tempX,yold,tempX,ynew-1,F_TreeListView.F_RootLineColor)
           else with F_TreeListView.canvas do begin
             pen.color:=F_TreeListView.F_RootLineColor;
             pen.Style:=psSolid;
             MoveTo(tempX,yold);
             LineTo(tempX,ynew);
           end;
        tempX:=tempX+LINE_DISTANCE;
      end;

      tempX:=tempX-LINE_DISTANCE div 2-3;

      //draw vertical item lines (ending items)
      case F_TreeListView.RootLineMode of
        lmDot:  begin
          F_TreeListView.DrawAlignDotLine(tempX-5,yold+F_TreeListView.RowHeight div 2,min(tempX,rec.right),yold + F_TreeListView.RowHeight div 2,F_TreeListView.F_RootLineColor);
          F_TreeListView.DrawAlignDotLine(tempX-5,yold,tempX-5,yold+F_TreeListView.RowHeight div 2,F_TreeListView.F_RootLineColor);
        end;
        lmSolid: with F_TreeListView.canvas do begin
           pen.color:=F_TreeListView.F_RootLineColor;
           pen.Style:=psSolid;
           MoveTo(tempX-5,yold+F_TreeListView.RowHeight div 2);
           LineTo(min(tempX,rec.right),yold + F_TreeListView.RowHeight div 2);
             MoveTo(tempX-5,yold);
             LineTo(tempX-5,yold+F_TreeListView.RowHeight div 2);
         end;
      end;
    end;

    if not defaultDraw then exit;

    tempX:=GetExtendingButtonPos;
    if (yold>F_TreeListView.F_VScroll.Top)
       and(tempX+9<=rec.right)
       and(SubItems.Count>0)  then begin
      tempColor:=F_TreeListView.Canvas.brush.Color;
      F_TreeListView.Canvas.pen.Style:=psSolid;
      F_TreeListView.Canvas.pen.Color:=clBlack;
      F_TreeListView.Canvas.brush.Style:=bsSolid;
      F_TreeListView.Canvas.brush.Color:=F_TreeListView.ButtonColor;
      //draw expanding/de button
      F_TreeListView.canvas.Rectangle(tempX,yold+F_TreeListView.RowHeight div 2-5,tempX+9,yold+F_TreeListView.RowHeight div 2+4);
      F_TreeListView.canvas.moveTo(tempX+2,yold+F_TreeListView.RowHeight div 2-1); //draw ---
      F_TreeListView.canvas.LineTo(tempX+7,yold+F_TreeListView.RowHeight div 2-1);
      if not Expanded then begin                                   //draw  |   also -|-
        F_TreeListView.canvas.moveTo(tempX+4,yold+F_TreeListView.RowHeight div 2-3);
        F_TreeListView.canvas.LineTo(tempX+4,yold+F_TreeListView.RowHeight div 2+2);
      end;
      F_TreeListView.Canvas.brush.Color:=tempColor;
    end;
  end;

begin
  //item bg color/font
  if SeemsSelected then begin
    F_TreeListView.Canvas.Brush.color:=F_TreeListView.SelectBackColor;
    F_TreeListView.Canvas.Brush.style:=bsSolid;
  end else if tlvoStriped in F_TreeListView.F_Options then begin
    F_TreeListView.canvas.Brush.Style:=bsSolid;
    if F_TreeListView.F_DrawingEvenItem then F_TreeListView.canvas.Brush.Color:=F_TreeListView.StripedEvenColor
    else F_TreeListView.canvas.Brush.Color:=F_TreeListView.StripedOddColor;
  end else F_TreeListView.Canvas.Brush.style:=bsClear;
  defaultDraw:=F_TreeListView.DoCustomItemDrawEvent(cdetPrePaint,self); //event handling
  yold:=F_TreeListView.DrawingYPos;
  ynew:=yold+F_TreeListView.RowHeight;
  if defaultDraw then
    if ynew>F_TreeListView.F_VScroll.Top then begin
      //clearing
      F_TreeListView.Canvas.FillRect(rect(0,yold,F_TreeListView.doubleBuffer.width,ynew));

      rec.top:=yold;
      rec.Bottom:=ynew;

      //draw text
      for i:=0 to F_TreeListView.F_Header.Sections.Count-1 do begin
        {$IFDEF allowHeaderDragging}
          recordId:=F_TreeListView.F_Header.Sections[i].OriginalIndex;
        {$ELSE}
          recordId:=i;
        {$ENDIF}
        if recordId>RecordItems.Count-1 then continue;
        rec.left:=F_TreeListView.F_Header.Sections[i].Left-F_TreeListView.F_HScroll.Position;
        rec.right:=rec.left+F_TreeListView.F_Header.Sections[i].Width;
        if assigned(F_TreeListView.F_CustomRecordItemPositioningEvent) then begin
          rec.top:=yold;
          rec.Bottom:=ynew;
          F_TreeListView.F_CustomRecordItemPositioningEvent(F_TreeListView, i, RecordItems[recordId], rec);
        end;
        RecordItems[recordId].selectFont(F_TreeListView.Canvas);
        if F_TreeListView.DoCustomRecordItemDrawEvent(cdetPrePaint,RecordItems[recordId],rec) then begin
          if recordId=0 then begin
            drawTreeColumnText;
            drawTreeColumn;
          end else
            F_TreeListView.drawTextRect(RecordItems[recordId].Text,0,F_TreeListView.F_Header.Sections[i].Alignment,rec,TreeListView.f_searchMarkVisible and (self=TreeListView.f_searchMarkItem) and (TreeListView.f_searchMarkCol=recordId));
          if not F_TreeListView.DoCustomRecordItemDrawEvent(cdetPostPaint,RecordItems[recordId],rec) then
            break;
        end;
      end;

      //draw focus rect
      if F_TreeListView.focused = self then
        DrawFocusRect( F_TreeListView.Canvas.Handle,rect(0,yold,F_TreeListView.F_VScroll.Left,ynew));

      //draw horizontal separation lines
      case F_TreeListView.HorizontalLineMode of
        lmSolid: with F_TreeListView.canvas do begin
                   pen.color:=F_TreeListView.F_HorizontalLineColor;
                   pen.Style:=psSolid;
                   MoveTo(0,ynew-1);
                   LineTo(F_TreeListView.doubleBuffer.Width,ynew-1);
                 end;
        lmDot:   F_TreeListView.DrawAlignDotLine(0,ynew-1,F_TreeListView.doubleBuffer.Width,ynew-1,F_TreeListView.F_HorizontalLineColor);
      end;
    end;

  F_TreeListView.DoCustomItemDrawEvent(cdetPostPaint,self) //event handling
end;

//Destroy
destructor TTreeListItem.Destroy;
begin
  if self=TreeListView.focused then TreeListView.focused:=nil;
  if self=TreeListView.F_TopItem then TreeListView.F_TopItem:=nil;
  if Selected then dec(TreeListView.f_selCount);
  F_RecordItems.onListEvent := nil;
  F_RecordItems.free;
  F_RecordItems.onListEvent := nil;
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
{$ifdef fpc}var temp:tbitmap;{$endif}
begin
  inherited;
  F_Items:=TTreeListItems.Create(nil,self);
  F_InvalidatedItems:=tlist.create;
  f_invalidateAll:=true;

  F_Options:=[tlvoToolTips,tlvoRightMouseSelects,tlvoStriped];
  ControlStyle:=ControlStyle+[csClickEvents,csFramed,csOpaque,csDoubleClicks];

  doubleBuffer:=graphics.TBitmap.Create;
  //didn't change anything: DoubleBuffered:=false; //we always use our own double buffer


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
  F_Header.Visible:=true;
  F_HeaderVisible:=true;
  with F_Header.Sections.Add do begin
    Caption:='';
    Width:=1000;
  end;
  F_Header.Left:=0;
  F_Header.Width:=9999; //this was 10000, however 10000 will crash Lazarus in design mode
  {$ifdef fpc}
  if font.Height=0 then begin
    temp:=tbitmap.create;
    temp.canvas.font.Assign(font);
    temp.canvas.TextOut(0,0,'a');
    F_Header.Height:=temp.canvas.TextHeight('ABC,')+2*GetSystemMetrics(SM_CYEDGE);
    temp.free;

    //F_Header.Height:=13+2*GetSystemMetrics(SM_CYEDGE)
   end else
    F_Header.Height:=abs(font.height)+2*GetSystemMetrics(SM_CYEDGE);
  {$endif}
  F_Header.OnSectionTrack:=_HeaderSectionTrack;
  F_Header.OnSectionResize:=_HeaderSectionResize;
  F_Header.OnSectionClick:=_HeaderSectionClick;
  {$ifdef allowHeaderDragging}F_Header.OnSectionSeparatorDblClick:=_HeaderSectionDblClick();
  F_Header.OnSectionEndDrag:=_HeaderSectionEndDrag;{$endif}
  F_Header.parent:=Self;

  F_ScrollStyle:=ssBoth;

  //Scrollbar initialisieren
  F_VScroll:=TScrollbar.Create(self);
  F_VScroll.Enabled:=false;
  F_VScroll.Visible:=true;
  F_VScroll.Kind:=sbVertical;
  F_VScroll.OnChange:=_VScrollChange;
  F_VScroll.TabStop:=false;
  F_VScroll.parent:=self;


  //Scrollbar initialisieren
  F_HScroll:=TScrollbar.Create(self);
  F_HScroll.Enabled:=false;
  F_HScroll.Visible:=true;
  F_HScroll.Kind:=sbHorizontal;
  F_HScroll.Left:=0;
  F_HScroll.SmallChange:=5;
  F_HScroll.OnChange:=_HScrollChange;
  F_HScroll.TabStop:=false;
  F_HScroll.parent:=self;

  RowHeight:=F_Header.Height-2*GetSystemMetrics(SM_CYEDGE);
  if font.Height>RowHeight then RowHeight:=font.Height+1;
  //if font.GetTextHeight('ÂB,')>RowHeight then RowHeight:=font.GetTextHeight('ÂB,')+1;

  if csDesigning in ComponentState then begin
    BeginUpdate;
    with items.Add('Constant example tree') do begin
      SubItems.Add('1').SubItems.add('1.1');
      SubItems.Add('2').RecordItemsText[1]:='record item';
      SubItems.Add('3');
    end;
    items.Add('More...').SubItems.Add('More...').SubItems.Add('More...');
    EndUpdate;
  end;

  {$ifdef android}
  F_PostMessageTimer := TTimer.Create(self);
  F_PostMessageTimer.Interval:=25;
  F_PostMessageTimer.Enabled:=false;
  F_PostMessageTimer.OnTimer:=PostMessageTimerTimer;

  F_ScrollStyle := ssNone;
  F_VScroll.Visible := false;
  F_HScroll.Visible := false;
  F_HeaderVisible := false;
  F_Header.Visible := false;
  F_Options := F_Options + [tlvoDragScrolling];
  {$endif}
end;


procedure TTreeListView.loaded;
begin
  inherited loaded;
  UpdateScrollBarPos;

end;

procedure TTreeListView.SetFocused(const AValue: TTreeListItem);
begin
  if AValue=F_Focused then exit;
  if tlioDeleting in InternOptions_tlio then exit;
  invalidateItem(F_Focused);
  invalidateItem(AValue);
  F_Focused:=AValue;
  DoSelect(F_Focused);
  if focused<>nil then
    ensureVisibility(focused);
  if f_searchMarkVisible then begin
    f_searchMarkVisible:=false;
    invalidateAll();
  end;
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

procedure TTreeListView.setTopItem(item: TTreeListItem);
begin
//todo
end;

function TTreeListView.GetTopItem: TTreeListItem;
begin
  if (F_TopItem=nil) then begin
    //y*RowHeight+GetTopPos>-RowHeight;
    F_TopItem:=Items.GetItemWithRealIndex(F_VScroll.Position,[]);
    if tlvoStripInvisibleItems in f_options then
      F_TopItemEven:=Items.RealIndexOf(F_TopItem,[ricCountCollapsedsubItems]) mod 2=0;
  end;
  result:=F_TopItem;
end;

function TTreeListView.GetTopItemEven: boolean;
begin
  if tlvoStripInvisibleItems in f_options then begin
    GetTopItem; //updates the variable in the line below
    result:=F_TopItemEven;
  end else result:=F_VScroll.Position mod 2 = 0;
end;

procedure TTreeListView.SetSortColumn(const AValue: longint);
begin
  if SortColumn=AValue then exit;
  F_SortColumn:=AValue;
  F_SortColumnInverted:=false;
  if tlvoSorted in F_Options then sort;
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
  internRepaint;
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
  sheduleInternRepaint;
end;

procedure TTreeListView.SearchBarClose(Sender: TObject);
begin
  UpdateScrollBarPos;
  UpdateScrollSizeV;
  SetFocus;
end;

procedure TTreeListView.SetOptions(const AValue: TTreeListViewOptions);
var toAdd,toRemove: TTreeListViewOptions;
    needRepaint: boolean;
begin
  if F_Options=AValue then exit;
  toRemove:=F_Options-AValue;
  toAdd:=AValue-F_Options;
  F_Options:=AValue;
  needRepaint:=false;

  if tlvoMultiSelect in toRemove then Selected:=focused;

  //do nothing with tlvoToolTips, tlvoRightMouseSelects

  if (tlvoHotTrackRecordTextItems in toRemove) and (hotTrackedRecordItem<>nil) then begin
    needRepaint:=true;
    hotTrackedRecordItem:=nil;
  end;

  if tlvoStripInvisibleItems in toRemove then needRepaint:=true
  else if tlvoStripInvisibleItems in toAdd then begin
    needRepaint:=true;
    F_TopItem:=nil;GetTopItem; //recalc top item
  end;

  if not needRepaint then
    needRepaint:=tlvoStriped in (toRemove+toAdd);

  {$ifdef allowHeaderDragging}
  if tlvoColumnsDragable in toRemove then F_Header.DragReorder:=false
  else if tlvoColumnsDragable in toAdd then F_Header.DragReorder:=true;
  {$endif}

  if tlvoSorted in toAdd then sort;

  if needRepaint then internRepaint;
end;

procedure TTreeListView.SetOption(const Option: TTreeListViewOption;
  const active: boolean);
begin
  if active then SetOptions(F_Options+[Option])
  else SetOptions(F_Options-[Option]);
end;

procedure TTreeListView.SearchBarShow(Sender: TObject);
begin
  UpdateScrollBarPos;
  UpdateScrollSizeV;
  F_SearchBar.SetFocus;
end;


procedure TTreeListView.SetBgColor(const AValue: TColor);
begin
  if F_BgColor=AValue then exit;
  F_BgColor:=AValue;
  sheduleInternRepaint();
end;

function TTreeListView.GetTopItemVisualIndex: integer;
begin
  result := F_VScroll.Position;
end;

procedure TTreeListView.SetButtonColor(const AValue: TColor);
begin
  if F_ButtonColor=AValue then exit;
  F_ButtonColor:=AValue;
  sheduleInternRepaint();
end;

procedure TTreeListView.SetColorSearchMark(const AValue: tcolor);
begin
  if f_colorSearchMark=AValue then exit;
  f_colorSearchMark:=AValue;
  sheduleInternRepaint();
end;

procedure TTreeListView.SetColorSearchMarkField(const AValue: tcolor);
begin
  if f_colorSearchMarkField=AValue then exit;
  f_colorSearchMarkField:=AValue;
  sheduleInternRepaint();
end;

procedure TTreeListView.SetExpandMode(const AValue: TExpandMode);
begin
  if F_ExpandMode=AValue then exit;
  F_ExpandMode:=AValue;
  sheduleInternRepaint();
end;

procedure TTreeListView.SetHeaderVisible(AValue: boolean);
begin
  if F_HeaderVisible=AValue then Exit;
  F_HeaderVisible := AValue;
  F_Header.Visible := F_HeaderVisible;
  UpdateScrollBarPos;
  UpdateScrollSize;
  sheduleInternRepaint();
end;

procedure TTreeListView.SetHorizontalLineColor(const AValue: TColor);
begin
  if F_HorizontalLineColor=AValue then exit;
  F_HorizontalLineColor:=AValue;
  sheduleInternRepaint();
end;

procedure TTreeListView.SetHorizontalLines(const AValue: TLineMode);
begin
  if F_HorizontalLines=AValue then exit;
  F_HorizontalLines:=AValue;
  sheduleInternRepaint();
end;

procedure TTreeListView.SetRootLineColor(const AValue: TColor);
begin
  if F_RootLineColor=AValue then exit;
  F_RootLineColor:=AValue;
  sheduleInternRepaint();
end;

procedure TTreeListView.SetRootLines(const AValue: TLineMode);
begin
  if F_RootLines=AValue then exit;
  F_RootLines:=AValue;
  sheduleInternRepaint();
end;

procedure TTreeListView.SetScrollStyle(AValue: TScrollStyle);
begin
  if F_ScrollStyle=AValue then Exit;
  F_ScrollStyle:=AValue;
  UpdateScrollBarPos;
  UpdateScrollSize;
  sheduleInternRepaint();
end;

procedure TTreeListView.SetSelectBackColor(const AValue: TColor);
begin
  if F_SelectBackColor=AValue then exit;
  F_SelectBackColor:=AValue;
  sheduleInternRepaint();
end;

procedure TTreeListView.SetStripedEvenColor(const AValue: TColor);
begin
  if F_StripedEvenColor=AValue then exit;
  F_StripedEvenColor:=AValue;
  sheduleInternRepaint();
end;

procedure TTreeListView.SetStripedOddColor(const AValue: TColor);
begin
  if F_StripedOddColor=AValue then exit;
  F_StripedOddColor:=AValue;
  sheduleInternRepaint();
end;

procedure TTreeListView.SetVerticalLineColor(const AValue: TColor);
begin
  if F_VerticalLineColor=AValue then exit;
  F_VerticalLineColor:=AValue;
  sheduleInternRepaint();
end;

procedure TTreeListView.SetVerticalLines(const AValue: TLineMode);
begin
  if F_VerticalLines=AValue then exit;
  F_VerticalLines:=AValue;
  sheduleInternRepaint();
end;

procedure TTreeListView.SearchBarSearch(sender: TObject; incremental,
  backwards: boolean);
var searchLocations: TStrings;
    searchOptions: cardinal;
begin
  if sender<>F_SearchBar then exit;
  if F_SearchBar.SearchLocation<0 then F_SearchBar.SearchLocation:=0;
  searchLocations:=F_SearchBar.SearchLocations;
  if F_SearchBar.SearchLocation>=searchLocations.count then
    exit;
  searchOptions:=cardinal(searchLocations.Objects[F_SearchBar.SearchLocation]);
  F_NewSearchBarFindState:=search(F_SearchBar.SearchText,searchOptions,backwards,incremental);
end;

function TTreeListView.DoCustomBackgroundDrawEvent (eventTyp_cdet:TCustomDrawEventTyp):boolean;
begin
  Result:=true;
  if assigned(F_CustomBgDraw) then F_CustomBgDraw(self,eventTyp_cdet,result);
end;
function TTreeListView.DoCustomItemDrawEvent(const eventTyp_cdet:TCustomDrawEventTyp;const item:TTreeListItem):boolean;
begin
  Result:=true;
  if assigned(F_CustomItemDraw) then F_CustomItemDraw(self,eventTyp_cdet,item,result);
end;
function TTreeListView.DoCustomRecordItemDrawEvent(const eventTyp_cdet:TCustomDrawEventTyp;const RecordItem:TTreeListRecordItem;const outrec: TRect):boolean;
begin
  Result:=true;
  if assigned(F_CustomRecordItemDraw) then begin
    F_DrawingRecordItemRect:=outrec;
    F_CustomRecordItemDraw(self,eventTyp_cdet,recordItem,result);
  end;
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
    list[i].SetSelections(list[i].Selected, false);
    removeMouseSelection(list[i].SubItems);
  end;
end;

procedure TTreeListView.setMouseSelection(list: TTreeListItems);
var i:longint;
begin
  BeginMultipleUpdate;
  for i:=0 to list.count-1 do begin
    list[i].SetSelections(list[i].Selected xor list[i].MouseSelected,false);
    setMouseSelection(list[i].SubItems);
  end;
  EndMultipleUpdate;
end;


procedure TTreeListView.DoSelect(item: TTreeListItem);
begin
  if assigned(F_OnSelect) and (item<>nil) then
    F_OnSelect(self,item);
end;

//Kommunikationsroutinen (Set- und Getfunktionen)
procedure TTreeListView.SetItems(const value:TTreeListItems);
begin
  F_Items.Assign(value);
end;

function TTreeListView.GetTopPos:integer;
begin
  result:=HeaderItemDistance+F_VScroll.Top-F_VScroll.Position*RowHeight;
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
  updateAll;
  if tlvoSorted in F_Options then sort;
end;

function TTreeListView.VisibleRowCount:longint;
begin
  if RowHeight=0 then result:=0
  else  result:=RealClientHeight div RowHeight;
end;

procedure TTreeListView.sort;
begin
  BeginMultipleUpdate;
  Items.Sort(CompareItems);
  invalidateAll();
  EndMultipleUpdate;
  if assigned(F_OnItemsSorted) then F_OnItemsSorted(self);
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
var startItem: TTreeListItem;
    found: TTreeListRecordItem;
    loopAround: boolean;

    i,column,textPos: longint;
begin
 if  f_searchActivated then exit; //search did call Application.ProcessMessages, which could call search
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

  //only search existing/visible columns
  for i:=0 to 31 do
    if (ColumnFromOriginalIndex(i)=nil)
       {$ifdef allowHeaderVisible}or not ColumnFromOriginalIndex(i).Visible{$endif} then
      searchFields:=searchFields and not (1 shl i);

  if extendSelection then
    found:=items.find(searchFor,searchFields,backward,@loopAround, startItem, f_searchMarkCol,f_searchMarkStart)
   else
    found:=items.find(searchFor,searchFields,backward,@loopAround, startItem, 0, 0);

  Result:=[];
  if loopAround then include(Result,fsLoopAround);

  if found=nil then begin
    if f_searchMarkVisible or F_HighlightAll then begin
      sheduleInternRepaint;
      f_searchMarkVisible:=false;
    end;
  end else begin
    include(Result,fsFound);
    column:=found.Index;
    textPos:=pos(lowercase(searchFor),lowercase(found.text));

    //if (F_HighlightAll) or (f_searchMarkVisible and ((found.Parent<>f_searchMarkItem)or(f_searchMarkCol<>column)or(f_searchMarkStart<>textPos))) then
    //  sheduleInternRepaint(); //direct repaint, since the search mark will just be drawn over the normal rendering

    if selCount<=1 then selected:=found.F_Parent;
    f_searchMarkItem:=found.F_Parent;
    f_searchMarkCol:=column;
    f_searchMarkStart:=textPos;
    f_searchMarkLen:=length(searchFor);
    f_searchMarkVisible:=true;
    ensureVisibility(f_searchMarkItem,f_searchMarkCol);

    sheduleInternRepaint(); //no need to hurry (this will be faster than internRepaint if the user holds return pressed)

    {if column=0 then
      drawTextRect(found.Text, found.parent.GetExtraTextIndentation(f_searchMarkCol),
                   taLeftJustify,found.Parent.getBounds(f_searchMarkCol), true)
    else
      drawTextRect(found.Text, 0,
                   ColumnFromOriginalIndex(f_searchMarkCol).Alignment,found.parent.getBounds(f_searchMarkCol), true);}
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
    F_SearchBar.OnShow:=SearchBarShow;
  end;
  F_NewSearchBarFindState := SearchBar.FindState;
  F_SearchBar.SubComponents:=[fscCloseButton, fscCaption, fscSelectLocation,
                              fscSearchForward, fscSearchBackwards, fscHighlight, fscStatus];
  F_SearchBar.SearchLocations.Clear;
  F_SearchBar.SearchLocations.AddObject('all',tobject(-1));
  for i:=0 to Columns.Count-1 do
    F_SearchBar.SearchLocations.AddObject(ColumnFromOriginalIndex(i).Text,tobject(1 shl i));
  F_SearchBar.SearchLocation:=0;

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


function TTreeListView.RealControlHeight(c: Twincontrol): longint;
var r:TRect;
begin
  if not c.IsVisible then exit(0);
  {$ifdef android}
  exit(c.Height);
  {$endif}
  GetWindowRect(c.Handle,r);
  result:=r.bottom-r.top;
end;

function TTreeListView.RealBaseClientWidth: longint;
begin
  result := {$ifdef android}Width{$else}ClientWidth{$endif};
end;

function TTreeListView.RealBaseClientHeight: longint;
begin
  result := {$ifdef android}Height{$else}ClientHeight{$endif};
end;

function TTreeListView.RealClientHeight: longint;
begin
  result := RealBaseClientHeight - RealControlHeight(F_Header)-HeaderItemDistance;
  if F_HScroll.Visible then result := result - RealControlHeight(F_HScroll);
  if F_SearchBar <>nil then result := result - RealControlHeight(F_SearchBar);
end;

procedure TTreeListView.DrawAlignDotLine(x,y:integer;const x2,y2:integer;const color:TColor);
var F_HeaderHeight:integer;
begin
  F_HeaderHeight:=F_VScroll.Top;
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

 procedure drawTextDef(s: string);
 begin
   SetBkMode(canvas.Handle, TRANSPARENT);
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

  if (temp.Left >= temp.Right) or (temp.Right <= 0) then exit;

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

//incase-sensitive, intelligent string compare (splits in text, number parts)
function striicmp(s1,s2: string):longint;
var t1,t2:string; //lowercase text
    i,j,ib,jb,p: longint;
begin
  t1:=lowercase(s1);
  t2:=lowercase(s2);
  i:=1;
  j:=1;
  while (i<=length(t1)) and (j<=length(t2)) do begin
    if (t1[i] in ['0'..'9']) and (t2[j] in ['0'..'9']) then begin
      ib:=i;
      jb:=j;
      while (t1[i] in ['0'..'9']) and (i<=length(t1)) do inc(i);
      while (t2[j] in ['0'..'9']) and (j<=length(t2)) do inc(j);
      if i-ib<j-jb then begin
        result:=-1; //find longer number
        exit;
      end;
      if i-ib>j-jb then begin
        result:=1;
        exit;
      end;
      for p:=0 to i-ib-1 do //numerical == lexical
        if t1[ib+p]<t2[jb+p] then begin
          result:=-1;
          exit;
        end else if t1[ib+p]>t2[jb+p] then begin
          result:=1;
          exit;
        end;
    end else begin
      if t1[i]<t2[j] then begin
        result:=-1;
        exit;
      end;
      if t1[i]>t2[j] then begin
        result:=1;
        exit;
      end;
      inc(i);
      inc(j);
    end;
  end;
  if length(t1)<length(t2) then begin
    result:=-1;
    exit;
  end;
  if length(t1)>length(t2) then begin
    result:=1;
    exit;
  end;
  result:=0;
end;

function TTreeListView.CompareItems(i1, i2: TTreeListItem): longint;
begin
  Result:=striicmp(i1.RecordItemsText[F_SortColumn],i2.RecordItemsText[F_SortColumn]);
  if assigned(F_OnCompareItems) then F_OnCompareItems(self,i1,i2,result);
  if F_SortColumnInverted then result:=-result;
end;

procedure TTreeListView.BeginMultipleUpdate;
begin
  inc(f_RedrawBlock);
end;

procedure TTreeListView.EndMultipleUpdate;
begin
  dec(f_RedrawBlock);
  if f_RedrawBlock=0 then updateAll;
end;

procedure TTreeListView._SubItemListEvent(list: TObjectList; typ: TListEventTyp);
begin
  case typ of
    levBeginEdit: BeginMultipleUpdate;
    levEndEdit: EndMultipleUpdate;
    else sheduleInternRepaint();
  end;
end;

procedure TTreeListView._RecordItemListEvent(list: TObjectList;
  typ: TListEventTyp);
begin
  case typ of
    levBeginEdit,levEndEdit: {ignore};
    else begin
      invalidateItem(TRecordItemList(list).Owner);
      internPaint;
    end;
  end;
end;

//Interne Kommunikation mit Unterkomponenten
procedure TTreeListView.updateAll();
begin
  if not HandleAllocated then exit;
  UpdateScrollBarPos;
  UpdateScrollSize;
  internPaint;
end;

procedure TTreeListView._HeaderSectionTrack(HeaderControl: TEventHeaderControl; Section: THeaderSection; Width: Integer; State: TSectionTrackState);
begin
  // UpdateScrollSizeH; disabled because this can be slow on Linux (or better this event is called a dozen times more than necessary). ...resize is called after the tracked ended anyways
  if assigned(F_HeaderSectionTrack) then F_HeaderSectionTrack(HeaderControl,Section,Width,State);
  sheduleInternRepaint;
end;
procedure TTreeListView._HeaderSectionResize(HeaderControl: TEventHeaderControl; Section: THeaderSection);
begin
  UpdateScrollSizeH;
  if assigned(F_HeaderSectionResize) then F_HeaderSectionResize(HeaderControl,Section);
  sheduleInternRepaint;
end;

procedure TTreeListView._HeaderSectionClick(HeaderControl: TEventHeaderControl; Section: THeaderSection);
var NewSortColumn,i:Longint;
    cursor: TPoint;
begin
  if not (tlvoSorted in F_Options) then exit;
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
    {$ifdef allowHeaderDragging}
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
  //UpdateScrollBarPos; no reason to call this at all, we didn't scroll
  //UpdateScrollSizeH; not really necessary since total size remained the same
  sheduleInternRepaint;
end;

procedure TTreeListView._HScrollChange(Sender: TObject);
begin
  //GTK2 send a high amount an scroll change events, so lets collect them
  //and draw the changes later
  //It would be possible to repaint the treelistview here, but if we do that
  //the scrollbar itself isn't updated by gtk
  //We can't use sheduleInternRepaint, because the header position has to be
  //updated with the correct scroll position, and moving a control will erase
  //the background
  if F_SheduledHScroll=0 then begin
    F_SheduledHScroll:=GetTickCount;
    internPostMessage(LM_USER_SHEDULED_EVENT,EVENT_HSCROLL);
  end else if F_SheduledHScroll+20<GetTickCount then begin
    //force repaint with at least 50 fps
    SendMessage(Handle,LM_USER_SHEDULED_EVENT,EVENT_HSCROLL,0);
  end;
end;

procedure TTreeListView._VScrollChange(Sender: TObject);
begin
  UpdateScrollBarPos;
  hotTrackedRecordItem:=nil;
  F_TopItem:=nil;
//  UpdateScrollSize;
  sheduleInternRepaint;
  if assigned(F_VScrollBarChange) then F_VScrollBarChange(F_VScroll);
end;

procedure TTreeListView.UpdateScrollBarPos;
var realHeight: longint;
begin
  if (tlioDeleting in InternOptions_tlio) or
     (tlioUpdating in InternOptions_tlio) or
     (f_RedrawBlock>0) then exit;

  F_HScroll.Visible := (F_ScrollStyle in [ssHorizontal, ssBoth])
                       or ((F_ScrollStyle in [ssAutoHorizontal, ssAutoBoth]) and F_HScroll.Enabled);
  F_VScroll.Visible := (F_ScrollStyle in [ssVertical, ssBoth])
                       or ((F_ScrollStyle in [ssAutoVertical, ssAutoBoth]) and F_VScroll.Enabled);

  RealHeight := RealBaseClientHeight;
  if F_SearchBar<>nil then if F_SearchBar.Visible then
    realheight:=realheight - F_SearchBar.Height;
  if F_HScroll.Visible then realheight := realheight - F_HScroll.Height;

  if F_VScroll.Visible then F_VScroll.Left:=RealBaseClientWidth-F_VScroll.Width
  else F_VScroll.Left := RealBaseClientWidth;
  if F_HeaderVisible then F_VScroll.Top:=F_Header.Height
  else F_VScroll.Top:=0;
  F_VScroll.Height:=max(1, realHeight - F_VScroll.top);

  F_HScroll.Top:=realHeight;
  F_HScroll.Width:=Max(1, F_VScroll.Left);

  if F_Header.left<>-F_HScroll.Position then
    F_Header.left:=-F_HScroll.Position;
end;

procedure TTreeListView.internPostMessage(Msg: Cardinal; WParam: WParam);
begin
  {$ifndef android}
  PostMessage(Handle, Msg, WParam, 0);
  {$else}
  F_PostMessage.Msg := Msg;
  F_PostMessage.WParam := WParam;
  F_PostMessage.LParam := 0;
  F_PostMessageTimer.Enabled := true;
  {$endif}
end;

{$ifdef android}
procedure TTreeListView.PostMessageTimerTimer(Sender: TObject);
var temp: TLMessage;
begin
  if F_PostMessage.Msg <> 0 then begin
    WndProc(F_PostMessage);
    F_PostMessage.Msg:=0;
    F_PostMessageTimer.Enabled:=false;
  end;
end;
{$endif}

procedure TTreeListView.UpdateScrollSizeH;
var
  i,j: Integer;
begin
  if (tlioDeleting in InternOptions_tlio) or
     (tlioUpdating in InternOptions_tlio) or
     (f_RedrawBlock>0) then exit;
  j:=0;
  for i:=0 to F_Header.Sections.Count-1 do begin
    j:=j+F_Header.Sections[i].Width;
  end;
  if j>=F_HScroll.width then begin
    F_HScroll.Enabled:=true;
    F_HScroll.max:=j;
    //F_HScroll.PageSize / F_HScroll.max = F_HScroll.width / j
    F_HScroll.PageSize:=F_HScroll.max*F_HScroll.width div j;
    F_HScroll.LargeChange:=F_HScroll.width;
  end else if F_HScroll.Enabled then begin
    F_HScroll.Enabled:=true; //s.a.
    F_HScroll.Position:=0;
    F_HScroll.Enabled:=false;
  end;
  if F_HScroll.Enabled <> F_HScroll.Visible then UpdateScrollBarPos;
end;

procedure TTreeListView.UpdateScrollSizeV;
var i:integer;
begin
  if (tlioDeleting in InternOptions_tlio) or
     (tlioUpdating in InternOptions_tlio) or
     (f_RedrawBlock>0) then exit;

  i:=Items.GetRealItemCount([])-VisibleRowCount; //Anzahl der nicht anzeigbaren Items
  if i-1>F_VScroll.Min then begin
    //F_VScroll.Enabled:=false;
    F_VScroll.Enabled:=true;
    F_VScroll.Max:=i-1+VisibleRowCount;
    F_VScroll.PageSize:=VisibleRowCount;
    F_VScroll.LargeChange:=VisibleRowCount;
  end else if F_VScroll.Enabled then begin
    F_VScroll.Enabled:=true; //disabling doesn't always work directly??
    F_VScroll.Position:=0;
    F_VScroll.Enabled:=false;
  end;
  //F_VScroll.PageSize:=(ClientHeight-F_HScroll.Height*3-F_Header.Height) div (F_VScroll.Max - F_VScroll.Min + 1);
  if F_VScroll.Enabled <> F_VScroll.Visible then UpdateScrollBarPos;
end;

procedure TTreeListView.selectRange(a, b: TTreeListItem;mouseSelect:boolean=false);
var meetA, meetB: boolean;
  procedure setSelection(list: TTreeListItems);
  var i:longint;
  begin
    for i:=0 to list.count-1 do begin
      if meetB then begin
        if mouseSelect then list[i].MouseSelected:=false
        else list[i].Selected:=false
      end else if meetA or (list[i]=a) then begin
        meetA:=true;
        if mouseSelect then list[i].MouseSelected:=true
        else list[i].Selected:=true;
        if list[i]=b then meetB:=true;
      end else
        if mouseSelect then list[i].MouseSelected:=false
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
begin
  UpdateScrollSizeH;
  UpdateScrollSizeV;
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
          LM_LBUTTONDBLCLK = WM_LBUTTONDBLCLK;
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
         TLMLButtonUp = TWMLBUTTONUP;
         TLMLButtonDblClk = TWMLButtonDblClk;
         TLMKeyDown = TWMKeyDown;
         TLMKeyUp = TWMKeyUp;
  {$endif}

  //const LM_USER_SHEDULED_REPAINT = LM_USER+1126;

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
      internPostMessage(LM_USER_SHEDULED_EVENT,EVENT_MOUSE_SCROLL); //draw only after all wheel messages are processed
    end;
    LM_USER_SHEDULED_EVENT: begin
      case message.WParam of
        EVENT_REPAINT:
          if F_SheduledRepaint <> 0 then
            internRepaint;
        EVENT_MOUSE_SCROLL:
          if (F_MouseWheelDelta<=-120) or (F_MouseWheelDelta>=120) then begin
            F_VScroll.Position:=F_VScroll.Position-F_MouseWheelDelta div 120;
            F_MouseWheelDelta:=0;
          end;
        EVENT_HSCROLL: begin
          hotTrackedRecordItem:=nil;
          UpdateScrollBarPos;
        //  UpdateScrollSize;
          internRepaint;
          F_SheduledHScroll:=0;
          if assigned(F_HScrollBarChange) then F_HScrollBarChange(F_HScroll);
        end;
      end;
    end;
    LM_MOUSEMOVE: begin
      inherited;
      if (GetTickCount>F_LastMouseMove) and (GetTickCount-F_LastMouseMove<20) then exit;
      F_LastMouseMove:=GetTickCount;
      shiftState:=KeyDataToShiftState(TLMMouseMove(message).Keys);
      if (F_ClickedItem<>nil) and (tlvoMultiSelect in F_Options) and   // \/ left or right, not both
          ((MK_LBUTTON and message.wParam <> 0)<>(MK_RBUTTON and message.wParam <> 0))
          and ((MK_LBUTTON and message.wParam <> 0) or
               ((tlvoRightMouseSelects in F_Options) and (MK_RBUTTON and message.wParam <> 0))) then begin
        F_RealMousePos:=point(TLMMouseMove(message).XPos+F_HScroll.Position,
                              TLMMouseMove(message).YPos+F_VScroll.Position*RowHeight);
        if F_MouseSelecting=msNone then begin
          if sqr(F_RealClickPos.x-F_RealMousePos.x)+ sqr(F_RealClickPos.y-F_RealMousePos.y)>100 then
            if MK_RBUTTON and message.wParam <> 0 then F_MouseSelecting:=msRight
            else F_MouseSelecting:=msLeft;
          if (F_ClickedItem<>nil)  and (F_MouseSelecting<>msNone) and (ssCtrl in shiftState) then
             F_ClickedItem.Selected := not F_ClickedItem.Selected;
        end;
        if F_MouseSelecting<>msNone then begin
          itemAtPos:=GetItemAtPos(TLMMouseMove(message).YPos);
          if itemAtPos<>nil then begin
            selectRange(F_ClickedItem,itemAtPos,(ssCtrl in shiftState));
            ensureVisibility(itemAtPos);
          end;
        end;
      end;


      if (TLMMouseMove(message).XPos<F_VScroll.Left) and (TLMMouseMove(message).YPos>F_VScroll.Top)
         and (TLMMouseMove(message).YPos<F_HScroll.Top) then
        tempRecordItem:=GetRecordItemAtPos(TLMMouseMove(message).XPos,TLMMouseMove(message).YPos)
       else
        tempRecordItem:=nil;
      if tempRecordItem<>nil then begin
        if tlvoToolTips in F_Options then
          if (ColumnFromOriginalIndex(tempRecordItem.Index)<>nil) and
            (tempRecordItem.GetNecessaryWidth(self)+10>ColumnFromOriginalIndex(tempRecordItem.Index).Width) then begin
            hint:=tempRecordItem.Text;
            ShowHint:=true;
          end else ShowHint:=false;
      end;
      IF tlvoHotTrackRecordTextItems in F_Options THEN
        if tempRecordItem<>hotTrackedRecordItem then begin
          if hotTrackedRecordItem<>nil then
            invalidateItem(hotTrackedRecordItem.Parent);
          hotTrackedRecordItem:=tempRecordItem;
          if hotTrackedRecordItem<>nil then begin
            invalidateItem(hotTrackedRecordItem.Parent);
            Cursor:=crHandPoint;
          end else Cursor:=crDefault;
          internPaint;
        end;
      if (tlvoDragScrolling in F_Options) {$ifndef android}and (MK_LBUTTON and message.wParam <> 0){$endif} then begin
         F_VScroll.Position := F_ScrollClickPos - (TLMMouseMove(message).YPos - RowHeight div 2 + F_ScrollClickPos*RowHeight - F_RealClickPos.y) div RowHeight
      end;
    end;
    LM_LBUTTONDOWN,LM_RBUTTONDOWN: begin
      if not (csDesigning in ComponentState) then
        SetFocus;
      if TLMLBUTTONDOWN(message).YPos<F_HScroll.top then begin
        shiftState:=KeyDataToShiftState(TLMLBUTTONDOWN(message).Keys);
        itemAtPos:=GetItemAtPos(TLMLBUTTONDOWN(message).YPos);
        if (message.msg=LM_LBUTTONDOWN) and (ExpandMode=emExpandByClick) then
          if (itemAtPos<>nil) and
             (TLMLBUTTONDOWN(message).XPos<itemAtPos.GetExtendingButtonPos+9) and
             (TLMLBUTTONDOWN(message).XPos>itemAtPos.GetExtendingButtonPos) then begin
            itemAtPos.Expanded:=not itemAtPos.Expanded;
            //todo: check: if itemAtPos=focused then internRepaint;
          end;;
        if (message.msg=LM_LBUTTONDOWN) or (tlvoRightMouseSelects in F_Options) then begin
          F_ClickedItem:=itemAtPos;
          F_ScrollClickPos := F_VScroll.Position;
          F_RealClickPos:=point(TLMLBUTTONDOWN(message).XPos+F_HScroll.Position,TLMLBUTTONDOWN(message).YPos+F_VScroll.Position*RowHeight);
          if itemAtPos <> nil then begin
            if (shiftState <> []) or not (tlvoDragScrolling in Options) then begin
              if (message.Msg = LM_LBUTTONDOWN) or not (itemAtPos.Selected) then
                nextToFocus:=itemAtPos;
              if (tlvoMultiSelect in F_Options) and (ssCtrl in shiftState) then
                nextToFocus.Selected:=not nextToFocus.Selected;
            end;
          end else if message.Msg = LM_LBUTTONDOWN then
              nextToFocus := nil;
          if (F_MouseSelecting<>msNone) and (nextToFocus = itemAtPos) then begin
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
      end;
      if message.msg=LM_LBUTTONUP then begin
        if (F_ClickedItem <> nil) and (F_ClickedItem = GetItemAtPos(TLMLButtonUp(message).YPos)) then begin
          if assigned(OnClickAtRecordItem) then begin
            tempRecordItem:=F_ClickedItem.GetRecordItemAtPos(self,TLMLButtonUp(message).XPos);
            if tempRecordItem<>nil then OnClickAtRecordItem(self,tempRecordItem);
          end;
          if assigned(OnClickAtItem) then OnClickAtItem(self,F_ClickedItem);
          if tlvoDragScrolling in Options then
            nextToFocus := F_ClickedItem;
        end;
      end {$ifdef openOwnPopupMenu} else if message.msg=LM_RBUTTONUP then begin
        GetCursorPos(cursorPos);
        if assigned(PopupMenu) then PopupMenu.PopUp(cursorPos.x,cursorPos.Y);

      end{$endif};
      inherited;
    end;
    LM_LBUTTONDBLCLK:begin
      if (TLMLButtonDblClk(message).YPos<F_HScroll.top) and (ExpandMode=emExpandByDoubleClick) then begin
        itemAtPos:=GetItemAtPos(TLMLButtonDblClk(message).YPos);
        if (itemAtPos<>nil) and
           (TLMLButtonDblClk(message).XPos<itemAtPos.GetExtendingButtonPos+9) and
           (TLMLButtonDblClk(message).XPos>itemAtPos.GetExtendingButtonPos) then begin
            itemAtPos.Expanded:=not itemAtPos.Expanded;
            //todo: check: if itemAtPos=focused then internRepaint;
          end;
      end;
      inherited;
    end;
    LM_KEYDOWN: begin
      shiftState:=KeyDataToShiftState(TLMKeyDown(message).KeyData);
      case TLMKeyDown(message).CharCode of
        VK_UP: begin
          if focused=nil then nextToFocus:=Items[0]
          else nextToFocus:=focused.GetPrevVisibleItem;
          message.Result:=1;
        end;
        VK_DOWN: begin
          if focused<>nil then nextToFocus:=focused.GetNextVisibleItem()
          else if items.count>0 then nextToFocus:=Items[0];//items.count-1];
          message.Result:=1;
        end;

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

        VK_RIGHT: begin
          if focused<>nil then begin
            if not focused.Expanded then focused.Expand;
            if focused.SubItems.Count>0 then nextToFocus:=focused.SubItems[0]
            else nextToFocus:=focused;
          end;
          message.Result:=1;
        end;

        VK_LEFT: begin
          if focused<>nil then begin
            if (focused.Expanded) and (focused.SubItems.Count>0) then focused.Collapse
            else if focused.Parent<>nil then nextToFocus:=focused.Parent;
            if nextToFocus=nil then nextToFocus:=focused;
          end;
          message.Result:=1; //keep focus
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
                      internPaint(true);
                      inherited;
                    end;
    LM_ERASEBKGND: message.Result:=1;
    LM_KEYUP:
      if (TLMKeyUp(message).CharCode = VK_APPS) and assigned(PopupMenu) then begin
        GetCursorPos(cursorPos);
        PopupMenu.PopUp(cursorPos.X,cursorPos.y);
      end else if (TLMKeyUp(message).CharCode = ord('F')) and (F_SearchBar<>nil) then begin
        shiftState:=KeyDataToShiftState(TLMKeyDown(message).KeyData);
        if ssCtrl in shiftState then
          F_SearchBar.Show
        else inherited;
      end else if (TLMKeyUp(message).CharCode = VK_ESCAPE) and (F_SearchBar<>nil) then
        F_SearchBar.Hide
      else inherited;
    else inherited;
  end;
  if nextToFocus<>nil then begin //select or focus
    if not (tlvoMultiSelect in F_Options) or (shiftState=[]) then begin
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

procedure TTreeListView.sheduleInternRepaint();
begin
  //the main reason for this deferred paint is that the lcl can be really
  //slow on gtk 2 (#13363), but it probably also improves the performance
  //on other widgetsets
  if not HandleAllocated then
     exit; //no handle to post sheduling message to (and painting makes no sense anyways if we aren't visible)
  if F_SheduledRepaint=0 then begin
    F_SheduledRepaint:=GetTickCount;
    internPostMessage(LM_USER_SHEDULED_EVENT,EVENT_REPAINT);
  end else if F_SheduledRepaint+20<GetTickCount then begin
    //force repaint with at least 50 fps
    internRepaint();
  end;
end;

procedure TTreeListView.internRepaint();
begin
  invalidateAll();
  F_SheduledRepaint:=0;
  internPaint;
end;

procedure TTreeListView.invalidateItem(item: TTreeListItem);
begin
  if f_invalidateAll then exit;
  if tlvoAlwaysFullRepaint in F_Options then
    f_invalidateAll:=true
   else if f_invalidatedItems.IndexOf(item)<0 then begin
    f_invalidatedItems.Add(item);
    if f_invalidatedItems.Count>height div RowHeight then begin
      f_invalidateAll:=true;
      f_invalidatedItems.Clear;
    end;
  end;
end;

procedure TTreeListView.invalidateAll();
begin
  f_invalidateAll:=true;
  f_TopItem:=nil; //invalidate top item
  f_invalidatedItems.Clear;
end;


procedure TTreeListView.internDraw();
var RealCanvasHandle:thandle;
  procedure switchDoubleBuffer(paintToBuffer: boolean);
  begin
    if paintToBuffer then begin
      //canvas.Lock;
      //doubleBuffer.Canvas.Lock;
      RealCanvasHandle:=canvas.handle;
      canvas.handle:=doubleBuffer.Canvas.Handle;
    end else begin
      canvas.Handle:=RealCanvasHandle;
      //doubleBuffer.Canvas.Unlock;
      //canvas.Unlock;
    end;
  end;
var i,xpos:longint;
    defaultDraw:boolean;
    curItem: TTreeListItem;
    stack: TItemHierarchyStack;
begin
  f_bufferComplete:= f_invalidateAll;
//  if f_invalidateAll then Beep;
  switchDoubleBuffer(true);
  try
    {$IFDEF allowHeaderDragging}
      for i:=0 to F_Header.Sections.Count-1 do
        if F_Header.Sections[i].OriginalIndex=0 then begin
          F_TreeSectionPos:=rect(F_Header.Sections[i].Left-F_HScroll.Position,0,F_Header.Sections[i].Right-F_HScroll.Position,0);
          break;
        end;
    {$ELSE}
      F_TreeSectionPos:=rect(F_Header.Sections[0].Left-F_HScroll.Position,0,F_Header.Sections[0].Right-F_HScroll.Position,0);
    {$ENDIF}
    F_DrawingEvenItem:=GetTopItemEven;

    with Canvas do begin
      //Background
      defaultDraw:=DoCustomBackgroundDrawEvent(cdetPrePaint);
      if defaultDraw then begin
        pen.Style:=psClear;
        brush.Style:=bsSolid;
        brush.color:=F_BgColor;
        FillRect(rect(0,F_VScroll.Top,doubleBuffer.Width,doubleBuffer.Height));
      end;

      //Items
      if TopItem <>nil then begin
        TreeColumnIndentation:=13;

        F_DrawingYPos:=TopPos+F_VScroll.Position*RowHeight;
        curItem:=TopItem;
        curitem.GetParentHierarchyStack(stack);
        while (curItem<>nil) and (F_DrawingYPos<=height) do begin
          if f_invalidateAll or (f_invalidatedItems.IndexOf(curItem)>=0) then
            curItem.Paint(stack);
          F_DrawingEvenItem:=not F_DrawingEvenItem;
          if (tlvoStripInvisibleItems in F_Options) and not curitem.Expanded then
            if (curitem.SubItems.GetRealItemCount([ricCountCollapsedSubItems]) and $1=0) then
              F_DrawingEvenItem:=not F_DrawingEvenItem;
          curItem:=curItem.GetNextFromHierarchyStack(stack,true);
          F_DrawingYPos:=F_DrawingYPos+RowHeight;
        end;
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
                       MoveTo(xpos,F_VScroll.Top);
                       LineTo(xpos,doubleBuffer.Height);
                     end;
            lmDot:   DrawAlignDotLine(xpos,F_VScroll.Top,xpos,doubleBuffer.Height,VerticalLineColor);
          end;
        end;
      end;
      DoCustomBackgroundDrawEvent(cdetPostPaint);
    end;
  finally
    switchDoubleBuffer(false);
  end;
end;


procedure TTreeListView.internPaint(calledFromPaintingEvent: boolean=false);
  function sortedrect(x1,y1,x2,y2:longint):TRect;
  begin
    if x1<=x2 then begin
      result.left:=x1;
      result.right:=x2;
    end else begin
      result.left:=x2;
      result.right:=x1;
    end;
    if y1<=y2 then begin
      result.top:=y1;
      result.bottom:=y2;
    end else begin
      result.top:=y2;
      result.bottom:=y1;
    end;
  end;
var ypos:integer;
 //   doubleBuffer:Tbitmap;
    curItem: TTreeListItem;
    stack: TItemHierarchyStack;
    outRect: Trect;
    newWidth, newHeight: longint;
begin
  if (tlioUpdating in InternOptions_tlio) or
     (tlioDeleting in InternOptions_tlio) or
     (f_items=nil) or
     (f_RedrawBlock>0) or (parent=nil) or
     (F_SheduledRepaint<>0)  or//we will be redrawn later anyways (notice that it isn't possible to draw now and set F_SheduledRepaint to 0 because if we are called from the lcl paint, we can't paint to the whole canvas here)
     (Width <= 0) or (Height <= 0)
     then exit;

  {$IFDEF FPC}{$IFNDEF WINDOWS}{$IFNDEF WIN32}{$IFNDEF LCLGTK2}
  if not calledFromPaintingEvent then begin
    {$IFDEF LCLQT}
    TQtWidget(Handle).setAttribute(4 {QtWA_OpaquePaintEvent}, true);
    TQtWidget(Handle).setAttribute(9 {QtWA_NoSystemBackground}, true);
    Update;
    {$ELSE}
    f_invalidateAll:=true;
    Repaint; //needed for android?, not sure about the other OS
    {$ENDIF}
    exit;
  end;
  {$ENDIF}{$ENDIF}{$ENDIF}{$ENDIF}

  f_RedrawBlock:=1000;
  newWidth:=width +128 - Width mod 128;//don't change the size for small control size changes
  newHeight:=height +128 - height mod 128;
  if (newWidth<>doubleBuffer.Width) or (newHeight <> doubleBuffer.Height) then begin
    {if (newWidth>doubleBuffer.Width) or (newHeight > doubleBuffer.Height) then }f_invalidateAll:=true;
    {$ifdef lcl}
    doubleBuffer.SetSize(newwidth,newheight);
    {$else}
    doubleBuffer.width:=newwidth;
    doubleBuffer.height:=newheight;
    {$endif}
  end else if ((F_LastPaintedWidth<>width) or (F_LastPaintedHeight<>height)) and not f_invalidateAll then begin
    //redraw if the size changes and custom drawing is enabled
    //  this is unnecessary in most cases but in the custom draw methode something
    //  very strange could be drawn which depends on the size (e.g. an always
    //  centerd water mark)
    f_invalidateAll:=assigned(F_CustomBgDraw) or Assigned(F_CustomItemDraw);
  end;
  F_LastPaintedWidth:=width;
  F_LastPaintedHeight:=height;
  if F_MouseSelectingFocusRectDraw then begin//remove old focus rect
    canvas.Brush.Style:=bsSolid;
    canvas.pen.style:=psSolid;
    canvas.Brush.Color:=clWhite;
    canvas.font.color:=clBlack;
    canvas.pen.Color:=clBlack;
    canvas.TextOut(width+10,10,' '); //WTF!!????? borland.public.delphi.vcl.components.writing/msg/330fe30ad70dd07f
    canvas.DrawFocusRect(F_MouseSelectingFocusRect);
    F_MouseSelectingFocusRectDraw:=false;
  end;
  if (f_invalidatedItems.count>0) or f_invalidateAll or (Assigned(F_SearchBar) and (F_NewSearchBarFindState <> F_SearchBar.FindState)) then
    internDraw();

  //small rect at the right side bottom where the scrollbars meet
  canvas.pen.Style:=psClear;
  canvas.brush.Style:=bsSolid;
  canvas.brush.Color:=clBtnFace;
  outRect:=rect(F_HScroll.left+F_HScroll.Width,F_VScroll.top+F_VScroll.Height,RealBaseClientWidth,RealBaseClientHeight);
  if (F_SearchBar<>nil) and (f_searchbar.Visible) then dec(outRect.Bottom, F_SearchBar.Height);
  canvas.FillRect(outRect);

  outRect:=rect(0,F_VScroll.Top,F_VScroll.Left {$ifndef lcl}-1{$endif},F_HScroll.top{$ifndef lcl}-1{$endif});
  if Assigned(F_SearchBar) and (F_NewSearchBarFindState <> F_SearchBar.FindState) then
    F_SearchBar.FindState := F_NewSearchBarFindState;
  if f_bufferComplete then
    canvas.CopyRect(outRect,doubleBuffer.canvas,outRect) //DoubleBuffer ausgeben
  else begin
    curItem:=TopItem;
    curitem.GetParentHierarchyStack(stack);
    ypos:=TopPos+F_VScroll.Position*RowHeight;
    while (curItem<>nil) and (ypos<=F_HScroll.Top) do begin
      if (f_invalidatedItems.IndexOf(curItem)>=0) then begin
        outRect.Top:=ypos;
        outRect.bottom:=min(ypos+RowHeight,F_HScroll.Top);
        canvas.CopyRect(outRect,doubleBuffer.canvas,outRect);
      end;
      curItem:=curItem.GetNextFromHierarchyStack(stack,true);
      ypos:=ypos+RowHeight;
    end;
  end;
  F_MouseSelectingFocusRectDraw:=F_MouseSelecting<>msNone;
  if F_MouseSelectingFocusRectDraw then begin
    F_MouseSelectingFocusRect:=sortedrect(F_RealClickPos.x-F_HScroll.Position,F_RealClickPos.y-F_VScroll.Position*RowHeight,
                                          F_RealMousePos.x-F_HScroll.Position,F_RealMousePos.y-F_VScroll.Position*RowHeight);
    {it is better to draw the selection rect on the scrollbar than
     to have a selection rect whose top/bottom border is visible if it should be
     invisible because the user selected more items than fit on the screen
    if F_MouseSelectingFocusRect.Top<F_Header.Height-1 then
      F_MouseSelectingFocusRect.Top:=F_Header.Height-1;
    if F_MouseSelectingFocusRect.Bottom>F_HScroll.Top+1 then
      F_MouseSelectingFocusRect.Bottom:=F_HScroll.top+1;}
    canvas.Brush.Style:=bsSolid;
    canvas.pen.style:=psSolid;
    canvas.Brush.Color:=clWhite;
    canvas.font.color:=clBlack;
    canvas.pen.Color:=clBlack;
    canvas.TextOut(width+10,10,' ');
    canvas.DrawFocusRect(F_MouseSelectingFocusRect);
  end;
  f_invalidatedItems.Clear;
  f_invalidateAll:=false;
  f_RedrawBlock:=0;
end;

//Ausgaberoutinen
procedure TTreeListView.Paint;
begin
  if not f_bufferComplete then
    f_invalidateAll:=true;
  internPaint(true);
  if F_SearchBar<>nil then
    F_SearchBar.Invalidate;
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
  f_invalidatedItems.free;
  DoubleBuffer.Free;
  F_SearchBar.free;F_SearchBar:=nil;
  inherited;
end;
procedure Register;
begin
  RegisterComponents('BeniBela', [TTreeListView]);
end;

{$ifdef lcl}
initialization
{$I treelistview.lrs}
{$endif}

end.



