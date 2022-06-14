unit treelistviewexampleu;

(* This example shows a possible usage of my tree list view

   The TTreeListView is created dynamically in the constructor TExampleForm.create (see below)

   If you wonder why there is no form and if you have to use it always in this
   strange way, the answer is no. The example just creates the form dynamically
   because this works in Lazarus and Delphi in the same way (and you can compile
   it even, if you haven't installed the packages).
*)

{$ifdef fpc}
{$mode delphi}
{$endif}
{$H+}

interface

uses
  Classes, SysUtils,ExtCtrls,Forms,TreeListView,StdCtrls,Controls,Graphics,menus
  {$IFDEF lcl}
  ,RTTIGrids
  {$ENDIF};



type

{ TExampleForm }

TExampleForm= class(TCustomForm)
  procedure ExampleFormShow(Sender: TObject);

  procedure TLClickRecordItem(sender: TObject; item: TTreeListRecordItem);
  procedure TLCollapsed(sender: TObject; item: TTreeListItem);
  procedure TLExpanded(sender: TObject; item: TTreeListItem);
  procedure TLSelect(sender: TObject; item: TTreeListItem);
  procedure FormClose(sender: TObject; var Action: TCloseAction);
  procedure TreeListView1ClickAtItem(sender: TObject; item: TTreeListItem);
public
    TreeListView1:TTreeListView;
    ImageList: TImageList;
    {$IFDEF lcl} grid: TTIPropertyGrid; {$ENDIF}
    constructor create(TheOwner: TComponent); override;
  end;

var
  Form: TExampleForm;
   running:boolean=true;
implementation


procedure createTree(plist: TTreeListItems; deep: longint);
var i:longint;
begin
  if deep<=0 then exit;
  for i:=1 to 5 do
    createTree(plist.Add('tree:'+IntToStr(deep)+','+IntToStr(i)).SubItems,deep-1)
end;


procedure TExampleForm.FormClose(sender: TObject; var Action: TCloseAction);
begin
  running:=false;
  Application.Terminate;
end;

procedure TExampleForm.TreeListView1ClickAtItem(sender: TObject; item: TTreeListItem);
begin
  if item = nil then exit;
  //if item.Parent = nil then TTreeListView(sender).Items.RemoveObject(item)
  //else item.Parent.SubItems.RemoveObject(item)
  item.RecordItemsText[5]:=inttostr(StrToIntDef(item.RecordItemsText[5], 0) + 1);
end;

procedure TExampleForm.ExampleFormShow(Sender: TObject);
begin
  TreeListView1.createSearchBar();
  {$ifdef lcl}
  grid.TIObject:=TreeListView1;
  {$endif}
end;

procedure TExampleForm.TLClickRecordItem(sender: TObject; item: TTreeListRecordItem);
var
  i: TTreeListItem;
begin
  Caption:=item.parent.Text + ' clicked on: '+item.Text;
end;

procedure TExampleForm.TLCollapsed(sender: TObject; item: TTreeListItem);
begin
  Caption:=item.Text + ' collapsed';
end;

procedure TExampleForm.TLExpanded(sender: TObject; item: TTreeListItem);
begin
  Caption:=item.Text + ' expanded';
end;

procedure TExampleForm.TLSelect(sender: TObject; item: TTreeListItem);
begin
  Caption:=item.Text + ' selected';
end;

constructor TExampleForm.create(TheOwner: TComponent);
//create form dynamically to support Delphi and Larazus
var i:longint;
    bmp:TBitmap;
    pmenu: TPopupMenu;
    mi: TMenuItem;
begin
  inherited {$ifndef fpc}createnew(theOwner){$endif};
  width:=900;
  height:=600;
  left:=(screen.width-width) div 2;
  top:=(screen.Height-height) div 2;

  TreeListView1:= TTreeListView.create(self);

  ImageList:=TImageList.Create(self);
  ImageList.Width:=14;
  ImageList.Height:=14;
  if FileExists('tv1.bmp') then begin
    bmp:=TBitmap.Create;
    bmp.LoadFromFile('tv1.bmp');
    ImageList.Add(bmp,nil);
    bmp:=TBitmap.Create;
    bmp.LoadFromFile('tv2.bmp');
    ImageList.Add(bmp,nil);
    bmp:=TBitmap.Create;
    bmp.LoadFromFile('tv3.bmp');
    ImageList.Add(bmp,nil);
    TreeListView1.Images:=ImageList;
  end;

  TreeListView1.Parent:=self;
  TreeListView1.Visible:=true;
  TreeListView1.Align:=alClient;
  TreeListView1.OnItemExpanded:=TLExpanded;
  TreeListView1.OnItemCollapsed:=TLCollapsed;
  TreeListView1.OnSelect:=TLSelect;
  TreeListView1.OnClickAtRecordItem:=TLClickRecordItem;
  TreeListView1.OnClickAtItem:=TreeListView1ClickAtItem;

  TreeListView1.Columns.clear;
  TreeListView1.Columns.add.Text:='Column 1';
  TreeListView1.Columns[0].Width:=170;
  with TreeListView1.Columns.Add do begin
    text:='Column 2';
    Width:=100;
  end;
  with TreeListView1.Columns.Add do begin
    text:='3';
    Width:=50;
  end;
  with TreeListView1.Columns.Add do begin
    text:='right';
    Width:=100;
    Alignment:=taRightJustify;
  end;
  with TreeListView1.Columns.Add do begin
    text:='center';
    Width:=100;
    Alignment:=taCenter;
  end;
  with TreeListView1.Columns.Add do begin
    text:='clicked';
    Width:=45;
  end;
  TreeListView1.Options:=TreeListView1.Options + [tlvoMultiSelect,tlvoHotTrackRecordTextItems,tlvoSorted];
  {$ifdef lcl}
  TreeListView1.Options:=TreeListView1.Options + [tlvoColumnsDragable];
  TreeListView1.createUserColumnVisibilityPopupMenu();
  {$endif}
  TreeListView1.HorizontalLineMode:=lmSolid;
  TreeListView1.VerticalLineMode:=lmDot;
  TreeListView1.BeginUpdate;
  with TreeListView1.Items.Add do begin
    Text:='Hallo';
    RecordItems.Add('Welt');
    RecordItems.Add('123456789');
    SubItems.Add('Welt 1').RecordItems.Add.Text:='erste Welt';
    SubItems.Add('Welt 2').RecordItems.Add.Text:='zweite Welt';
    with SubItems.Add('Welt 3') do begin
      RecordItems.Add.Text:='dritte Welt';
      RecordItems.Add.Text:='alpha';
      RecordItems.Add.Text:='beta';
      RecordItems.Add.Text:='gamma';
      SubItems.Add('Hier gibt es Giraffen').SubItems.Add('und Pinguine');
      SubItems.Add('Hier gibt es Sonnen');

    end;
    SubItems.Add('Welt 4').RecordItems.Add.Text:='vierte Welt';
  end;
  with TreeListView1.Items.Add do begin
    Text:='Good bye';
    SubItems.Add('Welt 1').RecordItems.Add.Text:='erste Welt';
    SubItems.Add('Welt 2').RecordItems.Add.Text:='zweite Welt';
    with SubItems.Add('Welt 3') do begin
      RecordItems.Add.Text:='dritte Welt';
      RecordItems.Add.Text:='alpha';
      RecordItems.Add.Text:='beta';
      RecordItems.Add.Text:='gamma';
      SubItems.Add('Hier gibt es Giraffen').SubItems.Add('und Pinguine');
      SubItems.Add('Hier gibt es Sonnen');
    end;
    SubItems.Add('Welt 4').RecordItems.Add.Text:='vierte Welt';
  end;
  for i := 0 to 20 do
    with TreeListView1.Items.Add(inttostr(i)) do begin
      RecordItemsText[1]:='10 - '+inttostr(i)+' = '+inttostr(10-i);
      RecordItemsText[2]:='20 - '+inttostr(i)+' = '+inttostr(20-i);
      RecordItemsText[3]:='30 - '+inttostr(i)+' = '+inttostr(30-i);
      RecordItemsText[4]:='40 - '+inttostr(i)+' = '+inttostr(40-i);
      //ImageTyp:=itListIndex;
      ImageIndex:=(i+1) mod 2 +1;
    end;

   with TreeListView1.Items.Add('LARGE TREE') do begin
     if FileExists('tvExt1.bmp') then begin
       ImageBitmap:=TBitmap.Create;
       imagebitmap.LoadFromFile('tvExt1.bmp');
       createTree(SubItems,3);
     end;
   end;
   TreeListView1.EndUpdate;
   pmenu:=TPopupMenu.create(self);
   mi:=TMenuItem.create(self);mi.Caption:='does nothing';
   pmenu.items.add(mi);
   TreeListView1.popupmenu:=pmenu;
   OnShow:=ExampleFormShow;
   visible:=true;
   onclose:=FormClose;

   {$IFDEF lcl}
   with TSplitter.Create(self) do begin
     Parent:=self;
     align:=alLeft;
   end;


   grid:=TTIPropertyGrid.Create(self);
   grid.Align:=alLeft;
   grid.Parent:=self;
   {$ENDIF}
end;

end.

