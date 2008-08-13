unit treelistviewexampleu;

{$ifdef fpc}
{$mode delphi}
{$endif}
{$H+}

interface

uses
  Classes, SysUtils,Forms,TreeListView,StdCtrls,Controls,Graphics;



type

{ TExampleForm }

TExampleForm= class(TCustomForm)

  procedure TLClickRecordItem(sender: TObject; parentItem: TTreeListItem;
    item: TTreeListRecordItem);
  procedure TLCollapsed(sender: TObject; item: TTreeListItem);
  procedure TLExpanded(sender: TObject; item: TTreeListItem);
  procedure TLSelect(sender: TObject; item: TTreeListItem);
  procedure FormClose(sender: TObject; var Action: TCloseAction);
public
    TreeListView1:TTreeListView;
    ImageList: TImageList;
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
procedure TExampleForm.TLClickRecordItem(sender: TObject;
  parentItem: TTreeListItem; item: TTreeListRecordItem);
begin
  Caption:=parentItem.Text + ' clicked on: '+item.Text;
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
begin
  inherited {$ifndef fpc}createnew(theOwner){$endif};
  width:=640;
  height:=400;
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
  TreeListView1.Sorted:=true;


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
  {$ifdef lcl}
  TreeListView1.ColumnsDragable:=true;
  TreeListView1.createUserColumnVisibilityPopupMenu();
  {$endif}
  TreeListView1.HorizontalLineMode:=lmSolid;
  TreeListView1.VerticalLineMode:=lmDot;
  TreeListView1.multiSelect:=true;
  TreeListView1.HotTrackSubTextItems:=true;
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

   TreeListView1.createSearchBar();
   visible:=true;
   onclose:=FormClose;
 end;

end.

