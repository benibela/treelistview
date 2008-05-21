unit treelistviewexampleu;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs,treelistview, ImgList ;

type
  TForm1 = class(TForm)
    ImageList1: TImageList;
    procedure FormCreate(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
    W32TreeListView1:TW32TreeListView;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
var i:longint;
begin
  W32TreeListView1:= TTreeListView.create(self);
  W32TreeListView1.Images:=ImageList1;
  W32TreeListView1.Parent:=self;
  W32TreeListView1.Visible:=true;
  W32TreeListView1.Align:=alClient;
{  W32TreeListView1.OnExpandItem:=W32TreeListView1ExpandItem;
  W32TreeListView1.OnKeyUp:=windowListKeyUp;
  W32TreeListView1.OnSelect:=windowListSelectItem;
  W32TreeListView1.OnDblClick:=windowListDblClick;}
  W32TreeListView1.sorted:=true;
  W32TreeListView1.Columns.clear;
  W32TreeListView1.Columns.add.Text:='Spalte 1';
  W32TreeListView1.Columns[0].Width:=220;
  with W32TreeListView1.Columns.Add do begin
    text:='Spalte 2';
    Width:=200;
  end;
  with W32TreeListView1.Columns.Add do begin
    text:='3';
    Width:=200;
  end;
  with W32TreeListView1.Columns.Add do begin
    text:='4';
    Width:=100;
  end;
  W32TreeListView1.HorizontalLineMode:=lmDot;
  W32TreeListView1.VerticalLineMode:=lmDot;
  W32TreeListView1.multiSelect:=true;
  with W32TreeListView1.Items.Add do begin
    Text:='Hallo';
    RecordItems.Add('Welt');
    RecordItems.Add('123456789');
    SubItems.Add('Welt 1').RecordItems.Add.Text:='erste Welt';
    SubItems.Add('Welt 2').RecordItems.Add.Text:='zweite Welt';
    with SubItems.Add('Welt 3') do begin
      RecordItems.Add.Text:='dritte Welt';
      SubItems.Add('Hier gibt es Kängurus').SubItems.Add('und Pinguine');
      SubItems.Add('Hier gibt es Sonnen');
    end;
    SubItems.Add('Welt 4').RecordItems.Add.Text:='vierte Welt';
  end;
  with W32TreeListView1.Items.Add do begin
    Text:='Schüss';
    SubItems.Add('Welt 1').RecordItems.Add.Text:='erste Welt';
    SubItems.Add('Welt 2').RecordItems.Add.Text:='zweite Welt';
    with SubItems.Add('Welt 3') do begin
      RecordItems.Add.Text:='dritte Welt';
      SubItems.Add('Hier gibt es Kängurus').SubItems.Add('und Pinguine');
      SubItems.Add('Hier gibt es Sonnen');
    end;
    SubItems.Add('Welt 4').RecordItems.Add.Text:='vierte Welt';
  end;
  for i := 0 to 10 do
    with W32TreeListView1.Items.Add(inttostr(i)) do begin
      RecordItemsText[1]:='10 - '+inttostr(i)+' = '+inttostr(10-i);
      RecordItemsText[2]:='20 - '+inttostr(i)+' = '+inttostr(20-i);
      ImageTyp:=itListIndex;
      IndexOrBitmap:=i mod 3;
    end;

end;

end.
