unit simpleviewForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Spin, muPDFView;

type

  { TForm1 }

  TForm1 = class ( TForm )
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    ComboBox1: TComboBox;
    ComboBox2: TComboBox;
    MetaTagEdit: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    muPDFView1: TmuPDFView;
    OpenDialog1: TOpenDialog;
    Panel1: TPanel;
    SpinEdit1: TSpinEdit;
    procedure Button1Click ( Sender: TObject ) ;
    procedure Button2Click ( Sender: TObject ) ;
    procedure RotationChange ( Sender: TObject ) ;
    procedure ShowFormat ( Sender: TObject ) ;
    procedure ShowMetaTag ( Sender: TObject ) ;
    procedure ZoomChange ( Sender: TObject ) ;
    procedure PagenumChange ( Sender: TObject ) ;
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.Button1Click ( Sender: TObject ) ;
begin
  if OpenDialog1.Execute then
    begin
      muPDFView1.LoadFromFile(OpenDialog1.FileName);
      SpinEdit1.MaxValue := muPDFView1.Pagecount;
      Caption := 'PDF Viewer - ' + muPDFView1.DocumentInfo('Title');
    end;
end;

procedure TForm1.Button2Click ( Sender: TObject ) ;
begin
  muPDFView1.TestTextFunctions;
end;

procedure TForm1.RotationChange ( Sender: TObject ) ;
var
  r: Integer;
begin
  if TryStrToInt(ComboBox2.Text, r)
     then muPDFView1.Rotation := r;
end;

procedure TForm1.ShowFormat ( Sender: TObject ) ;
begin
  ShowMessage('Format: ' + muPDFView1.Docformat);
end;

procedure TForm1.ShowMetaTag ( Sender: TObject ) ;
begin
  ShowMessage( MetaTagEdit.Text + ': ' + muPDFView1.DocumentInfo(MetaTagEdit.Text) );
end;

procedure TForm1.ZoomChange ( Sender: TObject ) ;
var
  n: integer;
begin
  if TryStrToInt(ComboBox1.Text, n)
     then muPDFView1.Zoom := n;
end;

procedure TForm1.PagenumChange ( Sender: TObject ) ;
begin
  muPDFView1.Pagenum := SpinEdit1.Value;
end;

end.

