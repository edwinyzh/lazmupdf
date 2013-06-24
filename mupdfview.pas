unit muPDFView;

{$mode objfpc}{$H+}

interface

uses
 Classes, SysUtils, Forms, Controls, ExtCtrls, libmupdf, BGRABitmap, Graphics;

type

{ TmuPDFView }

 TmuPDFView = class (TScrollBox)
 private
   FPagecount: integer;
   FPagenum: Integer;
   FRotation: integer;
   FZoom: integer;
   pdfCanvas: TPanel;
   pdfContext: fz_context;
   pdfDoc: fz_document;
   pdfPage: fz_page;
   pdfImage: TBGRABitmap;
   pdfPixmap: fz_pixmap;
   fselboxes: array[0..99] of fz_rect;
   fselcount: Integer;
   procedure SetPagenum ( AValue: Integer ) ;
   procedure SetRotation ( AValue: integer ) ;
   procedure SetZoom ( AValue: integer ) ;
   procedure paintPDF(Sender: TObject);
   procedure Loadpage;
   procedure ClearPdfContext;
 protected
   procedure Resize; override;
 public
   constructor Create ( AOwner: TComponent ) ; override;
   destructor Destroy; override;
   procedure LoadFromFile(FileName: TFilename);
   procedure LoadFromStream(Stream: TMemoryStream);
   procedure TestTextFunctions;
   property Zoom: integer read FZoom write SetZoom;
   property Rotation: integer read FRotation write SetRotation;
   property Pagenum: Integer read FPagenum write SetPagenum;
   property Pagecount: integer read FPagecount;
 end;

procedure Register;

const
  BORDER_WIDTH = 10;
  SHADOW_WIDTH = 4;

implementation

procedure Register;
begin
  RegisterComponents('Additional',[TmuPDFView]);
end;

{ TmuPDFView }

procedure TmuPDFView.SetPagenum ( AValue: Integer ) ;
begin
 if (FPagenum = AValue) or (FPagenum>FPagecount) or (FPagenum<1) then Exit;
 FPagenum := AValue;
  if pdfDoc<>nil then
  begin
    Loadpage;
  end;
end;

procedure TmuPDFView.SetRotation ( AValue: integer ) ;
begin
  if FRotation = AValue then Exit;
  FRotation := AValue;
end;

procedure TmuPDFView.SetZoom ( AValue: integer ) ;
begin
  if FZoom = AValue then Exit;
  FZoom := AValue;
  if pdfDoc<>nil then
  begin
    Loadpage;
  end;
end;

procedure TmuPDFView.paintPDF ( Sender: TObject ) ;
var
  x: Integer;
  y: Integer;
  shadowcentre: Integer;
  box: Integer;
  b: fz_rect;
  ir: fz_irect;
begin
  if pdfImage<>nil then
      begin
        if ClientWidth>pdfImage.Width then
            begin
              x := (ClientWidth - pdfImage.Width) div 2;
              pdfCanvas.Width := ClientWidth;
            end
             else
             begin
               x := BORDER_WIDTH;
               pdfCanvas.Width := pdfImage.Width + (BORDER_WIDTH * 2);
             end;
             y := BORDER_WIDTH;
             pdfCanvas.Height := pdfImage.Height + (BORDER_WIDTH * 2);
        pdfImage.Draw(pdfCanvas.Canvas, x, y);
        with pdfCanvas.Canvas.Pen do
             begin
               Width := 1;
               Color := clBlack;
               EndCap := pecSquare;
               JoinStyle := pjsMiter;
               pdfCanvas.Canvas.Frame(x, y, x + pdfImage.Width, y + pdfImage.Height);
               Width := SHADOW_WIDTH;
               Color := clDkGray;
               shadowcentre := SHADOW_WIDTH div 2;
               pdfCanvas.Canvas.MoveTo(x + SHADOW_WIDTH, y + shadowcentre + pdfImage.Height);
               pdfCanvas.Canvas.LineTo(x + shadowcentre + pdfImage.Width, y + pdfImage.Height + shadowcentre);
               pdfCanvas.Canvas.LineTo(x + shadowcentre + pdfImage.Width, y + SHADOW_WIDTH);
             end;
        if fselcount>0 then
            for box := 0 to fselcount-1 do
            begin
                b := fselboxes[box];

                fz_round_rect(@ir, @b);
                pdfCanvas.Canvas.Brush.Style := bsClear;
                pdfCanvas.Canvas.Pen.Width := 2;
                pdfCanvas.Canvas.Pen.Color := clSkyBlue;
                pdfCanvas.Canvas.Rectangle(ir.x0 + x, ir.y0 + y, ir.x1 + x, ir.y1 + y);

            end;
      end;
end;

procedure TmuPDFView.Loadpage;
var
  transfm: fz_matrix;
  boundbox: fz_rect;
  bbox: fz_irect;
  dev: fz_device;
  p1: Pointer;
begin
  if pdfPage<>nil then fz_free_page(pdfDoc, pdfPage);
  fselcount := 0;

  pdfPage := fz_load_page(pdfDoc, FPagenum - 1);

  // Calculate a transform to use when rendering. This transform
  // contains the scale and rotation. Convert zoom percentage to a
  // scaling factor. Without scaling the resolution is 72 dpi.

  fz_rotate(@transfm, rotation);
  fz_pre_scale(@transfm, FZoom / 100.0, FZoom / 100.0) ;

  // Take the page bounds and transform them by the same matrix that
  // we will use to render the page.

  fz_bound_page(pdfDoc, pdfPage, @boundbox);
  fz_transform_rect(@boundbox, @transfm);

 // Create a blank pixmap to hold the result of rendering. The
 // pixmap bounds used here are the same as the transformed page
 // bounds, so it will contain the entire page. The page coordinate
 // space has the origin at the top left corner and the x axis
 // extends to the right and the y axis extends down.

 fz_round_rect(@bbox, @boundbox);

 if pdfImage<>nil then
     begin
       fz_drop_pixmap(pdfContext, pdfPixmap);
       pdfImage.Free;
       pdfImage := nil;
     end;

 pdfImage := TBGRABitmap.Create(bbox.x1, bbox.y1);

 p1 := pdfImage.Data;

 pdfPixmap := fz_new_pixmap_with_bbox_and_data(pdfContext, fz_find_device_colorspace(pdfContext, PChar('DeviceBGR')), @bbox, pdfImage.Data);
 fz_clear_pixmap_with_value(pdfContext, pdfPixmap, $ff);

 pdfCanvas.Width := bbox.x1;
 pdfCanvas.Height := bbox.y1;

 // A page consists of a series of objects (text, line art, images,
 // gradients). These objects are passed to a device when the
 // interpreter runs the page. There are several devices, used for
 // different purposes:
 //
 //	draw device -- renders objects to a target pixmap.
 //
 //	text device -- extracts the text in reading order with styling
 //	information. This text can be used to provide text search.
 //
 //	list device -- records the graphic objects in a list that can
 //	be played back through another device. This is useful if you
 //	need to run the same page through multiple devices, without
 //	the overhead of parsing the page each time.

 // Create a draw device with the pixmap as its target.
 // Run the page with the transform.

 dev := fz_new_draw_device(pdfContext, pdfPixmap);
 fz_run_page(pdfDoc, pdfPage, dev, @transfm, nil);
 fz_free_device(dev);

 pdfImage.InvalidateBitmap;  //note that we have accessed directly to pixels
 paintPDF(nil);

end;

procedure TmuPDFView.ClearPdfContext;
begin
 if pdfPixmap<>nil then begin fz_drop_pixmap(pdfContext, pdfPixmap); pdfPixmap := nil; end;;
 if pdfImage<>nil then begin pdfImage.Free; pdfImage := nil; end;;
 if pdfPage<>nil then begin fz_free_page(pdfDoc, pdfPage); pdfPage := nil; end;
 if pdfDoc<>nil then begin fz_close_document(pdfDoc); pdfDoc := nil; end;
 if pdfContext<>nil then begin fz_free_context(pdfContext); pdfContext := nil; end;
 fselcount := 0;
end;

procedure TmuPDFView.Resize;
begin
  inherited Resize;
  Refresh;
end;

constructor TmuPDFView.Create ( AOwner: TComponent ) ;
begin
  inherited Create ( AOwner ) ;
  FZoom := 100;
  FRotation := 0;
  FPagenum := 0;
  fselcount := 0;
  self.resize;
  pdfCanvas := TPanel.Create(AOwner);
  with pdfCanvas do
       begin
         Parent := Self;
         Width := 0;
         Height := 0;
         OnPaint := @paintPDF;
       end;
  VertScrollBar.Tracking := true;
  pdfimage := nil;
  pdfContext := nil;
  pdfDoc := nil;
  pdfPage := nil;
end;

destructor TmuPDFView.Destroy;
begin
  ClearPdfContext;
  inherited Destroy;
end;

procedure TmuPDFView.LoadFromFile ( FileName: TFilename ) ;
begin
  ClearPdfContext;

  // Create a context to hold the exception stack and various caches.

  pdfContext := fz_new_context(nil, nil, FZ_STORE_UNLIMITED);

  // Open the PDF, XPS or CBZ document.

  pdfDoc := fz_open_document(pdfContext, PChar(filename));

  // Retrieve the number of pages.

  FPagecount := fz_count_pages(pdfDoc);

  // Load the page we want. Page numbering starts from zero.

  FPagenum := 1;

  Loadpage;

end;

procedure TmuPDFView.LoadFromStream ( Stream: TMemoryStream ) ;
begin

end;

procedure TmuPDFView.TestTextFunctions;
var
  sht: fz_text_sheet;
  bbox: fz_rect;
  pg: fz_text_page;
  dev: fz_device;
  m: fz_matrix;
  b: fz_rect;
  ir: fz_irect;
  selectedtext: string;
begin
  bbox.x0 := 0;
  bbox.y0 := 0;
  bbox.x1 := pdfImage.Width;
  bbox.y1 := pdfImage.Height;
  fz_rotate(@m, rotation);
  fz_pre_scale(@m, FZoom / 100.0, FZoom / 100.0) ;
  sht := fz_new_text_sheet(pdfContext);
  pg := fz_new_text_page(pdfContext, @bbox);
  dev := fz_new_text_device(pdfContext, sht, pg);
  fz_run_page(pdfDoc, pdfPage, dev, @m, nil);
  fselcount := fz_highlight_selection(pdfContext, pg, bbox, @fselboxes, 100);
  {$IFDEF LINUX}
  WriteLn('hits: ', fselcount);
  {$ENDIF}
  pdfCanvas.Invalidate;

  selectedtext := fz_copy_selection(pdfContext, pg, bbox);
  // now do something with selectedtext
end;

end.


