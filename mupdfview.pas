{
  TmuPDFView is a PDF viewer component for Lazarus

  Copyright (C) 2013 Malcolm Poole <mgpoole@users.sourceforge.net>

  This component is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}
unit muPDFView;

{$mode objfpc}{$H+}

interface

uses
 Classes, SysUtils, LResources, ctypes, Forms, Controls, ExtCtrls, libmupdf,
 Graphics, IntfGraphics, GraphType;

type

{ TmuPDFView }

 TmuPDFView = class (TScrollBox)
 private
   pdfCanvas: TPanel;
   FPagecount: integer;
   FPagenum: Integer;
   FRotation: Single;
   FZoom: integer;
   pdfContext: fz_context;
   pdfDoc: fz_document;
   pdfPage: fz_page;
   pdfImage: TBitmap;
   pdfPixmap: fz_pixmap;
   fMemoryStream: TMemoryStream;
   fselboxes: array[0..99] of fz_rect;
   fselcount: Integer;
   function GetFormat: string;
   procedure SetPagenum ( AValue: Integer ) ;
   procedure SetRotation ( AValue: Single ) ;
   procedure SetZoom ( AValue: integer ) ;
   procedure paintPDF(Sender: TObject);
   procedure Loadpage;
   procedure ClearPdfContext;
 protected
   procedure Resize; override;
 public
   constructor Create ( AOwner: TComponent ) ; override;
   destructor Destroy; override;
   procedure Paint; override;
   procedure LoadFromFile(FileName: TFilename);
   procedure SaveToFile(FileName: TFilename);
   procedure LoadFromStream(Stream: TMemoryStream);
   function DocumentInfo(MetaTag: string): string;
   procedure TestTextFunctions;
   property Zoom: integer read FZoom write SetZoom;
   property Rotation: Single read FRotation write SetRotation;
   property Pagenum: Integer read FPagenum write SetPagenum;
   property Pagecount: integer read FPagecount;
   property DocFormat: string read GetFormat;
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
 if (FPagenum = AValue) or (AValue>FPagecount) or (AValue<1) then Exit;
 FPagenum := AValue;
  if pdfDoc<>nil then
  begin
    Loadpage;
  end;
end;

function TmuPDFView.GetFormat: string;
var
  c: PChar;
  block: array[0..128] of char;
begin
 Result := '';
 if pdfDoc=nil then Exit;
 fz_meta(pdfDoc, FZ_META_FORMAT_INFO, @block, 128);
 Result := PChar(@block);
end;

procedure TmuPDFView.SetRotation ( AValue: Single ) ;
begin
  if FRotation = AValue then Exit;
  FRotation := AValue;
  if pdfDoc<>nil then
    begin
      Loadpage;
    end;
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
        if ClientHeight>pdfImage.Height then
            begin
              y := (ClientHeight - pdfImage.Height) div 2;
              pdfCanvas.Height := ClientHeight;
            end
             else
             begin
               y := BORDER_WIDTH;
               pdfCanvas.Height := pdfImage.Height + (BORDER_WIDTH * 2);
             end;

       pdfCanvas.Canvas.Draw(x, y, pdfImage);
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
  ImgFormatDescription: TRawImageDescription;
  intimg: TLazIntfImage;
  w: cint;
  h: cint;
begin
  if pdfPage<>nil then fz_free_page(pdfDoc, pdfPage);
  if pdfCanvas=nil then
      begin
        pdfCanvas := TPanel.Create(nil);
        with pdfCanvas do
             begin
               Parent := Self;
               Width := 0;
               Height := 0;
               OnPaint := @paintPDF;
             end;
      end;

  if pdfImage<>nil then
       begin
         fz_drop_pixmap(pdfContext, pdfPixmap);
         pdfImage.Free;
         pdfImage := nil;
       end;

  fselcount := 0;

  pdfPage := fz_load_page(pdfDoc, FPagenum - 1);

  // Calculate a transform to use when rendering. This transform
  // contains the scale and rotation. Convert zoom percentage to a
  // scaling factor. Without scaling the resolution is 72 dpi.

  fz_rotate(@transfm, frotation);
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

 w := bbox.x1-bbox.x0;
 h := bbox.y1-bbox.y0;

 pdfImage := TBitmap.Create;
 pdfImage.Width := w;
 pdfImage.Height := h;

 intimg := TLazIntfImage.Create(w, h);
 {$IFDEF MSWINDOWS}
 ImgFormatDescription.Init_BPP32_B8G8R8_BIO_TTB(w, h);
 intimg.DataDescription := ImgFormatDescription;
 pdfPixmap := fz_new_pixmap_with_bbox_and_data(pdfContext, fz_find_device_colorspace(pdfContext, PChar('DeviceBGR')), @bbox, intimg.PixelData);
 {$ELSE}
 ImgFormatDescription.Init_BPP32_R8G8B8A8_BIO_TTB(w, h);
 intimg.DataDescription := ImgFormatDescription;
 pdfPixmap := fz_new_pixmap_with_bbox_and_data(pdfContext, fz_find_device_colorspace(pdfContext, PChar('DeviceRGB')), @bbox, intimg.PixelData);
 {$ENDIF}
 fz_clear_pixmap_with_value(pdfContext, pdfPixmap, 255);

  // Create a draw device with the pixmap as its target.
 // Run the page with the transform.

 dev := fz_new_draw_device(pdfContext, pdfPixmap);
 fz_run_page(pdfDoc, pdfPage, dev, @transfm, nil);
 fz_free_device(dev);
 pdfCanvas.Width := w;
 pdfCanvas.Height := h;

 pdfImage.LoadFromIntfImage(intimg);
 intimg.Free;

 paintPDF(nil);

end;

procedure TmuPDFView.ClearPdfContext;
begin
 if pdfPixmap<>nil then begin fz_drop_pixmap(pdfContext, pdfPixmap); pdfPixmap := nil; end;;
 if pdfImage<>nil then begin pdfImage.Free; pdfImage := nil; end;;
 if pdfPage<>nil then begin fz_free_page(pdfDoc, pdfPage); pdfPage := nil; end;
 if pdfDoc<>nil then begin fz_close_document(pdfDoc); pdfDoc := nil; end;
 if pdfContext<>nil then begin fz_free_context(pdfContext); pdfContext := nil; end;
 if fMemoryStream<>nil then begin fMemoryStream.Free; fMemoryStream := nil; end;
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
  pdfCanvas := nil;
  VertScrollBar.Tracking := true;
  pdfimage := nil;
  pdfContext := nil;
  pdfDoc := nil;
  pdfPage := nil;
  fMemoryStream := nil;
end;

destructor TmuPDFView.Destroy;
begin
  ClearPdfContext;
  if pdfCanvas<>nil then pdfCanvas.Free;
  inherited Destroy;
end;

procedure TmuPDFView.Paint;
var
  bmp: TBitmap;
begin
  inherited Paint;
  if csDesigning in ComponentState then
      begin
        Canvas.Brush.Color := clWhite;
        Canvas.Rectangle(ClientRect);
        bmp := TBitmap.Create;
        try
          bmp.LoadFromLazarusResource('TmuPDFView');
          Canvas.Draw((Width-24) div 2, (Height-24) div 2, bmp);
        finally
          bmp.Free;
        end;
      end;
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

  // Load the first page

  FPagenum := 1;

  Loadpage;

end;

procedure TmuPDFView.SaveToFile(FileName: TFilename);
begin
 if fMemoryStream<> nil then
     fMemoryStream.SaveToFile(FileName)
 else fz_write_document(pdfDoc, PChar(FileName), nil); // NOTE: this did not work correctly when tested
end;

{
     TmuPDFView.LoadFromStream
     The Stream parameter must not be destroyed by the application.
     TmuPDFView will Free it when the PDFContext is destroyed
}
procedure TmuPDFView.LoadFromStream ( Stream: TMemoryStream ) ;
var
  s: fz_stream;
  f: PChar;
begin
 ClearPdfContext;

 fMemoryStream := Stream;

 // Create a context to hold the exception stack and various caches.

 pdfContext := fz_new_context(nil, nil, FZ_STORE_UNLIMITED);

 // attach a libfitz stream to Stream

 s := fz_open_memory(pdfContext, fMemoryStream.Memory, fMemoryStream.Size);

 // Open the PDF document.
 f := 'application/pdf';

 pdfDoc := fz_open_document_with_stream(pdfContext, f, s);

 // Retrieve the number of pages.

 FPagecount := fz_count_pages(pdfDoc);

 // Load first page

 FPagenum := 1;

 Loadpage;

end;

function TmuPDFView.DocumentInfo ( MetaTag: string ) : string;
var
  c: PChar;
  block: array[0..128] of Pointer;
begin
 Result := '';
 if pdfDoc=nil then Exit;
 c := PChar(MetaTag);
 block[0] := c;
 fz_meta(pdfDoc, FZ_META_INFO, @block, 128);
 Result := PChar(@block);
end;

procedure TmuPDFView.TestTextFunctions;
var
  sht: fz_text_sheet;
  bbox: fz_rect;
  pg: fz_text_page;
  dev: fz_device;
  m: fz_matrix;
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

  //selectedtext := fz_copy_selection(pdfContext, pg, bbox);
  // now do something with selectedtext
end;

initialization
{$I mupdfview.lrs}

end.



