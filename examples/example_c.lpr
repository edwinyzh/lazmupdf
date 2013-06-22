// Rendering a page of a PDF document to a PNG image in less than 100 lines.

// This is a direct translation to Pascal of mupdf-1.2-source/doc/example.c
// from the muPDF 1.2 source code.


program example_c;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes
  { you can add units after this },
  ctypes, libmupdf, sysutils;

procedure render (FileName: PChar; pagenumber, zoom, rotation: cint);
var
  ctx: fz_context;
  doc: fz_document;
  pagecount: cint;
  page: fz_page;
  transfm: fz_matrix;
  boundbox: fz_rect;
  bbox: fz_irect;
  pix: fz_pixmap;
  dev: fz_device;
  aB: pfz_rect;
  pb: pfz_irect;
begin
  // Create a context to hold the exception stack and various caches.

  ctx := fz_new_context(nil, nil, FZ_STORE_UNLIMITED);

  // Open the PDF, XPS or CBZ document.

  doc := fz_open_document(ctx, filename);

  // Retrieve the number of pages (not used in this example).

  pagecount := fz_count_pages(doc);

  // Load the page we want. Page numbering starts from zero.

  page := fz_load_page(doc, pagenumber - 1);

  // Calculate a transform to use when rendering. This transform
  // contains the scale and rotation. Convert zoom percentage to a
  // scaling factor. Without scaling the resolution is 72 dpi.

  fz_rotate(@transfm, rotation);
  fz_pre_scale(@transfm, zoom / 100.0, zoom / 100.0) ;

  // Take the page bounds and transform them by the same matrix that
  // we will use to render the page.

 fz_bound_page(doc, page, @boundbox);
 fz_transform_rect(@boundbox, @transfm);

 // Create a blank pixmap to hold the result of rendering. The
 // pixmap bounds used here are the same as the transformed page
 // bounds, so it will contain the entire page. The page coordinate
 // space has the origin at the top left corner and the x axis
 // extends to the right and the y axis extends down.

 fz_round_rect(@bbox, @boundbox);
 pix := fz_new_pixmap_with_bbox(ctx, fz_find_device_colorspace(ctx, PChar('DeviceRGB')), @bbox);
 fz_clear_pixmap_with_value(ctx, pix, $ff);

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

 dev := fz_new_draw_device(ctx, pix);
 fz_run_page(doc, page, dev, @transfm, nil);
 fz_free_device(dev);

 // Save the pixmap to a file.

 writeln('pixmap components: ',fz_pixmap_components(ctx, pix));

 fz_write_png(ctx, pix, '/tmp/out.png', 0);


  // Clean up.

  fz_drop_pixmap(ctx, pix);
  fz_free_page(doc, page);
  fz_close_document(doc);
  fz_free_context(ctx);

end;

var
  filename: String;
  pagenumber: cint;
  zoom: cint;
  rotation: cint;
begin
  if Paramcount<1 then
  begin
    writeln('no filename given');
    Exit;
  end;
  filename := ParamStr(1);
  pagenumber := 1;
  zoom := 100;
  rotation := 0;

  if Paramcount>1 then
     pagenumber := StrToInt(ParamStr(2));

  if Paramcount>2 then
     zoom := StrToInt(ParamStr(3));

  if Paramcount>3 then
     rotation := StrToInt(ParamStr(4));

  render(PChar(filename), pagenumber, zoom, rotation);
end.

