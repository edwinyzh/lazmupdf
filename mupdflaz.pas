{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit mupdflaz;

interface

uses
  muPDFView, libmupdf, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit ( 'muPDFView', @muPDFView.Register ) ;
end;

initialization
  RegisterPackage ( 'mupdflaz', @Register ) ;
end.
