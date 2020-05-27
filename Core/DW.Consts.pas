unit DW.Consts;

{*******************************************************}
{                                                       }
{                      Kastri                           }
{                                                       }
{         Delphi Worlds Cross-Platform Library          }
{                                                       }
{    Copyright 2020 Dave Nottage under MIT license      }
{  which is located in the root folder of this library  }
{                                                       }
{*******************************************************}

{$I DW.GlobalDefines.inc}

interface

// *ALL* of these are non-localisable constants! Define localisable constants in DW.Consts.Text
const
  cBitmapExtension = '.bmp';
  cPNGExtension = '.png';
  cSupportedImageExtensionsCaption = '(jpg, jpeg, png, bmp)';
  cSupportedImageExtensions = '*.jpg;*.jpeg;*.png;*.bmp';
  cSupportedImageExtensionsArray: array[0..3] of string = ('*.jpg', '*.jpeg', '*.png', '*.bmp');
  // https://en.wikipedia.org/wiki/Golden_ratio
  cGoldenRatio = 1.61803398875; 

implementation

end.

