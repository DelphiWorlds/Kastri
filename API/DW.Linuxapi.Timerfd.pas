unit DW.Linuxapi.Timerfd;

{*******************************************************}
{                                                       }
{                      Kastri                           }
{                                                       }
{         Delphi Worlds Cross-Platform Library          }
{                                                       }
{  Copyright 2020-2023 Dave Nottage under MIT license   }
{  which is located in the root folder of this library  }
{                                                       }
{*******************************************************}

{$I DW.GlobalDefines.inc}

interface

uses
  // Posix
  Posix.Fcntl, Posix.Base, Posix.Time;

const
  TFD_NONBLOCK = O_NONBLOCK;

function timerfd_create(clockid: Integer; flags: Integer): Integer; cdecl;
  external libc name _PU + 'timerfd_create';
function timerfd_settime(fd: Integer; flags: Integer; const new_value: Pitimerspec; old_value: Pitimerspec): Integer; cdecl;
  external libc name _PU + 'timerfd_settime';
function timerfd_gettime(fd: Integer; curr_value: Pitimerspec): Integer; cdecl;
  external libc name _PU + 'timerfd_gettime';

implementation

end.
