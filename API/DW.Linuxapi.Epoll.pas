unit DW.Linuxapi.Epoll;

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

uses
  // Posix
  Posix.Base, Posix.Signal;

const
  EPOLLIN = $01;
  EPOLLPRI = $02;
  EPOLLOUT = $04;
  EPOLLERR = $08;
  EPOLLHUP = $10;
  EPOLLRDNORM = $40;
  EPOLLRDBAND = $80;
  EPOLLWRNORM = $100;
  EPOLLWRBAND = $200;
  EPOLLMSG = $400;
  EPOLLRDHUP = $2000;
  EPOLLWAKEUP = 1 shl 29;
  EPOLLONESHOT = 1 shl 30;
  EPOLLET  = UInt32(1 shl 31);

  EPOLL_CTL_ADD = 1;
  EPOLL_CTL_DEL = 2;
  EPOLL_CTL_MOD = 3;

type
  epoll_data = record
    case Integer of
      0: (ptr: Pointer);
      1: (fd: Integer);
      2: (u32: UInt32);
      3: (u64: UInt64);
  end;

  epoll_event = packed record
    events: UInt32;
    data : epoll_data;
  end;
  pepoll_event = ^epoll_event;

  ptsigset = ^sigset_t;

function epoll_create(size: Integer): Integer; cdecl;
  external libc name _PU + 'epoll_create';
function epoll_create1(flags: Integer): Integer; cdecl;
  external libc name _PU + 'epoll_create1';
function epoll_ctl(epfd: Integer; op: Integer; fd: Integer; event: pepoll_event): Integer; cdecl;
  external libc name _PU + 'epoll_ctl';
function epoll_pwait(epfd: Integer; events: pepoll_event; maxevents, timeout: Integer; sigmask: ptsigset): Integer; cdecl;
  external libc name _PU + 'epoll_pwait';
function epoll_wait(epfd: Integer; events: pepoll_event; maxevents, timeout: Integer): Integer; cdecl;
  external libc name _PU + 'epoll_wait';

implementation

end.
