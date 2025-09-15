{*******************************************************************************
*                                                                              *
*                               EVENT HANDLER MODULE                           *
*                                                                              *
* The event handler module gives the ability to get and execute event          *
* callbacks. It is implemented in assembly language. This is the descriptor    *
* header.                                                                      *
*                                                                              *
*******************************************************************************}

module event;

uses simi8080; { 8080 simulator package }

type

   { Record to hold the callback vector. A callback must have a display and
     an address. }
   evthan = record display, addr: integer end;

{*******************************************************************************

Get event vector

Given a handler routine, returns the display/address pair needed to execute it.
As with all callbacks, this must be a top level routine only.

*******************************************************************************}

procedure getevt(procedure event(e: evttyp; var insaddr: integer; 
                                 accaddr: integer; var data: integer; 
                                 len: integer);
                 var evtvec: evthan); external;

{*******************************************************************************

Execute event

Executes an event handler based on its display/address pair.

*******************************************************************************}

procedure exeevt(evt: evttyp; var insaddr: integer; accaddr: integer; 
                 var data: integer; len: word; var evtvec: evthan); external;

begin
end.
