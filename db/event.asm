!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!                                                                              !
!                               EVENT HANDLER MODULE                           !
!                                                                              !
! The event handler module gives the ability to get and execute event          !
! callbacks.                                                                   !
!                                                                              !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        jmp     event_end       ! skip over module

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! Get event vector
!
! Given a handler routine, returns the display/address pair needed to execute it.
! As with all callbacks, this must be a top level routine only.
!
!
! procedure getevt(procedure event(e: evttyp; var insaddr: integer; 
!                                  accaddr: integer; var data: integer; 
!                                  len: integer);
!                  var evtvec: evthan); external;
!
! Parameter to register assignments:
!
! event  -> eax -> address
!           ebx -> frame
! evtvec -> ecx -> var address
!
! Note: the frame should be checked for level 0, and an error generated if not,
! since frames lower than that cannot be called.
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

getevt:
        push    ecx             ! save evtvec address
        mov     [ecx],eax       ! place address
        add     ecx,4           ! next
        mov     [ecx],ebx       ! place display
        pop     ecx             ! restore evtvec address
        ret             

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! Execute event
!
! Executes an event handler based on its display/address pair.
!
!
! procedure exeevt(var evtvec: evthan; evt: evttyp; var insaddr: word; 
!                  accaddr: word; var data: word; len: word); external;
!
! Parameter to register assignments:
!
! evt     -> eax -> event type
! insaddr -> ebx -> var address of instruction address
! accaddr -> ecx -> access address
! data    -> edx -> var address of data
! len     -> esi -> length of operation
! evtrec  -> edi -> address of event vector
!
! Note: we don't use the frame address.
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

exeevt:
        jmpl    [edi]           ! go to handler
!
! End of module
!
event_end:
