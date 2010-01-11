c*** GENCMC
c
      subroutine gencmc(ldbg)
      use chmstry
      use filunit
      use grid
      use pigsty
      use tracer
      use rtracchm
      use rtcmcchm
c
c----CAMx v5.10 090918
c
c-----------------------------------------------------------------------
c    Description:
c     Generate the rate and Jacobian terms needed by the RTRAC CMC 
c     solver
c
c    Copyright 1996-2007
c    ENVIRON International Corporation
c
c    Argument descriptions:
c     Inputs:
c       ldbg        L     flag to turn on diagnostic statements
c
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c
c    07/06/07   --gyarwood--    Original development
c
c-----------------------------------------------------------------------
c    Include files:
c-----------------------------------------------------------------------
c
      implicit none
      include "camx.prm"
c
c-----------------------------------------------------------------------
c    Argument declarations:
c-----------------------------------------------------------------------
c
      logical  ldbg
c
c-----------------------------------------------------------------------
c    External functions:
c-----------------------------------------------------------------------
c
      integer istrln
      integer idxspc
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      integer   i, j, k, l, n, icount, itmp, itm2, itm3
      logical   lok
      character*10   sptmp
      character*12   spctyp(4)
      real kdum(MXREACT,3), tdum(3), pdum(3)
c
c-----------------------------------------------------------------------
c    Data statments:
c-----------------------------------------------------------------------
c
      data spctyp  /'FAST        ',
     &              'SLOW        ',
     &              'EQUILIBRIUM ',
     &              'FIXED       '/
      data tdum    /  298.,  273., 273. /
      data pdum    / 1013., 1013., 500. /
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
c --- Report the mechanism
c
      if ( ldbg ) then
         if( mtype .EQ. 1) then
            write(idiag,'(/,A)') ' CAMx format reactions'
         elseif( mtype .EQ. 2) then
            write(idiag,'(/,A)') ' SCICHEM format reactions'
         endif
         do i = 1,nrxnrtc
            write(idiag,'(I3,A1,$)') i, ' '
            do j = 1,MXRCT
               if( j .LT. nrct(i) ) then
                  write(idiag,'(A,A2,$)') namrct(i,j), '+ '
               elseif( j .EQ. nrct(i) ) then
                  write(idiag,'(A,A2,$)') namrct(i,j), ' '
               else
                  write(idiag,'(A12,$)') ' '
               endif
            enddo
            write(idiag,'(A3,$)') '-> '
            do j = 1,nprd(i)
               if( DABS(prdcoef(i,j)) .NE. 1.0D0 ) then
                  write(idiag,'(F6.3,X,A,$)') 
     &                                    ABS(prdcoef(i,j)), namprd(i,j)
               else
                  write(idiag,'(A,$)') namprd(i,j)
               endif
               if( j .LT. nprd(i) ) then
                  if( prdcoef(i,j+1) .LT. 0.0D0 ) then
                     write(idiag,'(A2,$)') '- '
                  else
                     write(idiag,'(A2,$)') '+ '
                  endif
               endif
            enddo
            write(idiag,'(X)') 
c            write(idiag,'(A3,I2,1P20E12.3)') '; ',
c     &            MOD(ityprtc(i),100), (rkprmrtc(i,j),j=1,nrkprm(i))
         enddo
c
         do i = 1,nrxnrtc
            write(idiag,'(I3,A1,$)') i, ' '
            do j = 1,MXRCT
               if( j .LT. nrct(i) ) then
                  write(idiag,'(3A,A3,$)') '[',namrct(i,j),']', ' + '
               elseif( j .EQ. nrct(i) ) then
                  write(idiag,'(3A,A3,$)') '[',namrct(i,j),']', ' '
               else
                  write(idiag,'(A15,$)') ' '
               endif
            enddo
            write(idiag,'(A3,$)') '-> '
            do j = 1,nprd(i)
               if( prdcoef(i,j) .NE. 1.0D0 ) then
                  write(idiag,'(A,F6.3,A,X,3A,$)') 
     &                       '(',prdcoef(i,j),')','[',namprd(i,j),']'
               else
                  write(idiag,'(3A,$)') '[',namprd(i,j),']'
               endif
               if( j .LT. nprd(i) ) then
                  write(idiag,'(A3,$)') ' + '
               endif
            enddo
            write(idiag,'(X)') 
         enddo
c
         if( ktype .EQ. 1) then
            write(idiag,'(/,A)') ' CAMx format rate expressions'
         elseif( ktype .EQ. 2) then
            write(idiag,'(/,A)') ' SCICHEM format rate expressions'
         endif
c
         write(idiag,'(A)') ' Rxn Type  Parameters'
         do i = 1,nrxnrtc
            write(idiag,'(2I4,1P20E12.3)') 
     &          i, MOD(ityprtc(i),100), (rkprmrtc(i,j), j=1,nrkprm(i))
         enddo
      endif
c
c --- Check that any requested fixed species (type = 4) are available
c     Robust against ngas=0 or nrad=0
c
      do k = 1,ngasrtc
         lok = .false.
         if( itypsp(k) .LT. 1 .OR. itypsp(k) .GT. 4 ) then
            write(iout,'(//,3A,/,A)') ' Error in GENCMC:',
     &        ' RTRAC species: ', spnmrt(k),
     &        ' must be type 1 through 4 '
            call camxerr()
         endif
         if( itypsp(k) .NE. 4 ) lok = .true.
         if( spnmrt(k) .EQ. nam_M 
     &      .OR. spnmrt(k) .EQ. nam_O2 
     &        .OR. spnmrt(k) .EQ. nam_N2 
     &          .OR. spnmrt(k) .EQ. nam_H2O 
     &            .OR. spnmrt(k) .EQ. nam_H2 
     &              .OR. spnmrt(k) .EQ. nam_CH4 ) lok = .true.
         if( idxspc(spnmrt(k),spname,10,ngas) .NE. 0 ) lok = .true.
         if( idxspc(spnmrt(k),nmrad,10,nrad) .NE. 0 ) lok = .true.
         if( .NOT. lok ) then
            write(iout,'(//,3A,/,A)') ' Error in GENCMC:',
     &        ' RTRAC species: ', spnmrt(k),
     &        ' cannot be of type fixed concentration'      
            call camxerr()
         endif
      enddo
c
c --- Force any CAMx mechanism species to be fixed (type = 4)
c     and set pointers to locate concentrations for the fixed species
c
      do k = 1,ngasrtc
         idxfix(k) = -999
         if( spnmrt(k) .EQ. nam_M ) then
           idxfix(k) = 1
           itypsp(k) = 4
         elseif( spnmrt(k) .EQ. nam_O2 ) then
           idxfix(k) = 2
           itypsp(k) = 4
         elseif( spnmrt(k) .EQ. nam_N2 ) then
           idxfix(k) = 3
           itypsp(k) = 4
         elseif( spnmrt(k) .EQ. nam_H2O ) then
           idxfix(k) = 4
           itypsp(k) = 4
         elseif( spnmrt(k) .EQ. nam_H2 ) then
           idxfix(k) = 5
           itypsp(k) = 4
         elseif( spnmrt(k) .EQ. nam_CH4 ) then
           idxfix(k) = 6
           itypsp(k) = 4
         else
           itmp = 0
           if( ngas .GT. 0 ) itmp = idxspc(spnmrt(k),spname,10,ngas)
           if( itmp .NE. 0 ) then
              idxfix(k) = 6 + itmp
              itypsp(k) = 4
           endif
           itmp = 0
           if( nrad .GT. 0 ) itmp = idxspc(spnmrt(k),nmrad,10,nrad)
           if( itmp .NE. 0 ) then
              idxfix(k) = 6 + ngas + itmp
              itypsp(k) = 4
           endif
         endif
      enddo
c
c --- Sort species names by type: 1=fast, 2=slow, 3=eqm, 4=fixed
c     then count number of each type
c
      do l = 1,ngasrtc+1
         do k = 1,ngasrtc-1
            if( itypsp(k) .GT. itypsp(k+1)) then
               itmp = itypsp(k+1)
               itm2 = idxfix(k+1)
               sptmp = spnmrt(k+1)
               itypsp(k+1) = itypsp(k)
               idxfix(k+1) = idxfix(k)
               spnmrt(k+1) = spnmrt(k)
               itypsp(k) = itmp
               idxfix(k) = itm2
               spnmrt(k) = sptmp
            endif
         enddo
      enddo
      nfstrtc = 0
      nslortc = 0
      neqmrtc = 0
      nfixrtc = 0
      do k = 1,ngasrtc
         if( itypsp(k) .EQ. 1 ) nfstrtc = nfstrtc+1
         if( itypsp(k) .EQ. 2 ) nslortc = nslortc+1
         if( itypsp(k) .EQ. 3 ) neqmrtc = neqmrtc+1
         if( itypsp(k) .EQ. 4 ) nfixrtc = nfixrtc+1
      enddo
      if( nfstrtc .LT. 1 ) goto 8003
      if( nslortc .GT. MXSLO ) goto 8004
      if( neqmrtc .GT. MXEQM ) goto 8005
c
c --- Note: we later subtract NFIXRTC from NGASRTC
c
c --- Species diagnostics
c
      if ( ldbg ) then
         write(idiag, '(//,A)') ' Species sorted by GENCMC'
         write(idiag, '(/,A)')  ' Rxn  Name        Type        IdxFix'
         do i = 1, ngasrtc
            if( idxfix(i) .GT. 0 ) then
            write(idiag, '(I4,2X,A,2X,A,I6)') 
     &                      i, spnmrt(i), spctyp(itypsp(i)), idxfix(i)
            else
            write(idiag, '(I4,2X,A,2X,2A)') 
     &                      i, spnmrt(i), spctyp(itypsp(i)), '   N/A'
            endif
         enddo
         write(idiag, '(/,A,I4)') ' Number of fast species  = ', nfstrtc
         write(idiag, '(A,I4)')   ' Number of slow species  = ', nslortc
         write(idiag, '(A,I4)')   ' Number of eqm species   = ', neqmrtc
         write(idiag, '(A,I4,/)') ' Number of fixed species = ', nfixrtc
      endif
c
c --- Index the reactants and products
c
      do n = 1,nrxnrtc
         do k = 1, nrct(n)
            idxrct(n,k) = idxspc(namrct(n,k),spnmrt,10,ngasrtc)
         enddo
         do k = 1,nprd(n)
            idxprd(n,k) = idxspc(namprd(n,k),spnmrt,10,ngasrtc)
         enddo
      enddo
c
c --- Populate reaction data for fast species by counting and indexing 
c     the reactants and products
c
      do n = 1,nrxnrtc
         nrctfst(n) = 0
         if( nrct(n) .GT. 0 ) then
            do k = 1,nrct(n)
               if( idxrct(n,k) .LE. nfstrtc) then
                  nrctfst(n) = nrctfst(n)+1
                  idxrctfst(n,nrctfst(n)) = idxrct(n,k)
               endif
            enddo
         endif
         nprdfst(n) = 0
         if( nprd(n) .GT. 0 ) then
            do k = 1,nprd(n)
               if( idxprd(n,k) .LE. nfstrtc) then
                  nprdfst(n) = nprdfst(n)+1
                  idxprdfst(n,nprdfst(n)) = idxprd(n,k)
                  prdcofst(n,nprdfst(n)) = prdcoef(n,k)
               endif
            enddo
         endif
      enddo
c
c --- Populate Jacobian terms for fast species
c     for each reaction, first do reactant terms, then product terms
c
c --- Reactant terms
c
      njactrm = 0
      do n = 1,nrxnrtc
         if( nrctfst(n) .GT. 0 ) then
            do i = 1,nrctfst(n)
            do j = 1,nrctfst(n)
               njactrm = njactrm+1
               if( njactrm .GT. MXJACTRM-1 ) goto 8000
               ipd(njactrm) = idxrctfst(n,i)
               jpd(njactrm) = idxrctfst(n,j)
               idrxjac(njactrm) = n
               do k = 1,MXRCT-1
                  idspjac(njactrm,k) = MXTRSP + 1
               enddo
               nspjac(njactrm) = 0
               icount = 0
               do k = 1,nrct(n)
                  if( idxrct(n,k) .NE. idxrctfst(n,j) ) then
                     nspjac(njactrm) = nspjac(njactrm)+1
                     if( nspjac(njactrm) .GT. MXRCT-1 ) goto 8001
                     idspjac(njactrm,nspjac(njactrm)) = idxrct(n,k)
                  else
                     icount = icount+1
                     if( icount .GT. 1 ) then
                        nspjac(njactrm) = nspjac(njactrm)+1
                        if( nspjac(njactrm) .GT. MXRCT-1 ) goto 8001
                        idspjac(njactrm,nspjac(njactrm)) = idxrct(n,k)
                     endif
                  endif
               enddo
               coefjac(njactrm) = -1.0D0
c
c --- Look for an opportunity to combine two terms
c
               if( icount .EQ. 2 
     &            .AND. idrxjac(njactrm) .EQ. idrxjac(njactrm-1) 
     &              .AND. ipd(njactrm) .EQ. ipd(njactrm-1) 
     &                 .AND. jpd(njactrm) .EQ. jpd(njactrm-1) ) then
                  njactrm = njactrm - 1
                  coefjac(njactrm) = coefjac(njactrm) - 1.0D0 
               endif
            enddo
            enddo
         endif
c
c --- Product terms
c
         if( nprdfst(n) .GT. 0 .AND. nrctfst(n) .GT. 0 ) then
            do i = 1,nprdfst(n)
            do j = 1,nrctfst(n)
               njactrm = njactrm+1
               if( njactrm .GT. MXJACTRM-1 ) goto 8000
               ipd(njactrm) = idxprdfst(n,i)
               jpd(njactrm) = idxrctfst(n,j)
               idrxjac(njactrm) = n
               do k = 1,MXRCT-1
                  idspjac(njactrm,k) = MXTRSP + 1
               enddo
               nspjac(njactrm) = 0
               icount = 0
               do k = 1,nrct(n)
                  if( idxrct(n,k) .NE. idxrctfst(n,j) ) then
                     nspjac(njactrm) = nspjac(njactrm)+1
                     if( nspjac(njactrm) .GT. MXRCT-1 ) goto 8002
                     idspjac(njactrm,nspjac(njactrm)) = idxrct(n,k)
                  else
                     icount = icount+1
                     if( icount .GT. 1 ) then
                        nspjac(njactrm) = nspjac(njactrm)+1
                        if( nspjac(njactrm) .GT. MXRCT-1 ) goto 8002
                        idspjac(njactrm,nspjac(njactrm)) = idxrct(n,k)
                     endif
                  endif
               enddo
               coefjac(njactrm) = prdcofst(n,i)
c
c --- Look for an opportunity to combine two terms
c
               if( icount .EQ. 2 
     &            .AND. idrxjac(njactrm) .EQ. idrxjac(njactrm-1) 
     &              .AND. ipd(njactrm) .EQ. ipd(njactrm-1) 
     &                 .AND. jpd(njactrm) .EQ. jpd(njactrm-1) ) then
                  njactrm = njactrm - 1
                  coefjac(njactrm) = coefjac(njactrm) + prdcofst(n,i)
               endif
            enddo
            enddo
         endif
      enddo
c
c --- Sort Jacobian terms 
c        sort by pd(ipd,jpd) is easy to read for QA
c        sort by idspjac(k,1) may run faster 
c
      itm3 = njactrm+1
      do l = 1,njactrm+1
         do k = 1,njactrm-1
c            itmp = nfstrtc*ipd(k)+jpd(k)
c            itm2 = nfstrtc*ipd(k+1)+jpd(k+1)
            itm2 = idspjac(k+1,1)
            itmp = idspjac(k,1)
c            itm2 = k+1 ! defeat sort
c            itmp = k
            if( itmp .GT. itm2 ) then
               ipd(itm3) = ipd(k+1)
               jpd(itm3) = jpd(k+1)
               idrxjac(itm3) = idrxjac(k+1)
               nspjac(itm3) = nspjac(k+1)
               coefjac(itm3) = coefjac(k+1)
               ipd(k+1) = ipd(k)
               jpd(k+1) = jpd(k)
               idrxjac(k+1) = idrxjac(k)
               nspjac(k+1) = nspjac(k)
               coefjac(k+1) = coefjac(k)
               ipd(k) = ipd(itm3)
               jpd(k) = jpd(itm3)
               idrxjac(k) = idrxjac(itm3)
               nspjac(k) = nspjac(itm3)
               coefjac(k) = coefjac(itm3)
               do i = 1,MXRCT-1
                  idspjac(itm3,i) = idspjac(k+1,i)
                  idspjac(k+1,i) = idspjac(k,i)
                  idspjac(k,i) = idspjac(itm3,i)
               enddo
            endif
            scoefjac(k) = SNGL(coefjac(k))
         enddo
      enddo
c
      if( ldbg  .AND. ijac.EQ.2) then
         write(idiag,'(/,A,/)') ' Jacobian (pd) data:'
         itm2 = -1
         do i=1,njactrm
            itmp = nfstrtc*ipd(i)+jpd(i)
            if( itmp .NE. itm2 ) then
               itm2 = itmp
               write(idiag,'(5A,F6.3,A,I3,A,3(2A))')
     &         ' pd(',spnmrt(ipd(i)), ',', spnmrt(jpd(i)), ') = ',
     &         coefjac(i),'  * k(' , idrxjac(i),')',
     &         (' * ', spnmrt(idspjac(i,k)),k=1,nspjac(i))
c write FORTRAN
c               write(idiag,'(A,I4,A,I4,A,F6.3,A,I3,A,3(A,I4,A))')
c     &         '      pd(',ipd(i), ',', jpd(i), ') = ',
c     &         coefjac(i),'D0  * rkrtc(' , idrxjac(i),')',
c     &         (' * y(', (idspjac(i,k)),')',k=1,nspjac(i))
            else
               write(idiag,'(27X,A,F6.3,A,I3,A,3(2A))')
     &         '& + (',
     &         coefjac(i),') * k(' , idrxjac(i),')',
     &         (' * ', spnmrt(idspjac(i,k)),k=1,nspjac(i))
c write FORTRAN
c               write(idiag,'(5X,A,7X,A,F6.3,A,I3,A,3(A,I4,A))')
c     &         '&','+ (',
c     &         coefjac(i),'D0) * rkrtc(' , idrxjac(i),')',
c     &         (' * y(', (idspjac(i,k)),')',k=1,nspjac(i))
            endif
         enddo
         write(idiag, '(3(A,I5))') ' Number of Jacobian terms = ',
     &                          njactrm, ' (MXJACTRM = ',MXJACTRM,')'
      endif
c
c --- Subtract the number of fixed species from NGASRTC
c
      ngasrtc = ngasrtc - nfixrtc
c
c --- Populate reaction data for any slow species 
c
      if( nslortc .GT. 0 ) then
         do i = 1,nslortc
            idslo(i) = nfstrtc+i
            nslgain(i) = 0
            nslloss(i) = 0
            do n = 1,nrxnrtc
               if( nprd(n) .GT. 0 ) then
                  do k = 1,nprd(n)
                     if( idxprd(n,k) .EQ. idslo(i) ) then
                        nslgain(i) = nslgain(i)+1
                        islgain(i,nslgain(i)) = n
                        prdcoslo(i,nslgain(i)) = prdcoef(n,k)
                        spdcoslo(i,nslgain(i)) = SNGL(prdcoef(n,k))
                     endif
                  enddo
               endif
               if( nrct(n) .GT. 0 ) then
                  do k = 1,nrct(n)
                     if( idxrct(n,k) .EQ. idslo(i) ) then
                        nslloss(i) = nslloss(i)+1
                        islloss(i,nslloss(i)) = n
                     endif
                  enddo
               endif
            enddo
         enddo
      endif
c
      if ( ldbg .AND. nslortc .GT. 0 ) then
         write(idiag,'(//,A)') ' Slow species data:'
         do n = 1,nslortc
            write(idiag,'(/,A,I5,2X,2A)') 
     &               ' Species = ', n, ' Name = ', spnmrt(idslo(n))
            write(idiag,'(A,I5)') ' NRXLOSS = ', nslloss(n)
            write(idiag,'(10X,10I6)') (islloss(n,i),i=1,nslloss(n))
            write(idiag,'(A,I5)') ' NRXGAIN = ', nslgain(n)
            write(idiag,'(10X,10I6)') (islgain(n,i),i=1,nslgain(n))
            write(idiag,'(10X,10F6.3)') (prdcoslo(n,i),i=1,nslgain(n))
         enddo
      endif
c
c --- Populate reaction data for any equilibrium species 
c
      if( neqmrtc .GT. 0 ) then
         nrxupdt = 0
         do i = 1,neqmrtc
            ideqm(i) = nfstrtc+nslortc+i
            nrxgain(i) = 0
            nrxloss(i) = 0
            do n = 1,nrxnrtc
               if( nprd(n) .GT. 0 ) then
                  do k = 1,nprd(n)
                     if( idxprd(n,k) .EQ. ideqm(i) ) then
                        nrxgain(i) = nrxgain(i)+1
                        irxgain(i,nrxgain(i)) = n
                        prdcoeqm(i,nrxgain(i)) = prdcoef(n,k)
                        spdcoeqm(i,nrxgain(i)) = SNGL(prdcoef(n,k))
                     endif
                  enddo
               endif
               if( nrct(n) .GT. 0 ) then
                  itmp = 0
                  do k = 1,nrct(n)
                     if( idxrct(n,k) .EQ. ideqm(i) 
     &                                       .AND. itmp .EQ. 0 ) then
                        nrxloss(i) = nrxloss(i)+1
                        irxloss(i,nrxloss(i)) = n
                        itmp = 1
                        itm2 = 0
                        if( nrxupdt .EQ. 0 ) then
                           nrxupdt = 1
                           irxupdt(1) = irxloss(i,nrxloss(i))
                        else
                           do j = 1,nrxupdt
                              if( j .EQ. n ) itm2 = 1
                           enddo
                           if( itm2 .EQ. 0 ) then
                              nrxupdt = nrxupdt+1
                              irxupdt(nrxupdt) = irxloss(i,nrxloss(i))
                           endif
                        endif
                     endif
                  enddo
               endif
            enddo
         enddo
      endif
c
      if ( ldbg .AND. neqmrtc .GT. 0 ) then
         write(idiag,'(//,A)') ' Equilibrium species data:'
         do n = 1,neqmrtc
            write(idiag,'(/,A,I5,2X,2A)') 
     &               ' Species = ', n, ' Name = ', spnmrt(ideqm(n))
            write(idiag,'(A,I5)') ' NRXLOSS = ', nrxloss(n)
            write(idiag,'(10X,10I6)') (irxloss(n,i),i=1,nrxloss(n))
            write(idiag,'(A,I5)') ' NRXGAIN = ', nrxgain(n)
            write(idiag,'(10X,10I6)') (irxgain(n,i),i=1,nrxgain(n))
            write(idiag,'(10X,10F6.3)') (prdcoeqm(n,i),i=1,nrxgain(n))
         enddo
         write(idiag,'(/,A,I5)') ' NRXUPDT = ', nrxupdt
         write(idiag,'(11X,10I5)') (irxupdt(i),i=1,nrxupdt)
      endif
c
c --- Provide diagnostic info for checking rate expressions
c
      write(idiag,'(/,a,/,/,a)')
     &      ' Diagnostic info for checking RTCMC rate expressions',
     &      ' Rates at three temps and pressures in ppm-n min-1'
      do i=1,3
        if( ktype .EQ. 1 ) call krtc(tdum(i), pdum(i))
        if( ktype .EQ. 2 ) call ksci(tdum(i), pdum(i))
        do j=1,nrxnrtc
          kdum(j,i)=rkrtc(j)/60.0D0
        enddo
      enddo
      write(idiag,'(A,3F12.1)') ' Temp =  ', tdum
      write(idiag,'(A,3F12.1)') ' Pres =  ', pdum
      write(idiag,'(A)') ' Rxn Typ'
      write(idiag,'(2I4,1P3E12.4)')
     &     (j,MOD(ityprtc(j),100),kdum(j,1),
     &      kdum(j,2),kdum(j,3),j=1,nrxnrtc)
c
c --- Load original RTRAC species vectors
c
      nrtgas = ngasrtc
      nrtaero = 0
      ntotsp = ngasrtc
      nrtrac = ngasrtc
      lrtgas = .true.
      lrtaero = .false.
c
c  --- call routine to allocate the arrays ---
c
      call alloc_tracer_specs(ngrid,ncol,nrow,nlay,tectyp)
      call alloc_rtracchm(ngrid,ncol,nrow,nspec,ntotsp)
c
c  --- if doing sampling grids, allocate the arrays ---
c
      if( lsample ) 
     &      call alloc_tracer_sample(nsample,ncolsmp,nrowsmp,nrtsmpcels)
c
      write(idiag,'(A)') 'Final RTCMC gas species values'
      write(idiag,'(A,A)') '     Name       LowBnd(ppm) DryDep(m/s)',
     &                     ' WetDep(1/s)'
      do i = 1, ngasrtc
         ptname(i) = spnmrt(i)
         lsamap(i) = i
         lsagas(i) = .TRUE.
         loutsa(i) = .TRUE.
         rtlbnd(i) = BNDLPT
         do j = 1,ngasschm
           if (spnmrt(i).eq.spnmschm(j)) then
             dryrtc(i) = depvschm(j)
             wetrtc(i) = wetschm(j)
           endif
         enddo
         write(idiag,'(i4,1x,a10,3e12.3)') i,spnmrt(i),rtlbnd(i),
     &                                     dryrtc(i),wetrtc(i)
      enddo
      write(idiag,*)
c
c-----------------------------------------------------------------------
c    Return point:
c-----------------------------------------------------------------------
c
      return
c
c-----------------------------------------------------------------------
c    Error handling:
c-----------------------------------------------------------------------
c
 8000 write(iout,'(//,A,/)') ' ERROR in GENCMC:'
      write(iout,'(A,I4,/)') ' Processing reaction number',n
      write(iout,'(2(A,/),A,I8,A)') 
     &  ' Number of Jacobian terms for the requested RTRAC mechanism',
     &  ' exceeds the maximum set by the parameter MXJACTRM',
     &  ' (MXJACTRM = ',MXJACTRM,')',
     &  ' Increase MXJACTRM and re-compile CAMx.'
      call camxerr()
c
 8001 write(iout,'(//,A,/)') ' ERROR in GENCMC:'
      write(iout,'(A,I4,/)') ' Processing reaction number',n
      write(iout,'(A,/)') 
     &  ' Building Jacobian for the REACTANTS',
     &  ' nspjac(njactrm) > MXRCT-1, which should not happen'
      write(iout,'(A,2I4)') ' nspjac(njactrm) and MXRCT are: ', 
     &                                       nspjac(njactrm), MXRCT
      write(iout,'(A,3I4)') ' i, j, k values are: ', i, j, k
      call camxerr()
c
 8002 write(iout,'(//,A,/)') ' ERROR in GENCMC:'
      write(iout,'(A,I4,/)') ' Processing reaction number',n
      write(iout,'(A,/)') 
     &  ' Building Jacobian for the PRODUCTS',
     &  ' nspjac(njactrm) > MXRCT-1, which should not happen'
      write(iout,'(A,2I4)') ' nspjac(njactrm) and MXRCT are: ', 
     &                                       nspjac(njactrm), MXRCT
      write(iout,'(A,3I4)') ' i, j, k values are: ', i, j, k
c
 8003 write(iout,'(//,A,/)') ' ERROR in GENCMC:'
      write(iout,'(A)') ' There must be at least one fast species'
      call camxerr()
c
 8004 write(iout,'(//,A,/)') ' ERROR in GENCMC:'
      write(iout,'(A,I5)') 
     &   ' Increase the parameter MXSLO in RTCMC.COM to ', nslortc
      call camxerr()
c
 8005 write(iout,'(//,A,/)') ' ERROR in GENCMC:'
      write(iout,'(A,I5)') 
     &   ' Increase the parameter MXEQM in RTCMC.COM to ', neqmrtc
      call camxerr()
c
      end
