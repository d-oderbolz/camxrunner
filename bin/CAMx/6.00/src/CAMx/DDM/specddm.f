c**** SPECDDM
c
      subroutine specddm( )
      use filunit
      use chmstry
      use grid
      use tracer
c
c----CAMx v6.00 130506
c
c-----------------------------------------------------------------------
c    Description:
c-----------------------------------------------------------------------
c
c   This routine sets up the species names and pointers into the species
c   for all of the DDM species.  Pointers will be set up for both the
c   concentration array and the emissions array.
c
c     Copyright 1996 - 2013
c     ENVIRON International Corporation
c
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c
c     03/23/99   --gwilson--    Original development
c     10/03/03   --gwilson--    Added flag for gaseous species
c     07/16/07   --bkoo--       Revised for HDDM
c     06/11/08   --bkoo--       Added rate constant sensitivity
c     11/12/09   --gwilson--    Added initialization of factor for
c                               applying new type of top boundary
c
c-----------------------------------------------------------------------
c    Include files:
c-----------------------------------------------------------------------
c
      include 'camx.prm'
      include 'flags.inc'
c
c-----------------------------------------------------------------------
c    Argument declarations:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      character*10 caffec, cinflu, srcnam
      character*3  edgnam(5)
      integer      mvec4d, ispc, iaffec, iddm, mvecedge, nedge, i, l
      logical      lout, lsns
c
c-----------------------------------------------------------------------
c    Data statements:
c-----------------------------------------------------------------------
c
      data nedge /5/
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
c  --- load the names of the boundary edges ---
c
      edgnam(IDXBWS) = 'WST'
      edgnam(IDXBES) = 'EST'
      edgnam(IDXBST) = 'STH'
      edgnam(IDXBNT) = 'NTH'
      edgnam(IDXBTP) = 'TOP'
c
c  --- echo the table of species to the output file ---
c
      write(idiag,*) ' '
      write(idiag,*) ' Affected   Influencing   Source',
     &                   '                       Long            Short' 
      write(idiag,*) ' Species      Species      Type ',
     &                   '      Group   Region   Name            Name' 
      write(idiag,'(1X,79A)') ('-',i=1,79)
c
c  --- calculate the number of DDM families per model species ---
c
      if( lbndry ) then
          nbdic = 5 * nbcddm + nicddm
      else
          nbdic = nbcddm + nicddm
      endif

      nddmsp = nbdic + nemddm * nregin * ngroup + nrateddm + nhddm
c
c  --- calculate the number of DDM species needed and allocate
c      some arrays ---
c
      ntotsp = nspec * nddmsp
      
      lsns = .NOT. lmpi
      mvecedge = MAX(ncol(1),nrow(1))
      call alloc_ddm(lsns,ngrid,ncol,nrow,nlay,nspec,
     &                    nreact,nrateddm,nddmsp,nhddm,mvecedge)
c
c  --- loop over the modeled species and setup the names ---
c
      do ispc=1,ngas
c
c  --- set the pointer into the array for this family ---
c
          iptddm(ispc) = (ispc-1)*nddmsp + 1
          iaffec = ispc
          caffec = spname(ispc)
c
c  --- set the flag for determining if this species should
c      be output to average file ---
c
          lout = .FALSE.
          do i=1,navspc
            if( lavmap(i) .EQ. ispc ) lout = .TRUE.
          enddo
c
c  --- intialize the index into the array ---
c
          idxddm = iptddm(ispc) - 1
c
c  --- loop over all of the DDM initial condition species ---
c
          do iddm = 1,nicddm
             cinflu = icddmsp(iddm)
             srcnam = 'IC'
c
c  --- call routine to fill the family of species names ---
c
             call filspddm(iaffec,caffec,cinflu,
     &                                    srcnam,idxddm,0,0,lout,0.0)
          enddo
c
c  --- loop over all of the DDM boundary condition species ---
c
          do iddm = 1,nbcddm
             cinflu = bcddmsp(iddm)
c
c  --- if the stratify boundary is off, just fill the one species name ---
c
             if( .NOT. lbndry ) then
                srcnam = 'BCALL'
c
c  --- call routine to fill the family of species names ---
c
                call filspddm(iaffec,caffec,cinflu,
     &                                     srcnam,idxddm,0,0,lout,1.0)
c
c  --- otherwise, loop over all of the boundary edges ---
c
             else
                do i=1,nedge
                   srcnam = 'BC'//edgnam(i)(1:3)
c
c  --- call routine to fill the family of species names ---
c
                   call filspddm(iaffec,caffec,cinflu,
     &                                      srcnam,idxddm,0,0,lout,1.0)
                enddo
             endif
          enddo
c
c  --- loop over all of the DDM emissions condition species ---
c
          do iddm = 1,nemddm
             cinflu = emddmsp(iddm)
             srcnam = 'EM'
c
c  --- call routine to fill the family of species names ---
c
              call filspddm(iaffec,caffec,cinflu,srcnam,idxddm,
     &                                    MAX(1,ngroup),nregin,lout,0.0)
          enddo
c
c  --- loop over all of the rate constant sensitivity groups ---
c
          do iddm = 1,nrateddm
             cinflu = rateddm(iddm)
             srcnam = 'RATE'
c
c  --- call routine to fill the family of species names ---
c
             call filspddm(iaffec,caffec,cinflu,
     &                                    srcnam,idxddm,iddm,0,lout,0.0)
          enddo
c
c  --- loop over all of the HDDM sensitivity groups ---
c
          do iddm = 1,nhddm
             cinflu = ' '
             srcnam = 'HDDM'
c
c  --- call routine to fill the family of species names ---
c
             call filspddm(iaffec,caffec,cinflu,
     &                                    srcnam,idxddm,iddm,0,lout,0.0)
          enddo
c
c  --- get then next affect species ---
c
      enddo
      ntotsp = idxddm
      ipttim = ntotsp + 1
      nsaspc = ntotsp
      write(idiag,'(1X,79A)') ('-',i=1,79)
      write(idiag,*) 
c
c  --- set the flag for gaseous species ---
c
      lsagas = .FALSE.
      do i=iptddm(nrad+1),iptddm(ngas)+nddmsp-1
         lsagas(i) = .TRUE.
      enddo
c
c  --- initialize all of the tracers concs to zero to start off ---
c
      mvec4d = 0
      do i=1,ngrid
         mvec4d = mvec4d + ncol(i) * nrow(i) * nlay(i)
      enddo
      mvec4d = mvec4d * ntotsp
      do i=1,mvec4d
         ptconc(i) = 0.
      enddo
      do l=1,ntotsp
         do i=1,MXRECP
            conrcp(l,i) = 0.
         enddo
      enddo
c
c  --- return to calling routine ---
c
      goto 9999
c
c-----------------------------------------------------------------------
c    Error messages:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c    Return point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
