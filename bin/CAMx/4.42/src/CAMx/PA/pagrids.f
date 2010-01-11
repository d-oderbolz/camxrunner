      subroutine pagrids()
c
c
c----CAMx v4.42 070603
c
c     Calculates the affected grid cells for each Process Analysis sub-domain
c
c     Copyright 1996-2007
c     ENVIRON International Corporation
c
c     Modifications:
c        none
c
c     Input arguments:
c        none
c
c     Output arguments:
c        none
c
c     Subroutines Called:
c
c     Called by:
c        STARTUP
c
      include "camx.prm"
      include "filunit.com"
      include "grid.com"
      include "procan.com"
c
c-----Entry point
c
c
c-----write header for table of cells to the diag file ---
c
      write(idiag,'(//,15X,A,/)')
     &        '*** Model Cells Treated by Process Analysis ***'
c
c-----initialize flags for Process Analysis
c
      do i = 1,MXVEC3D
         ipacl_3d(i) = -9
      enddo
      do i = 1,MXVEC3D
         ipacl_3d(i) = -9
      enddo
c
      npa_cels = 0
      do ig = 1, npadom
         igrd = ipagrd(ig)
         write(idiag,'(5(5X,A))') 'Sub-Domain','Grid #','I-Cell',
     &                                           '  J-Cell','   Layer'
         write(idiag,'(3X,100A)') ('-',i=1,60)
         do j = j_sw(ig), j_ne(ig)
            do i = i_sw(ig), i_ne(ig)
               do k = b_lay(ig), t_lay(ig)
                  npa_cels  = npa_cels + 1
                  if( npa_cels .GT. MXPACEL ) goto 7000
                  ipax   ( npa_cels ) = i
                  ipay   ( npa_cels ) = j
                  ipaz   ( npa_cels ) = k
                  ipanst ( npa_cels ) = ipagrd(ig)
                  ipadom ( npa_cels ) = ig
c
c  --- calculate the offset for this grid ---
c
                  n2d = i + (j-1)*ncol(igrd)
                  ipacl_2d ( iptr2d(igrd)-1+n2d ) = npa_cels
                  n3d = i + ncol(igrd)*(j - 1) + 
     &                                ncol(igrd)*nrow(igrd)*(k - 1)
                  ipacl_3d ( iptr3d(igrd)-1+n3d ) = npa_cels
c
c  --- check to see if the cell is contained in any nest ---
c
                  if( idfin( iptr2d(igrd)-1+n2d ) .GT. 0 ) goto 7001
c
c  --- write this cell to the table in diag file ---
c
                 write(idiag,'(5X,I5,5(8X,I5))') ig, ipagrd(ig), i, j, k
               enddo
            enddo
         enddo
         write(idiag,'(3X,100A)') ('-',i=1,60)
      enddo
      write(idiag,*)
      goto 9999
c
c-----error messages----
c
 7000 continue 
      ncells = 0
      do i = 1, npadom
         ncells = ncells + (j_ne(i)-j_sw(i)+1) * 
     &               (i_ne(i)-i_sw(i)+1) * (t_lay(i)-b_lay(i)+1)
      enddo
      write(iout,'(//,a)') 'ERROR in PAGRIDS:'
      write(iout,'(1X,2A,I3)') 'Number of grid cells exceeds max ',
     &                            'for Process Analysis domain: ',ig
      write(iout,'(1X,A)') 
     &     'Increase the parameter MXPACEL in include file procan.com'
      write(iout,'(1X,A,I5)')
     &         'You need room for at least this many cells: ',ncells
      call camxerr()
c
 7001 continue
      write(iout,'(//,A)') 'ERROR in PAGRIDS:'
      write(iout,'(1X,2A)') 'Process Analysis domains cannot ',
     &                     'contain cells that are included in a nest.'
      write(iout,'(1X,A,I3)') 'Please redefine PA domain #',ig
      call camxerr()
c
 9999 continue
      return
      end
