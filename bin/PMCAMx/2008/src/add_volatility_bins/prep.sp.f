      program prep
c
c PREP converts inputs in lumpup directory to those for PMCAMx with 43 size section
c    - Modified by jgj (02/01/06)
c    - Created by bkoo (03/01/03)
c    - Files converted : ptsr, lamb, init, bndr, topc
c    - Routine called  : bswap (bswap.cvt.f)
c    - Caution         : compilation option (Makefile)
c      
c     Adds 8 volatility bins to splitted emission, ic, bc and topconc files
c    - Modified by dco (02/17/09) - Read input and output files in a 
c    - AHOMAP style, can convert more than 1 file in one go
c
c  $Id$
c     
c  add_volatility_bins <<EOT
c  # of input files   | 3
c  Type (1=PT,2=AREA) | 1
c  Input File         | path/to/ifile/1
c  Output File        | path/to/ofile/1
c  Type (1=PT,2=AREA) | 1
c  Input File         | path/to/ifile/2
c  Output File        | path/to/ofile/2
c  Type (1=PT,2=AREA) | 5
c  Input File         | path/to/ifile/3
c  Output File        | path/to/ofile/3
c  EOT
c
c  Recognized file types:
c  1 - Point sources file
c  2 - Gridded Emisions
c  3 - Initial conditions?
c  4 - Boundary conditions
c  5 - Topconc

C This variable holds the unprocessed input parameters later
      character*200 inrec
       
C Input and output file descriptors
      data ifout,ifin /10,20/
       
      parameter(MXCOLA = 97)
      parameter(MXROWA = 90)
      parameter(MXLAYA = 16)
      parameter(MXSPEC = 189)
      parameter(MXPTSRC = 130000)
      parameter(MX1D = 97)
      parameter(naer = 13)
      parameter(ndate = 1)
      parameter(nsec1 = 40)
c      test
      character*120     fin,fout, inline
      character*3       date(ndate)
      character         spcchr
      character*10      spcname(MXSPEC)
      character*10      aernam(naer),topname
      character*10      tmpnam,tmpnam2,tmpnam3,tmpnam4

      character*166      fname1,fname2
c
      integer           len
      integer           k2, isp, n, l
      integer           i, j, k, m
      integer           isund, spcnum ! to convert aerosol species number
      integer           name(10),note(60),mspec(10,MXSPEC)
      character*4       lspec(10)
      integer           ione,nspec,ibdate,iedate,iutm,nx,ny,nz,idum
      integer           izero,nstk
      integer           iedge(4),ncell(4),icell(MX1D)
      integer           kcell
      integer           newspc(10,MXSPEC),tmpspc(10),newtot(5)
      integer           lmap(26,5)
      integer           newnsp,isecbeg,isecend
      integer           aerflag
c
      real        btime,etime
      real        rdum,xorg,yorg,delx,dely
      real        conc(MXCOLA,MXROWA,MXLAYA,MXSPEC),tcnc
      real        bcnc(MX1D,MXLAYA,4,MXSPEC)
      real        ecnc(MXCOLA,MXROWA,MXSPEC)
      real        flow(MXPTSRC),pcnc(MXPTSRC,MXSPEC)
      real        xstk(MXPTSRC),ystk(MXPTSRC),hstk(MXPTSRC)
      real        dstk(MXPTSRC),tstk(MXPTSRC),vstk(MXPTSRC)
      real        sum(MXPTSRC)
      real        sum2(MXCOLA,MXROWA)
      real        sum3(MXCOLA,MXROWA,MXLAYA)
      real        sum4(MX1D,MXLAYA,4)
      real        sum5
      real        sec(10,4)
      real        MW  !molecular weight
      
      MW = 250

c     NOTE: to alternate the following 2 lines you should modify additional lines below carefully!
c     Number of species per filetype
      data newtot /143, 143, 52, 52, 111/ ! if you merge OTHER into CRUST

c      These are the mass ratio that each volatility bin gets (after Table 1 of 
c      http://www.agu.org/journals/jd/jd0818/2007JD009735/ (Shrivastava, 2008)
c      one entry per species. 
c
c     The species we are looking at are (i is the size bin)
c     APO_i_1  (Lowest volatility bin, aerosol)
c     APO_i_2  (aerosol)
c     APO_i_3  (aerosol)
c     APO_i_4  (aerosol)
c     APO_i_5  (aerosol)
c     POC_i    (Primary Organic carbon)
c     CPO_i_1  (Lowest volatility bin, gasphase)
c     CPO_i_2  (gasphase)
c     CPO_i_3  (gasphase)
c     CPO_i_4  (gasphase)
c     CPO_i_5  (gasphase)

      data sec    /0.03,0.06,0.09,0.14,0.18,0.0,0.3,0.4,0.5,0.8,  ! IP=1
     &             0.03,0.06,0.09,0.14,0.18,0.0,0.3,0.4,0.5,0.8,  ! IP=2
     &             0.0,0.0,0.0,0.0,0.0,1.0,0.0,0.0,0.0,0.0,       ! IP=3
     &             0.0,0.0,0.0,0.0,0.0,1.0,0.0,0.0,0.0,0.0/       ! IP=4


C-- Read Parameters from STDIN. The first 20 characters are discarded (comments for readability)

       read(*,'(20x,a)') inrec
       read(inrec,*) nfiles
       write(*,*)'Number of files to be processed: ',nfiles

C-- Now all our processing will take place in this loop because each file is treated independently
      do ng = 1,nfiles 

         read(*,'(20x,a)') inrec
         read(inrec,*) ip
         write(*,*)'Filetype: ',ip

C        Read the input filename and open the file
         read(*,'(20x,a)') inrec
         open(ifin,file=inrec,form='unformatted',status='unknown')
         write(*,*)'Opened input file: ',inrec
         
C        Read the output filename and open the file
         read(*,'(20x,a)') inrec
         open(ifout,file=inrec,form='unformatted',status='unknown')
         write(*,*)'Opened output file: ',inrec


c 10   continue

c     Are we looking at a topconc file here?
      if(ip.eq.5) goto 950


c
c read & write header portion
c
      read(ifin) name,note,ione,nspec,ibdate,btime,iedate,etime
      write(*,*)nspec
      write(ifout) name,note,ione,newtot(ip),ibdate,btime,iedate,etime
c     read(ifin) rdum,rdum,iutm,xorg,yorg,delx,dely,nx,ny,nz,idum,
c    &idum,rdum,rdum,rdum
        read(ifin) xorg,yorg,rdum,rdum,iutm,delx,dely,nx,ny,nz
        write(ifout) xorg,yorg,rdum,rdum,iutm,delx,dely,nx,ny,nz
c      write(ifout) rdum,rdum,iutm,xorg,yorg,delx,dely,nx,ny,nz,
c    &idum,idum,rdum,rdum,rdum
      read(ifin) izero,izero,nx,ny
c      nx=97
      write(ifout) izero,izero,nx,ny
c
      read(ifin) ((mspec(n,l),n=1,10),l=1,nspec)
c
c convert species names
c
      if ((ip.eq.1).or.(ip.eq.2)) then
      newnsp=0
      write (6,*) nspec
       isecend = 10  
         write (tmpnam3,'(a3)') 'APO'
         write (tmpnam4,'(a3)') 'CPO'
      do l = 1,nspec
      
c 76 is size bin 1 asf.
          if((l.eq.76).or.(l.eq.77).or.(l.eq.78).or.(l.eq.79).or.
     &      (l.eq.80).or.(l.eq.81).or.(l.eq.82).or.(l.eq.83)) then

        if (l.eq.76) then !tmpnam3(1:3)1 
              do isec=1,10

            if (isec.le.5) then
                newnsp=newnsp+1
              write(tmpnam,'(i1,a1,i1)') isec, '_',1
                 tmpnam2 = tmpnam3(1:3)//tmpnam(1:3)
              write(6,*) 'species name', tmpnam2
                 else
                  end if

                  if (isec.eq.10) then
              newnsp=newnsp+1
              write(tmpnam,'(i2,a1,i1)') isec, '_',1
                tmpnam2 = tmpnam3(1:3)//tmpnam(1:4)
              write(6,*) 'species name', tmpnam2
                 else
                  end if

            read(tmpnam2,'(10a1)')(tmpspc(n),n=1,10)
            call bswap(tmpspc,10,0)

            do n = 1, 10
              newspc(n,newnsp) = tmpspc(n)
            end do   ! end for newspc line above
               end do     ! end of isec = 1, isecend

             else
       if (l.eq.77) then !tmpnam3(1:3)2
           do isec = 1, isecend
 
          if (isec.le.5) then
                newnsp=newnsp+1
              write(tmpnam,'(i1,a1,i1)') isec, '_',2
                 tmpnam2 = tmpnam3(1:3)//tmpnam(1:3)
              write(6,*) 'species name', tmpnam2
                 else
                  end if

                  if (isec.eq.10) then
              newnsp=newnsp+1
              write(tmpnam,'(i2,a1,i1)') isec, '_',2
                tmpnam2 = tmpnam3(1:3)//tmpnam(1:4)
              write(6,*) 'species name', tmpnam2
                 else
                  end if

            read(tmpnam2,'(10a1)')(tmpspc(n),n=1,10)
            call bswap(tmpspc,10,0)

            do n = 1, 10
              newspc(n,newnsp) = tmpspc(n)
            end do   ! end for newspc line above
               end do     ! end of isec = 1, isecend
 


           else
        if (l.eq.78) then !tmpnam3(1:3)3
           do isec = 1, isecend
             if (isec.le.5) then
                newnsp=newnsp+1
              write(tmpnam,'(i1,a1,i1)') isec, '_',3
                 tmpnam2 = tmpnam3(1:3)//tmpnam(1:3)
              write(6,*) 'species name', tmpnam2
                 else
                  end if

                  if (isec.eq.10) then
              newnsp=newnsp+1
              write(tmpnam,'(i2,a1,i1)') isec, '_',3
                tmpnam2 = tmpnam3(1:3)//tmpnam(1:4)
              write(6,*) 'species name', tmpnam2
                 else
                  end if

            read(tmpnam2,'(10a1)')(tmpspc(n),n=1,10)
            call bswap(tmpspc,10,0)

            do n = 1, 10
              newspc(n,newnsp) = tmpspc(n)
            end do   ! end for newspc line above
               end do     ! end of isec = 1, isecend



         else
        if (l.eq.79) then !tmpnam3(1:3)4
           do isec = 1, isecend
            if (isec.le.5) then
                newnsp=newnsp+1
              write(tmpnam,'(i1,a1,i1)') isec, '_',4
                 tmpnam2 = tmpnam3(1:3)//tmpnam(1:3)
              write(6,*) 'species name', tmpnam2
                 else
                  end if

                  if (isec.eq.10) then
              newnsp=newnsp+1
              write(tmpnam,'(i2,a1,i1)') isec, '_',4
                tmpnam2 = tmpnam3(1:3)//tmpnam(1:4)
              write(6,*) 'species name', tmpnam2
                 else
                  end if

            read(tmpnam2,'(10a1)')(tmpspc(n),n=1,10)
            call bswap(tmpspc,10,0)

            do n = 1, 10
              newspc(n,newnsp) = tmpspc(n)
            end do   ! end for newspc line above
               end do     ! end of isec = 1, isecend

         else
        if (l.eq.80) then !tmpnam3(1:3)5
           do isec = 1, isecend
             if (isec.le.5) then
                newnsp=newnsp+1
              write(tmpnam,'(i1,a1,i1)') isec, '_',5
                 tmpnam2 = tmpnam3(1:3)//tmpnam(1:3)
              write(6,*) 'species name', tmpnam2
                 else
                  end if

                  if (isec.eq.10) then
              newnsp=newnsp+1
              write(tmpnam,'(i2,a1,i1)') isec, '_',5
                tmpnam2 = tmpnam3(1:3)//tmpnam(1:4)
              write(6,*) 'species name', tmpnam2
                 else
                  end if

            read(tmpnam2,'(10a1)')(tmpspc(n),n=1,10)
            call bswap(tmpspc,10,0)

            do n = 1, 10
              newspc(n,newnsp) = tmpspc(n)
            end do   ! end for newspc line above
               end do     ! end of isec = 1, isecend

         else
        if (l.eq.81) then !tmpnam3(1:3)6
           do isec = 1, isecend
           if (isec.le.5) then
                newnsp=newnsp+1
              write(tmpnam,'(i1,a1,i1)') isec, '_',6
                 tmpnam2 = tmpnam3(1:3)//tmpnam(1:3)
              write(6,*) 'species name', tmpnam2
                 else
                  end if

                  if (isec.eq.10) then
              newnsp=newnsp+1
              write(tmpnam,'(i2,a1,i1)') isec, '_',6
                tmpnam2 = tmpnam3(1:3)//tmpnam(1:4)
              write(6,*) 'species name', tmpnam2
                 else
                  end if

            read(tmpnam2,'(10a1)')(tmpspc(n),n=1,10)
            call bswap(tmpspc,10,0)

            do n = 1, 10
              newspc(n,newnsp) = tmpspc(n)
            end do   ! end for newspc line above
               end do     ! end of isec = 1, isecend


       else
        if (l.eq.82) then !tmpnam3(1:3)7
           do isec = 1, 10
              if (isec.le.5) then
                newnsp=newnsp+1
              write(tmpnam,'(i1,a1,i1)') isec, '_',7
                 tmpnam2 = tmpnam3(1:3)//tmpnam(1:3)
              write(6,*) 'species name', tmpnam2
                 else
                  end if

                  if (isec.eq.10) then
              newnsp=newnsp+1
              write(tmpnam,'(i2,a1,i1)') isec, '_',7
                tmpnam2 = tmpnam3(1:3)//tmpnam(1:4)
              write(6,*) 'species name', tmpnam2
                 else
                  end if

            read(tmpnam2,'(10a1)')(tmpspc(n),n=1,10)
            call bswap(tmpspc,10,0)

            do n = 1, 10
              newspc(n,newnsp) = tmpspc(n)
            end do   ! end for newspc line above
               end do     ! end of isec = 1, isecend

           else
        if (l.eq.83) then !tmpnam3(1:3)8
           do isec = 1, 10

                if (isec.le.5) then
                newnsp=newnsp+1
c                 write(6,*)('isec is',isec)
              write(tmpnam,'(i1,a1,i1)') isec, '_',8
               tmpnam2 = tmpnam3(1:3)//tmpnam(1:3)
              write(6,*) 'species name', tmpnam2

                 else
                  end if

                  if (isec.eq.10) then
              newnsp=newnsp+1
c              write(6,*)('isec is',isec)
              write(tmpnam,'(i2,a1,i1)') isec, '_',8
               tmpnam2 = tmpnam3(1:3)//tmpnam(1:4)
              write(6,*) 'species name', tmpnam2
                 else
                  end if

              if ((isec.eq.6).or.(isec.eq.7).or.
     &           (isec.eq.8).or.(isec.eq.9)) then
      
                      newnsp=newnsp+1
c                         write(6,*)('isec is',isec)
                   write(tmpnam,'(i1)') isec
                  tmpnam2 = tmpnam4(1:3)//tmpnam(1:1)
                   write(6,*) 'species name', tmpnam2
                 else
                 end if

            read(tmpnam2,'(10a1)')(tmpspc(n),n=1,10)
            call bswap(tmpspc,10,0)

            do n = 1, 10
              newspc(n,newnsp) = tmpspc(n)
            end do   ! end for newspc line above
          end do     ! end of isec = 1, isecend


                 else
               end if                     !end of label 40
         end if  
           end if          
            end if
            end if
            end if
            end if
            end if          !from 76 8 ifs so 8 end ifs

          else    !check for species 76-83
               !for label 20
          newnsp = newnsp +1
          do n=1, 10
            newspc(n,newnsp) = mspec(n,l)
           end do
             end if
c           write(6,*)('species name', (mspec(n,l),n=1,10))
           end do      ! end of species loop
             write (6,*) 'newnsp is', newnsp
c
      if(newnsp.ne.newtot(ip)) stop'ERROR: species number mismatch.'
      write(ifout) ((newspc(n,l),n=1,10),l=1,newnsp)
c      write(6,*) ((newspc(n,l),n=1,10),l=1,newnsp)
c
         end if ! if ip=1 or ip=2
      if(ip.eq.1) then ! point source emissions
         read(ifin) ione,nstk
         write(ifout) ione,nstk
         read(ifin) (xstk(n),ystk(n),hstk(n),dstk(n),tstk(n),vstk(n),
     &               n=1,nstk)
         write(ifout) (xstk(n),ystk(n),hstk(n),dstk(n),tstk(n),
     &                 vstk(n),n=1,nstk)
      end if
c
      if(ip.eq.4) then ! boundary conditions
         do ib = 1, 4
            read(ifin) ione,iedge(ib),ncell(ib),(icell(n),idum,idum,
     &                 idum,n=1,ncell(ib))
            write(ifout) ione,iedge(ib),ncell(ib),(icell(n),idum,idum,
     &                   idum,n=1,ncell(ib))
         end do
      end if
c
c read & write time-variant portion
c
 100  read(ifin,end=900) ibdate,btime,iedate,etime
      write(ifout) ibdate,btime,iedate,etime
c
      newnsp = 0
c
      if(ip.eq.1) then
        read(ifin) ione,nstk
        write(ifout) ione,nstk
        read(ifin) (idum,idum,kcell,flow(n),rdum,n=1,nstk)
        write(ifout) (idum,idum,kcell,flow(n),rdum,n=1,nstk)
        do l = 1, nspec
          read(ifin) ione,(lspec(n),n=1,10),(pcnc(i,l),i=1,nstk)
            if (l.eq.76) then
              do isec =1,6
             newnsp = newnsp +1
              write(ifout) ione,(lspec(n),n=1,10),
     &                     (pcnc(i,l)*sec(isec,ip),i=1,nstk) !for OC aerosol g/hr
              end do
               
          else
             if (l.eq.77) then
              do isec =1,6
             newnsp = newnsp +1
              write(ifout) ione,(lspec(n),n=1,10),
     &                     (pcnc(i,l)*sec(isec,ip),i=1,nstk) !for OC aerosol g/hr
              end do
               
            else
             if (l.eq.78) then
              do isec =1,6
             newnsp = newnsp +1
              write(ifout) ione,(lspec(n),n=1,10),
     &                     (pcnc(i,l)*sec(isec,ip),i=1,nstk) !for OC aerosol g/hr
              end do
               

             else
             if (l.eq.79) then
              do isec =1,6
             newnsp = newnsp +1
              write(ifout) ione,(lspec(n),n=1,10),
     &                     (pcnc(i,l)*sec(isec,ip),i=1,nstk) !for OC aerosol g/hr
              end do
               

            else
             if (l.eq.80) then
              do isec =1,6
             newnsp = newnsp +1
              write(ifout) ione,(lspec(n),n=1,10),
     &                     (pcnc(i,l)*sec(isec,ip),i=1,nstk) !for OC aerosol g/hr
              end do
               
          
              else
             if (l.eq.81) then
              do isec =1,6
             newnsp = newnsp +1
              write(ifout) ione,(lspec(n),n=1,10),
     &                     (pcnc(i,l)*sec(isec,ip),i=1,nstk) !for OC aerosol g/hr
              end do
               

                else
             if (l.eq.82) then
              do isec =1,6
             newnsp = newnsp +1
              write(ifout) ione,(lspec(n),n=1,10),
     &                     (pcnc(i,l)*sec(isec,ip),i=1,nstk) !for OC aerosol g/hr
              end do
               
         
            else
             if (l.eq.83) then
              do isec =1,5
             newnsp = newnsp +1
              write(ifout) ione,(lspec(n),n=1,10),
     &                    (pcnc(i,l)*sec(isec,ip),i=1,nstk) !for OC aerosol g/hr
              end do
                do isec=7,10
              newnsp=newnsp+1
               write(ifout) ione,(lspec(n),n=1,10),
     &                     ((pcnc(i,76)+pcnc(i,77)+pcnc(i,78)+pcnc(i,79)
     &                     +pcnc(i,80)+pcnc(i,81)+pcnc(i,82)+pcnc(i,83)
     &                     )*1/MW*sec(isec,ip),i=1,nstk)
             end do
              do isec = 6,6
              newnsp = newnsp +1
              write(ifout) ione,(lspec(n),n=1,10),
     &           (pcnc(i,l)*sec(isec,ip),i=1,nstk) !for OC aerosol g/hr
              end do

               
          else
            newnsp = newnsp + 1
            write(ifout) ione,(lspec(n),n=1,10),(pcnc(i,l),i=1,nstk)
          end if
          end if
          end if
         end if
         end if
         end if
         end if
         end if

        end do
      if(newnsp.ne.newtot(ip)) stop'ERROR: species number mismatch.'
      end if
c
c
      if(ip.eq.2) then
        do l = 1, nspec
          read(ifin) ione,(lspec(n),n=1,10),
     &              ((ecnc(i,j,l),i=1,nx),j=1,ny)

        if (l.eq.76) then
              do isec =1,6
             newnsp = newnsp +1
              write(ifout) ione,(lspec(n),n=1,10),
     &                     ((ecnc(i,j,l)*sec(isec,ip),i=1,nx),j=1,ny) !for OC aerosol g/hr
              end do

          else
             if (l.eq.77) then
              do isec =1,6
             newnsp = newnsp +1
              write(ifout) ione,(lspec(n),n=1,10),
     &                     ((ecnc(i,j,l)*sec(isec,ip),i=1,nx),j=1,ny) !for OC aerosol g/hr
              end do

            else
             if (l.eq.78) then
              do isec =1,6
             newnsp = newnsp +1
              write(ifout) ione,(lspec(n),n=1,10),
     &                    ((ecnc(i,j,l)*sec(isec,ip),i=1,nx),j=1,ny) !for OC aerosol g/hr
              end do


             else
             if (l.eq.79) then
              do isec =1,6
             newnsp = newnsp +1
              write(ifout) ione,(lspec(n),n=1,10),
     &                    ((ecnc(i,j,l)*sec(isec,ip),i=1,nx),j=1,ny) !for OC aerosol g/hr
              end do


            else
             if (l.eq.80) then
              do isec =1,6
             newnsp = newnsp +1
              write(ifout) ione,(lspec(n),n=1,10),
     &                    ((ecnc(i,j,l)*sec(isec,ip),i=1,nx),j=1,ny) !for OC aerosol g/hr
              end do


              else
             if (l.eq.81) then
              do isec =1,6
             newnsp = newnsp +1
              write(ifout) ione,(lspec(n),n=1,10),
     &                     ((ecnc(i,j,l)*sec(isec,ip),i=1,nx),j=1,ny) !for OC aerosol g/hr
              end do

              else
             if (l.eq.82) then
              do isec =1,6
             newnsp = newnsp +1
              write(ifout) ione,(lspec(n),n=1,10),
     &                     ((ecnc(i,j,l)*sec(isec,ip),i=1,nx),j=1,ny) !for OC aerosol g/hr
              end do

c
            else
             if (l.eq.83) then
              do isec =1,5
             newnsp = newnsp +1
              write(ifout) ione,(lspec(n),n=1,10),
     &                     ((ecnc(i,j,l)*sec(isec,ip),i=1,nx),j=1,ny) !for OC aerosol g/hr
              end do
                do isec=7,10
              newnsp=newnsp+1
               write(ifout) ione,(lspec(n),n=1,10),
     &            (((ecnc(i,j,76)+ecnc(i,j,77)+ecnc(i,j,78)+ecnc(i,j,79)
     &             +ecnc(i,j,80)+ecnc(i,j,81)+ecnc(i,j,82)+ecnc(i,j,83)
     &             )*1/MW*sec(isec,ip),i=1,nx),j=1,ny)
             end do
            do isec =6,6
             newnsp = newnsp +1
              write(ifout) ione,(lspec(n),n=1,10),
     &                     ((ecnc(i,j,l)*sec(isec,ip),i=1,nx),j=1,ny) !for OC aerosol g/hr
                end do
              
          else
            newnsp = newnsp + 1
            write(ifout) ione,(lspec(n),n=1,10),
     &                   ((ecnc(i,j,l),i=1,nx),j=1,ny)
          end if
          end if
          end if
         end if
         end if
         end if
         end if
         end if
      
         end do

c      if(newnsp.ne.newtot(ip)) stop'ERROR: species number mismatch.'
       end if
c
c maybe intital conditions?
      if(ip.eq.3) then
         write (tmpnam3,'(a3)') 'POA'
         write (tmpnam4,'(a3)') 'CGP'
             isecend=10
           newnsp = 0
        do l = 1, nspec
          do k = 1, nz
            read(ifin) ione,(lspec(n),n=1,10),((conc(i,j,k,l),i=1,nx),
     &      j=1,ny)
          end do

           
c      write (6,*) nspec
c      do l = 1,nspec
          if((l.eq.15).or.(l.eq.16).or.(l.eq.17).or.(l.eq.18).or.
     &    (l.eq.19).or.(l.eq.20)) then

        if (l.eq.15) then !tmpnam3(1:3)1

                
              newnsp=newnsp+1
              write(tmpnam,'(i1,a1,i2)') 1, '_',isecend
                tmpnam2 = tmpnam3(1:3)//tmpnam(1:4)
              write(6,*) 'species name', tmpnam2
          else
           if (l.eq.16) then
              newnsp=newnsp+1
              write(tmpnam,'(i1,a1,i2)') 2, '_',isecend
                tmpnam2 = tmpnam3(1:3)//tmpnam(1:4)
              write(6,*) 'species name', tmpnam2
          else
             if(l.eq.17) then
            newnsp=newnsp+1
              write(tmpnam,'(i1,a1,i2)') 3, '_',isecend
                tmpnam2 = tmpnam3(1:3)//tmpnam(1:4)
              write(6,*) 'species name', tmpnam2
           else
             if(l.eq.18) then
             newnsp=newnsp+1
              write(tmpnam,'(i1,a1,i2)') 4, '_',isecend
                tmpnam2 = tmpnam3(1:3)//tmpnam(1:4)
              write(6,*) 'species name', tmpnam2
            else
             if(l.eq.19) then
                newnsp=newnsp+1
              write(tmpnam,'(i1,a1,i2)') 5, '_',isecend
                tmpnam2 = tmpnam3(1:3)//tmpnam(1:4)
              write(6,*) 'species name', tmpnam2
           else
            if(l.eq.20) then
                newnsp=newnsp+1
              write(tmpnam,'(i1,a1,i2)') 6, '_',isecend
                tmpnam2 = tmpnam3(1:3)//tmpnam(1:4)
              write(6,*) 'species name', tmpnam2
             
              else
             end if
             end if
             end if
             end if
               end if
                end if

            read(tmpnam2,'(10a1)')(tmpspc(n),n=1,10)
            call bswap(tmpspc,10,0)

            do n = 1, 10
              newspc(n,newnsp) = tmpspc(n)
            end do   ! end for newspc line above

               else ! if l is not between 15-20
           newnsp = newnsp +1
          do n=1, 10
            newspc(n,newnsp) = mspec(n,l)
           end do
             end if
c           write(6,*)('species name', (mspec(n,l),n=1,10))
           end do      ! end of species loop
             write (6,*) 'newnsp after naming step is', newnsp
             if(newnsp.ne.newtot(ip)) 
     &       stop'ERROR: species number mismatch.'
      
             write(ifout) ((newspc(n,l),n=1,10),l=1,newnsp)
               
             newnsp=0.0       
             do l=1,nspec

             newnsp=newnsp+1
             do k = 1, nz
                write(ifout) ione,(lspec(n),n=1,10),
     &          ((conc(i,j,k,l),i=1,nx),j=1,ny)
 
                      
              end do !k
             end do !nspec
             write (6,*) 'newnsp after addition  step is', newnsp
             
      if(newnsp.ne.newtot(ip)) stop 'ERROR: species number mismatch.'
      end if
c
      if(ip.eq.4) then
        do l = 1, nspec
          do ib = 1, 4
            read(ifin) ione,(lspec(n),n=1,10),iedge(ib),
     &     ((bcnc(i,k,ib,l),k=1,nz),i=1,ncell(ib))
          end do
          if(lmap(l,ip).ne.0) then
            isecend = int(sec(nsec1+1,lmap(l,ip)))
         if(lmap(l,ip).eq.4) then
           
                 do isec=1,isecend-6
              newnsp=newnsp+1
              do ib = 1, 4
                write(ifout) ione,(lspec(n),n=1,10),iedge(ib),
     &         ((bcnc(i,k,ib,l)*0.0*sec(isec,lmap(l,ip)),k=1,nz),       !Put  zero OC for all other bins
     &         i=1,ncell(ib)) 
              end do !ib
               end do !isec

          do isec = isecend-5, isecend
              newnsp = newnsp +1
              do ib = 1, 4
                  write(ifout) ione,(lspec(n),n=1,10),iedge(ib),
     &        ((bcnc(i,k,ib,l)*0.0+0.16666667,k=1,nz),i=1,ncell(ib))
   
              end do
            end do

            else   
            do isec = 1, isecend
              newnsp = newnsp +1
              do ib = 1, 4
                write(ifout) ione,(lspec(n),n=1,10),iedge(ib),
     &          ((bcnc(i,k,ib,l)*sec(isec,lmap(l,ip)),k=1,nz),
     &          i=1,ncell(ib))   !for all other aerosol species
              end do
            end do
          end if
          else !lmap(l,ip=0)
            newnsp = newnsp + 1
            do ib = 1, 4
              write(ifout) ione,(lspec(n),n=1,10),iedge(ib),
     &        ((bcnc(i,k,ib,l),k=1,nz),i=1,ncell(ib))!for non-size sectionated species
            end do
          end if
        end do
      if(newnsp.ne.newtot(ip)) stop'ERROR: species number mismatch.'
      end if
c
      goto 100
 900  write(6,*)'END of INPUT FILE.'
 
      close(ifin)
      close(ifout)
 
      end do ! File loop
 
      goto 999
      
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c ascii file - top concentration input file
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

 950  newnsp = 0

      do l = 1, 26
        read(ifin,'(a10,f10.9)') topname,tcnc
         if(lmap(l,ip).ne.0)then
 120      if(lmap(l,ip).eq.4)then
          isecend = int(sec(nsec1+1,lmap(l,ip)))  
         isecbeg = 0
 130     do isec = 1, isecend
            newnsp = newnsp + 1
            len = INDEX(aernam(lmap(l,ip)),' ')
 140  if ((isec+isecbeg).le.6) then
              write(tmpnam,'(i1,a1,i1)') 1, '_',isec + isecbeg
              topname = aernam(lmap(l,ip))(1:len-1)//tmpnam(1:3)
              write(6,*) 'species name',topname
      else
 150     if ((isec+isecbeg).lt.13) then
              write(tmpnam,'(i1,a1,i1)') 2, '_',isec + isecbeg-6
              topname = aernam(lmap(l,ip))(1:len-1)//tmpnam(1:3)
              write(6,*) 'species name',topname
      else
         if ((isec+isecbeg).lt.19) then
              write(tmpnam,'(i1,a1,i1)') 3, '_',isec + isecbeg-12
              topname = aernam(lmap(l,ip))(1:len-1)//tmpnam(1:3)
              write(6,*) 'species name',topname
      else
         if ((isec+isecbeg).lt.25) then
              write(tmpnam,'(i1,a1,i1)') 4, '_',isec + isecbeg-18
              topname = aernam(lmap(l,ip))(1:len-1)//tmpnam(1:3)
              write(6,*) 'species name',topname
      else
         if ((isec+isecbeg).lt.31) then
              write(tmpnam,'(i1,a1,i1)') 5, '_',isec + isecbeg-24
              topname = aernam(lmap(l,ip))(1:len-1)//tmpnam(1:3)
              write(6,*) 'species name',topname
         else
          if ((isec+isecbeg).lt.32) then
              write(tmpnam,'(a4)') 'CGP6' 
              topname = tmpnam(1:4)
              write(6,*) 'species name',topname
        else
          if ((isec+isecbeg).lt.33) then
              write(tmpnam,'(a4)') 'CGP7' 
              topname = tmpnam(1:4)
              write(6,*) 'species name',topname
        else
          if ((isec+isecbeg).lt.34) then
              write(tmpnam,'(a4)') 'CGP8' 
              topname = tmpnam(1:4)
              write(6,*) 'species name',topname
       else
          if ((isec+isecbeg).lt.35) then
              write(tmpnam,'(a4)') 'CGP9' 
              topname = tmpnam(1:4)
              write(6,*) 'species name',topname
         else
           if ((isec+isecbeg).lt.41) then
              write(tmpnam,'(i2,a1,i1)') 10, '_',isec + isecbeg-34
              topname = aernam(lmap(l,ip))(1:len-1)//tmpnam(1:4)
              write(6,*) 'species name',topname
            
               end if                !end of label 50  
            end if
          end if                    !end of label 40
           end if
          end if 
            end if
           end if
           end if
           end if
           end if !from 140 10 ifs so 10 end ifs
            write(ifout,'(a10,f10.9)') topname,
     &      tcnc*sec(isec,lmap(l,ip))
            
          end do     ! end of isec = 1, isecend


       else                     !for label 20 lmap(l,ip).ne.4
         isecend = int(sec(nsec1+1,lmap(l,ip))) 
          if(isecend.eq.2)then
            isecbeg = 6
          else
            isecbeg = 0
         end if
 160     do isec = 1, isecend
            newnsp = newnsp + 1
            len = INDEX(aernam(lmap(l,ip)),' ')
 170        if ((isec+isecbeg).lt.10) then
              write(tmpnam,'(a1,i1)') '_',isec + isecbeg
              topname = aernam(lmap(l,ip))(1:len-1)//tmpnam(1:2)
              write(6,*) 'species name',topname
          else  
          end if      !end of label 70
            
            write(ifout,'(a10,f10.9)') topname,
     &      tcnc*sec(isec,lmap(l,ip))
          end do     ! end of isec = 1, isecend
      end if       !for label 20, lmap(l,ip eq.4)
         end if ! if lmap(l,ip.ne.0)
       if(lmap(l,ip).eq.0)then      !if lmap(l,ip)equals zero
          newnsp = newnsp +1
          write(ifout,'(a10,f10.9)') topname, tcnc

        end if  !end of  if to check if lmap(l,ip).eq.0

      end do      ! end of species loop


      write(*,*)'newnsp=',newnsp,'newtot(ip)=',newtot(ip)
      if(newnsp.ne.newtot(ip)) stop'ERROR: species number mismatch.'
c
 990  close(ifin)
      close(ifout)
 
c
 999  continue
c

      end
