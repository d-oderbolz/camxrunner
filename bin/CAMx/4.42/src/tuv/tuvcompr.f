      PROGRAM tuvcompr
c
c compare two CAMx look-up tables
c Greg Yarwood, 6/4/99 - gyarwood@envion.org
c
       parameter (nozn=5,nalb=5,nhaze=3,nhght=11,nzen=10,mxrxn=19)
       real   rk1(nzen,mxrxn,nhght,nhaze,nalb,nozn)
       real   rk2(nzen,mxrxn,nhght,nhaze,nalb,nozn)
       real   camxozn(nzen), camxhaze(nhaze) 
       real   camxalb(nalb)
       real   z(nhght)
       real   rxn1(mxrxn,nzen), rxn2(mxrxn,nzen)
       character*180  fname
       character*20  zlabel
       character*12  oznlab
       character*8   alblab
       character*11  hazelab

 
c** ENTRY POINT
 
       write (*, *) 'Calculate ratio of 2 camx.rate files: file2/file1'
       write (*, *) 
       write (*, *) 'Name of input file 1'
       read (*, '(20x,A)') fname
       write (6, '(A)') fname
       open (10, file = fname, status="old")

       write (*, *) 'Name of input file 2'
       read (*, '(20x,A)') fname
       write (*, '(A)') fname
       open (11, file = fname, status="old")

       write (*, *) 'Name of output file'
       read (*, '(20x,A)') fname
       write (*, '(A)') fname
       open (20, file = fname, status="unknown")

       write (*, '(A)') 'Number of photo reactions on files'
       read (*, '(20x,i10)') nrxn
       write (*, *) nrxn


c** read file 1
 
      do iozn = 1,nozn
        do ialb = 1,nalb
          do ihaze = 1,nhaze
            read(10,'(a12,f7.3,a8,f7.3,a11,f7.3)') 
     &          oznlab,camxozn(iozn),alblab,camxalb(ialb),
     &          hazelab,camxhaze(ihaze)
c            write(*,'(a12,f7.3,a8,f7.3,a11,f7.3)') 
c     &          oznlab,camxozn(iozn),alblab,camxalb(ialb),
c     &          hazelab,camxhaze(ihaze)
            do ihght = 1,nhght
              read(10,'(f7.3,a20)') z(ihght), zlabel
              do irxn = 1,nrxn
                read(10,'(10(E12.0))') 
     &              (rk1(izen,irxn,ihght,ihaze,ialb,iozn),
     &              izen=1,nzen)
c                write(*,'(10(E12.0))') 
c     &              (rk1(izen,irxn,ihght,ihaze,ialb,iozn),
c     &              izen=1,nzen)
              enddo
            enddo
          enddo
        enddo
      enddo


c** read file 2
 
      do iozn = 1,nozn
        do ialb = 1,nalb
          do ihaze = 1,nhaze
            read(11,'(a12,f7.3,a8,f7.3,a11,f7.3)') 
     &          oznlab,camxozn(iozn),alblab,camxalb(ialb),
     &          hazelab,camxhaze(ihaze)
            do ihght = 1,nhght
              read(11,'(f7.3,a20)') z(ihght), zlabel
              do irxn = 1,nrxn
                read(11,'(10(E12.0))') 
     &              (rk2(izen,irxn,ihght,ihaze,ialb,iozn),
     &              izen=1,nzen)
              enddo
            enddo
          enddo
        enddo
      enddo

c
c** write an output file for analysis e.g., Excel Pivot Table

      write(20,'(a15,10f10.0)')
     &   'oz al hz ht rx ',
     &   0., 10., 20., 30., 40., 50., 60.,70., 78., 86.
      do iozn = 1,nozn
        do ialb = 1,nalb
          do ihaze = 1,nhaze
            do ihght = 1,nhght
              do irxn = 1,nrxn
                write(20,'(5i3,10(1PE10.3))') 
     &              iozn,ialb,ihaze,ihght,irxn,
     &              (rk2(izen,irxn,ihght,ihaze,ialb,iozn)
     &               /rk1(izen,irxn,ihght,ihaze,ialb,iozn),
     &              izen=1,nzen)
              enddo
            enddo
          enddo
        enddo
      enddo

c
c** print a highly condensed summary to the screen

      do irxn = 1,nrxn
        do izen = 1,nzen
          rxn1(irxn,izen)=0.0
          rxn2(irxn,izen)=0.0
        enddo
      enddo

      do izen = 1,nzen
        do irxn = 1,nrxn
          do ihght = 1,nhght
            do ihaze = 1,nhaze
              do ialb = 1,nalb
                do iozn = 1,nozn
                  rxn1(irxn,izen) = rxn1(irxn,izen) 
     &               + rk1(izen,irxn,ihght,ihaze,ialb,iozn)
                  rxn2(irxn,izen) = rxn2(irxn,izen) 
     &               + rk2(izen,irxn,ihght,ihaze,ialb,iozn)
                enddo
              enddo
            enddo
          enddo
        enddo
      enddo

      write(*,*)'Ratio (file2/file1) of average photolysis rates'
      write(*,*)
      write(*,*)' Zenith angle of   20     60     86 degrees'
      do irxn = 1,nrxn
        write(6,'(A,i2,A,3f7.3)') '  Rxn ', irxn, ' ratio =', 
     &    rxn2(irxn,3)/rxn1(irxn,3), rxn2(irxn,7)/rxn1(irxn,7),
     &      rxn2(irxn,10)/rxn1(irxn,10)
      enddo
      write(*,*)'See the output file for details'
      stop
      end
