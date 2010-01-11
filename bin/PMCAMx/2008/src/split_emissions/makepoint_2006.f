       program readpoints
       
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C  Converts point source emissions to bins
C  Written by Alexandra Tsimpidi
C  Modified by Daniel Oderbolz to be called with an argument
C  We use the same idea as the other CAMx preprocessors,
C  you can pass a number of input files like this:
C  
C  split_emissions_point <<EOT
C  # of input files  | 2
C  Input File        | path/to/ifile/1
C  Output File       | path/to/ofile/1
C  Input File        | path/to/ifile/2
C  Output File       | path/to/ofile/2
C  EOT
C
C  $Id$
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C This variable holds the unprocessed parameters later
       character*200 inrec
       
C Input and output file descriptors      
       data ifout,ifin /10,11/
       
       
       character*4 ifile(10),note(60),ptspec(10,222),ptspec1(10)
       character*4 ptspecnew(10,222),PSO4_1(10),PSO4_2(10),PSO4_3(10)
       character*4 PSO4_4(10),PSO4_5(10),PSO4_6(10),PSO4_7(10)
       character*4 PSO4_8(10)
       character*4 NA_1(10),NA_2(10),NA_3(10),NA_4(10)
       character*4 NA_5(10),NA_6(10),NA_7(10),NA_8(10)
       character*4 PCL_1(10),PCL_2(10),PCL_3(10),PCL_4(10)
       character*4 PCL_5(10),PCL_6(10),PCL_7(10),PCL_8(10)
       character*4 PNO3_1(10),PNO3_2(10),PNO3_3(10),PNO3_4(10)
       character*4 PNO3_5(10),PNO3_6(10),PNO3_7(10),PNO3_8(10)
       character*4 POC_1(10),POC_2(10),POC_3(10),POC_4(10)
       character*4 POC_5(10),POC_6(10),POC_7(10),POC_8(10)
       character*4 PNH4_1(10),PNH4_2(10),PNH4_3(10),PNH4_4(10)
       character*4 PNH4_5(10),PNH4_6(10),PNH4_7(10),PNH4_8(10)
       character*4 PEC_1(10),PEC_2(10),PEC_3(10),PEC_4(10)
       character*4 PEC_5(10),PEC_6(10),PEC_7(10),PEC_8(10)
       character*4 PH2O_1(10),PH2O_2(10),PH2O_3(10),PH2O_4(10)
       character*4 PH2O_5(10),PH2O_6(10),PH2O_7(10),PH2O_8(10)
       character*4 CRST_1(10),CRST_2(10),CRST_3(10),CRST_4(10)
       character*4 CRST_5(10),CRST_6(10),CRST_7(10),CRST_8(10)
       character*4 SOA1_1(10),SOA1_2(10),SOA1_3(10),SOA1_4(10)
       character*4 SOA1_5(10),SOA1_6(10),SOA1_7(10),SOA1_8(10)
       character*4 SOA2_1(10),SOA2_2(10),SOA2_3(10),SOA2_4(10)
       character*4 SOA2_5(10),SOA2_6(10),SOA2_7(10),SOA2_8(10)
       character*4 SOA3_1(10),SOA3_2(10),SOA3_3(10),SOA3_4(10)
       character*4 SOA3_5(10),SOA3_6(10),SOA3_7(10),SOA3_8(10)
       character*4 SOA4_1(10),SOA4_2(10),SOA4_3(10),SOA4_4(10)
       character*4 SOA4_5(10),SOA4_6(10),SOA4_7(10),SOA4_8(10)
       dimension xloc(12000),yloc(12000),xstk(12000),ystk(12000)
       dimension dstk(12000),tstk(12000),vstk(12000),hstk(12000)
       dimension flowrat(12000),effph(12000),ptemis(12000,222)
       dimension ptemisnew(12000,222)
       character*10 ptfil,infil,ptspc
       integer*4 ii(12000),jj(12000)

       character*398 fname,fn
       character*26 n1,n2,n3,n4,n5,n6,n7,m1,m2,m3,m4,m5,m6,m7
       character*26 n8,n9,n10,n11,n12,n13,n14,m8,m9,m10,m11,m12,m13
       character*26 m14,n15,n16,n17,n18,n19,n20,n21,m15,m16,m17,m18,
     & m19,m20,m21
       character*26 n22,n23,n24,n25,n26,m22,m23,m24,m25,m26
       character*26 n27,n28,n29,n30,n31,m27,m28,m29,m30,m31
       character*26 l1,k1


       real*4 map(70,70)

C  1 is the size bin 1 (smallest). This is used for the header
       data PSO4_1 /'P','S','O','4','_','1','','','',''/
       data PSO4_2 /'P','S','O','4','_','2','','','',''/
       data PSO4_3 /'P','S','O','4','_','3','','','',''/
       data PSO4_4 /'P','S','O','4','_','4','','','',''/
       data PSO4_5 /'P','S','O','4','_','5','','','',''/
       data PSO4_6 /'P','S','O','4','_','6','','','',''/
       data PSO4_7 /'P','S','O','4','_','7','','','',''/
       data PSO4_8 /'P','S','O','4','_','8','','','',''/
       data PNO3_1 /'P','N','O','3','_','1','','','',''/
       data PNO3_2 /'P','N','O','3','_','2','','','',''/
       data PNO3_3 /'P','N','O','3','_','3','','','',''/
       data PNO3_4 /'P','N','O','3','_','4','','','',''/
       data PNO3_5 /'P','N','O','3','_','5','','','',''/
       data PNO3_6 /'P','N','O','3','_','6','','','',''/
       data PNO3_7 /'P','N','O','3','_','7','','','',''/
       data PNO3_8 /'P','N','O','3','_','8','','','',''/
       data PNH4_1 /'P','N','H','4','_','1','','','',''/
       data PNH4_2 /'P','N','H','4','_','2','','','',''/
       data PNH4_3 /'P','N','H','4','_','3','','','',''/
       data PNH4_4 /'P','N','H','4','_','4','','','',''/
       data PNH4_5 /'P','N','H','4','_','5','','','',''/
       data PNH4_6 /'P','N','H','4','_','6','','','',''/
       data PNH4_7 /'P','N','H','4','_','7','','','',''/
       data PNH4_8 /'P','N','H','4','_','8','','','',''/
       data PH2O_1 /'P','H','2','O','_','1','','','',''/
       data PH2O_2 /'P','H','2','O','_','2','','','',''/
       data PH2O_3 /'P','H','2','O','_','3','','','',''/
       data PH2O_4 /'P','H','2','O','_','4','','','',''/
       data PH2O_5 /'P','H','2','O','_','5','','','',''/
       data PH2O_6 /'P','H','2','O','_','6','','','',''/
       data PH2O_7 /'P','H','2','O','_','7','','','',''/
       data PH2O_8 /'P','H','2','O','_','8','','','',''/
       data NA_1 /'N','A','_','1',' ',' ','','','',''/
       data NA_2 /'N','A','_','2',' ',' ','','','',''/
       data NA_3 /'N','A','_','3',' ',' ','','','',''/
       data NA_4 /'N','A','_','4',' ',' ','','','',''/
       data NA_5 /'N','A','_','5',' ',' ','','','',''/
       data NA_6 /'N','A','_','6',' ',' ','','','',''/
       data NA_7 /'N','A','_','7',' ',' ','','','',''/
       data NA_8 /'N','A','_','8',' ',' ','','','',''/
       data PCL_1 /'P','C','L','_','1',' ','','','',''/
       data PCL_2 /'P','C','L','_','2',' ','','','',''/
       data PCL_3 /'P','C','L','_','3',' ','','','',''/
       data PCL_4 /'P','C','L','_','4',' ','','','',''/
       data PCL_5 /'P','C','L','_','5',' ','','','',''/
       data PCL_6 /'P','C','L','_','6',' ','','','',''/
       data PCL_7 /'P','C','L','_','7',' ','','','',''/
       data PCL_8 /'P','C','L','_','8',' ','','','',''/
       data POC_1 /'P','O','C','_','1',' ','','','',''/
       data POC_2 /'P','O','C','_','2',' ','','','',''/
       data POC_3 /'P','O','C','_','3',' ','','','',''/
       data POC_4 /'P','O','C','_','4',' ','','','',''/
       data POC_5 /'P','O','C','_','5',' ','','','',''/
       data POC_6 /'P','O','C','_','6',' ','','','',''/
       data POC_7 /'P','O','C','_','7',' ','','','',''/
       data POC_8 /'P','O','C','_','8',' ','','','',''/
       data PEC_1 /'P','E','C','_','1',' ','','','',''/
       data PEC_2 /'P','E','C','_','2',' ','','','',''/
       data PEC_3 /'P','E','C','_','3',' ','','','',''/
       data PEC_4 /'P','E','C','_','4',' ','','','',''/
       data PEC_5 /'P','E','C','_','5',' ','','','',''/
       data PEC_6 /'P','E','C','_','6',' ','','','',''/
       data PEC_7 /'P','E','C','_','7',' ','','','',''/
       data PEC_8 /'P','E','C','_','8',' ','','','',''/
       data CRST_1 /'C','R','S','T','_','1','','','',''/
       data CRST_2 /'C','R','S','T','_','2','','','',''/
       data CRST_3 /'C','R','S','T','_','3','','','',''/
       data CRST_4 /'C','R','S','T','_','4','','','',''/
       data CRST_5 /'C','R','S','T','_','5','','','',''/
       data CRST_6 /'C','R','S','T','_','6','','','',''/
       data CRST_7 /'C','R','S','T','_','7','','','',''/
       data CRST_8 /'C','R','S','T','_','8','','','',''/
       data SOA1_1 /'S','O','A','1','_','1','','','',''/
       data SOA1_2 /'S','O','A','1','_','2','','','',''/
       data SOA1_3 /'S','O','A','1','_','3','','','',''/
       data SOA1_4 /'S','O','A','1','_','4','','','',''/
       data SOA1_5 /'S','O','A','1','_','5','','','',''/
       data SOA1_6 /'S','O','A','1','_','6','','','',''/
       data SOA1_7 /'S','O','A','1','_','7','','','',''/
       data SOA1_8 /'S','O','A','1','_','8','','','',''/
       data SOA2_1 /'S','O','A','2','_','1','','','',''/
       data SOA2_2 /'S','O','A','2','_','2','','','',''/
       data SOA2_3 /'S','O','A','2','_','3','','','',''/
       data SOA2_4 /'S','O','A','2','_','4','','','',''/
       data SOA2_5 /'S','O','A','2','_','5','','','',''/
       data SOA2_6 /'S','O','A','2','_','6','','','',''/
       data SOA2_7 /'S','O','A','2','_','7','','','',''/
       data SOA2_8 /'S','O','A','2','_','8','','','',''/
       data SOA3_1 /'S','O','A','3','_','1','','','',''/
       data SOA3_2 /'S','O','A','3','_','2','','','',''/
       data SOA3_3 /'S','O','A','3','_','3','','','',''/
       data SOA3_4 /'S','O','A','3','_','4','','','',''/
       data SOA3_5 /'S','O','A','3','_','5','','','',''/
       data SOA3_6 /'S','O','A','3','_','6','','','',''/
       data SOA3_7 /'S','O','A','3','_','7','','','',''/
       data SOA3_8 /'S','O','A','3','_','8','','','',''/
       data SOA4_1 /'S','O','A','4','_','1','','','',''/
       data SOA4_2 /'S','O','A','4','_','2','','','',''/
       data SOA4_3 /'S','O','A','4','_','3','','','',''/
       data SOA4_4 /'S','O','A','4','_','4','','','',''/
       data SOA4_5 /'S','O','A','4','_','5','','','',''/
       data SOA4_6 /'S','O','A','4','_','6','','','',''/
       data SOA4_7 /'S','O','A','4','_','7','','','',''/
       data SOA4_8 /'S','O','A','4','_','8','','','',''/
       
C-- Read Parameters from STDIN. The first 20 characters are discarded (comments for readability)

       read(*,'(20x,a)') inrec
       read(inrec,*) nfiles
       write(*,*)'Number of files to be processed: ',nfiles

C-- Now all our processing will take place in this loop because each file is treated independently
      do ng = 1,nfiles 
       
C        Read the input filename and open the file       
         read(*,'(20x,a)') inrec
         open(ifin,file=inrec,form='unformatted',status='unknown')
         write(*,*)'Opened input file: ',inrec
         
C        Read the output filename and open the file       
         read(*,'(20x,a)') inrec
         open(ifout,file=inrec,form='unformatted',status='unknown')
         write(*,*)'Opened output file: ',inrec
       
       
       read(ifin) ifile,note,nseg,nptspc,idat1,tim1,idat2,tim2
       read(ifin) orgx,orgy,izone,utmx,utmy,dx,dy,nx,ny,nz

       read(ifin) (idum,idum,idum,idum,n=1,nseg)
       read(ifin) ((ptspec(n,l),n=1,10),l=1,nptspc)
       read(ifin) idum,nptsrc
       read(ifin) (xloc(n),yloc(n),hstk(n),dstk(n),tstk(n),vstk(n),
     &             n=1,nptsrc)
c      nptspcnew=nptspc+60
       nptspcnew=99

       do l=1,nptspcnew
        do n=1,10
         if (l.le.27) then
          ptspecnew(n,l) = ptspec(n,l)
         end if
         if (l.eq.28) ptspecnew(n,l)= PSO4_1(n)
         if (l.eq.29) ptspecnew(n,l)= PSO4_2(n)
         if (l.eq.30) ptspecnew(n,l)= PSO4_3(n)
         if (l.eq.31) ptspecnew(n,l)= PSO4_4(n)
         if (l.eq.32) ptspecnew(n,l)= PSO4_5(n)
         if (l.eq.33) ptspecnew(n,l)= PSO4_6(n)
         if (l.eq.34) ptspecnew(n,l)= PSO4_7(n)
         if (l.eq.35) ptspecnew(n,l)= PSO4_8(n)
         if (l.eq.36) ptspecnew(n,l)= PNO3_1(n)
         if (l.eq.37) ptspecnew(n,l)= PNO3_2(n)
         if (l.eq.38) ptspecnew(n,l)= PNO3_3(n)
         if (l.eq.39) ptspecnew(n,l)= PNO3_4(n)
         if (l.eq.40) ptspecnew(n,l)= PNO3_5(n)
         if (l.eq.41) ptspecnew(n,l)= PNO3_6(n)
         if (l.eq.42) ptspecnew(n,l)= PNO3_7(n)
         if (l.eq.43) ptspecnew(n,l)= PNO3_8(n)
         if (l.eq.44) ptspecnew(n,l)= PNH4_1(n)
         if (l.eq.45) ptspecnew(n,l)= PNH4_2(n)
         if (l.eq.46) ptspecnew(n,l)= PNH4_3(n)
         if (l.eq.47) ptspecnew(n,l)= PNH4_4(n)
         if (l.eq.48) ptspecnew(n,l)= PNH4_5(n)
         if (l.eq.49) ptspecnew(n,l)= PNH4_6(n)
         if (l.eq.50) ptspecnew(n,l)= PNH4_7(n)
         if (l.eq.51) ptspecnew(n,l)= PNH4_8(n)
         if (l.eq.52) ptspecnew(n,l)= PH2O_1(n)
         if (l.eq.53) ptspecnew(n,l)= PH2O_2(n)
         if (l.eq.54) ptspecnew(n,l)= PH2O_3(n)
         if (l.eq.55) ptspecnew(n,l)= PH2O_4(n)
         if (l.eq.56) ptspecnew(n,l)= PH2O_5(n)
         if (l.eq.57) ptspecnew(n,l)= PH2O_6(n)
         if (l.eq.58) ptspecnew(n,l)= PH2O_7(n)
         if (l.eq.59) ptspecnew(n,l)= PH2O_8(n)
         if (l.eq.60) ptspecnew(n,l)= NA_1(n)
         if (l.eq.61) ptspecnew(n,l)= NA_2(n)
         if (l.eq.62) ptspecnew(n,l)= NA_3(n)
         if (l.eq.63) ptspecnew(n,l)= NA_4(n)
         if (l.eq.64) ptspecnew(n,l)= NA_5(n)
         if (l.eq.65) ptspecnew(n,l)= NA_6(n)
         if (l.eq.66) ptspecnew(n,l)= NA_7(n)
         if (l.eq.67) ptspecnew(n,l)= NA_8(n)
         if (l.eq.68) ptspecnew(n,l)= PCL_1(n)
         if (l.eq.69) ptspecnew(n,l)= PCL_2(n)
         if (l.eq.70) ptspecnew(n,l)= PCL_3(n)
         if (l.eq.71) ptspecnew(n,l)= PCL_4(n)
         if (l.eq.72) ptspecnew(n,l)= PCL_5(n)
         if (l.eq.73) ptspecnew(n,l)= PCL_6(n)
         if (l.eq.74) ptspecnew(n,l)= PCL_7(n)
         if (l.eq.75) ptspecnew(n,l)= PCL_8(n)
         if (l.eq.76) ptspecnew(n,l)= POC_1(n)
         if (l.eq.77) ptspecnew(n,l)= POC_2(n)
         if (l.eq.78) ptspecnew(n,l)= POC_3(n)
         if (l.eq.79) ptspecnew(n,l)= POC_4(n)
         if (l.eq.80) ptspecnew(n,l)= POC_5(n)
         if (l.eq.81) ptspecnew(n,l)= POC_6(n)
         if (l.eq.82) ptspecnew(n,l)= POC_7(n)
         if (l.eq.83) ptspecnew(n,l)= POC_8(n)
         if (l.eq.84) ptspecnew(n,l)= PEC_1(n)
         if (l.eq.85) ptspecnew(n,l)= PEC_2(n)
         if (l.eq.86) ptspecnew(n,l)= PEC_3(n)
         if (l.eq.87) ptspecnew(n,l)= PEC_4(n)
         if (l.eq.88) ptspecnew(n,l)= PEC_5(n)
         if (l.eq.89) ptspecnew(n,l)= PEC_6(n)
         if (l.eq.90) ptspecnew(n,l)= PEC_7(n)
         if (l.eq.91) ptspecnew(n,l)= PEC_8(n)
         if (l.eq.92) ptspecnew(n,l)= CRST_1(n)
         if (l.eq.93) ptspecnew(n,l)= CRST_2(n)
         if (l.eq.94) ptspecnew(n,l)= CRST_3(n)
         if (l.eq.95) ptspecnew(n,l)= CRST_4(n)
         if (l.eq.96) ptspecnew(n,l)= CRST_5(n)
         if (l.eq.97) ptspecnew(n,l)= CRST_6(n)
         if (l.eq.98) ptspecnew(n,l)= CRST_7(n)
         if (l.eq.99) ptspecnew(n,l)= CRST_8(n)

        end do
       end do


       write(ifout) ifile,note,nseg,nptspcnew,idat1,tim1,idat2,tim2
       write(ifout) orgx,orgy,izone,utmx,utmy,dx,dy,nx,ny,nz

       write(ifout) (idum,idum,idum,idum,n=1,nseg)
       write(ifout) ((ptspecnew(n,l),n=1,10),l=1,nptspcnew)
       write(ifout) idum,nptsrc
       write(ifout) (xloc(n),yloc(n),hstk(n),dstk(n),tstk(n),vstk(n),
     &             n=1,nptsrc)


 10    read(ifin,end=900) idat1,tim1,idat2,tim2
       write(ifout) idat1,tim1,idat2,tim2


       read(ifin) idum,npts
       write(ifout) idum,npts
       read(ifin) (idum,idum,idum,flowrat(n),effph(n),n=1,npts)
       write(ifout) (idum,idum,idum,flowrat(n),effph(n),n=1,npts)


        write(6,*)idat1,tim1,idat2,tim2
        do ll = 1,nptspc
        read(ifin) idum,(ptspec1(i),i=1,10),(ptemis(n,ll),n=1,npts)
        end do
        do ll = 1,nptspcnew
         do n = 1,npts
           if(ll.le.27) then
           ptemisnew(n,ll) = ptemis(n,ll)
           end if

         if(ll.eq.28) ptemisnew(n,ll)=0.06*ptemis(n,28)
         if(ll.eq.29) ptemisnew(n,ll)=0.10*ptemis(n,28)
         if(ll.eq.30) ptemisnew(n,ll)=0.15*ptemis(n,28)
         if(ll.eq.31) ptemisnew(n,ll)=0.24*ptemis(n,28)
         if(ll.eq.32) ptemisnew(n,ll)=0.12*ptemis(n,28)
         if(ll.eq.33) ptemisnew(n,ll)=0.10*ptemis(n,28)
         if(ll.eq.34) ptemisnew(n,ll)=0.15*ptemis(n,28)
         if(ll.eq.35) ptemisnew(n,ll)=0.08*ptemis(n,28)
         if(ll.eq.36) ptemisnew(n,ll)=0.09*ptemis(n,29)
         if(ll.eq.37) ptemisnew(n,ll)=0.09*ptemis(n,29)
         if(ll.eq.38) ptemisnew(n,ll)=0.06*ptemis(n,29)
         if(ll.eq.39) ptemisnew(n,ll)=0.08*ptemis(n,29)
         if(ll.eq.40) ptemisnew(n,ll)=0.03*ptemis(n,29)
         if(ll.eq.41) ptemisnew(n,ll)=0.05*ptemis(n,29)
         if(ll.eq.42) ptemisnew(n,ll)=0.44*ptemis(n,29)
         if(ll.eq.43) ptemisnew(n,ll)=0.16*ptemis(n,29)
         if(ll.eq.44) ptemisnew(n,ll)=0.06*ptemis(n,30)
         if(ll.eq.45) ptemisnew(n,ll)=0.12*ptemis(n,30)
         if(ll.eq.46) ptemisnew(n,ll)=0.24*ptemis(n,30)
         if(ll.eq.47) ptemisnew(n,ll)=0.26*ptemis(n,30)
         if(ll.eq.48) ptemisnew(n,ll)=0.12*ptemis(n,30)
         if(ll.eq.49) ptemisnew(n,ll)=0.08*ptemis(n,30)
         if(ll.eq.50) ptemisnew(n,ll)=0.12*ptemis(n,30)
         if(ll.eq.51) ptemisnew(n,ll)=0.00*ptemis(n,30)
         if(ll.eq.52) ptemisnew(n,ll)=0.12*ptemis(n,31)
         if(ll.eq.53) ptemisnew(n,ll)=0.12*ptemis(n,31)
         if(ll.eq.54) ptemisnew(n,ll)=0.12*ptemis(n,31)
         if(ll.eq.55) ptemisnew(n,ll)=0.12*ptemis(n,31)
         if(ll.eq.56) ptemisnew(n,ll)=0.13*ptemis(n,31)
         if(ll.eq.57) ptemisnew(n,ll)=0.13*ptemis(n,31)
         if(ll.eq.58) ptemisnew(n,ll)=0.13*ptemis(n,31)
         if(ll.eq.59) ptemisnew(n,ll)=0.13*ptemis(n,31)
         if(ll.eq.60) ptemisnew(n,ll)=0.00*ptemis(n,32)
         if(ll.eq.61) ptemisnew(n,ll)=0.00*ptemis(n,32)
         if(ll.eq.62) ptemisnew(n,ll)=0.00*ptemis(n,32)
         if(ll.eq.63) ptemisnew(n,ll)=0.00*ptemis(n,32)
         if(ll.eq.64) ptemisnew(n,ll)=0.10*ptemis(n,32)
         if(ll.eq.65) ptemisnew(n,ll)=0.00*ptemis(n,32)
         if(ll.eq.66) ptemisnew(n,ll)=0.40*ptemis(n,32)
         if(ll.eq.67) ptemisnew(n,ll)=0.50*ptemis(n,32)
         if(ll.eq.68) ptemisnew(n,ll)=0.00*ptemis(n,33)
         if(ll.eq.69) ptemisnew(n,ll)=0.00*ptemis(n,33)
         if(ll.eq.70) ptemisnew(n,ll)=0.00*ptemis(n,33)
         if(ll.eq.71) ptemisnew(n,ll)=0.00*ptemis(n,33)
         if(ll.eq.72) ptemisnew(n,ll)=0.10*ptemis(n,33)
         if(ll.eq.73) ptemisnew(n,ll)=0.00*ptemis(n,33)
         if(ll.eq.74) ptemisnew(n,ll)=0.40*ptemis(n,33)
         if(ll.eq.75) ptemisnew(n,ll)=0.50*ptemis(n,33)
         if(ll.eq.76) ptemisnew(n,ll)=0.32*ptemis(n,34)
         if(ll.eq.77) ptemisnew(n,ll)=0.24*ptemis(n,34)
         if(ll.eq.78) ptemisnew(n,ll)=0.20*ptemis(n,34)
         if(ll.eq.79) ptemisnew(n,ll)=0.12*ptemis(n,34)
         if(ll.eq.80) ptemisnew(n,ll)=0.08*ptemis(n,34)
         if(ll.eq.81) ptemisnew(n,ll)=0.04*ptemis(n,34)
         if(ll.eq.82) ptemisnew(n,ll)=0.00*ptemis(n,34)
         if(ll.eq.83) ptemisnew(n,ll)=0.00*ptemis(n,34)
         if(ll.eq.84) ptemisnew(n,ll)=0.32*ptemis(n,35)
         if(ll.eq.85) ptemisnew(n,ll)=0.24*ptemis(n,35)
         if(ll.eq.86) ptemisnew(n,ll)=0.20*ptemis(n,35)
         if(ll.eq.87) ptemisnew(n,ll)=0.12*ptemis(n,35)
         if(ll.eq.88) ptemisnew(n,ll)=0.08*ptemis(n,35)
         if(ll.eq.89) ptemisnew(n,ll)=0.04*ptemis(n,35)
         if(ll.eq.90) ptemisnew(n,ll)=0.00*ptemis(n,35)
         if(ll.eq.91) ptemisnew(n,ll)=0.00*ptemis(n,35)
      if(ll.eq.92) ptemisnew(n,ll)=0.15*(ptemis(n,36)+ptemis(n,37))
      if(ll.eq.93) ptemisnew(n,ll)=0.15*(ptemis(n,36)+ptemis(n,37))
      if(ll.eq.94) ptemisnew(n,ll)=0.17*(ptemis(n,36)+ptemis(n,37))
      if(ll.eq.95) ptemisnew(n,ll)=0.17*(ptemis(n,36)+ptemis(n,37))
      if(ll.eq.96) ptemisnew(n,ll)=0.18*(ptemis(n,36)+ptemis(n,37))
      if(ll.eq.97) ptemisnew(n,ll)=0.18*(ptemis(n,36)+ptemis(n,37))
      if(ll.eq.98) ptemisnew(n,ll)=0.45*(ptemis(n,38)+ptemis(n,39))
      if(ll.eq.99) ptemisnew(n,ll)=0.55*(ptemis(n,38)+ptemis(n,39))

          end do
         end do
         do ll = 1,nptspcnew
          write(ifout) idum,(ptspecnew(m,ll),m=1,10),
     &(ptemisnew(n,ll),n=1,npts)
         end do


       goto 10
 900   continue
       end do 

       end


