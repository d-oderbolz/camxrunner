       program readareas

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C  Converts gridded emissions to bins
C  Written by Alexandra Tsimpidi
C  Modified by Daniel Oderbolz to be called with an argument
C  We use the same idea as the other CAMx preprocessors,
C  you can pass a number of input files like this:
C  
C  split_emissions_area <<EOT
C  # of input files   | 2
C  Input File         | path/to/ifile/1
C  Output File        | path/to/ofile/1
C  Input File         | path/to/ifile/2
C  Output File        | path/to/ofile/2
C  EOT
C
C
C  $Id$
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C This variable holds the unprocessed parameters later
       character*200 inrec
       
C Input and output file descriptors      
       data ifout,ifin /10,11/
       
       character*4 ifile(10),note(60),arspec(10,39)
       character*4 arspecnew(10,123),arspec1(10)
       character*4 PSO4_1(10),PSO4_2(10),PSO4_3(10),PSO4_4(10)
       character*4 PSO4_5(10),PSO4_6(10),PSO4_7(10),PSO4_8(10)
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
       character*4 CRST_9(10),CRST_10(10)
       
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

       character*398 fname,fn
       character*38 n1,n2,n3,n4,n5,n6,n7,m1,m2,m3,m4,m5,m6,m7
       character*38 n8,n9,n10,n11,n12,n13,n14,m8,m9,m10,m11,m12,m13,m14
       character*38 n15,n16,n17,n18,n19,n20,n21,m15,m16,m17,m18,m19,m20
       character*38 m21,n22,n23,n24,n25,n26,m22,m23,m24,m25,m26,n27,m27
       character*38 l1,k1

       real*4 aremiss(60,60,39),aremissnew(60,60,99)
       real*4 aremiss2(60,60),aremiss3(60,60)
       integer*4 i,j,ll,k,m
           
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


C        Reading the header of the old file
         read(ifin) ifile,note,nseg,narspc,idat1,tim1,idat2,tim2
         read(ifin) orgx,orgy,izone,utmx,utmy,dx,dy,nx,ny,nz
         read(ifin) (idum,idum,idum,idum,n=1,nseg)
         read(ifin) ((arspec(n,l),n=1,10),l=1,narspc)
C           narspcnew=narspc+60

C        Specification of number of species?
         narspcnew=99
         do l=1,narspcnew
         
         
C        What does this loop do? Loop through time?
          do n=1,10
        
C   27 is the index at which particle emissions start in these files (CHECK)
           if (l.le.27) then
            arspecnew(n,l) = arspec(n,l)
           end if
           
C     Header generation (PMCAMx needs a matrix of names at the beginning)        
           if (l.eq.28) arspecnew(n,l)= PSO4_1(n) 
           if (l.eq.29) arspecnew(n,l)= PSO4_2(n) 
           if (l.eq.30) arspecnew(n,l)= PSO4_3(n) 
           if (l.eq.31) arspecnew(n,l)= PSO4_4(n) 
           if (l.eq.32) arspecnew(n,l)= PSO4_5(n) 
           if (l.eq.33) arspecnew(n,l)= PSO4_6(n)
           if (l.eq.34) arspecnew(n,l)= PSO4_7(n)
           if (l.eq.35) arspecnew(n,l)= PSO4_8(n) 
           if (l.eq.36) arspecnew(n,l)= PNO3_1(n)
           if (l.eq.37) arspecnew(n,l)= PNO3_2(n)
           if (l.eq.38) arspecnew(n,l)= PNO3_3(n)
           if (l.eq.39) arspecnew(n,l)= PNO3_4(n)
           if (l.eq.40) arspecnew(n,l)= PNO3_5(n)
           if (l.eq.41) arspecnew(n,l)= PNO3_6(n)
           if (l.eq.42) arspecnew(n,l)= PNO3_7(n)
           if (l.eq.43) arspecnew(n,l)= PNO3_8(n)
           if (l.eq.44) arspecnew(n,l)= PNH4_1(n)
           if (l.eq.45) arspecnew(n,l)= PNH4_2(n)
           if (l.eq.46) arspecnew(n,l)= PNH4_3(n)
           if (l.eq.47) arspecnew(n,l)= PNH4_4(n)
           if (l.eq.48) arspecnew(n,l)= PNH4_5(n)
           if (l.eq.49) arspecnew(n,l)= PNH4_6(n)
           if (l.eq.50) arspecnew(n,l)= PNH4_7(n)
           if (l.eq.51) arspecnew(n,l)= PNH4_8(n)
           if (l.eq.52) arspecnew(n,l)= PH2O_1(n)
           if (l.eq.53) arspecnew(n,l)= PH2O_2(n)
           if (l.eq.54) arspecnew(n,l)= PH2O_3(n)
           if (l.eq.55) arspecnew(n,l)= PH2O_4(n)
           if (l.eq.56) arspecnew(n,l)= PH2O_5(n)
           if (l.eq.57) arspecnew(n,l)= PH2O_6(n)
           if (l.eq.58) arspecnew(n,l)= PH2O_7(n)
           if (l.eq.59) arspecnew(n,l)= PH2O_8(n)
           if (l.eq.60) arspecnew(n,l)= NA_1(n) 
           if (l.eq.61) arspecnew(n,l)= NA_2(n) 
           if (l.eq.62) arspecnew(n,l)= NA_3(n) 
           if (l.eq.63) arspecnew(n,l)= NA_4(n) 
           if (l.eq.64) arspecnew(n,l)= NA_5(n) 
           if (l.eq.65) arspecnew(n,l)= NA_6(n) 
           if (l.eq.66) arspecnew(n,l)= NA_7(n) 
           if (l.eq.67) arspecnew(n,l)= NA_8(n) 
           if (l.eq.68) arspecnew(n,l)= PCL_1(n) 
           if (l.eq.69) arspecnew(n,l)= PCL_2(n) 
           if (l.eq.70) arspecnew(n,l)= PCL_3(n) 
           if (l.eq.71) arspecnew(n,l)= PCL_4(n) 
           if (l.eq.72) arspecnew(n,l)= PCL_5(n) 
           if (l.eq.73) arspecnew(n,l)= PCL_6(n) 
           if (l.eq.74) arspecnew(n,l)= PCL_7(n) 
           if (l.eq.75) arspecnew(n,l)= PCL_8(n) 
           if (l.eq.76) arspecnew(n,l)= POC_1(n)
           if (l.eq.77) arspecnew(n,l)= POC_2(n)
           if (l.eq.78) arspecnew(n,l)= POC_3(n)
           if (l.eq.79) arspecnew(n,l)= POC_4(n)
           if (l.eq.80) arspecnew(n,l)= POC_5(n)
           if (l.eq.81) arspecnew(n,l)= POC_6(n)
           if (l.eq.82) arspecnew(n,l)= POC_7(n)
           if (l.eq.83) arspecnew(n,l)= POC_8(n)
           if (l.eq.84) arspecnew(n,l)= PEC_1(n)
           if (l.eq.85) arspecnew(n,l)= PEC_2(n)
           if (l.eq.86) arspecnew(n,l)= PEC_3(n)
           if (l.eq.87) arspecnew(n,l)= PEC_4(n)
           if (l.eq.88) arspecnew(n,l)= PEC_5(n)
           if (l.eq.89) arspecnew(n,l)= PEC_6(n)
           if (l.eq.90) arspecnew(n,l)= PEC_7(n)
           if (l.eq.91) arspecnew(n,l)= PEC_8(n)
           if (l.eq.92) arspecnew(n,l)= CRST_1(n)
           if (l.eq.93) arspecnew(n,l)= CRST_2(n)
           if (l.eq.94) arspecnew(n,l)= CRST_3(n)
           if (l.eq.95) arspecnew(n,l)= CRST_4(n)
           if (l.eq.96) arspecnew(n,l)= CRST_5(n)
           if (l.eq.97) arspecnew(n,l)= CRST_6(n)
           if (l.eq.98) arspecnew(n,l)= CRST_7(n)
           if (l.eq.99) arspecnew(n,l)= CRST_8(n)

          end do
         end do 
         
C        Writing the header of the new file   
         write(ifout) ifile,note,nseg,narspcnew,idat1,tim1,idat2,tim2
         write(ifout) orgx,orgy,izone,utmx,utmy,dx,dy,nx,ny,nz
         write(ifout) (idum,idum,idum,idum,n=1,nseg)
         write(ifout) ((arspecnew(n,l),n=1,10),l=1,narspcnew)

 10   read(ifin,end=900) idat1,tim1,idat2,tim2
     
      write(ifout) idat1,tim1,idat2,tim2

      write(6,*)idat1,tim1,idat2,tim2
      do k = 1,narspc
        read(ifin) idum,(arspec1(m),m=1,10),((aremiss(i,j,k),
     &i=1,70),j=1,70)
      end do
      do k = 1,narspcnew
       do j = 1,70
        do i =1,70
         if(k.le.27) then
         aremissnew(i,j,k) = aremiss(i,j,k)
         end if
         
C    Size distribution comes from measurements (normal size distribution)

         if(k.eq.28) aremissnew(i,j,k)=0.06*aremiss(i,j,28)
         if(k.eq.29) aremissnew(i,j,k)=0.10*aremiss(i,j,28)
         if(k.eq.30) aremissnew(i,j,k)=0.15*aremiss(i,j,28)
         if(k.eq.31) aremissnew(i,j,k)=0.24*aremiss(i,j,28)
         if(k.eq.32) aremissnew(i,j,k)=0.12*aremiss(i,j,28)
         if(k.eq.33) aremissnew(i,j,k)=0.10*aremiss(i,j,28)
         if(k.eq.34) aremissnew(i,j,k)=0.15*aremiss(i,j,28)
         if(k.eq.35) aremissnew(i,j,k)=0.08*aremiss(i,j,28)
         if(k.eq.36) aremissnew(i,j,k)=0.10*aremiss(i,j,29)
         if(k.eq.37) aremissnew(i,j,k)=0.22*aremiss(i,j,29)
         if(k.eq.38) aremissnew(i,j,k)=0.15*aremiss(i,j,29)
         if(k.eq.39) aremissnew(i,j,k)=0.10*aremiss(i,j,29)
         if(k.eq.40) aremissnew(i,j,k)=0.03*aremiss(i,j,29)
         if(k.eq.41) aremissnew(i,j,k)=0.07*aremiss(i,j,29)
         if(k.eq.42) aremissnew(i,j,k)=0.10*aremiss(i,j,29)
         if(k.eq.43) aremissnew(i,j,k)=0.23*aremiss(i,j,29)
         if(k.eq.44) aremissnew(i,j,k)=0.06*aremiss(i,j,30)
         if(k.eq.45) aremissnew(i,j,k)=0.12*aremiss(i,j,30)
         if(k.eq.46) aremissnew(i,j,k)=0.24*aremiss(i,j,30)
         if(k.eq.47) aremissnew(i,j,k)=0.26*aremiss(i,j,30)
         if(k.eq.48) aremissnew(i,j,k)=0.12*aremiss(i,j,30)
         if(k.eq.49) aremissnew(i,j,k)=0.08*aremiss(i,j,30)
         if(k.eq.50) aremissnew(i,j,k)=0.12*aremiss(i,j,30)
         if(k.eq.51) aremissnew(i,j,k)=0.00*aremiss(i,j,30)
         if(k.eq.52) aremissnew(i,j,k)=0.12*aremiss(i,j,31)
         if(k.eq.53) aremissnew(i,j,k)=0.12*aremiss(i,j,31)
         if(k.eq.54) aremissnew(i,j,k)=0.12*aremiss(i,j,31)
         if(k.eq.55) aremissnew(i,j,k)=0.12*aremiss(i,j,31)
         if(k.eq.56) aremissnew(i,j,k)=0.13*aremiss(i,j,31)
         if(k.eq.57) aremissnew(i,j,k)=0.13*aremiss(i,j,31)
         if(k.eq.58) aremissnew(i,j,k)=0.13*aremiss(i,j,31)
         if(k.eq.59) aremissnew(i,j,k)=0.13*aremiss(i,j,31)
         if(k.eq.60) aremissnew(i,j,k)=0.00*aremiss(i,j,32)
         if(k.eq.61) aremissnew(i,j,k)=0.00*aremiss(i,j,32)
         if(k.eq.62) aremissnew(i,j,k)=0.00*aremiss(i,j,32)
         if(k.eq.63) aremissnew(i,j,k)=0.00*aremiss(i,j,32)
         if(k.eq.64) aremissnew(i,j,k)=0.00*aremiss(i,j,32)
         if(k.eq.65) aremissnew(i,j,k)=0.20*aremiss(i,j,32)
         if(k.eq.66) aremissnew(i,j,k)=0.40*aremiss(i,j,32)
         if(k.eq.67) aremissnew(i,j,k)=0.40*aremiss(i,j,32)
         if(k.eq.68) aremissnew(i,j,k)=0.00*aremiss(i,j,33)
         if(k.eq.69) aremissnew(i,j,k)=0.00*aremiss(i,j,33)
         if(k.eq.70) aremissnew(i,j,k)=0.00*aremiss(i,j,33)
         if(k.eq.71) aremissnew(i,j,k)=0.00*aremiss(i,j,33)
         if(k.eq.72) aremissnew(i,j,k)=0.00*aremiss(i,j,33)
         if(k.eq.73) aremissnew(i,j,k)=0.20*aremiss(i,j,33)
         if(k.eq.74) aremissnew(i,j,k)=0.40*aremiss(i,j,33)
         if(k.eq.75) aremissnew(i,j,k)=0.40*aremiss(i,j,33)
         if(k.eq.76) aremissnew(i,j,k)=0.32*aremiss(i,j,34)
         if(k.eq.77) aremissnew(i,j,k)=0.24*aremiss(i,j,34)
         if(k.eq.78) aremissnew(i,j,k)=0.20*aremiss(i,j,34)
         if(k.eq.79) aremissnew(i,j,k)=0.12*aremiss(i,j,34)
         if(k.eq.80) aremissnew(i,j,k)=0.08*aremiss(i,j,34)
         if(k.eq.81) aremissnew(i,j,k)=0.04*aremiss(i,j,34)
         if(k.eq.82) aremissnew(i,j,k)=0.00*aremiss(i,j,34)
         if(k.eq.83) aremissnew(i,j,k)=0.00*aremiss(i,j,34)
         if(k.eq.84) aremissnew(i,j,k)=0.32*aremiss(i,j,35)
         if(k.eq.85) aremissnew(i,j,k)=0.24*aremiss(i,j,35)
         if(k.eq.86) aremissnew(i,j,k)=0.20*aremiss(i,j,35)
         if(k.eq.87) aremissnew(i,j,k)=0.12*aremiss(i,j,35)
         if(k.eq.88) aremissnew(i,j,k)=0.08*aremiss(i,j,35)
         if(k.eq.89) aremissnew(i,j,k)=0.04*aremiss(i,j,35)
         if(k.eq.90) aremissnew(i,j,k)=0.00*aremiss(i,j,35)
         if(k.eq.91) aremissnew(i,j,k)=0.00*aremiss(i,j,35)
      if(k.eq.92) aremissnew(i,j,k)=0.15*(aremiss(i,j,36)
     &+aremiss(i,j,37))
      if(k.eq.93) aremissnew(i,j,k)=0.15*(aremiss(i,j,36)
     &+aremiss(i,j,37))
      if(k.eq.94) aremissnew(i,j,k)=0.17*(aremiss(i,j,36)
     &+aremiss(i,j,37))
      if(k.eq.95) aremissnew(i,j,k)=0.17*(aremiss(i,j,36)
     &+aremiss(i,j,37))
      if(k.eq.96) aremissnew(i,j,k)=0.18*(aremiss(i,j,36)
     &+aremiss(i,j,37))
      if(k.eq.97) aremissnew(i,j,k)=0.18*(aremiss(i,j,36)
     &+aremiss(i,j,37))
      if(k.eq.98) aremissnew(i,j,k)=0.45*(aremiss(i,j,38)
     &+aremiss(i,j,39))
      if(k.eq.99) aremissnew(i,j,k)=0.55*(aremiss(i,j,38)
     &+aremiss(i,j,39))

         end do
        end do
       end do
       do k = 1,narspcnew
         write(ifout) idum,(arspecnew(m,k),m=1,10),
     &((aremissnew(i,j,k),i=1,70),j=1,70)
       end do
       goto 10

 900   continue
       end do
       

       end


