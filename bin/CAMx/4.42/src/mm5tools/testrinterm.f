      program testinterm
c
c    Lesen des intermediate files fuer REGRIDDER
c
      integer*4 ifv
      character*24 hdate
      real xfcst
      character*9 field
c
;;      fln='ON84:1993-03-13_00'
      fln='ON84:yo1_fg:2001-05-10_00:00:00'
c
      open(2,file=fln,status='old',form='unformatted')
      read(2),ifv
      read(2),hdate,xfcst,field
      close (2)
      print *,ifv
      print *,hdate,xfcst,field
      close (2)

      end
       
