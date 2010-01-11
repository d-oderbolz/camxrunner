     subroutine prep_tc (instring,num1,num2)
c
c     Just an idea, not used yet
c     Questions: Do I ned to oepn the files differently?
c
c     Input arguments:
c        ifin            File descriptor of input
c        ifout           File descriptor of output
c
c
c

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

      end
