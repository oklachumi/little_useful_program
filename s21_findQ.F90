program main
implicit real*8(a-h,j-z)
dimension dinput(59999999,3) !,dib(999999,2) !564067 274980
parameter(pi=3.14159265358979d0,onedB_para=0.7943282347242815d0)

iflag_dB = 3 ! 1 ==> 1dB, 3 ==> 3dB
if (iflag_dB==3) then
  open(3,file='3dB_result_field.txt') !,position='append',iostat=intvar)
  write(3,'(20(1x,a16))')'max_freq(GHz)','max_dB','FWHMl_freq(GHz)','FWHMl_dB','FWHMr_freq(GHz)','FWHMr_dB','BW','df(MHz)','Q'
  write(*,'(20(1x,a16))')'max_freq(GHz)','max_dB','FWHMl_freq(GHz)','FWHMl_dB','FWHMr_freq(GHz)','FWHMr_dB','BW','df(MHz)','Q'
endif

if (iflag_dB==1) then
  open(4,file='1dB_result.txt') !,position='append',iostat=intvar)
  write(4,'(20(1x,a16))')'max_freq(GHz)','max_dB','FWHMl_freq(GHz)','FWHMl_dB','FWHMr_freq(GHz)','FWHMr_dB','BW','df(MHz)','Q'
endif

it=1 ! itstart=1, itend=it-1
istart=0

open(2,file='s11_.csv') ! out_wot=without taper, out_wt=with taper
read(2,*)

do i = 1,7
  read(2,*)
enddo

do while(.not.eof(2))
  read(2,*,err=78)temp1,temp2
  if (istart==0) then
    dinput(it,1) = temp1        ! freq(GHz)
    dinput(it,2) = temp2        ! S11(dB)
    !dinput(it,2) = 10d0**(dinput(it,2)/10d0)-1d0  ! in power
    dinput(it,2) = 10d0**(dinput(it,2)/20d0)-1d0  ! in field amp
    it = it + 1                 ! itstart=1, itend=it-1
  endif
enddo
78 continue
itend = it - 1

dbmax = 0d0
do it = 1, itend
  if (dabs(dinput(it,2))>=dbmax) then
    dbmax = dabs(dinput(it,2))
    imaxdb = it
  endif
enddo

dminl = 1d50
dminr = 1d50
do it = 1, itend

  if (iflag_dB==1) then
    if (dabs(dinput(it,2))>=dabs(dinput(imaxdb,2))*onedB_para) then
      if (it<imaxdb) then
        if (dabs(dinput(it,2))<dminl) then
          dminl=dabs(dinput(it,2))
          iFWHMl=it
        endif
      else
        if (dabs(dinput(it,2))<dminr) then
          dminr=dabs(dinput(it,2))
          iFWHMr=it
        endif
      endif
    endif
  endif
    
  if (iflag_dB==3) then
    if (dabs(dinput(it,2))>=dabs(dinput(imaxdb,2))/2d0) then
      if (it<imaxdb) then
        if (dabs(dinput(it,2))<dminl) then
          dminl=dabs(dinput(it,2))
          iFWHMl=it
        endif
      else
        if (dabs(dinput(it,2))<dminr) then
          dminr=dabs(dinput(it,2))
          iFWHMr=it        
        endif
      endif
    endif
  endif 
     
enddo

df      = dabs(dinput(iFWHMl,1)-dinput(iFWHMr,1))
freqmax = dinput(imaxdb,1)
BW      = df/freqmax
Q       = 1d0/BW
!write(*,*)

!'max_freq(GHz)','max_dB','FWHMl_freq(GHz)','FWHMl_dB','FWHMr_freq(GHz)','FWHMr_dB','BW'
if (iflag_dB==3) then
write(3,'(20(1x,1pd16.9))')freqmax/1d9,-dbmax,dinput(iFWHMl,1)/1d9,dinput(iFWHMl,2),dinput(iFWHMr,1)/1d9,dinput(iFWHMr,2),BW,df/1d6,Q
write(*,'(20(1x,1pd16.9))')freqmax/1d9,-dbmax,dinput(iFWHMl,1)/1d9,dinput(iFWHMl,2),dinput(iFWHMr,1)/1d9,dinput(iFWHMr,2),BW,df/1d6,Q
endif

if (iflag_dB==1) then
write(4,'(20(1x,1pd16.9))')freqmax/1d9,-dbmax,dinput(iFWHMl,1)/1d9,dinput(iFWHMl,2),dinput(iFWHMr,1)/1d9,dinput(iFWHMr,2),BW,df/1d6,Q
endif

!enddo

stop
end
