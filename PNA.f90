program PNA
implicit real*8(a,b,d-h,j-z), complex*16(c)
character*5 dum
dimension freq(2001),S11(2001,3),S12(2001,3),S21(2001,3),S22(2001,3),Loss(2001,1),Lossdb(2001,1),Lossrev(2001,1),Lossrevdb(2001,1)

indata=1601
start_f=3.4d9
stop_f=3.6d9
del_f=(stop_f-start_f)/(indata-1)
!ij_jump=int(0.1d9/del_f)

open (7,file='XB-71M_Q_SW_1.2P_2.4N_1.5P_QLNA.s2p')
!open (7,file='lna_6.8n_duprx_nm_100p_18n_tube.s2p')
open (17,file='output.txt')
write (17,'(5(1x,a25))')'Freq.(Hz)','Loss(dB)','|S11|^2+|S12|^2<1 lossy','port1 Cu correct'

do i = 1,8
  read (7,*)
enddo

!SXX(ij,1) in dB, SXX(ij,2) in ANG
!20log10|SXX|, <SXX
do ij = 1,indata
  read (7,*) freq(ij),S11(ij,1),S11(ij,2),S21(ij,1),S21(ij,2),S12(ij,1),S12(ij,2),S22(ij,1),S22(ij,2)
enddo
!freq(ij),S11(ij,1),S11(ij,2),S12(ij,1),S12(ij,2),S21(ij,1),S21(ij,2),S22(ij,1),S22(ij,2)

!	  do j=1,indata 
!	  freq(j)=start_f+del_f*(j-1)
!	  S11(j,3)=10.d0*dlog10(S11(j,1)**2+S11(j,2)**2)
!	  S21(j,3)=10.d0*dlog10(S21(j,1)**2+S21(j,2)**2)
!	  S12(j,3)=10.d0*dlog10(S12(j,1)**2+S12(j,2)**2)
!	  S22(j,3)=10.d0*dlog10(S22(j,1)**2+S22(j,2)**2)
!	  enddo
!
!	  do j=1,indata
!	  write(17,'(5(1x,1pd16.9))') freq(j),S11(j,3),S21(j,3),S12(j,3),S22(j,3)
!	  write(*,'(5(1x,1pd16.9))') freq(j),S11(j,3),S21(j,3),S12(j,3),S22(j,3)
!	  enddo

do ij = 1,indata,1
  freq(ij) = start_f+del_f*(ij-1)
  S11(ij,3) = 10d0**(S11(ij,1)/20d0) !|SXX| in MAG
  S21(ij,3) = 10d0**(S21(ij,1)/20d0)
  S12(ij,3) = 10d0**(S12(ij,1)/20d0)
  S22(ij,3) = 10d0**(S22(ij,1)/20d0)

  Loss(ij,1) = S21(ij,3)**2/(1d0-S11(ij,3)**2)
  !Loss(ij,1) = S12(ij,3)**2/(1d0-S22(ij,3)**2)
  Lossdb(ij,1) = -10d0*dlog10(Loss(ij,1))
  Lossrev(ij,1) = S12(ij,3)**2/(1d0-S22(ij,3)**2)
  Lossrevdb(ij,1) = -10d0*dlog10(Lossrev(ij,1))
enddo

write (*,'(5(1x,a25))')'Freq.(Hz)','Loss(dB)','|S11|^2+|S12|^2<1 lossy','port1 Cu correct' !,'S12','S21','S22'
do ij = 1,indata,800  !30~40GHz !ij=1,801,8 
    write(17,'(5(1x,1pd25.9))') freq(ij),Lossdb(ij,1),S11(ij,3)**2+S12(ij,3)**2 !,S21(ij,3),S12(ij,3),S22(ij,3)
    write(*,'(5(1x,1pd25.9))') freq(ij),Lossdb(ij,1),S11(ij,3)**2+S12(ij,3)**2 !,S21(ij,3),S12(ij,3),S22(ij,3)
    !write(*,'(5(1x,1pd16.9))') S11(ij,3)**2+S21(ij,3)**2
enddo

write (17,'(5(1x,a25))')'Freq.(Hz)','Loss(dB)','|S11|^2+|S12|^2<1 lossy','Rev port2 ANT'
write (*,'(5(1x,a25))')'Freq.(Hz)','Loss(dB)','|S11|^2+|S12|^2<1 lossy','Rev port2 ANT' !,'S12','S21','S22'
do ij = 1,indata,800  !30~40GHz !ij=1,801,8 
    write(17,'(5(1x,1pd25.9))') freq(ij),Lossrevdb(ij,1),S11(ij,3)**2+S12(ij,3)**2
    write(*,'(5(1x,1pd25.9))') freq(ij),Lossrevdb(ij,1),S11(ij,3)**2+S12(ij,3)**2 !,S21(ij,3),S12(ij,3),S22(ij,3)  
    !write(*,'(5(1x,1pd16.9))') S11(ij,3)**2+S21(ij,3)**2, Lossrev(ij,1)  
enddo

stop
end


