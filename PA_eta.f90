program PA
implicit real*8(a-h,j-z)
parameter pi = 3.141592653589793238462d0

open(17,file='PA_eta.txt')

write(17,'(5(1x,a25))') 'conduction angle','eta_DE(Drain Efficiency)'

do alpha = 0.001d0,360d0,0.1d0 !ialpha = conduction angle
  alpharad = alpha*pi/180d0
  eta_DE = (alpharad-dsin(alpharad))/(2d0*(2d0*dsin(alpharad/2d0)-alpharad*dcos(alpharad/2d0)))
  write(17,'(5(1x,1pd25.9))')alpha, eta_DE
enddo





stop
end


