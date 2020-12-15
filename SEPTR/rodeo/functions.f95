module functions
implicit none
contains

function phaseEffect (phase, multEarly, multInter, multLate) result (res)
  double precision, intent(in):: phase, multEarly, multInter, multLate
  double precision:: res
  double precision, parameter:: SMALL=1d-6
  if (phase .lt. -SMALL) then
    res= multEarly
  else if (phase .gt. SMALL) then
    res= multLate
  else
    res= multInter
  end if
end function

! Growth rate as function of temperature; Ratkowsky et al., 1982
! https://www.ncbi.nlm.nih.gov/pmc/articles/PMC216584/pdf/jbacter00260-0019.pdf
function mu(t, t0, b) result (res)
  double precision, intent(in):: t, b, t0
  double precision:: res
  res= (b * (t - t0))**2d0
end function

!rm(list=ls())
!
!# temperatures
!t <- 5:37
!
!plot(range(t), c(0,3), type="n")
!abline(h=1, lty=3)
!
!# arrhenius
!mu <- 2
!theta <- 1.08
!tRef <- 37
!lines(t, mu * theta^(t-tRef), lty=1)
!
!# ratkowsky1982
!b <- 0.045
!t0 <- 4
!lines(t, (b*(t-t0))^2, lty=2)

end module
