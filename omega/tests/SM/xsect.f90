! xsect.f90 --

module gauss
  use omega_kinds
  implicit none
  private

  public :: gauss1

  real (kind = double), dimension(4), private, parameter :: X_LOW = (/ &
       9.6028985649753623E-01_double, 7.9666647741362674E-01_double, &
       5.2553240991632899E-01_double, 1.8343464249564980E-01_double /)
  real (kind = double), dimension(4), private, parameter :: W_LOW = (/ &
       1.0122853629037626E-01_double, 2.2238103445337447E-01_double, &
       3.1370664587788729E-01_double, 3.6268378337836198E-01_double /)
  real (kind = double), dimension(8), private, parameter :: X_HIGH = (/ &
       9.8940093499164993E-01_double, 9.4457502307323258E-01_double, &
       8.6563120238783174E-01_double, 7.5540440835500303E-01_double, &
       6.1787624440264375E-01_double, 4.5801677765722739E-01_double, &
       2.8160355077925891E-01_double, 9.5012509837637440E-02_double /)
  real (kind = double), dimension(8), private, parameter :: W_HIGH = (/ &
       2.7152459411754095E-02_double, 6.2253523938647893E-02_double, &
       9.5158511682492785E-02_double, 1.2462897125553387E-01_double, &
       1.4959598881657673E-01_double, 1.6915651939500254E-01_double, &
       1.8260341504492359E-01_double, 1.8945061045506850E-01_double /)

contains

  pure function weighted_sum (f, midpoint, halfwidth, x, w) result (integral)
    real (kind = double) :: integral
    real (kind = double), intent(in) :: midpoint, halfwidth
    real (kind = double), dimension(:), intent(in) :: x, w
    interface
       pure function f (x) result (fx)
         use omega_kinds
         implicit none
         real (kind = double) :: fx
         real (kind = double), intent(in) :: x
       end function f
    end interface
    real (kind = double) :: delta
    integer :: i
    integral = 0
    do i = 1, size (x)
       delta = halfwidth * x(i)
       integral = integral + w(i) * (f (midpoint + delta) + f (midpoint - delta))
    end do
    integral = halfwidth * integral
  end function weighted_sum

  function gauss1 (f, a, b, eps) result (integral)
    real (kind = double) :: integral
    real (kind = double), intent(in) :: a, b, eps
    interface
       pure function f (x) result (fx)
         use omega_kinds
         implicit none
         real (kind = double) :: fx
         real (kind = double), intent(in) :: x
       end function f
    end interface
    real (kind = double) :: current_a, current_b, midpoint, halfwidth, &
         sum_low, sum_high, smallest_interval
    smallest_interval = epsilon (200 * (b - a))
    integral = 0
    if (b == a) then
       return
    end if
    current_b = a
    DIVISIONS: do
       current_a = current_b
       current_b = b
       SUBDIVIDE: do
          midpoint = 0.5_double * (current_b + current_a)
          halfwidth = 0.5_double * (current_b - current_a)
          sum_low = weighted_sum (f, midpoint, halfwidth, X_LOW, W_LOW)
          sum_high = weighted_sum (f, midpoint, halfwidth, X_HIGH, W_HIGH)
          if (abs (sum_high - sum_low) <= eps * (1 + abs (sum_high))) then
             integral = integral + sum_high
             if (current_b == b) then
                return
             else
                cycle DIVISIONS
             end if
          else if (abs (halfwidth) >= smallest_interval) then
             current_b = midpoint
             cycle SUBDIVIDE
          else
             print *, 'gauss: too high accuracy required'
             integral = 0
             return
          end if
       end do SUBDIVIDE
    end do DIVISIONS
  end function gauss1
 
end module gauss

module integrands
  use omega_kinds
  implicit none
  private
  public :: square, root, sine
contains
  pure function square (x) result (x2)
    real (kind = double) :: x2
    real (kind = double), intent(in) :: x
    x2 = x * x
  end function square
  pure function root (x) result (rootx)
    real (kind = double) :: rootx
    real (kind = double), intent(in) :: x
    rootx = sqrt (x)
  end function root
  pure function sine (x) result (sinex)
    real (kind = double) :: sinex
    real (kind = double), intent(in) :: x
    sinex = sin (x)
  end function sine
end module integrands

module differential
  use omega_kinds
  use omega_constants
  use omega_utils
  use kinematics
  implicit none
  private

  public :: dsigma_dcosth, dsigma_dcosth_pol

  ! picobarn
  real (kind = omega_prec), public, parameter :: &
       HBARC2 = 0.38937966E9_omega_prec

contains

  pure function phase_space (roots, p1, p3) result (ps)
    real (kind = double) :: ps
    real (kind = double), intent(in) :: roots
    real (kind = double), dimension(0:), intent(in) :: p1, p3
    ! sqrt ((roots**2 - m(1)**2 - m(2)**2)**2 - 4*(m(1)*m(2))**2) / (2 * roots)
    ! sqrt ((roots**2 - m(3)**2 - m(4)**2)**2 - 4*(m(3)*m(4))**2) / (2 * roots)
    ps = HBARC2 * sqrt (dot_product (p3(1:), p3(1:)) / dot_product (p1(1:), p1(1:))) &
           / (32*PI) / roots**2
  end function phase_space

  pure function dsigma_dcosth (omega, m, roots, costh, states) result (sigma)
    real (kind = double) :: sigma
    real (kind = double), dimension(:), intent(in) :: m
    real (kind = double), intent(in) :: roots, costh
    integer, dimension(:), intent(in), optional :: states
    interface
       pure function omega (k, s) result (amp)
         use omega_kinds
         implicit none
         real(kind=omega_prec), dimension(0:,:), intent(in) :: k
         integer, dimension(:), intent(in) :: s
         complex(kind=omega_prec) :: amp
       end function omega
    end interface
    real (kind = double), dimension(0:3,4) :: p
    real (kind = double) :: phi
    integer, dimension(size(p,dim=2)) :: nstates
    if (max (m(1) + m(2), m(3) + m(4)) > roots) then
       sigma = 0
    else
       if (present (states)) then
          nstates = states
       else
          nstates = 2
       end if
       phi = 0
       call beams (roots, m(1), m(2), p(:,1), p(:,2))
       call decay2 (roots, m(3), m(4), costh, phi, p(:,3), p(:,4))
       sigma = phase_space (roots, p(:,1), p(:,3)) * omega_sum (omega, p, states) 
    end if
  end function dsigma_dcosth

  pure function dsigma_dcosth_pol (omega, m, roots, costh, s) result (sigma)
    real (kind = double) :: sigma
    real (kind = double), dimension(:), intent(in) :: m
    real (kind = double), intent(in) :: roots, costh
    integer, dimension(:), intent(in) :: s
    interface
       pure function omega (k, s) result (amp)
         use omega_kinds
         implicit none
         real(kind=omega_prec), dimension(0:,:), intent(in) :: k
         integer, dimension(:), intent(in) :: s
         complex(kind=omega_prec) :: amp
       end function omega
    end interface
    real (kind = double), dimension(0:3,4) :: p
    real (kind = double) :: phi
    complex (kind = double) :: t
    if (max (m(1) + m(2), m(3) + m(4)) > roots) then
       sigma = 0
    else
       phi = 0
       call beams (roots, m(1), m(2), p(:,1), p(:,2))
       call decay2 (roots, m(3), m(4), costh, phi, p(:,3), p(:,4))
       t = omega (p, s) 
       sigma = phase_space (roots, p(:,1), p(:,3)) * t * conjg (t)
    end if
  end function dsigma_dcosth_pol

end module differential

module omega_cross_sections
  use omega_kinds
  use omega_amplitudes
  use differential
  implicit none
  private
  public :: wpwm, wpwm_pol, zz, zz_pol
  ! Global variables to facilitate integration:
  real (kind = omega_prec), public, save :: roots = 200
  integer, dimension(4), public, save :: spins4 = 0
contains
  function zz (costh) result (sigma)
    real (kind = double) :: sigma
    real (kind = double), intent(in) :: costh
    sigma = dsigma_dcosth (oepem_zz, &
         (/ mass(11), mass(11), mass(23), mass(23) /), roots, costh, (/ 2, 2, 3, 3 /))
  end function zz
  function zz_pol (costh) result (sigma)
    real (kind = double) :: sigma
    real (kind = double), intent(in) :: costh
    sigma = dsigma_dcosth_pol (oepem_zz, &
         (/ mass(11), mass(11), mass(23), mass(23) /), roots, costh, spins4)
  end function zz_pol
  function wpwm (costh) result (sigma)
    real (kind = double) :: sigma
    real (kind = double), intent(in) :: costh
    sigma = dsigma_dcosth (oepem_wpwm, &
         (/ mass(11), mass(11), mass(24), mass(24) /), roots, costh, (/ 2, 2, 3, 3 /))
  end function wpwm
  function wpwm_pol (costh) result (sigma)
    real (kind = double) :: sigma
    real (kind = double), intent(in) :: costh
    sigma = dsigma_dcosth_pol (oepem_wpwm, &
         (/ mass(11), mass(11), mass(24), mass(24) /), roots, costh, spins4)
  end function wpwm_pol
end module omega_cross_sections

program xsect
  use omega_kinds
  use omega_constants
  use omega_cross_sections
  use omega_parameters
  use gauss
  implicit none
  ! real(kind=double) :: a, b, eps, int
  ! eps = 1e-6
  ! read *, a, b
  ! int = gauss1 (square, a, b, eps)
  ! print *, int, (b**3 - a**3) / 3
  ! int = gauss1 (root, a, b, eps)
  ! print *, int, (b**1.5_double - a**1.5_double) / 1.5_double
  real(kind=double) :: roots_min, roots_max, sigma, eps, theta, costh
  real(kind=double), dimension(-1:1,-1:1,-1:1,-1:1) :: sigma_pol
  real(kind=double), dimension(-1:1,-1:1) :: sigma_pol2
  ! real(kind=double) :: sigmaz
  integer :: i, steps, i1, i2, i3, i4
  eps = 1e-6
  steps = 20
  call setup_parameters ()
  read *, roots_min, roots_max, theta
  costh = cos (PI * theta / 180)
  ! qw = 0
  ! igzww = 0
  ! igzww = - igzww
  do i = 0, steps
     roots = (roots_min * (steps - i) + roots_max * i) / steps
     sigma = gauss1 (wpwm, -costh, costh, eps)
     ! sigmaz = gauss1 (zz, -costh, costh, eps)
     ! print *, roots, sigma, sigmaz
     sigma_pol = 0
     sigma_pol2 = 0
     do i1 = -1, 1, 2
        do i2 = -1, 1, 2
           do i3 = -1, 1
              do i4 = -1, 1
                 spins4 = (/ i1, i2, i3, i4 /)
                 sigma_pol(i1,i2,i3,i4) = gauss1 (wpwm_pol, -costh, costh, eps)
                 sigma_pol2(i3,i4) = sigma_pol2(i3,i4) &
                      + sigma_pol(i1,i2,i3,i4) / 4
              end do
           end do
        end do
     end do
     print *, roots, sigma, sigma_pol2
  end do
end program xsect
