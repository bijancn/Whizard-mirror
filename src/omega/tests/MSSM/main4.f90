! $Id: main4.f90 1203 2009-10-08 11:39:11Z jr_reuter $

program main4
  use kinds
  use tao_random_numbers
  use testbed_old
  use rambo
  use omega_amplitudes4
  use omega_parameters_mssm_4, only: &
       setup_parameters_mssm => setup_parameters, &
       gh1ww, gh2ww, &
       g_yuk13_3, g_yuk14_3  !!, &
       !!! sinckm12, sinckm13, sinckm23

  ! use omega_helas_amplitudes
  use madgraph4

  real(kind=single) :: roots
  integer :: n, tolerance
  character (len=8) :: mode

  call setup_parameters_mssm ()
  call setup_parameters ()
  call read_parameters (roots, n, tolerance, mode)
  ghww = 0
  ghbb = 0
  sinckm12 = 0
  sinckm13 = 0
  sinckm23 = 0
  call export_parameters_to_madgraph ()
  gh2ww = 0
  gh1ww = ghww
  g_yuk13_3 = 0
  g_yuk14_3 = ghbb

  !!!  This fails unless the interferences are switched off
  !!!  because the color factors are missing
  ! call check4_madgraph ("u dbar -> u dbar", n, oudb_udb, sudb_udb, udb_udb, &
  !    real (roots, kind=default), (/ mass(2), mass(1), mass(2), mass(1) /), &
  !    tolerance = tolerance, mode = mode)

  !!!  This fails becasue MADGRAPH is incomplete
  ! call check4_madgraph ("Z Z -> H H", n, ozz_hh, szz_hh, zz_hh, &
  !      real (roots, kind=default), (/ mass(23), mass(23), mass(25), mass(25) /), &
  !      states = (/ 3, 3, 1, 1 /), tolerance = tolerance, mode = mode)

  call check4_madgraph ("d dbar -> W+ W-", n, odbd_wpwm, sdbd_wpwm, dbd_wpwm, &
       real (roots, kind=default), (/ mass(1), mass(1), mass(24), mass(24) /), &
       states = (/ 2, 2, 3, 3 /), tolerance = tolerance, mode = mode)

  call check4_madgraph ("dbar d -> W+ W-", n, odbd_wpwm, sdbd_wpwm, dbd_wpwm, &
       real (roots, kind=default), (/ mass(1), mass(1), mass(24), mass(24) /), &
       states = (/ 2, 2, 3, 3 /), tolerance = tolerance, mode = mode)
  
  call check4_madgraph ("b bbar -> W+ W-", n, obbb_wpwm, sbbb_wpwm, bbb_wpwm, &
       real (roots, kind=default), (/ mass(5), mass(5), mass(24), mass(24) /), &
       states = (/ 2, 2, 3, 3 /), tolerance = tolerance, mode = mode)
  
  ! call ward4 (n, obbb_wpwm, bbb_wpwm, real (roots, kind=default), &
  !      (/ mass(5), mass(5), mass(24), mass(24) /), &
  !      3, states = (/ 2, 2, 3, 3 /), tolerance = tolerance, mode = mode)
  ! 
  ! call ward4 (n, obbb_wpwm, bbb_wpwm, real (roots, kind=default), &
  !      (/ mass(5), mass(5), mass(24), mass(24) /), &
  !      4, states = (/ 2, 2, 3, 3 /), tolerance = tolerance, mode = mode)
  ! 
  ! call ward_omega (n, obbb_wpwm, real (roots, kind=default), &
  !      (/ mass(5), mass(5), mass(24), mass(24) /), &
  !      3, states = (/ 2, 2, 3, 3 /), tolerance = tolerance, mode = mode)
  ! 
  ! call ward_omega (n, obbb_wpwm, real (roots, kind=default), &
  !      (/ mass(5), mass(5), mass(24), mass(24) /), &
  !      4, states = (/ 2, 2, 3, 3 /), tolerance = tolerance, mode = mode)

  call check4_madgraph ("W+ W- -> W+ W-", n, owpwm_wpwm, swpwm_wpwm, wpwm_wpwm, &
       real (roots, kind=default), (/ mass(24), mass(24), mass(24), mass(24) /), &
       states = (/ 3, 3, 3, 3 /), tolerance = tolerance, mode = mode)
  
  call check4_madgraph ("W+ W- -> Z Z", n, owpwm_zz, swpwm_zz, wpwm_zz, &
       real (roots, kind=default), (/ mass(24), mass(24), mass(23), mass(23) /), &
       states = (/ 3, 3, 3, 3 /), tolerance = tolerance, mode = mode)
  
  call check4_madgraph ("W+ W- -> Z A", n, owpwm_za, swpwm_za, wpwm_za, &
       real (roots, kind=default), (/ mass(24), mass(24), mass(23), 0.0_default /), &
       states = (/ 3, 3, 3, 2 /), tolerance = tolerance, mode = mode)
  
  call check4_madgraph ("W+ W- -> A A", n, owpwm_aa, swpwm_aa, wpwm_aa, &
       real (roots, kind=default), (/ mass(24), mass(24), 0.0_default, 0.0_default /), &
       states = (/ 3, 3, 2, 2 /), tolerance = tolerance, mode = mode)
  
  call check4_madgraph ("e+ e- -> W+ W-", n, oepem_wpwm, sepem_wpwm, epem_wpwm, &
       real (roots, kind=default), (/ mass(11), mass(11), mass(24), mass(24) /), &
       states = (/ 2, 2, 3, 3 /), tolerance = tolerance, mode = mode)
  
  call check4_madgraph ("e+ e- -> e+ e-", n, oepem_epem, sepem_epem, epem_epem, &
       real (roots, kind=default), &
       (/ mass(11), mass(11), mass(11), mass(11) /), &
       tolerance = tolerance, mode = mode)
  
  call check4_madgraph ("e+ e- -> nue nuebar", n, oepem_veve, sepem_veve, epem_veve, &
       real (roots, kind=default), &
       (/ mass(11), mass(11), 0.0_default, 0.0_default /), &
       tolerance = tolerance, mode = mode)

  call check4_madgraph ("e+ e- -> mu+ mu-", n, oepem_mumu, sepem_mumu, epem_mumu, &
       real (roots, kind=default), &
       (/ mass(11), mass(11), mass(13), mass(13) /), &
       tolerance = tolerance, mode = mode)

  call check4_madgraph ("e- e- -> e- e-", n, oemem_emem, semem_emem, emem_emem, &
       real (roots, kind=default), (/ mass(11), mass(11), mass(11), mass(11) /), &
       symmetry = reshape ((/ -1, 1, 2, -1, 3, 4 /), (/ 3, 2/)), &
       tolerance = tolerance, mode = mode)

  call check4_madgraph ("e- A -> e- A", n, oema_ema, sema_ema, ema_ema, &
       real (roots, kind=default), &
       (/ mass(11), 0.0_default, mass(11), 0.0_default /), &
       tolerance = tolerance, mode = mode)

  ! call ward_omega (n, oema_ema, real (roots, kind=default), &
  !      (/ mass(11), 0.0_default, mass(11), 0.0_default /), &
  !      4, tolerance = tolerance, mode = mode)

  call check4_madgraph ("e+ e- -> A A", n, oepem_aa, sepem_aa, epem_aa, &
       real (roots, kind=default), (/ mass(11), mass(11), 0.0_default, 0.0_default /), &
       symmetry = reshape ((/ 1, 3, 4 /), (/ 3, 1/)), &
       tolerance = tolerance, mode = mode)

  ! call ward4 (n, oepem_aa, epem_aa, real (roots, kind=default), &
  !      (/ mass(11), mass(11), 0.0_default, 0.0_default /), &
  !      3, tolerance = tolerance, mode = mode)
  ! 
  ! call ward4 (n, oepem_aa, epem_aa, real (roots, kind=default), &
  !      (/ mass(11), mass(11), 0.0_default, 0.0_default /), &
  !      4, tolerance = tolerance, mode = mode)
  ! 
  ! call ward_omega (n, oepem_aa, real (roots, kind=default), &
  !      (/ mass(11), mass(11), 0.0_default, 0.0_default /), &
  !      3, tolerance = tolerance, mode = mode)
  ! 
  ! call ward_omega (n, oepem_aa, real (roots, kind=default), &
  !      (/ mass(11), mass(11), 0.0_default, 0.0_default /), &
  !      4, tolerance = tolerance, mode = mode)

  call check4_madgraph ("e+ e- -> Z A", n, oepem_za, sepem_za, epem_za, &
       real (roots, kind=default), (/ mass(11), mass(11), mass(23), 0.0_default /), &
       states = (/ 2, 2, 3, 2 /), &
       tolerance = tolerance, mode = mode)

  call check4_madgraph ("e+ e- -> Z Z", n, oepem_zz, sepem_zz, epem_zz, &
       real (roots, kind=default), &
       (/ mass(11), mass(11), mass(23), mass(23) /), states = (/ 2, 2, 3, 3 /), &
       symmetry = reshape ((/ 1, 3, 4 /), (/ 3, 1/)), &
       tolerance = tolerance, mode = mode)

  call check4_madgraph ("A A -> e+ e-", n, oaa_epem, saa_epem, aa_epem, &
       real (roots, kind=default), &
       (/ 0.0_default, 0.0_default, mass(11), mass(11) /), &
       symmetry = reshape ((/ 1, 1, 2 /), (/ 3, 1/)), &
       tolerance = tolerance, mode = mode)

  call check4_madgraph ("Z A -> e+ e-", n, oza_epem, sza_epem, za_epem, &
       real (roots, kind=default), &
       (/ mass(23), 0.0_default, mass(11), mass(11) /), states = (/ 3, 2, 2, 2 /), &
       tolerance = tolerance, mode = mode)

  call check4_madgraph ("Z Z -> e+ e-", n, ozz_epem, szz_epem, zz_epem, &
       real (roots, kind=default), &
       (/ mass(23), mass(23), mass(11), mass(11) /), states = (/ 3, 3, 2, 2 /), &
       symmetry = reshape ((/ 1, 1, 2 /), (/ 3, 1/)), &
       tolerance = tolerance, mode = mode)

  call check4_madgraph ("Z Z -> nue nuebar", n, ozz_veve, szz_veve, zz_veve, &
       real (roots, kind=default), &
       (/ mass(23), mass(23), 0.0_default, 0.0_default /), states = (/ 3, 3, 2, 2 /), &
       symmetry = reshape ((/ 1, 1, 2 /), (/ 3, 1/)), &
       tolerance = tolerance, mode = mode)

  call check4_madgraph ("Z A -> u ubar", n, oza_uub, sza_uub, za_uub, &
       real (roots, kind=default), &
       (/ mass(23), 0.0_default, mass(2), mass(2) /), states = (/ 3, 2, 2, 2 /), &
       tolerance = tolerance, mode = mode)

  call check4_madgraph ("Z A -> d dbar", n, oza_ddb, sza_ddb, za_ddb, &
       real (roots, kind=default), &
       (/ mass(23), 0.0_default, mass(1), mass(1) /), states = (/ 3, 2, 2, 2 /), &
       tolerance = tolerance, mode = mode)

end program main4

