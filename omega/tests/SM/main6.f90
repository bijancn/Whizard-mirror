! main6.f90 --

program main6
  use kinds
  use tao_random_numbers
  use testbed_old
  use rambo
  use omega_amplitudes6
  ! use omega_helas_amplitudes
  use madgraph6
  implicit none

  real(kind=single) :: roots
  integer :: n, tolerance
  character (len=8) :: mode

  call setup_parameters ()
  call read_parameters (roots, n, tolerance, mode)
  call export_parameters_to_madgraph ()

  call check6_madgraph ("e+ e- -> nue nuebar b bbar", n, &
       oepem_vevebbb, sepem_vevebbb, epem_vevebbb, real (roots, kind=default), &
       (/ mass(11), mass(11), 0.0_default, 0.0_default, mass(5), mass(5) /), &
       tolerance = tolerance, mode = mode)

  call check6_madgraph ("W+ W- -> u ubar s sbar", n, &
       owpwm_uubssb, swpwm_uubssb, wpwm_uubssb, real (roots, kind=default), &
       (/ mass(24), mass(24), mass(2), mass(2) , mass(3), mass(3) /), &
       states = (/ 3, 3, 2, 2, 2, 2 /), tolerance = tolerance, mode = mode)

  call check6_madgraph ("e- e+ -> nue nuebar W+ W-", n, &
       oemep_vevewpwm, semep_vevewpwm, emep_vevewpwm, real (roots, kind=default), &
       (/ mass(11), mass(11), 0.0_default, 0.0_default, mass(24), mass(24) /), &
       states = (/ 2, 2, 2, 2, 3, 3 /), tolerance = tolerance, mode = mode)

  call check6_madgraph ("e- e+ -> e- e+ W+ W-", n, &
       oemep_emepwpwm, semep_emepwpwm, emep_emepwpwm, real (roots, kind=default), &
       (/ mass(11), mass(11), mass(11), mass(11), mass(24), mass(24) /), &
       states = (/ 2, 2, 2, 2, 3, 3 /), tolerance = tolerance, mode = mode)

  call check6_madgraph ("e- e+ -> e- nuebar W+ A (2 groves)", n, &
       oemep_emvewpa_groves, semep_emvewpa, emep_emvewpa, real (roots, kind=default), &
       (/ mass(11), mass(11), mass(11), 0.0_default, mass(24), 0.0_default /), &
       states = (/ 2, 2, 2, 2, 3, 2 /), tolerance = tolerance, mode = mode)

  call check6_madgraph ("e- e+ -> e- nuebar W+ A", n, &
       oemep_emvewpa, semep_emvewpa, emep_emvewpa, real (roots, kind=default), &
       (/ mass(11), mass(11), mass(11), 0.0_default, mass(24), 0.0_default /), &
       states = (/ 2, 2, 2, 2, 3, 2 /), tolerance = tolerance, mode = mode)

  call check6_madgraph ("e+ e- -> mu- numubar tau+ nutau", n, &
       oepem_muvmtavt, sepem_muvmtavt, epem_muvmtavt, real (roots, kind=default), &
       (/ mass(11), mass(11), mass(13), 0.0_default, mass(15), 0.0_default /), &
       tolerance = tolerance, mode = mode)

  call check6_madgraph ("e+ e- -> e+ nue e- nuebar", n, &
       oepem_epveemve, sepem_epveemve, epem_epveemve, real (roots, kind=default), &
       (/ mass(11), mass(11), mass(11), 0.0_default, mass(11), 0.0_default /), &
       tolerance = tolerance, mode = mode)

  call check6_madgraph ("e+ e- -> mu+ mu- A A", n, &
       oepem_mumuaa, sepem_mumuaa, epem_mumuaa, real (roots, kind=default), &
       (/ mass(11), mass(11), mass(13), mass(13), 0.0_default, 0.0_default /), &
       symmetry = reshape ((/ 1, 5, 6 /), (/ 3, 1/)), tolerance = tolerance, mode = mode)

  call check6_madgraph ("e+ e- -> e+ e- A A", n, &
       oepem_epemaa, sepem_epemaa, epem_epemaa, real (roots, kind=default), &
       (/ mass(11), mass(11), mass(11), mass(11), 0.0_default, 0.0_default /), &
       symmetry = reshape ((/ 1, 5, 6 /), (/ 3, 1/)), tolerance = tolerance, mode = mode)

  call check6_madgraph ("mu- e- -> mu- e- A A", n, &
       omuem_muemaa, smuem_muemaa, muem_muemaa, real (roots, kind=default), &
       (/ mass(13), mass(11), mass(13), mass(11), 0.0_default, 0.0_default /), &
       symmetry = reshape ((/ 1, 5, 6 /), (/ 3, 1/)), tolerance = tolerance, mode = mode)

  call check6_madgraph ("e- e- -> e- e- A A", n, &
       oemem_ememaa, semem_ememaa, emem_ememaa, real (roots, kind=default), &
       (/ mass(11), mass(11), mass(11), mass(11), 0.0_default, 0.0_default  /), &
       symmetry = reshape ((/ -1, 3, 4, 1, 5, 6 /), (/ 3, 2/)), &
       tolerance = tolerance, mode = mode)
  
  call check6_madgraph ("e+ e- -> A A A A", n, &
       oepem_aaaa, sepem_aaaa, epem_aaaa, real (roots, kind=default), &
       (/ mass(11), mass(11), 0.0_default, 0.0_default, 0.0_default, 0.0_default /), &
       symmetry = reshape &
       ((/ 1, 3, 4, 1, 3, 5, 1, 3, 6, 1, 4, 5, 1, 4, 6, 1, 5, 6 /), (/ 3, 6/)), &
       tolerance = tolerance, mode = mode)

  call check6_madgraph ("e+ e- -> e+ e- e+ e-", n, &
       oepem_epemepem, sepem_epemepem, epem_epemepem, real (roots, kind=default), &
       (/ mass(11), mass(11), mass(11), mass(11), mass(11), mass(11)  /), &
       symmetry = reshape ((/ -1, 3, 5, -1, 4, 6 /), (/ 3, 2/)), &
       tolerance = tolerance, mode = mode)

end program main6





