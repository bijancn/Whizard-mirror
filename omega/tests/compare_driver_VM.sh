#! /bin/sh
# $Id: compare_driver.sh 6040 2014-07-23 15:17:31Z bchokoufe $
########################################################################

tag=VM
odefault="$1"
ovm="$2"
oparams="$2 $3"
shift 3

models="QED QCD SM"

modules=""

########################################################################
while read module threshold abs_threshold n roots model mode process; do

  case $module in

   '#'*) # skip comments
     ;;

   '')   # skip empty lines
     ;;

    *)
      ########################################################################
      modules="$modules $module"
      eval threshold_$module=$threshold
      eval abs_threshold_$module=$abs_threshold
      eval n_$module=$n
      eval roots_$module=$roots
      eval process_$module="'$process'"
      eval model_$module="'$model'"
      ########################################################################

      ov1="`echo $odefault | sed s/%%%/$model/g`"
      #echo "running $ov1 -$mode '$process'" 1>&2
      $ov1  "$@" \
        -target:parameter_module parameters_$model \
        -target:module amplitude_compare_${tag}_v1_${module} \
        -$mode "$process" 2>/dev/null
      ov2="`echo $ovm | sed s/%%%/$model/g`"
      #bc_file="`echo $process | sed 's/->/to/g' | sed 's/ /_/g'`".hbc
      bc_file=$module.hbc
      eval bc_file_$module=$bc_file
      #echo "running $ov2 -$mode '$process', saving to $bc_file" 1>&2
      $ov2 "$@" -$mode "$process" 2>/dev/null 1> $bc_file
    ;;
  esac

done
########################################################################

for model in $models; do
  ovp="`echo $oparams | sed s/%%%/$model/g`"
  #params_file="`echo 'parameters_wrapper_%%%' | sed s/%%%/$model/g`".f90
  #echo "writing parameter file $params_file with '$ovp'" 1>&2
  $ovp 2>/dev/null
done

for module in $modules; do

    for mode in v1 v2; do

      if [ $mode == v2 ]; then
        eval bc_file="\${bc_file_$module}"
        eval model="\${model_$module}"

cat <<EOF
module amplitude_compare_${tag}_${mode}_${module}
  use omegavm95
  use iso_varying_string, string_t => varying_string
  use parameters_wrapper_${model}
  use parameters_${model}
  implicit none
  private
  type(vm_t) :: vm
  type(string_t) :: bytecode_file
  public :: number_particles_in, number_particles_out, number_spin_states, &
    spin_states, number_flavor_states, flavor_states, number_color_indices, &
    number_color_flows, color_flows, number_color_factors, color_factors, &
    color_sum, new_event, reset_helicity_selection, is_allowed, get_amplitude, &
    init

  contains

     pure function number_particles_in () result (n)
       integer :: n
       n = vm%number_particles_in ()
     end function number_particles_in

     pure function number_particles_out () result (n)
       integer :: n
       n = vm%number_particles_out ()
     end function number_particles_out

     pure function number_spin_states () result (n)
       integer :: n
       n = vm%number_spin_states ()
     end function number_spin_states

     pure subroutine spin_states (a)
       integer, dimension(:,:), intent(out) :: a
       call vm%spin_states (a)
     end subroutine spin_states

     pure function number_flavor_states () result (n)
       integer :: n
       n = vm%number_flavor_states ()
     end function number_flavor_states

     pure subroutine flavor_states (a)
       integer, dimension(:,:), intent(out) :: a
       call vm%flavor_states (a)
     end subroutine flavor_states

     pure function number_color_indices () result (n)
       integer :: n
       n = vm%number_color_indices ()
     end function number_color_indices

     pure function number_color_flows () result (n)
       integer :: n
       n = vm%number_color_flows ()
     end function number_color_flows

     pure subroutine color_flows (a, g)
       integer, dimension(:,:,:), intent(out) :: a
       logical, dimension(:,:), intent(out) :: g
       call vm%color_flows (a, g)
     end subroutine color_flows

     pure function number_color_factors () result (n)
       integer :: n
       n = vm%number_color_factors ()
     end function number_color_factors

     pure subroutine color_factors (cf)
       use omega_color
       type(omega_color_factor), dimension(:), intent(out) :: cf
       call vm%color_factors (cf)
     end subroutine color_factors

     !pure unless OpenMP
     !pure function color_sum (flv, hel) result (amp2)
     function color_sum (flv, hel) result (amp2)
       use kinds
       integer, intent(in) :: flv, hel
       real(kind=default) :: amp2
       amp2 = vm%color_sum (flv, hel)
     end function color_sum

     subroutine new_event (p)
       use kinds
       real(kind=default), dimension(0:3,*), intent(in) :: p
       call vm%new_event (p)
     end subroutine new_event

     subroutine reset_helicity_selection (threshold, cutoff)
       use kinds
       real(kind=default), intent(in) :: threshold
       integer, intent(in) :: cutoff
       call vm%reset_helicity_selection (threshold, cutoff)
     end subroutine reset_helicity_selection

     pure function is_allowed (flv, hel, col) result (yorn)
       logical :: yorn
       integer, intent(in) :: flv, hel, col
       yorn = vm%is_allowed (flv, hel, col)
     end function is_allowed

     pure function get_amplitude (flv, hel, col) result (amp_result)
       use kinds
       complex(kind=default) :: amp_result
       integer, intent(in) :: flv, hel, col
       amp_result = vm%get_amplitude(flv, hel, col)
     end function get_amplitude

     subroutine init ()
        bytecode_file = '$bc_file'
        call init_parameters ()
        call initialize_vm (vm, bytecode_file)
     end subroutine
end module amplitude_compare_${tag}_${mode}_${module}
EOF
        init="call init()"
      else
        init=""
      fi

cat <<EOF
module interface_compare_${tag}_${mode}_${module}
  use omega_interface
  use amplitude_compare_${tag}_${mode}_${module}
  implicit none
  private
  public :: load
contains
  function load () result (p)
    type(omega_procedures) :: p
    $init
    p%number_particles_in => number_particles_in
    p%number_particles_out => number_particles_out
    p%number_spin_states => number_spin_states
    p%spin_states => spin_states
    p%number_flavor_states => number_flavor_states
    p%flavor_states => flavor_states
    p%number_color_indices => number_color_indices
    p%number_color_flows => number_color_flows
    p%color_flows => color_flows
    p%number_color_factors => number_color_factors
    p%color_factors => color_factors
    p%color_sum => color_sum
    p%new_event => new_event
    p%reset_helicity_selection => reset_helicity_selection
    p%is_allowed => is_allowed
    p%get_amplitude => get_amplitude
  end function load
end module interface_compare_${tag}_${mode}_${module}

EOF

    done

done

########################################################################

cat <<EOF
program compare_driver
  use kinds
  use compare_lib
EOF

for module in $modules; do
    for mode in v1 v2; do
cat <<EOF
  use interface_compare_${tag}_${mode}_${module}, load_${mode}_${module} => load
EOF
    done
done

for model in $models; do
cat <<EOF
  use parameters_$model, init_parameters_$model => init_parameters
EOF
done

cat <<EOF
  implicit none
  integer, parameter :: N = 1000
  real(kind=default), parameter :: ROOTS = 1000
  integer, parameter :: SEED = 42
  integer :: failures, attempts, failed_processes, attempted_processes
  failed_processes = 0
  attempted_processes = 0
EOF

for model in $models; do
cat <<EOF
  call init_parameters_$model ()
EOF
done

for module in $modules; do

eval process="\${process_$module}"
eval n="\${n_$module}"
eval threshold="\${threshold_$module}"
eval abs_threshold="\${abs_threshold_$module}"
eval roots="\${roots_$module}"

cat <<EOF
  print *, "checking process '$process'"
  call check (load_v1_$module (), load_v2_$module (), &
              roots = real ($roots, kind=default), &
              threshold = real ($threshold, kind=default), &
              n = $n, seed = SEED, &
              abs_threshold = real ($abs_threshold, kind=default), &
              failures = failures, attempts = attempts)
  if (failures > 0) then
     print *, failures, " failures in ", attempts, " attempts"
     failed_processes = failed_processes + 1
  end if
EOF
done

cat <<EOF
  if (failed_processes > 0) then
     print *, failed_processes, " failed processes in ", attempted_processes, " attempts"
     stop 1
  end if
end program compare_driver
EOF

exit 0
