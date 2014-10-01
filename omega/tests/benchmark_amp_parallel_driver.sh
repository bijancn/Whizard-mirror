#! /bin/sh
# benchmark_amp_parallel.sh - Compare between amplitude
# parallelization and phase space parallelization
########################################################################

odefault="$1"
ovm="$2"
oparams="$2 $3"
shift 3

models="QED QCD SM"

modules=""

########################################################################
while read module n roots model mode process; do

  case $module in

   '#'*) # skip comments
     ;;

   '')   # skip empty lines
     ;;

    *)
      ########################################################################
      modules="$modules $module"
      eval n_$module=$n
      eval roots_$module=$roots
      eval process_$module="'$process'"
      eval model_$module="'$model'"
      ########################################################################

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
  $ovp 2>/dev/null
done

########################################################################

cat <<EOF
program benchmark_amp_parallel
  use kinds
  use benchmark_lib
  use omp_lib
  use omegavm95
  use iso_varying_string, string_t => varying_string
EOF

for model in $models; do
cat <<EOF
  use parameters_$model, init_parameters_$model => init_parameters
  use parameters_wrapper_$model, initialize_vm_$model => initialize_vm
EOF
done

cat <<EOF
  implicit none
  type(vm_t), dimension(:), allocatable :: vms
  type(vm_t) :: vm
  integer, parameter :: SEED = 42
  integer :: max_threads, num_threads, i_threads, i, nout
  real(double) :: wtime_start
  real(double), dimension(:), allocatable :: elapsed_ps, elapsed_amp
  type(string_t) :: bytecode_file
  real(kind=default), dimension(:,:,:), allocatable :: p

  write(*, "(A)") '#All times in ms/phasespacepoint. All colors and helicities computed.'
  write(*, "(A)") '#Either the phase space is parallelized or the computation of a single amplitude'
  write(*, "(A)") '#Process  threads  phasespace_parallel    amplitude_parallel    amplitude/helicity'
  max_threads = omp_get_max_threads ()
  allocate (elapsed_ps(max_threads))
  allocate (elapsed_amp(max_threads))
  call omp_set_dynamic (.false.)
EOF

for model in $models; do
cat <<EOF
  call init_parameters_$model ()
EOF
done

for module in $modules; do

eval process="\${process_$module}"
eval n="\${n_$module}"
eval roots="\${roots_$module}"
eval bc_file="\${bc_file_$module}"
eval model="\${model_$module}"

cat <<EOF
  bytecode_file = '$bc_file'
  call initialize_vm_$model (vm, bytecode_file)
  nout = vm%number_particles_out ()
  allocate (p(0:3, nout+2, max_threads))
  do i = 1, max_threads
    call beams (${roots}.0_default, 0.0_default, 0.0_default, p(:,1,i), p(:,2,i))
  end do

  do num_threads = 1, max_threads
     call omp_set_num_threads (num_threads)
     allocate (vms(num_threads))
     do i_threads = 1, num_threads
        call initialize_vm_$model (vms(i_threads), bytecode_file)
     end do

     wtime_start = omp_get_wtime ()
     !\$omp parallel do
     do i_threads = 1, num_threads
        do i = 1, $n / num_threads
           call massless_isotropic_decay (${roots}.0_default, p(:,3:, i_threads))
           call vms(i_threads)%new_event (p(:,:,i_threads))
        end do
     end do
     !\$omp end parallel do
     elapsed_ps(num_threads) = omp_get_wtime () - wtime_start
     deallocate(vms)

     wtime_start = omp_get_wtime ()
     do i = 1, $n
        call massless_isotropic_decay (${roots}.0_default, p(:,3:, 1))
        call vm%new_event (p(:,:,1))
        !print *, vm%color_sum (1, 1)
        !print *, i
     end do
     elapsed_amp(num_threads) = omp_get_wtime () - wtime_start
  end do

  deallocate(p)
  elapsed_amp = elapsed_amp * 1000 / $n
  elapsed_ps = elapsed_ps * 1000 / $n
  do num_threads = 1, max_threads
     write(*, "(A,7X,I2,5X,F19.5,F19.5,F19.5,F19.5,F19.5)") "$module", num_threads, &
                     elapsed_ps(num_threads), elapsed_amp(num_threads), &
                     elapsed_amp(num_threads) / elapsed_ps(num_threads), &
                     elapsed_ps(1) / elapsed_ps(num_threads), &
                     elapsed_amp(1) / elapsed_amp(num_threads)
  end do
EOF
done

cat <<EOF
end program benchmark_amp_parallel
EOF

exit 0
