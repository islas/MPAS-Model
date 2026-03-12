module init_atm_calc_bdy_idx
  use mpas_derived_types
  use mpas_pool_routines
  use mpas_stream_manager
  use mpas_dmpar
  use mpas_log, only : mpas_log_write

  contains


  !***********************************************************************
  !
  !  function read_bdyMask_contiguous
  !
  !> \brief   Computed number of boundary cells
  !> \author  Anthony Islas
  !> \date    09 March 2026
  !> \details 
  !>  Compute the number of boundary cells across the entire domain
  !
  !-----------------------------------------------------------------------
  function read_bdyMask_contiguous(domain, readableDimensions, globalDimName, bdyMaskDimName, &
                           nMask, readStartOut, readEndOut, bdyIdxToID, idToBdyIndex) result(ierr)
    use mpas_io

    implicit none

    type(domain_type), pointer :: domain
    type (mpas_pool_type), intent(inout) :: readableDimensions
    character (len=*), intent(in) :: globalDimName, bdyMaskDimName
    integer, intent(out) :: nMask
    integer, intent(out), optional :: readStartOut, readEndOut
    integer, dimension(:), pointer, optional :: bdyIdxToID, idToBdyIndex
    integer :: ierr

    type (mpas_streamManager_type), pointer :: streamManager

    ! Portion to read for this proc    
    integer, pointer :: globalDimSize
    integer :: nRead, readStart, readEnd, i
    integer, dimension(:), pointer :: readIndices
    integer, dimension(:), pointer :: bdyMask, bdyIdxToID_tmp

    ! IO
    character (len=StrKIND) :: filename, streamID
    type (MPAS_stream_list_type), pointer :: input_stream
    type (MPAS_streamInfo_type) :: streamInfo
    type (MPAS_IO_Handle_type) :: inputHandle
    integer :: ioType

    ierr = 0
    call mpas_pool_get_dimension(readableDimensions, globalDimName, globalDimSize)

    call mpas_dmpar_get_index_range(domain % dminfo, 1, globalDimSize, readStart, readEnd)
    nRead = readEnd - readStart + 1

    ! Each proc will read this amount of cells
    ierr = domain % core % get_mesh_stream( domain % configs, streamInfo, streamID )
    call mpas_get_stream_filename(domain % streamManager, streamID, filename=filename)


    ! Determine stream io_type
    call MPAS_stream_mgr_get_property(domain % streamManager, streamID, &
                                  MPAS_STREAM_PROPERTY_IOTYPE, ioType, ierr)

    ! Try to open file
    inputHandle = MPAS_io_open(trim(filename), MPAS_IO_READ, ioType, domain % ioContext, ierr=ierr)

    ! If to determine if file was opened or not.
    if ( ierr /= MPAS_IO_NOERR ) then
      call mpas_log_write(' ')
      call mpas_log_write('  *** unable to open stream file '// trim(filename) //' for ' // trim(bdyMaskDimName) //' compute')
      ierr = 1
      return
    end if

    ! We are good to read from inputHandle
    allocate(readIndices(nRead))
    allocate(bdyMask(nRead))

    do i = 1, nRead
      readIndices(i) = i + readStart - 1
    end do

    call MPAS_io_inq_var(inputHandle, bdyMaskDimName, ierr=ierr)
    if (ierr /= 0) then
      call mpas_log_write('Error $i', intArgs=[ierr])
    end if

    call MPAS_io_set_var_indices(inputHandle, bdyMaskDimName, readIndices, ierr=ierr)
    call mpas_io_get_var(inputHandle, bdyMaskDimName, bdyMask, ierr)

    ! Done with IO
    call MPAS_io_close(inputHandle, ierr)

    if (present(idToBdyIndex)) then
      allocate(idToBdyIndex(nRead))
      idToBdyIndex(:) = 0
    end if

    if (present(bdyIdxToID)) then
      ! nRead is the max we could have
      allocate(bdyIdxToID_tmp(nRead))
    end if

    ! Count amount of bdyMask greater than 0
    nMask = 0
    do i = 1, nRead
      if (bdyMask(i) > 0) then
        nMask = nMask + 1
        if (present(idToBdyIndex)) then
          ! This is in local bdyIndex indexing to this contiguous read
          idToBdyIndex(i) = nMask
        end if
        if (present(bdyIdxToID)) then
          ! This is already in global cell indexing
          bdyIdxToID_tmp(nMask) = i + readStart - 1
        end if
      end if
    end do

    if (present(bdyIdxToID)) then
      allocate(bdyIdxToID(nMask))
      bdyIdxToID(:) = bdyIdxToID_tmp(:nMask)
      deallocate(bdyIdxToID_tmp)
    end if

    call mpas_log_write('This proc has $i ' // trim(bdyMaskDimName) // ' greater than 0', intArgs=[nMask])

    deallocate(readIndices)
    deallocate(bdyMask)

    if (present(readStartOut)) then
      readStartOut = readStart
    end if
    if (present(readEndOut)) then
      readEndOut = readEnd
    end if

  end function read_bdyMask_contiguous


  !***********************************************************************
  !
  !  function compute_dim_nbdy_cells
  !
  !> \brief   Computed number of boundary cells
  !> \author  Anthony Islas
  !> \date    09 March 2026
  !> \details 
  !>  Compute the number of boundary cells across the entire domain
  !
  !-----------------------------------------------------------------------
  function compute_dim_nbdy_cells(streamManager, blocklist, readableDimensions) result(ierr)
    use mpas_io

    implicit none

    type (mpas_streamManager_type), intent(inout) :: streamManager !< Input: Stream manager structure
    type (block_type), pointer :: blocklist !< Input/Output: Linked List of blocks
    type (mpas_pool_type), intent(inout) :: readableDimensions
    integer :: ierr

    type(domain_type), pointer :: domain

    ! ! Portion to read for this proc
    ! integer, pointer :: nCells
    ! integer :: nReadCells, readCellStart, readCellEnd, nBdyCell_local, nBdyCell, i
    integer :: nBdyCell_local, nBdyCell
    ! integer, dimension(:), pointer :: readIndices
    ! integer, dimension(:), pointer :: bdyMaskCell
    integer, dimension(:), pointer :: nBdyCellCollect

    ierr = 0
    domain => blocklist % domain

    ierr = read_bdyMask_contiguous(domain, readableDimensions, 'nCells', 'bdyMaskCell', nBdyCell_local )
    call mpas_dmpar_sum_int(domain % dminfo, nBdyCell_local, nBdyCell)
    
    ! Write value to readable dimensions
    call mpas_pool_add_dimension(readableDimensions, 'nBdyCells', nBdyCell)
    call mpas_log_write('Calculated nBdyCells = $i', intArgs=[nBdyCell])

  end function compute_dim_nbdy_cells


  !***********************************************************************
  !
  !  function mpas_bdy_decomp
  !
  !> \brief   Computed number of boundary cells
  !> \author  Anthony Islas
  !> \date    09 March 2026
  !> \details 
  !>  Compute the number of boundary cells across the entire domain
  !
  !-----------------------------------------------------------------------
  function mpas_bdy_decomp(block, streamManager, globalDimSize, numBlocks, ownedIndices &
                           decompDimName, bdyIndexName, cellIDName, &
                           globalDimName, bdyMaskDimName ) result(ierr)
    use mpas_io

    implicit none
    nBdyCell / nCells / bdyMaskCell / bdyCellIndexToCellID / cellIDToBdyCellIndex

    type (block_type), intent(in) :: block
    type (mpas_streamManager_type), intent(inout) :: manager
    integer, intent(in) :: globalDimSize
    integer, intent(in) :: numBlocks
    integer, dimension(:), pointer :: ownedIndices

    character (len=*), intent(in) :: decompDimName, bdyIndexName, cellIDName
    character (len=*), intent(in) :: globalDimName, bdyMaskDimName

    integer :: ierr

    type(domain_type), pointer :: domain
    logical :: needCompute

    ! Compute decomposition on the fly
    integer :: nRead, readStart, readEnd, nBdy, i
    integer, dimension(:), pointer :: bdyIdxToID, idToBdyIndex
    integer, dimension(:), pointer :: nBdyCollect

    ! IO
    character (len=StrKIND) :: filename, streamID
    type (MPAS_stream_list_type), pointer :: input_stream
    type (MPAS_streamInfo_type) :: streamInfo
    type (MPAS_IO_Handle_type) :: inputHandle
    integer :: ioType

    domain => block % domain

    ! Check if IO fields exist, and if not compute decomp
    ierr = domain % core % get_mesh_stream( domain % configs, streamInfo, streamID )
    call mpas_get_stream_filename(domain % streamManager, streamID, filename=filename)

    ! Determine stream io_type
    call MPAS_stream_mgr_get_property(domain % streamManager, streamID, &
                                  MPAS_STREAM_PROPERTY_IOTYPE, ioType, ierr)

    inputHandle = MPAS_io_open(trim(filename), MPAS_IO_READ, ioType, domain % ioContext, ierr=ierr)

    if ( ierr /= MPAS_IO_NOERR ) then
      call mpas_log_write(' ')
      call mpas_log_write('  *** unable to open stream file '// trim(filename) //' for ' // trim(decompDimName) //' decomposition')
      ierr = 1
      return
    end if

    needCompute = .false.
    call MPAS_io_inq_var(inputHandle, decompDimName, ierr=ierr)
    if (ierr /= 0) then
      needCompute = .true.
    end if
    call MPAS_io_inq_var(inputHandle, bdyIndexName, ierr=ierr)
    if (ierr /= 0) then
      needCompute = .true.
    end if
    call MPAS_io_inq_var(inputHandle, cellIDName, ierr=ierr)
    if (ierr /= 0) then
      needCompute = .true.
    end if

    ierr = 0

    if (needCompute) then
      ! close the file and let other routines handle IO now
      call mpas_io_close(inputHandle, ierr)

      ! call mpas_get_blocks_per_proc(dminfo, dminfo % my_proc_id, blocks_per_proc)
      ! if blocks_per_proc > 1 err
      
      
    else
      ! We can simply utilize the block cells owned to directly read the decomposition

    end if
  end function mpas_bdy_decomp

end module init_atm_calc_bdy_idx
