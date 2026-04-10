module init_atm_calc_bdy_idx
  use mpas_derived_types
  use mpas_pool_routines
  use mpas_stream_manager
  use mpas_dmpar
  use mpas_log, only : mpas_log_write

  type id_mapping_list
    integer :: id            = -1
    integer :: id_local_idx  = -1
    integer :: bdy_local_idx = -1
    type(id_mapping_list), pointer :: next => null()
    type(id_mapping_list), pointer :: tail => null()
    type(id_mapping_list), pointer :: head => null()

    contains

      procedure :: insert
      procedure :: length
      procedure :: to_array
      final     :: id_mapping_list_destructor
  end type id_mapping_list

contains

  subroutine insert(self, id, id_local, bdy_local)
    class(id_mapping_list), pointer, intent(in) :: self
    integer, intent(in) :: id, id_local, bdy_local
    class(id_mapping_list), pointer :: node
    if (.not. associated(self % head) .and. .not. associated(tail)) then
      self % head = self
      self % tail = self

      node => self
    else
      if (associated(self % tail % next)) then
        deallocate(self % tail % next)
        nullify(self % taill % next)
      end if

      allocate(self % tail % next)
      node => self % tail % next
    end if

    node % head => self % head
    node % tail => node

    node % id = id
    node % id_local_idx = id_local
    node % bdy_local_idx = bdy_local
  end subroutine insert

  function length( self ) result(list_length)
    class(id_mapping_list), pointer, intent(in) :: self
    integer, intent(out) :: list_length

    class(id_mapping_list), pointer :: node

    node => self
    list_length = 0
    do while (associated(node))
      list_length = list_length + 1
      node => node % next
    end do
  end function size

  subroutine to_array(self, arr)
    class(id_mapping_list), pointer, intent(in) :: self
    integer, allocatable, dimension(:,:), intent(out) :: arr

    class(id_mapping_list), pointer :: node
    integer :: list_length, i

    if (allocated(arr)) then
      deallocate(arr)
    end if

    list_length = self % length()
    allocate(arr(list_length,3))

    node => self
    i = 1
    do while (associated(node))
      arr(i,1) = node % id
      arr(i,2) = node % id_local_idx
      arr(i,3) = node % bdy_local_idx
      node => node % next
      i = i + 1
    end do
  end subroutine to_array

  subroutine id_mapping_list_destructor(self)
    class(id_mapping_list), pointer, intent(in) :: self
    if (associated(self % next)) then
      deallocate(self % next)
      nullify(self % next)
    end if
    self % head => null()
    self % tail => null()
  end subroutine id_mapping_list_destructor

  !***********************************************************************
  !
  !  function open_mesh_stream
  !
  !> \brief   Quick helper function to open main mesh stream file
  !> \author  Anthony Islas
  !> \date    06 April 2026
  !> \details 
  !>  Gets the mesh stream for the current core and opens it
  !
  !-----------------------------------------------------------------------
  function open_mesh_stream(domain, handle) result(ierr)
    use mpas_io

    implicit none

    type(domain_type), pointer :: domain

    ! IO
    character (len=StrKIND) :: filename, streamID
    type (MPAS_stream_list_type), pointer :: input_stream
    type (MPAS_streamInfo_type) :: streamInfo
    type (MPAS_IO_Handle_type), intent(out) :: handle
    integer :: ioType

    ierr = 0
    call mpas_get_stream_filename(domain % streamManager, streamID, filename=filename)

    ! Determine stream io_type
    call MPAS_stream_mgr_get_property(domain % streamManager, streamID, &
                                  MPAS_STREAM_PROPERTY_IOTYPE, ioType, ierr)

    ! Try to open file
    handle = MPAS_io_open(trim(filename), MPAS_IO_READ, ioType, domain % ioContext, ierr=ierr)

    ! If to determine if file was opened or not.
    if ( ierr /= MPAS_IO_NOERR ) then
      call mpas_log_write(' ')
      call mpas_log_write('  *** unable to open stream file '// trim(filename) //' for boundary dimension computation')
      ierr = 1
    end if

  end function open_mesh_stream

  !***********************************************************************
  !
  !  function read_bdyMask_contiguous
  !
  !> \brief   Computed number of boundary cells
  !> \author  Anthony Islas
  !> \date    09 March 2026
  !> \details 
  !>  Compute the number of boundary cells across the entire domain and provide
  !>  the read in values. Upon returning bdyIdxToID and idToBdyIndex will be
  !>  allocated and filled with these values if present.
  !
  !-----------------------------------------------------------------------
  function read_bdyMask_contiguous(domain, mesh_handle, globalDimName, bdyMaskDimName, &
                           nMask, readStartOut, readEndOut, bdyIdxToID, idToBdyIndex) result(ierr)
    use mpas_io

    implicit none

    type(domain_type), pointer :: domain
    ! type (mpas_pool_type), intent(inout) :: readableDimensions
    character (len=*), intent(in) :: globalDimName, bdyMaskDimName
    integer, intent(out) :: nMask
    integer, intent(out), optional :: readStartOut, readEndOut
    integer, dimension(:), optional, allocatable, intent(out) :: bdyIdxToID, idToBdyIndex
    integer :: ierr

    type (mpas_streamManager_type), pointer :: streamManager

    ! Portion to read for this proc    
    integer, pointer :: globalDimSize
    integer :: nRead, readStart, readEnd, i
    integer, dimension(:), allocatable :: readIndices, bdyIdxToID_tmp
    integer, dimension(:), pointer :: bdyMask

    ! IO
    type (MPAS_IO_Handle_type) :: mesh_handle

    ierr = 0
    ! call mpas_pool_get_dimension(readableDimensions, globalDimNames, globalDimSize)
    ! Maybe do this instead, so that the value is always the global?
    call mpas_io_inq_dim(inputHandle, globalDimName, globalDimSize, ierr)


    call mpas_dmpar_get_index_range(domain % dminfo, 1, globalDimSize, readStart, readEnd)
    ! Each proc will read this amount of cells
    nRead = readEnd - readStart + 1

    allocate(readIndices(nRead))
    allocate(bdyMask(nRead))

    do i = 1, nRead
      readIndices(i) = i + readStart - 1
    end do

    call MPAS_io_inq_var(mesh_handle, bdyMaskDimName, ierr=ierr)
    if (ierr /= 0) then
      call mpas_log_write('Error $i', intArgs=[ierr])
    end if

    call MPAS_io_set_var_indices(mesh_handle, bdyMaskDimName, readIndices, ierr=ierr)
    call mpas_io_get_var(mesh_handle, bdyMaskDimName, bdyMask, ierr)

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
  !  function compute_dim_nbdy
  !
  !> \brief   Computed number of boundary elements in a particular dimension
  !> \author  Anthony Islas
  !> \date    09 March 2026
  !> \details 
  !>  Compute the number of boundary elements across the entire domain for
  !>  the dimension named by bdyMaskDimName. After computing the total boundary
  !>  elements, the nbdyDimName is populated in the readableDimensions. This
  !>  allows functions downstream to act as if this newly computed dimension
  !>  had been read in from the start from the mesh file.
  !
  !-----------------------------------------------------------------------
  function compute_dim_nbdy(streamManager, blocklist, readableDimensions, &
                            globalDimName, bdyMaskDimName, nbdyDimName ) result(ierr)
    use mpas_io

    implicit none

    type (mpas_streamManager_type), intent(inout) :: streamManager !< Input: Stream manager structure
    type (block_type), pointer :: blocklist !< Input/Output: Linked List of blocks
    type (mpas_pool_type), intent(inout) :: readableDimensions
    character (len=*), intent(in) :: globalDimName, bdyMaskDimName, nbdyDimName
    integer :: ierr

    type(domain_type), pointer :: domain
    integer :: nbdy_local, nbdy
    type (MPAS_IO_Handle_type) :: mesh_handle

    ierr = 0
    domain => blocklist % domain

    ierr = open_mesh_stream( domain, mesh_handle )
    if ( ierr /= 0 ) then
      ! I don't think we can recover from this...
      call mpas_log_write('Could not open mesh stream file for ' // nbdyDimName // ' computation!', &
                           messageType=MPAS_LOG_ERR )
    end if

    ierr = read_bdyMask_contiguous(domain, mesh_handle, globalDimName, bdyMaskDimName, nbdy_local )

    ! Done with IO
    call MPAS_io_close(mesh_handle, ierr)

    call mpas_dmpar_sum_int(domain % dminfo, nbdy_local, nbdy)
    
    ! Write value to readable dimensions
    call mpas_pool_add_dimension(readableDimensions, nbdyDimName, nbdy)
    call mpas_log_write('Calculated '//trim(nbdyDimName)//' = $i', intArgs=[nbdy])
  end function compute_dim_nbdy


  !***********************************************************************
  !
  !  function compute_dim_nbdy_cells
  !
  !> \brief   Computed number of boundary cells
  !> \author  Anthony Islas
  !> \date    06 April 2026
  !> \details 
  !>  Compute the number of boundary cells using ancillary function
  !
  !-----------------------------------------------------------------------
  function compute_dim_nbdy_cells(streamManager, blocklist, readableDimensions ) result(ierr)
    implicit none

    type (mpas_streamManager_type), intent(inout) :: streamManager !< Input: Stream manager structure
    type (block_type), pointer :: blocklist !< Input/Output: Linked List of blocks
    type (mpas_pool_type), intent(inout) :: readableDimensions
    integer :: ierr
    ierr = compute_dim_nbdy( streamManager, blocklist, readableDimensions, &
                             'nCells', 'bdyMaskCell', 'nBdyCells' )
  end function compute_dim_nbdy_cells

  !***********************************************************************
  !
  !  function compute_dim_nbdy_edges
  !
  !> \brief   Computed number of boundary edges
  !> \author  Anthony Islas
  !> \date    06 April 2026
  !> \details 
  !>  Compute the number of boundary edges using ancillary function
  !
  !-----------------------------------------------------------------------
  function compute_dim_nbdy_edges(streamManager, blocklist, readableDimensions ) result(ierr)
    implicit none

    type (mpas_streamManager_type), intent(inout) :: streamManager !< Input: Stream manager structure
    type (block_type), pointer :: blocklist !< Input/Output: Linked List of blocks
    type (mpas_pool_type), intent(inout) :: readableDimensions
    integer :: ierr
    ierr = compute_dim_nbdy( streamManager, blocklist, readableDimensions, &
                             'nEdges', 'bdyMaskEdge', 'nBdyEdges' )
  end function compute_dim_nbdy_edges

  !***********************************************************************
  !
  !  function compute_dim_nbdy_vertices
  !
  !> \brief   Computed number of boundary vertices
  !> \author  Anthony Islas
  !> \date    06 April 2026
  !> \details 
  !>  Compute the number of boundary vertices using ancillary function
  !
  !-----------------------------------------------------------------------
  function compute_dim_nbdy_vertices(streamManager, blocklist, readableDimensions ) result(ierr)
    implicit none

    type (mpas_streamManager_type), intent(inout) :: streamManager !< Input: Stream manager structure
    type (block_type), pointer :: blocklist !< Input/Output: Linked List of blocks
    type (mpas_pool_type), intent(inout) :: readableDimensions
    integer :: ierr
    ierr = compute_dim_nbdy( streamManager, blocklist, readableDimensions, &
                             'nVertices', 'bdyMaskVertex', 'nBdyVertices' )
  end function compute_dim_nbdy_vertices

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
                           decompDimName, idxToIDName, &
                           globalDimName, bdyMaskDimName ) result(ierr)
    use mpi_f08
    use mpas_io

    implicit none
    ! nBdyCell / nCells / bdyMaskCell / bdyCellIndexToCellID / cellIDToBdyCellIndex

    type (block_type), intent(in) :: block
    type (mpas_streamManager_type), intent(inout) :: manager
    integer, intent(in) :: globalDimSize
    integer, intent(in) :: numBlocks
    integer, dimension(:), pointer :: ownedIndices

    character (len=*), intent(in) :: decompDimName, idxToIDName
    character (len=*), intent(in) :: globalDimName, bdyMaskDimName

    integer :: ierr

    type(domain_type), pointer :: domain
    logical :: needCompute

    ! Compute decomposition at runtime
    integer :: nRead, readStart, readEnd, nBdy
    integer :: i, j, proc, nprocs, nown, curr_proc
    integer, dimension(:), pointer :: bdyMask_block, indexToID_block

    integer, dimension(:), allocatable :: bdyIdxToID_cont, idToBdyIndex_cont
    integer, dimension(:), allocatable :: nbdy_global, nbdy_csum
    integer, dimension(:), allocatable :: nread_global, nread_csum
    integer, dimension(:), allocatable :: nreqbyproc_block, nreqbyproc_global
    integer, dimension(:), allocatable :: reqindices_arr
    type(id_mapping_list), allocatable, dimension(:) :: reqindices_block, reqindices_global

    ! IO
    type (MPAS_IO_Handle_type) :: mesh_handle

    domain => block % domain

    ierr = open_mesh_stream( domain, mesh_handle )
    if ( ierr /= 0 ) then
      ! I don't think we can recover from this...
      call mpas_log_write('Could not open mesh stream file for ' // nbdyDimName // ' computation!', &
                           messageType=MPAS_LOG_ERR )
    end if

    needCompute = .false.
    call MPAS_io_inq_var(inputHandle, decompDimName, ierr=ierr)
    if (ierr /= 0) then
      needCompute = .true.
    end if

    ierr = 0
    proc = domain % dminfo % my_proc_id
    nprocs = domain % dminfo % nprocs

    if (needCompute) then
      ! Allocate data structures needed for this section
      allocate(nbdy_global(nprocs))
      allocate(nbdy_csum(nprocs))
      allocate(nread_global(nprocs))
      allocate(nread_csum(nprocs))
      allocate(reqindices_global(nprocs))
      allocate(reqindices_block(nprocs))
      allocate(nreqbyproc_global(nprocs))
      allocate(nreqbyproc_block(nprocs))

      allocate(idTo(nprocs))

      ! call mpas_get_blocks_per_proc(dminfo, dminfo % my_proc_id, blocks_per_proc)
      ! if blocks_per_proc > 1 err
      ierr = read_bdyMask_contiguous(domain, mesh_handle, &
                                     globalDimName, bdyMaskDimName, &
                                     nBdy, readStart, readEnd, bdyIdxToID_cont, idToBdyIndex_cont)

      call mpi_allgather( nbdy, 1, MPI_INTEGER, nbdy_global, 1, MPI_INTEGER, domain % dm_dminfo % comm, ierr )
      call mpi_allgather( readEnd - readStart, 1, MPI_INTEGER, nread_global, 1, MPI_INTEGER, domain % dm_dminfo % comm, ierr )

      ! Do cummulative sum (exclusive) for nbdy and (inclusive) for nread
      ! TODO: consider using the same buffer for message gather and cummulative sums
      nbdy_csum(1) = 0
      nbdy_csum(2:nprocs) = nbdy_global(1:nprocs - 1)
      do i = 3, nprocs
        nbdy_csum(i) = nbdy_csum(i-1) + nbdy_csum(i)
      end do

      nbdy_csum(1:nprocs) = nbdy_global(1:nprocs)
      do i = 2, nprocs
        nbdy_csum(i) = nbdy_csum(i-1) + nbdy_csum(i)
      end do

      ! Convert to global space by adding the cummulative sum
      idToBdyIndex(:) = idToBdyIndex(:) + nbdy_csum(proc)

      ! Also read in the bdy mask for this block
      call mpas_pool_get_array( block % allFields, idxToIDName // '_blk', indexToID_block )
      call MPAS_io_set_var_indices(mesh_handle, bdyMaskDimName, indexToID_block, ierr=ierr)
      call mpas_io_get_var(mesh_handle, bdyMaskDimName, bdyMask_block, ierr)

      j = 0
      nown = size(indexToID_block)
      do i = 1, nown
        if (bdyMask_block(i) > 0) then
          j = j + 1
          ! May be a little slow for constantly regenerating the mask
          curr_proc = findloc(nread_csum >= indexToID_block(i), value=.true.)
          nreqbyproc_block(curr_proc) = 1
          reqindices_block(curr_proc) % insert(indexToID_block(i), i, j)
        end if
      end do

      call mpas_dmpar_sum_int_array( domain % dminfo, nprocs, nreqbyproc_block, nreqbyproc_global )

      do curr_proc = 1, nprocs
        if (associated(reqindices_block(curr_proc) % head) .and.&
            associated(reqindices_block(curr_proc) % tail) ) then
           reqindices_arr
        end if
      end do




    else
      ! We can simply utilize the block cells owned to directly read the decomposition
      i = 0
    end if
  end function mpas_bdy_decomp

end module init_atm_calc_bdy_idx
