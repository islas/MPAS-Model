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

    ! Only valid within the head node
    integer, dimension(:,:), pointer :: arr => null()

    type(id_mapping_list), pointer :: next => null()
    type(id_mapping_list), pointer :: tail => null()
    type(id_mapping_list), pointer :: head => null()

    contains

      procedure :: insert
      procedure :: length
      procedure :: gen_array
      final     :: id_mapping_list_destructor
  end type id_mapping_list

contains

  subroutine insert(self, id, id_local, bdy_local)
    class(id_mapping_list), target, intent(inout) :: self
    integer, intent(in) :: id, id_local, bdy_local
    class(id_mapping_list), pointer :: node
    if (.not. associated(self % head) .and. .not. associated(self % tail)) then
      self % head => self
      self % tail => self

      node => self
    else
      if (associated(self % head % tail % next)) then
        deallocate(self % head % tail % next)
        nullify(self % head % tail % next)
      end if

      allocate(self % head % tail % next)
      node => self % head % tail % next
      self % head % tail => node
    end if

    node % head => self % head

    node % id = id
    node % id_local_idx = id_local
    node % bdy_local_idx = bdy_local
  end subroutine insert

  function length( self ) result(list_length)
    class(id_mapping_list), target, intent(in) :: self
    integer :: list_length

    class(id_mapping_list), pointer :: node

    node => self
    list_length = 0
    do while (associated(node))
      list_length = list_length + 1
      node => node % next
    end do
  end function length

  subroutine gen_array(self, recompute)
    class(id_mapping_list), target, intent(in) :: self
    
    integer, pointer, dimension(:,:) :: arr
    logical, optional :: recompute

    class(id_mapping_list), pointer :: node
    integer :: list_length, i

    if (associated(self % head % arr)) then
      if (present(recompute) .and. recompute) then
        deallocate(self % head % arr)
        nullify(self % head % arr)
      else
        arr => self % head % arr
        return
      end if
    end if

    list_length = self % length()
    ! The fourth index here is for the final global mapping
    allocate(self % head % arr(list_length,4))
    arr => self % head % arr

    node => self
    i = 1
    do while (associated(node))
      arr(i,1) = node % id
      arr(i,2) = node % id_local_idx
      arr(i,3) = node % bdy_local_idx
      node => node % next
      i = i + 1
    end do
  end subroutine gen_array

  subroutine id_mapping_list_destructor(self)
    type(id_mapping_list), intent(inout) :: self
    if (associated(self % next)) then
      deallocate(self % next)
      nullify(self % next)
    end if
    self % head => null()
    self % tail => null()
    if (associated(self % arr)) then
      deallocate(self % arr)
      nullify(self % arr)
    end if
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
    integer :: ioType, ierr

    ierr = 0
    ierr = domain % core % get_mesh_stream( domain % configs, streamInfo, streamID )
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
    integer :: globalDimSize
    integer :: nRead, readStart, readEnd, i
    integer, dimension(:), allocatable :: readIndices, bdyIdxToID_tmp
    integer, dimension(:), pointer :: bdyMask

    ! IO
    type (MPAS_IO_Handle_type) :: mesh_handle

    ierr = 0
    ! call mpas_pool_get_dimension(readableDimensions, globalDimNames, globalDimSize)
    ! Maybe do this instead, so that the value is always the global?
    call mpas_io_inq_dim(mesh_handle, globalDimName, globalDimSize, ierr)


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
                            globalDimName, bdyMaskDimName, nBdyDimName ) result(ierr)
    use mpas_io

    implicit none

    type (mpas_streamManager_type), intent(inout) :: streamManager !< Input: Stream manager structure
    type (block_type), pointer :: blocklist !< Input/Output: Linked List of blocks
    type (mpas_pool_type), intent(inout) :: readableDimensions
    character (len=*), intent(in) :: globalDimName, bdyMaskDimName, nBdyDimName
    integer :: ierr

    type(domain_type), pointer :: domain
    integer :: nBdy_local, nBdy
    type (MPAS_IO_Handle_type) :: mesh_handle

    ierr = 0
    domain => blocklist % domain

    ierr = open_mesh_stream( domain, mesh_handle )
    if ( ierr /= 0 ) then
      ! I don't think we can recover from this...
      call mpas_log_write('Could not open mesh stream file for ' // nBdyDimName // ' computation!', &
                           messageType=MPAS_LOG_ERR )
    end if

    ierr = read_bdyMask_contiguous(domain, mesh_handle, globalDimName, bdyMaskDimName, nBdy_local )

    ! Done with IO
    call MPAS_io_close(mesh_handle, ierr)

    call mpas_dmpar_sum_int(domain % dminfo, nBdy_local, nBdy)
    
    ! Write value to readable dimensions
    call mpas_pool_add_dimension(readableDimensions, nBdyDimName, nBdy)
    call mpas_log_write('Calculated '//trim(nBdyDimName)//' = $i', intArgs=[nBdy])
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
  !  function mpas_bdy_decomp
  !
  !> \brief   Computed number of boundary cells
  !> \author  Anthony Islas
  !> \date    09 March 2026
  !> \details 
  !>  Compute the number of boundary cells across the entire domain
  !
  !-----------------------------------------------------------------------
  function mpas_bdy_decomp(block, ownedIndices, &
                           primitive, primitivePlural ) result(ierr)
#ifdef MPAS_USE_MPI_F08
    use mpi_f08
#else
   use mpi
#endif
    use mpas_io

    implicit none

    type (block_type), intent(in) :: block
    integer, dimension(:), pointer :: ownedIndices
    character (len=*), intent(in) :: primitive, primitivePlural

    integer :: ierr

    type(domain_type), pointer :: domain
    logical :: needCompute

    ! Compute decomposition at runtime
    integer :: nRead, readStart, readEnd, nBdy, nBdy_block
    integer :: i, j, k, id, bdyid, proc, nprocs, curr_proc
    integer, pointer :: nown
    integer, dimension(:), pointer :: bdyMask_block, indexToID_block

    ! When reading in bdyIdxToID_local takes the place of the reqindices_block index tracking
    integer, dimension(:), allocatable :: bdyIdxToID_cont, idToBdyIdx_cont, bdyIdxToID_local
    integer, dimension(:), allocatable :: nBdy_global, nBdy_csum
    integer, dimension(:), allocatable :: nread_global, nread_csum
    integer, dimension(:), allocatable :: nreqbyproc_block, nreqbyproc_global
    type(id_mapping_list), allocatable, dimension(:) :: reqindices_block, reqindices_global

    ! MPI communication
#ifdef MPAS_USE_MPI_F08
    type(mpi_request), dimension(:,:), allocatable :: requests
    type(mpi_status) :: probe_status
#else
    integer, dimension(:,:), allocatable :: requests
    integer :: probe_status(MPI_STATUS_SIZE)
#endif

    ! IO
    type (MPAS_IO_Handle_type) :: mesh_handle

    ! Generated field
    integer, dimension(:), pointer :: bdyIdxToID, idToBdyIdx
    integer, dimension(:), pointer :: bdyIdxToID_block, idToBdyIdx_block


    ! Names
    ! IN (Cell,Edge,Vert(ex|ices))
    ! decompDimName   : nBdyCells
    ! idxToIDName     : indexToCellID
    ! globalDimName   : nCells
    ! bdyMaskDimName  : bdyMaskCell
    ! OUT
    ! bdyIdxToIDName  : bdyCellIndexToCellID 
    ! idToBdyIdxName  : cellIDToBdyCellIndex
    character (len=:), allocatable :: capPrimitive, capPrimitivePlural
    character (len=:), allocatable :: decompDimName, idxToIDName
    character (len=:), allocatable :: globalDimName, bdyMaskDimName
    character (len=:), allocatable :: bdyIdxToIDName, idToBdyIdxName
    capPrimitive = achar(iachar(primitive(1:1)) - 32) // primitive(2:)
    capPrimitivePlural = achar(iachar(primitivePlural(1:1)) - 32) // primitivePlural(2:)

    decompDimName  = 'nBdy' // capPrimitivePlural
    idxToIDName    = 'indexTo' // capPrimitive // 'ID'
    globalDimName  = 'n' // capPrimitivePlural
    bdyMaskDimName = 'bdyMask' // capPrimitive
    bdyIdxToIDName = 'bdy' // capPrimitive // 'IndexTo' // capPrimitive // 'ID'
    idToBdyIdxName = primitive // 'IDToBdy' // capPrimitive // 'Index'


    domain => block % domain
    call mpas_pool_get_array( block % allFields, idxToIDName // '_blk', indexToID_block )
    call mpas_pool_get_dimension( block % dimensions, globalDimName, nown )
    ! nown = size(indexToID_block)

    ierr = open_mesh_stream( domain, mesh_handle )
    if ( ierr /= 0 ) then
      ! I don't think we can recover from this...
      call mpas_log_write('Could not open mesh stream file for ' // decompDimName // ' computation!', &
                           messageType=MPAS_LOG_ERR )
    end if

    needCompute = .false.
    call MPAS_io_inq_var(mesh_handle, decompDimName, ierr=ierr)
    if (ierr /= 0) then
      needCompute = .true.
    end if

    ierr = 0
    proc = domain % dminfo % my_proc_id + 1
    nprocs = domain % dminfo % nprocs

    if (needCompute) then
      ! Allocate data structures needed for this section
      allocate(nBdy_global(nprocs))
      allocate(nBdy_csum(nprocs))
      allocate(nread_global(nprocs))
      allocate(nread_csum(nprocs))
      allocate(reqindices_global(nprocs))
      allocate(reqindices_block(nprocs))
      allocate(nreqbyproc_global(nprocs))
      allocate(nreqbyproc_block(nprocs))

      allocate(requests(nprocs,2), source=MPI_REQUEST_NULL)

      ! call mpas_get_blocks_per_proc(dminfo, dminfo % my_proc_id, blocks_per_proc)
      ! if blocks_per_proc > 1 err
      ierr = read_bdyMask_contiguous(domain, mesh_handle, &
                                     globalDimName, bdyMaskDimName, &
                                     nBdy, readStart, readEnd, bdyIdxToID_cont, idToBdyIdx_cont)

      call mpas_log_write('Gathering nbdy read and contiguous read regions...')
      call mpi_allgather( nBdy, 1, MPI_INTEGER, nBdy_global, 1, MPI_INTEGER, domain % dminfo % comm, ierr )
      call mpi_allgather( readEnd - readStart + 1, 1, MPI_INTEGER, nread_global, 1, MPI_INTEGER, domain % dminfo % comm, ierr )

      ! Do cummulative sum (exclusive) for nBdy and (inclusive) for nread
      ! TODO: consider using the same buffer for message gather and cummulative sums
      nBdy_csum(1) = 0
      nBdy_csum(2:nprocs) = nBdy_global(1:nprocs - 1)
      do i = 3, nprocs
        nBdy_csum(i) = nBdy_csum(i-1) + nBdy_csum(i)
      end do

      nread_csum(1:nprocs) = nread_global(1:nprocs)
      do i = 2, nprocs
        nread_csum(i) = nread_csum(i-1) + nread_csum(i)
      end do

      ! Convert to global space by adding the cummulative sum
      idToBdyIdx_cont(:) = idToBdyIdx_cont(:) + nBdy_csum(proc)

      call mpas_log_write('Reading ' // bdyMaskDimName // ' for current block indices...')
      ! Also read in the bdy mask for this block
      allocate(bdyMask_block(nown))
      call MPAS_io_set_var_indices(mesh_handle, bdyMaskDimName, indexToID_block(1:nown), ierr=ierr)
      call mpas_io_get_var(mesh_handle, bdyMaskDimName, bdyMask_block, ierr)

      call mpas_io_close(mesh_handle)

      j = 0
      do i = 1, nown
        if (bdyMask_block(i) > 0) then
          j = j + 1
          ! May be a little slow for constantly regenerating the mask
          curr_proc = findloc(nread_csum >= indexToID_block(i), value=.true., dim=1)
          nreqbyproc_block(curr_proc) = 1
          call reqindices_block(curr_proc) % insert(indexToID_block(i), i, j)
        end if
      end do
      deallocate(bdyMask_block)
      nBdy_block = j

      call mpas_log_write('Calculating number of requests across all ranks...')
      call mpas_dmpar_sum_int_array( domain % dminfo, nprocs, nreqbyproc_block, nreqbyproc_global )

      call mpas_log_write('Sending requests for ids...')
      ! Send indices we own to process that read them in (msg tag 0)
      do curr_proc = 1, nprocs
        if (associated(reqindices_block(curr_proc) % head) .and.&
            associated(reqindices_block(curr_proc) % tail) ) then
           call reqindices_block(curr_proc) % gen_array()
           call mpi_isend( reqindices_block(curr_proc) % arr(:,1), &
                           size(reqindices_block(curr_proc) % arr, 1), &
                           MPI_INTEGER, &
                           curr_proc - 1, 0, &
                           domain % dminfo % comm, &
                           requests(curr_proc,1), ierr)
        end if
      end do

      call mpas_log_write('Receiving requests for ids...')
      ! Receive indices we read in from processes that own them (msg tag 0)
      do curr_proc = 1, nreqbyproc_global(proc)
        ! This effectively serializes the non-blocking send that happened
        call mpi_probe(MPI_ANY_SOURCE, 0, domain % dminfo % comm, probe_status, ierr)
        call mpi_get_count(probe_status, MPI_INTEGER, i, ierr)

        ! We are only keeping the indices
        allocate(reqindices_global(curr_proc) % arr(i,1))
        call mpi_recv( reqindices_global(curr_proc) % arr(:,1), i, MPI_INTEGER, &
#ifdef MPAS_USE_MPI_F08
                       probe_status % mpi_source, &
#else
                       probe_status(MPI_SOURCE), &
#endif
                       0, domain % dminfo % comm, MPI_STATUS_IGNORE, ierr)
      end do

      ! Respond to requests (msg tag 1)
      call mpas_log_write('Sending responses of bdy indices...')
      do curr_proc = 1, nprocs
        if (associated(reqindices_global(curr_proc) % arr)) then
          i = 0
          if (curr_proc > 1) then
            i = nread_csum(curr_proc - 1)
          end if
          ! Send bdy index (already in global space) as indexed by the 
          ! requested index, adjusted by the start of the contiguous read
          call mpi_isend(idToBdyIdx_cont(reqindices_global(curr_proc) % arr(:,1) - i), &
                         size(reqindices_global(curr_proc) % arr, 1), &
                         MPI_INTEGER, &
                         curr_proc - 1, 1, &
                         domain % dminfo % comm, &
                         requests(curr_proc, 2), ierr)
        end if
      end do

      ! Receive responses for our own requests that we sent out (msg tag 1)
      call mpas_log_write('Receiving responses of bdy indices...')
      do curr_proc = 1, nprocs
        if (associated(reqindices_block(curr_proc) % head) .and.&
            associated(reqindices_block(curr_proc) % tail) ) then
          ! Re-use the requests first slot which uses the same indices
          call mpi_irecv(reqindices_block(curr_proc) % arr(:,4), &
                         size(reqindices_block(curr_proc) % arr, 1), &
                         MPI_INTEGER, &
                         curr_proc - 1, 1, &
                         domain % dminfo % comm, &
                         requests(curr_proc,1), ierr)
        end if
      end do

      ! Wait for sends and recvs to clear for current non-blocking set
      call mpas_log_write('Waiting for all communication to finalize...')
      call mpi_waitall( nprocs, requests(:,1), MPI_STATUSES_IGNORE, ierr) !reshape(requests, [nprocs*2]),
      call mpi_waitall( nprocs, requests(:,2), MPI_STATUSES_IGNORE, ierr) !reshape(requests, [nprocs*2]),

      ! The reqindices_block valid arrays have the mappings now
      call mpas_log_write('Creating 1D integer fields...')
      call create_mapping_field( block % allFields, bdyIdxToIDName, decompDimName, nBdy_block )
      call create_mapping_field( block % allFields, bdyIdxToIDName // '_blk', decompDimName, nBdy_block )
      call create_mapping_field( block % allFields, idToBdyIdxName, globalDimName, nown )
      call create_mapping_field( block % allFields, idToBdyIdxName // '_blk', globalDimName, nown )

      call mpas_pool_get_array( block % allFields, bdyIdxToIDName, bdyIdxToID )
      call mpas_pool_get_array( block % allFields, bdyIdxToIDName // '_blk', bdyIdxToID_block )
      call mpas_pool_get_array( block % allFields, idToBdyIdxName, idToBdyIdx )
      call mpas_pool_get_array( block % allFields, idToBdyIdxName // '_blk', idToBdyIdx_block )

      call mpas_log_write('Assigning mapping fields...')
      do curr_proc = 1, nprocs
        if (associated(reqindices_block(curr_proc) % head) .and.&
            associated(reqindices_block(curr_proc) % tail) ) then
          ! Loop over all received indices and assign to maps, use k here for i,j mapping consistency
          do k = 1, size(reqindices_block(curr_proc) % head % arr, 1)
            id    = reqindices_block(curr_proc) % head % arr(k,1)
            i     = reqindices_block(curr_proc) % head % arr(k,2)
            j     = reqindices_block(curr_proc) % head % arr(k,3)
            bdyid = reqindices_block(curr_proc) % head % arr(k,4)

            bdyIdxToID(j) = id
            idToBdyIdx(i) = bdyid
            ! Reindexed mapping
            bdyIdxToID_block(j) = i
            idToBdyIdx_block(i) = j
          end do
        end if
      end do
    else
      ! We can simply utilize the block cells owned to directly read the decomposition
      ! Read in values
      call MPAS_io_set_var_indices(mesh_handle, idToBdyIdxName, indexToID_block(1:nown), ierr=ierr)
      call mpas_io_get_var(mesh_handle, idToBdyIdxName, idToBdyIdx, ierr)

      nBdy_block = 0
      ! Re-use this temporarily
      allocate(bdyIdxToID_cont(size(idToBdyIdx)))
      allocate(bdyIdxToID_local(size(idToBdyIdx)))
      do i = 1, size(idToBdyIdx)
        if (idToBdyIdx(i) > 0) then
          nBdy_block = nBdy_block + 1
          ! Get the true mapping and record the mapping of local bdy index to local id index
          bdyIdxToID_cont(nBdy_block) = idToBdyIdx(i)
          bdyIdxToID_local(nBdy_block) = i
        end if
      end do

      call MPAS_io_set_var_indices(mesh_handle, bdyIdxToIDName, bdyIdxToID_cont(1:nBdy_block), ierr=ierr )
      call mpas_io_get_var(mesh_handle, bdyIdxToIDName, bdyIdxToID, ierr )

      call mpas_io_close(mesh_handle)

      ! The reqindices_block valid arrays have the mappings now
      call create_mapping_field( block % allFields, bdyIdxToIDName, decompDimName, nBdy_block )
      call create_mapping_field( block % allFields, bdyIdxToIDName // '_blk', decompDimName, nBdy_block )
      call create_mapping_field( block % allFields, idToBdyIdxName, globalDimName, nown )
      call create_mapping_field( block % allFields, idToBdyIdxName // '_blk', globalDimName, nown )

      call mpas_pool_get_array( block % allFields, bdyIdxToIDName, bdyIdxToID )
      call mpas_pool_get_array( block % allFields, bdyIdxToIDName // '_blk', bdyIdxToID_block )
      call mpas_pool_get_array( block % allFields, idToBdyIdxName, idToBdyIdx )
      call mpas_pool_get_array( block % allFields, idToBdyIdxName // '_blk', idToBdyIdx_block )

      do j = 1, size(bdyIdxToID)
        i     = bdyIdxToID_local(j)
        id    = bdyIdxToID(j)
        bdyid = idToBdyIdx(i)

        bdyIdxToID(j) = id
        idToBdyIdx(i) = bdyid
        ! Reindexed mapping
        bdyIdxToID_block(j) = i
        idToBdyIdx_block(i) = j
      end do
    end if

    ! idToBdyIdx (global index) and bdyIdxToID_block (block index) should exist and be set
    call mpas_log_write('Gathering owned indices for ' // decompDimName // '...')
    allocate(ownedIndices(nBdy_block))
    do j = 1, nBdy_block
      ! Demonstrate mapping
      call mpas_log_write('Block $i owns ' // primitive // ' ID $i of bdy ' // primitive // ' index $i', &
        intArgs=[block%blockID, bdyIdxToID(j), idToBdyIdx(bdyIdxToID_block(j))])
      ownedIndices(j) = idToBdyIdx(bdyIdxToID_block(j))
    end do
  end function mpas_bdy_decomp

  subroutine create_mapping_field(pool, fieldname, dimname, dimsize)
    type (mpas_pool_type), intent(inout) :: pool
    character (len=*), intent(in) :: fieldname, dimname
    integer, intent(in) :: dimsize

    type (field1dInteger), pointer :: bdy_mapping_field
    logical :: exists

    call mpas_pool_get_field(pool, fieldname, bdy_mapping_field)
    if (.not. associated(bdy_mapping_field)) then
      exists = .false.
    else
      exists = .true.
    end if
    if (.not. exists) then
      allocate(bdy_mapping_field)
      bdy_mapping_field % fieldName = fieldname
      bdy_mapping_field % dimNames(1) = dimname
      bdy_mapping_field % dimSizes(1) = dimsize
      bdy_mapping_field % isDecomposed = .true.
      bdy_mapping_field % hasTimeDimension = .false.
      bdy_mapping_field % isActive = .true.
      bdy_mapping_field % isVarArray = .false.
      bdy_mapping_field % isPersistent = .true.
      allocate(bdy_mapping_field % attLists(1))
      allocate(bdy_mapping_field % attLists(1) % attList)
      bdy_mapping_field % attLists(1) % attList % attName = ''
      bdy_mapping_field % attLists(1) % attList % attType = -1
      nullify(bdy_mapping_field % attLists(1) % attList % next)
      nullify(bdy_mapping_field % attLists(1) % attList % attValueIntA)
      nullify(bdy_mapping_field % attLists(1) % attList % attValueRealA)
      call mpas_pool_add_field(pool, fieldname, bdy_mapping_field)
    end if
    ! Always init
    allocate(bdy_mapping_field % array(dimsize))
  end subroutine create_mapping_field


  function mpas_bdy_decomp_cells(block, manager, globalDimSize, numBlocks, ownedIndices) result(ierr)!{{{
    type (block_type), intent(in) :: block
    type (mpas_streamManager_type), intent(inout) :: manager
    integer, intent(in) :: globalDimSize
    integer, intent(in) :: numBlocks
    integer, dimension(:), pointer :: ownedIndices
    integer :: ierr

    ierr = mpas_bdy_decomp( block, ownedIndices, 'cell', 'cells' )
  end function mpas_bdy_decomp_cells


  function mpas_bdy_decomp_edges(block, manager, globalDimSize, numBlocks, ownedIndices) result(ierr)!{{{
    type (block_type), intent(in) :: block
    type (mpas_streamManager_type), intent(inout) :: manager
    integer, intent(in) :: globalDimSize
    integer, intent(in) :: numBlocks
    integer, dimension(:), pointer :: ownedIndices
    integer :: ierr

    ierr = mpas_bdy_decomp( block, ownedIndices, 'cell', 'cells' )
  end function mpas_bdy_decomp_edges

end module init_atm_calc_bdy_idx
