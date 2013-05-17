/*
!
! Copyright (C) 1991-2005  ; All Rights Reserved ; ATMET, LLC
! 
! This file is free software; you can redistribute it and/or modify it under the
! terms of the GNU General Public License as published by the Free Software 
! Foundation; either version 2 of the License, or (at your option) any later version.
! 
! This software is distributed in the hope that it will be useful, but WITHOUT ANY 
! WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A 
! PARTICULAR PURPOSE.  See the GNU General Public License for more details.
!
! You should have received a copy of the GNU General Public License along with this 
! program; if not, write to the Free Software Foundation, Inc., 
! 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
!======================================================================================
*/

#if defined(SUN) || defined(ALPHA) || defined(SGI) || defined (PC_LINUX1)

#define fh5f_open          fh5f_open_
#define fh5f_create        fh5f_create_
#define fh5f_close         fh5f_close_
#define fh5d_open          fh5d_open_
#define fh5d_close         fh5d_close_
#define fh5s_get_ndims     fh5s_get_ndims_
#define fh5s_get_dims      fh5s_get_dims_
#define fh5_prepare_read   fh5_prepare_read_
#define fh5d_read          fh5d_read_
#define fh5_close_read     fh5_close_read_
#define fh5_prepare_write  fh5_prepare_write_
#define fh5_write          fh5_write_
#define fh5_close_write    fh5_close_write_

#endif
