! This is compact version of "netcdf.inc" file: in contains only
! subset of declarations which are actually used by ROMS code and
! associated utilities. 
!
      integer nf_char,      nf_int,    nf_float,   nf_double,
     &        nf_nowrite,   nf_write,  nf_clobber, nf_nofill,
     &        nf_64bit_offset,
     &        nf_unlimited, nf_global, nf_noerr,   nf_enameinuse,
     &        nf_max_dims
      parameter(nf_char = 2,   nf_nowrite = 0,  nf_unlimited = 0,
     &          nf_int = 4,    nf_write = 1,    nf_global = 0,
     &          nf_float = 5,  nf_clobber = 0,  nf_noerr = 0,
     &          nf_64bit_offset = 512,
     &          nf_double = 6, nf_nofill = 256, nf_enameinuse = -42,
     &          nf_max_dims = 100)

!DL: prevent the following substitutions if PGI is set, so that the
! netcdf installation under /opt/netcdf/3.6.2 (on Brutus) can be used:
!#if defined X__IFC || defined PGI
#ifdef X__IFC
# define nf_strerror nf_strerror_
# define nf_create nf_create_
# define nf_open nf_open_
# define nf_set_fill nf_set_fill_
# define nf_redef nf_redef_
# define nf_enddef nf_enddef_
# define nf_sync nf_sync_
# define nf_close nf_close_
# define nf_inq nf_inq_
# define nf_inq_nvars nf_inq_nvars_
# define nf_def_dim nf_def_dim_
# define nf_inq_dimid nf_inq_dimid_
# define nf_inq_dim nf_inq_dim_
# define nf_inq_dimname nf_inq_dimname_
# define nf_inq_dimlen nf_inq_dimlen_
# define nf_rename_dim nf_rename_dim_
# define nf_inq_att nf_inq_att_
# define nf_inq_attname nf_inq_attname_
# define nf_inq_attlen nf_inq_attlen_
# define nf_copy_att nf_copy_att_
# define nf_rename_att nf_rename_att_
# define nf_put_att_text nf_put_att_text_
# define nf_get_att_text nf_get_att_text_
# define nf_put_att_int nf_put_att_int_
# define nf_get_att_int nf_get_att_int_
# define nf_put_att_real nf_put_att_real_
# define nf_get_att_real nf_get_att_real_
# define nf_put_att_double nf_put_att_double_
# define nf_get_att_double nf_get_att_double_
# define nf_def_var nf_def_var_
# define nf_inq_var nf_inq_var_
# define nf_inq_varid nf_inq_varid_
# define nf_inq_varname nf_inq_varname_
# define nf_inq_varndims nf_inq_varndims_
# define nf_rename_var nf_rename_var_
# define nf_put_var_text nf_put_var_text_
# define nf_get_var_text nf_get_var_text_
# define nf_put_var_int nf_put_var_int_
# define nf_get_var_int nf_get_var_int_
# define nf_put_var_real nf_put_var_real_
# define nf_get_var_real nf_get_var_real_
# define nf_put_var_double nf_put_var_double_
# define nf_get_var_double nf_get_var_double_
# define nf_put_var1_text nf_put_var1_text_
# define nf_get_var1_text nf_get_var1_text_
# define nf_put_var1_int nf_put_var1_int_
# define nf_put_var1_real nf_put_var1_real_
# define nf_get_var1_real nf_get_var1_real_
# define nf_put_var1_double nf_put_var1_double_
# define nf_get_var1_double nf_get_var1_double_
# define nf_put_vara_text nf_put_vara_text_
# define nf_get_vara_text nf_get_vara_text_
# define nf_put_vara_int nf_put_vara_int_
# define nf_get_vara_int nf_get_vara_int_
# define nf_put_vara_real nf_put_vara_real_
# define nf_get_vara_real nf_get_vara_real_
# define nf_put_vara_double nf_put_vara_double_
# define nf_get_vara_double nf_get_vara_double_
# define nf_get_var1_int nf_get_var1_int_
# define nf_inq_ndims nf_inq_ndims_
# define nf_inq_unlimdim nf_inq_unlimdim_
#endif


      character*80   nf_strerror
      external       nf_strerror

      integer nf_open,   nf_close,         nf_create,
     &  nf_sync,         nf_enddef,        nf_redef,
     &  nf_set_fill,     nf_inq,           nf_inq_nvars, 
     &  nf_def_var,      nf_def_dim,       nf_copy_att,
     &  nf_rename_var,   nf_rename_dim,    nf_rename_att,
     &  nf_inq_var,      nf_inq_dim,       nf_inq_att,
     &  nf_inq_varid,    nf_inq_dimid,     nf_inq_attname,
     &  nf_inq_varname,  nf_inq_dimname,   nf_inq_attlen,
     &  nf_inq_varndims, nf_inq_dimlen,    nf_inq_ndims,
     &  nf_put_att_text, nf_get_att_text,  nf_inq_unlimdim,
     &  nf_put_att_int,  nf_put_att_real,  nf_put_att_double,
     &  nf_get_att_int,  nf_get_att_real,  nf_get_att_double,
     &  nf_put_var_text, nf_put_var1_text, nf_put_vara_text,
     &  nf_get_var_text, nf_get_var1_text, nf_get_vara_text,
     &  nf_put_var_int,  nf_put_var_real,  nf_put_var_double,
     &  nf_get_var_int,  nf_get_var_real,  nf_get_var_double,
     &  nf_put_var1_int, nf_put_var1_real, nf_put_var1_double,
     &  nf_get_var1_int, nf_get_var1_real, nf_get_var1_double,
     &  nf_put_vara_int, nf_put_vara_real, nf_put_vara_double,
     &  nf_get_vara_int, nf_get_vara_real, nf_get_vara_double

      external nf_open,  nf_close,         nf_create,
     &  nf_sync,         nf_enddef,        nf_redef,
     &  nf_set_fill,     nf_inq,           nf_inq_nvars,
     &  nf_def_var,      nf_def_dim,       nf_copy_att,
     &  nf_rename_var,   nf_rename_dim,    nf_rename_att,
     &  nf_inq_var,      nf_inq_dim,       nf_inq_att,
     &  nf_inq_varid,    nf_inq_dimid,     nf_inq_attname,
     &  nf_inq_varname,  nf_inq_dimname,   nf_inq_attlen,
     &  nf_inq_varndims, nf_inq_dimlen,    nf_inq_ndims,
     &  nf_put_att_text, nf_get_att_text,  nf_inq_unlimdim,
     &  nf_put_att_int,  nf_put_att_real,  nf_put_att_double,
     &  nf_get_att_int,  nf_get_att_real,  nf_get_att_double,
     &  nf_put_var_text, nf_put_var1_text, nf_put_vara_text,
     &  nf_get_var_text, nf_get_var1_text, nf_get_vara_text,
     &  nf_put_var_int,  nf_put_var_real,  nf_put_var_double,
     &  nf_get_var_int,  nf_get_var_real,  nf_get_var_double,
     &  nf_put_var1_int, nf_put_var1_real, nf_put_var1_double,
     &  nf_get_var1_int, nf_get_var1_real, nf_get_var1_double,
     &  nf_put_vara_int, nf_put_vara_real, nf_put_vara_double,
     &  nf_get_vara_int, nf_get_vara_real, nf_get_vara_double

