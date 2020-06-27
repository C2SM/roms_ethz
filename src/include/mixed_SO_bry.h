#ifdef ICE_MODEL
# define IOUT linew(1)
# define IUOUT liunw(1)
# define IEOUT lienw(1)
#endif

#ifdef ICE_MODEL
#if (defined WEST_AIRADIATION   && defined WEST_AINUDGING)   || \
     defined WEST_AICLAMPED
# define WEST_AIOBC
#endif
#if (defined EAST_AIRADIATION   && defined EAST_AINUDGING)   || \
     defined EAST_AICLAMPED
# define EAST_AIOBC
#endif
#if (defined SOUTH_AIRADIATION  && defined SOUTH_AINUDGING)  || \
     defined SOUTH_AICLAMPED
# define SOUTH_AIOBC
#endif
#if (defined NORTH_AIRADIATION  && defined NORTH_AINUDGING)  || \
     defined NORTH_AICLAMPED
# define NORTH_AIOBC
#endif

#if (defined WEST_HIRADIATION   && defined WEST_HINUDGING)   || \
     defined WEST_HICLAMPED
# define WEST_HIOBC
#endif
#if (defined EAST_HIRADIATION   && defined EAST_HINUDGING)   || \
     defined EAST_HICLAMPED
# define EAST_HIOBC
#endif
#if (defined SOUTH_HIRADIATION  && defined SOUTH_HINUDGING)  || \
     defined SOUTH_HICLAMPED
# define SOUTH_HIOBC
#endif
#if (defined NORTH_HIRADIATION  && defined NORTH_HINUDGING)  || \
     defined NORTH_HICLAMPED
# define NORTH_HIOBC
#endif

#if (defined WEST_HSNRADIATION   && defined WEST_HSNNUDGING)   || \
     defined WEST_HSNCLAMPED
# define WEST_HSNOBC
#endif
#if (defined EAST_HSNRADIATION   && defined EAST_HSNNUDGING)   || \
     defined EAST_HSNCLAMPED
# define EAST_HSNOBC
#endif
#if (defined SOUTH_HSNRADIATION  && defined SOUTH_HSNNUDGING)  || \
     defined SOUTH_HSNCLAMPED
# define SOUTH_HSNOBC
#endif
#if (defined NORTH_HSNRADIATION  && defined NORTH_HSNNUDGING)  || \
     defined NORTH_HSNCLAMPED
# define NORTH_HSNOBC
#endif

#if (defined WEST_TIRADIATION   && defined WEST_TINUDGING)   || \
     defined WEST_TICLAMPED
# define WEST_TIOBC
#endif
#if (defined EAST_TIRADIATION   && defined EAST_TINUDGING)   || \
     defined EAST_TICLAMPED
# define EAST_TIOBC
#endif
#if (defined SOUTH_TIRADIATION  && defined SOUTH_TINUDGING)  || \
     defined SOUTH_TICLAMPED
# define SOUTH_TIOBC
#endif
#if (defined NORTH_TIRADIATION  && defined NORTH_TINUDGING)  || \
     defined NORTH_TICLAMPED
# define NORTH_TIOBC
#endif

#if (defined WEST_SFWATRADIATION   && defined WEST_SFWATNUDGING) || \
     defined WEST_SFWATCLAMPED
# define WEST_SFWATOBC
#endif
#if (defined EAST_SFWATRADIATION   && defined EAST_SFWATNUDGING) || \
     defined EAST_SFWATCLAMPED
# define EAST_SFWATOBC
#endif
#if (defined SOUTH_SFWATRADIATION  && defined SOUTH_SFWATNUDGING) || \
     defined SOUTH_SFWATCLAMPED
# define SOUTH_SFWATOBC
#endif
#if (defined NORTH_SFWATRADIATION  && defined NORTH_SFWATNUDGING) || \
     defined NORTH_SFWATCLAMPED
# define NORTH_SFWATOBC
#endif

#if (defined WEST_AGEICERADIATION   && defined WEST_AGEICENUDGING) || \
     defined WEST_AGEICECLAMPED
# define WEST_AGEICEOBC
#endif
#if (defined EAST_AGEICERADIATION   && defined EAST_AGEICENUDGING) || \
     defined EAST_AGEICECLAMPED
# define EAST_AGEICEOBC
#endif
#if (defined SOUTH_AGEICERADIATION  && defined SOUTH_AGEICENUDGING) || \
     defined SOUTH_AGEICECLAMPED
# define SOUTH_AGEICEOBC
#endif
#if (defined NORTH_AGEICERADIATION  && defined NORTH_AGEICENUDGING) || \
     defined NORTH_AGEICECLAMPED
# define NORTH_AGEICEOBC
#endif

#if (defined WEST_SIG11RADIATION   && defined WEST_SIG11NUDGING) || \
     defined WEST_SIG11CLAMPED
# define WEST_SIG11OBC
#endif
#if (defined EAST_SIG11RADIATION   && defined EAST_SIG11NUDGING) || \
     defined EAST_SIG11CLAMPED
# define EAST_SIG11OBC
#endif
#if (defined SOUTH_SIG11RADIATION  && defined SOUTH_SIG11NUDGING) || \
     defined SOUTH_SIG11CLAMPED
# define SOUTH_SIG11OBC
#endif
#if (defined NORTH_SIG11RADIATION  && defined NORTH_SIG11NUDGING) || \
     defined NORTH_SIG11CLAMPED
# define NORTH_SIG11OBC
#endif

#if (defined WEST_SIG22RADIATION   && defined WEST_SIG22NUDGING) || \
     defined WEST_SIG22CLAMPED
# define WEST_SIG22OBC
#endif
#if (defined EAST_SIG22RADIATION   && defined EAST_SIG22NUDGING) || \
     defined EAST_SIG22CLAMPED
# define EAST_SIG22OBC
#endif
#if (defined SOUTH_SIG22RADIATION  && defined SOUTH_SIG22NUDGING) || \
     defined SOUTH_SIG22CLAMPED
# define SOUTH_SIG22OBC
#endif
#if (defined NORTH_SIG22RADIATION  && defined NORTH_SIG22NUDGING) || \
     defined NORTH_SIG22CLAMPED
# define NORTH_SIG22OBC
#endif

#if (defined WEST_SIG12RADIATION   && defined WEST_SIG12NUDGING) || \
     defined WEST_SIG12CLAMPED
# define WEST_SIG12OBC
#endif
#if (defined EAST_SIG12RADIATION   && defined EAST_SIG12NUDGING) || \
     defined EAST_SIG12CLAMPED
# define EAST_SIG12OBC
#endif
#if (defined SOUTH_SIG12RADIATION  && defined SOUTH_SIG12NUDGING) || \
     defined SOUTH_SIG12CLAMPED
# define SOUTH_SIG12OBC
#endif
#if (defined NORTH_SIG12RADIATION  && defined NORTH_SIG12NUDGING) || \
     defined NORTH_SIG12CLAMPED
# define NORTH_SIG12OBC
#endif

#if (defined WEST_MIRADIATION   && defined WEST_MINUDGING)   || \
     defined WEST_MICLAMPED
# define WEST_MIOBC
#endif
#if (defined EAST_MIRADIATION   && defined EAST_MINUDGING)   || \
     defined EAST_MICLAMPED
# define EAST_MIOBC
#endif
#if (defined SOUTH_MIRADIATION  && defined SOUTH_MINUDGING)  || \
     defined SOUTH_MICLAMPED
# define SOUTH_MIOBC
#endif
#if (defined NORTH_MIRADIATION  && defined NORTH_MINUDGING)  || \
     defined NORTH_MICLAMPED
# define NORTH_MIOBC
#endif
#endif
