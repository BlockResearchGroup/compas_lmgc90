
Programming rules
=================

General informations on Software development
--------------------------------------------

Know the language! This implies to know:

- the syntax of the language
- what you can do with what feature of the language (OOP with C++ and Python, not with Fortra9x and C)
- understand what you want to do and how

.. code-block:: fortran

   function f
     integer(kind=4) :: f
     logical is_first = .true.

     if( is_first ) then
       is_first = .false.
       f = 1
       return
     end if

     f = 2
   end function

   print * , f(), f()

Having good coding practice! They generally are:

  - commenting the code
  - write tests
  - choosing naming convention

Some general tools to use:

 - A good text editor or IDE : geany, eclipse 
 - Automatic documentation : doxygen, docstrings
 - Debugger   : gdb (>=7.2?), ddt
 - Profiling  : valgrind, gprof

Short example 
^^^^^^^^^^^^^

Check the influence of implementation and compilation options on restitution time.
Try the following code

.. code-block:: fortran

  integer(kind=4), parameter :: n=10000, m=10000
  integer(kind=4) :: i, j
  integer(kind=4), dimension(n,m) :: tab

  do i = 1, n
    do j = 1, m
      tab(i,j) = 2*tab(i,j) + 1
    end do
  end do
  
Then try this version

.. code-block:: fortran

  integer(kind=4), parameter :: n=10000, m=10000
  integer(kind=4) :: i, j
  integer(kind=4), dimension(n,m) :: tab

    do j = 1, m
  do i = 1, n
      tab(i,j) = 2*tab(i,j) + 1
    end do
  end do
  

+--------------+-----+-----+
| compil option+ -O0 | -O3 |
+--------+-----+-----+-----+
| time   | i,j | 1.92| 1.52|
|        +-----+-----+-----+
|        | j,i | 0.82| 0.3 |
+--------+-----+-----+-----+

Reminder on Fortran 9x
----------------------

Module
^^^^^^

A module is a code block gathering type definition, variables declarations and functions.
By default everything in a module is public to other modules/porgram.
It is possible to hide parts of module using the `private` option, or
to use only a part of a module using the `only` option.

.. code-block:: fortran

  module test

    implicit none

    private 

    integer(kind=4) :: fact = 4

    public f

    contains

    subroutine f(r,v)
      implicit none
      real(kind=8) :: r, v

      r  = v * fact
    end subroutine
   
  end module


  program p

    use test, only: f

    implicit none

    real(kind=8) :: val
    
    call f(val,3.d0)

    !fact = 3 ! not possible
    print * , val

  end program


Derived type
^^^^^^^^^^^^

A derived type is a user defined type allowing to gather
data in a single structure. The content of a derived type
is subject to `public`/`private` options.

The two ways to modify the content of a derived type variable are:

  - acces to the field directly if the content is public
  - write accesor functions if the content is private

.. code-block:: fortran

  module testing

    private 

   public :: f

   type T_test
      private 
      integer(kind=4) :: id = 3
      real(kind=8) :: value = 0
    end type

    type(T_test), public :: toto

    contains

    subroutine f(t,v)
      implicit none
      type(T_test) :: t
      real(kind=8) :: v

      t%value = v * t%id
    end subroutine

  end module

  program p

    use testing

    implicit none

    !toto%ID = 3 !not possible

    call f(toto,5.d0)

  end program


Coding rules within LMGC90
^^^^^^^^^^^^^^^^^^^^^^^^^^

The general structure of a module in the core of LMGC90 is:

 - a derived type
 - subroutines related to the data of this derived type
 - all data of the derived type, usually on the form of an array

Thus the derived types and variables of a module are private and
only subroutines are made public. This subroutines are getter/setter
which works with the indices of the data to modify. Sometimes there
is a getter on the pointer itself.

Each module is responsible for the memory allocation of its data. There
may be a function sizing the array, but it is never possible to give in
input an array allocated outside LMGC90. Thus there are getter on pointer
but **never** setter on pointer.

A derived type starts with `T_`. If a derived type is named
`T_xxx`, then there is an array of this type (usually allocatable)
named `xxx` in the module. In the module there is function which
allocate the array. All methods should, when possible, work only
on one element of the array; in that case the index (id) of the
array is the first input data. In that case the loop parameter is
often named `ixxx` or `i_xxx`.

Having method working only on one object at a time in the Core allows
to have different wrapper functions to work on all of them or just a
subset in the ChiPy wrapper part. Thus all openMP directives for
parallelization should be in the ChiPy part. The only exception is
the contact solver module where there is no choice but to put these
directives within the Core module.

Here are general fake example on module implementation and naming habit:

.. code-block:: fortran

  module RBDY2

    private

    type T_body
      integer(kind=4) :: id
      real(kind=8)    :: val
    end type

    type(T_body), dimension(12) :: bdyty

    public set_value

    contains

    subroutine set_value(ibdyty, i4, r8)
      implicit none
      integer(kind=4), intent(in) :: ibdyty, i4
      real(kind=8)   , intent(in) :: r8
  
      bdyty(ibdyty)%id  = i4
      bdyty(ibdyty)%val = r8
    end subroutine

  end module

Whenever possible a module always has this structure. There are some cases when
it is not possible. In the `shared` directory, there is a set of modules which
are accessible to every other modules. In these are defined some data structure
(like the general entity index list `G_i_list`), or global variables (like `H`
the time step).

Naming habits:

 - bdyty  : BoDY TYpe array
 - blmty  : Bulk eLeMent TYpe array
 - tacty  : conTACtor TYpe array
 - nodty  : NODe TYpe array
 - gpv    : Gauss Point Value
 - dof    : Degrees Of Freedom
 - drvdof : Driven Degrees Of Freedom
 - vlocy  : Velocity
 - i4     : integer on 4 bytes
 - r8     : real on 8 bytes
 - cx     : string of size 'x'
 - clin   : character line
 - G_clin : global character line

Parameters management within LMGC90
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

There are many external parameters coming either from Python or the DATBOX files reading.
These parameters are sometimes strings, and the Core should choose actions depending on
the value of the string. This is done with the `select case` statement in Fortran. But
doing a selection using is costly in terms of operation. It is best to do the selection
on an integer id. The `parameters` module is in charge of this task.

This module holds a list of id and through two functions, maps the integer id and the
string. These functions do not throw an error if an unknown id or name is given in input.
It is up to the calling function to check the results and manage the result if the need
arise.

General ideas
^^^^^^^^^^^^^

Never ever use tabulation !

Check if a function exists before wanting to coding it, or if a combination of
existing one would help.

At the very least doxygen comment any new function and its inputs/outputs.
In doing so, there are some rules to respect in order to obtain a "nice"
output (at least with sphinx):

 - Parameters definition must be the last block
 - Do not use the `'` character, but the `"` instead
 - Try to avoid the `_` charcter at the end of a word
