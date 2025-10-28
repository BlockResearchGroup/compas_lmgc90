#ifndef _wrap_visitor_h
#define _wrap_visitor_h

  /**
   * @fn void visitor_setVisitor(char * what, int where)
   * @brief Decide of the action of the visitor
   *
   * Set the action undertaken by the visitor. No default value.
   * Currently only "write" value is possible for "what" parameter,
   * in this case you must provide an unit file in "where" parameter
   * 
   * @cond PYDOC
   * usage : melimelo_setVisiton(what, where)
   * @param[in] what (string)   : set action to do during the visit
   * @param[in] where (integer) : unit used during the visit
   * @endcond
   *
   * @cond CDOC
   * @param[in] what (char[5]) : set action to do during the visit
   * @param[in] where (int)    : unit used during the visit
   * @endcond
   */
  extern "C" void visitor_setVisitor(char * what, int where);

  /**
   * @fn int visitor_openFile(char * file_name, int where)
   * @brief open a file for writing within fortran
   *
   * Open a file to write in within the fortran part of chipy library.
   * The result correspond to the iostat parameter of the OPEN fortran
   * command.
   *
   * @cond PYDOC
   * usage : iostat = visitor_openFile(file_name, where)
   * @param[in] file_name (string) : name of file to open
   * @param[in] where (integer)    : unit number referencing the opened file
   * @return err (integer)         : result of the opening command
   * @endcond
   *
   * @cond CDOC
   * @param[in] file_name (char*) : name of file to open
   * @param[in] where (int)       : number of unit referencing the opened file
   * @return err (int)            : result of the opening command
   * @endcond
   */
  extern "C" int visitor_openFile(char * file_name, int where);

  /**
   * @fn void visitor_closeFile(int where)
   * @brief close a fortran file
   *
   * Close a file previously opened
   *
   * @cond PYDOC
   * @param[in] where (integer) : unit referencing the file to close
   * @endcond
   *
   * @cond CDOC
   * @param[in] where (int) : unit referencing the file to close
   * @endcond
   */
  extern "C" void visitor_closeFile(int where);

#endif /* _wrap_visitor_h */
