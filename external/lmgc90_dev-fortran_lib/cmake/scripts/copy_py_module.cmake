# Copy ${DIR_TO_COPY}  directory tree with all its .py files
# excluding .svn directories to ${DEST} directory.
# 
# It is not included in a CMakeLists.txt to be able to run
# the command as an input script so that pre_lmgc is up to date
# when typing 'make'

FILE(COPY ${DIR_TO_COPY}
     DESTINATION ${DEST}
     FILES_MATCHING PATTERN "*.py"
     PATTERN .svn EXCLUDE
     PATTERN doc EXCLUDE
    )
