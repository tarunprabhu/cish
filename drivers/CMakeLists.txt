set(CISHCC ${CMAKE_CURRENT_BINARY_DIR}/cishcc)
set(CISHCXX ${CMAKE_CURRENT_BINARY_DIR}/cish++)

configure_file(cishcc.py ${CISHCC}  @ONLY)
configure_file(cishcc.py ${CISHCXX} @ONLY)

# FIXME: A better way to do this would probably be to create a single
# copy of cishcc.py and then create symlinks to it named cishcc and cish++
file(
  COPY ${CISHCC}
  DESTINATION ${CMAKE_PROJECT_BINDIR}
  FILE_PERMISSIONS
  OWNER_READ OWNER_WRITE OWNER_EXECUTE
  GROUP_READ GROUP_EXECUTE
  WORLD_READ WORLD_EXECUTE)

file(
  COPY ${CISHCXX}
  DESTINATION ${CMAKE_PROJECT_BINDIR}
  FILE_PERMISSIONS
  OWNER_READ OWNER_WRITE OWNER_EXECUTE
  GROUP_READ GROUP_EXECUTE
  WORLD_READ WORLD_EXECUTE)
