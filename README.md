**deadwood** finds and removes files and directories that haven't been looked at
for a while.

Given one or more paths, it checks the contained files and subdirectories to
determine if they are fresh or stale. **Files** are considered stale if their
access timestamp is older than the specified age. **Directories** are considered
stale if none of the files contained within them or within any of their
recursive subdirectories are fresh. As soon as one fresh file is found, the
entire directory is considered fresh.

`dw [-a <age>] [-r] <path>*`

Without arguments, dw will print a list of all old files and directories it
finds.

**-a, --age <age>**  
Maximum age in days (default: 30)

**-r, --remove**  
remove stale files and directories

### Build notes

The master branch is configured to build using CCL via `make`. If you want to
build using ECL, check out `ecl-build`, then run the build script build-ecl.lisp
via ECL. The binary built using ECL is considerably smaller and its size can be
further reduced by stripping. It takes noticeably longer to run which should
however not be an issue in an expectable use case such as a weekly background
job.
