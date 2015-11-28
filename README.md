NAME
sbi.scm — a Silly Basic Interpreter

SYNOPSIS
sbi.scm filename

DESCRIPTION
The SB interpreter reads in an SBIR program from the file whose name is
specified in the argument list, stores it in a list, and then interprets that intermediate
representation. During interpretation, numbers are read from the
standard input and results written to the standard output. Error messages
are printed to the standard error. The first error, whether during compilation
or interpretation, causes a message to be printed and the program to exit with
an exit code of 1.

OPTIONS
None.

OPERANDS
The single filename argument specifies an SBIR program to be run.

EXIT STATUS
If the program completes without error, 0 is returned. If not, 1 is returned.
