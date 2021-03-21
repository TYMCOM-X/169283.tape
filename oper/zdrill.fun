        ZDRILL.FUN

        FUNCTIONAL SPECIFICATION FOR THE DRILL PROGRAM

        ZDRILL is a program which  reads  a  drill  file  generated  by
WIRRAP  and  generates  in turn a file used either to generate a paper-
tape which will drive an automatic drilling machine, or to  generate  a
full-size  plot  on  a  ZETA-plotter  which  may  be used to verify the
positions of the holes to be drilled by the drilling machine.

        Control is via commands input from the terminal, in response to
questions typed by the program.  As a response to the  first  question,
instructions for using the program are (optionally) typed.  ZDRILL then
requests  the  name of the input file, and verifies it as a valid drill
file.  This MUST be an old file.  The program then asks if a drill tape
is to be generated for the drilling machine.  If so, the  program  asks
for  the  name  of the output file to be used.  If this is an OLD file,
the program asks the user to  verify  it's  usage,  then  the  file  is
written.   If the user does not want a tape file generated, the program
asks if he wants a plot generated.   If so, it is done, and the program
exits.  If not, the program exits immediately.
