# dka-2-mka

Application transforms a deterministic FSM without unreachable states to a minimal 
FSM. The input FSM is transformed to a fully defined one if necessary and afterward 
minimalization is done using an algorithm introduced in TIN, which is built on 
computation of k-indistinguishability.

The name of the state added during FSM completion (sink state) is chosen as the first
vacant number in the sequence 0, 1, 2, ... The merged states are always named after 
the smallest element in relevant group (according to lexicographical order).

# Usage

To build the application, run "make" or "make ARGS='{parameters}'" for build options 
specification. Application can be run using the command:
    ./dka-2-mka {-i|-t|-t -i|-i -t} [filename]
Parameter '-i' transforms the FSM to inner representation and then outputs it in its
original form. Parameter '-t' denotes minimalization of the FSM and then output of
the minimal FSM. If run with both parameters ('-i -t' or '-t -i'), the application
executes both commands in the given order. 
If no filename is specified, the program reads from stdin.

# Testing

The application has been tested on a set of tests, which can be found in subfolder "Tests",
with the use of a script dka-2-mka-test.py. To run the tests, execute "make test".
The set of tests and the script have been developed in collaboration with Ondrej Vales 
(xvales03) and Matej Marusak (xmarus06). My contibution are tests error-test1.in - 
error-test8.in and test1.in - test20.in and a major part of the testing script.

The testing set is also available at https://github.com/Bihanojko/dka-2-mka-tests, where 
the exact contibution of every collaborator can be found. 
