#!/usr/bin/env python3
import sys

with open(sys.argv[1], 'r') as fl:
    states = fl.readline().strip().split(",")
    initState = fl.readline().strip()
    endStates = fl.readline().strip().split(",")
    rules = []
    for line in fl:
        rules.append(line.strip().split(","))

with open(sys.argv[2], 'r') as fl:
    refStates = fl.readline().strip().split(",")
    refInitState = fl.readline().strip()
    refEndStates = fl.readline().strip().split(",")
    refRules = []
    for line in fl:
        refRules.append(line.strip().split(","))

#########################################################################
#                    HERE NORMALIZE YOUR OUTPUT                         #
#########################################################################

#########################################################################

with open(sys.argv[1], 'w') as fl:
    fl.write(",".join(states) + "\n")
    fl.write(initState + "\n")
    fl.write(",".join(endStates) + "\n")
    for rule in rules:
        fl.write(",".join(rule) + "\n")
