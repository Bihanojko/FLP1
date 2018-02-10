from __future__ import print_function
import os
import sys
import shutil
import filecmp


OutputFilepath = './Tests/Output/'
if not os.path.exists(OutputFilepath):
    os.makedirs(OutputFilepath + '-i')
    os.makedirs(OutputFilepath + '-t')


InputFiles = os.listdir(os.curdir + '/Tests/Input')
os.system('make')
failures = 0


for inputFile in sorted(InputFiles):
    os.system('./dka-2-mka -i ./Tests/Input/' + inputFile + ' > ./Tests/Output/-i/' + inputFile)
    os.system('./dka-2-mka -t ./Tests/Input/' + inputFile + ' > ./Tests/Output/-t/' + inputFile)


sys.stdout.write("TESTING PARAMETER -i\n")
for idx, inputFile in enumerate(sorted(InputFiles)):
    intro = '\tTEST ' + str(idx + 1) + ': '
    reference_output_path = './Tests/Input/' + inputFile

    if filecmp.cmp(OutputFilepath + '-i/' + inputFile, reference_output_path, False):
        sys.stdout.write(intro + "\033[0;32m" + "SUCCESS!\n" + "\033[0;0m")
    else:
        sys.stdout.write(intro + "\033[1;31m" + "FAIL!" + "\033[0;0m" + " (" + inputFile + ")\n")
        failures += 1


sys.stdout.write("\nTESTING PARAMETER -t\n")
for idx, inputFile in enumerate(sorted(InputFiles)):
    intro = '\tTEST ' + str(idx + 1) + ': '
    reference_output_path = './Tests/RefOutput/' + inputFile

    if filecmp.cmp(OutputFilepath + '-t/' + inputFile, reference_output_path, False):
        sys.stdout.write(intro + "\033[0;32m" + "SUCCESS!\n" + "\033[0;0m")
    else:
        sys.stdout.write(intro + "\033[1;31m" + "FAIL!" + "\033[0;0m" + " (" + inputFile + ")\n")
        failures += 1


print('\nAll tests checked, totalling ' + str(failures) + ' errors!')
