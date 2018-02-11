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

    with open(OutputFilepath + '-i/' + inputFile, 'r') as o:
        with open(reference_output_path, 'r') as ro:
            failed = False
            oContent = '\n'.join([x for x in o.read().split("\n") if x.strip()!=''])
            roContent = '\n'.join([x for x in ro.read().split("\n") if x.strip()!=''])
            for lineO, lineRO in zip(oContent, roContent):
                if lineO != lineRO and not (lineO + lineRO).isspace():
                    failed = True
            if failed:
                sys.stdout.write(intro + "\033[1;31m" + "FAIL!" + "\033[0;0m" + " (" + inputFile + ")\n")
                failures += 1
            else: 
                sys.stdout.write(intro + "\033[0;32m" + "SUCCESS!\n" + "\033[0;0m")

sys.stdout.write("\nTESTING PARAMETER -t\n")
for idx, inputFile in enumerate(sorted(InputFiles)):
    intro = '\tTEST ' + str(idx + 1) + ': '
    reference_output_path = './Tests/RefOutput/' + inputFile

    with open(OutputFilepath + '-t/' + inputFile, 'r') as o:
        with open(reference_output_path, 'r') as ro:
            failed = False            
            oContent = '\n'.join([x for x in o.read().split("\n") if x.strip()!=''])
            roContent = '\n'.join([x for x in ro.read().split("\n") if x.strip()!=''])
            for lineO, lineRO in zip(oContent, roContent):
                if lineO != lineRO and not (lineO + lineRO).isspace():
                    failed = True
            if failed:
                sys.stdout.write(intro + "\033[1;31m" + "FAIL!" + "\033[0;0m" + " (" + inputFile + ")\n")
                failures += 1
            else: 
                sys.stdout.write(intro + "\033[0;32m" + "SUCCESS!\n" + "\033[0;0m")


print('\nAll tests checked, totalling ' + str(failures) + ' errors!')
