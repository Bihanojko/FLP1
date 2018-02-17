from __future__ import print_function
import os
import sys
import shutil
import filecmp

NumericSortSuffix = ''
if len(sys.argv) >= 2 and sys.argv[1] == '-numeric':
    NumericSortSuffix = 'Num'


OutputFilepath = './Tests/Output/'
if not os.path.exists(OutputFilepath):
    os.makedirs(OutputFilepath + '-i')
    os.makedirs(OutputFilepath + '-t')


InputFiles = os.listdir(os.curdir + '/Tests/Input')
failures = 0


for inputFile in sorted(InputFiles):
    os.system('./dka-2-mka -i ./Tests/Input/' + inputFile + ' > ./Tests/Output/-i/' + inputFile)
    os.system('./dka-2-mka -t ./Tests/Input/' + inputFile + ' > ./Tests/Output/-t/' + inputFile)

for outputFile in os.listdir(os.curdir + '/Tests/Output/-t/'):
    filepath = os.path.join("./Tests/Output/-t/", outputFile)
    refpath = os.path.join("./Tests/RefOutput/", outputFile)
    os.system('./normalize.py ' + filepath + " " + refpath)


sys.stdout.write("TESTING PARAMETER -i\n")
for idx, inputFile in enumerate(sorted(InputFiles)):
    intro = '\tTEST ' + str(idx + 1) + ': '
    reference_output_path = './Tests/Input/' + inputFile

    with open(OutputFilepath + '-i/' + inputFile, 'r') as o:
        with open(reference_output_path, 'r') as ro:
            failed = False
            oContent = '\n'.join([x for x in o.read().split("\n") if x.strip()!=''])
            roContent = '\n'.join([x for x in ro.read().split("\n") if x.strip()!=''])
            isError = inputFile[:5] == 'error'
            isEmpty = oContent == ""
            for lineO, lineRO in zip(oContent, roContent):
                if lineO != lineRO:
                    failed = True
            if isEmpty != isError:
                failed = True
            if failed:
                sys.stdout.write(intro + "\033[1;31m" + "FAIL!" + "\033[0;0m" + " (" + inputFile + ")\n")
                failures += 1
            else: 
                sys.stdout.write(intro + "\033[0;32m" + "SUCCESS!\n" + "\033[0;0m")

sys.stdout.write("\nTESTING PARAMETER -t\n")
for idx, inputFile in enumerate(sorted(InputFiles)):
    intro = '\tTEST ' + str(idx + 1) + ': '
    reference_output_path = './Tests/RefOutput' + NumericSortSuffix + '/' + inputFile

    with open(OutputFilepath + '-t/' + inputFile, 'r') as o:
        with open(reference_output_path, 'r') as ro:
            failed = False            
            oContent = '\n'.join([x for x in o.read().split("\n") if x.strip()!=''])
            roContent = '\n'.join([x for x in ro.read().split("\n") if x.strip()!=''])
            for lineO, lineRO in zip(oContent, roContent):
                if lineO != lineRO and not (lineO + lineRO).isspace():
                    failed = True
            if oContent != roContent:
                failed = True
            if failed:
                sys.stdout.write(intro + "\033[1;31m" + "FAIL!" + "\033[0;0m" + " (" + inputFile + ")\n")
                failures += 1
            else: 
                sys.stdout.write(intro + "\033[0;32m" + "SUCCESS!\n" + "\033[0;0m")


print('\nAll tests checked, totalling ' + str(failures) + ' errors!')
