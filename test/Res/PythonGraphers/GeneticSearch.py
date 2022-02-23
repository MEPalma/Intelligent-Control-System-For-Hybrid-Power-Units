import os
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from scipy import stats


# Directory single point mutation.
_dirSPM = os.path.join(os.path.dirname(os.path.abspath(__file__)), '../GeneticSearch/SinglePointMutation/')
# Directory multi point mutation.
_dirMPM = os.path.join(os.path.dirname(os.path.abspath(__file__)), '../GeneticSearch/MultiPointMutation/')


_genSizesAll = [5, 10, 30, 80, 100, 200]
_genSizes = [100, 200] #, 30, 80, 100, 200]
_genSizesDirNames = list(map(lambda i: "GenSize_" + str(i), _genSizes))

_mutChancesAll = [0.0, 0.1, 0.3, 0.5]
_mutChances = [0.1, 0.3]
_mutChancesDirNames = list(map(lambda i: "MutChance_" + str(i).replace(".", "_"), _mutChances))

_problemNameDir = "Problem6"

_runsAll = [1, 2, 3, 4, 5, 6]
_runs = [1, 2, 3, 4, 5, 6]
_runsDirNames = list(map(lambda i: "Run" + str(i), _runs))

_csvFileName = "GADebug.csv"


def getFilePath(rootDir, genSizeDirName, mutChanceDirName, runDirName):
    return rootDir + genSizeDirName + "/" + mutChanceDirName + "/" + _problemNameDir + "/" + runDirName + "/" + _csvFileName


def getGenNumAndFitness(filepath):
    try:
        df = pd.read_csv(filepath)
        genNums = df[" Generation"].to_numpy()
        genFitness = df[" BestGene"].to_numpy()
        return genNums, genFitness

    except:
        print("[EXCLUDED]: " + filepath)
        return np.array([]), np.array([])


def getTimeElapsedAndFitness(filepath):
    try:
        df = pd.read_csv(filepath)
        genRawTimes = df["TimeStamp(ms)"].to_numpy()
        genTimeElapsed = [0]
        tmp = 0
        for i in range(1, len(genRawTimes)):
            diff = (genRawTimes[i] - genRawTimes[i-1])/1000
            tmp += diff
            genTimeElapsed.append(tmp)

        genFitness = df[" BestGene"].to_numpy()
        return genTimeElapsed, genFitness

    except:
        print("[EXCLUDED]: " + filepath)
        return np.array([]), np.array([])


def getGenNumAndStdDev(filepath):
    try:
        df = pd.read_csv(filepath)
        genNums = df[" Generation"].to_numpy()
        genStdDev = df[" StandardDeviation"].to_numpy()
        return genNums, genStdDev

    except:
        print("[EXCLUDED]: " + filepath)
        return np.array([]), np.array([])


def main():
    # Generation vs best fitness:
    #
    figT1, axisT1 = plt.subplots(nrows=len(_genSizes), ncols=len(_mutChances))
    figT1.suptitle('SinglePointCrossover(Green) and MultiPointCrossover(Red)', fontsize=8)
    #
    minFitness = float("inf")
    #
    for genSize_i in range(len(_genSizes)):
        genSize = _genSizesDirNames[genSize_i]
        #
        for mutChance_i in range(len(_mutChances)):
            mutChance = _mutChancesDirNames[mutChance_i]
            #
            axisT1[genSize_i][mutChance_i].set_title("GenSize: {}%, Mut: {}%".format(str(_genSizes[genSize_i]), str(_mutChances[mutChance_i]*100)), fontsize=6)
            axisT1[genSize_i][mutChance_i].set_xlabel("Generation Number", fontsize=6)
            axisT1[genSize_i][mutChance_i].set_ylabel("Fitness (USD)", fontsize=6)
            axisT1[genSize_i][mutChance_i].grid(zorder=0)
            for runNum in _runsDirNames:
                #
                filepathSPM = getFilePath(_dirSPM, genSize, mutChance, runNum)
                (genNumsSPM, genBestsSPM) = getGenNumAndFitness(filepathSPM)
                axisT1[genSize_i][mutChance_i].plot(genNumsSPM, genBestsSPM, color='g', alpha=0.5)
                #
                filepathMPM = getFilePath(_dirMPM, genSize, mutChance, runNum)
                (genNumsMPM, genBestsMPM) = getGenNumAndFitness(filepathMPM)
                axisT1[genSize_i][mutChance_i].plot(genNumsMPM, genBestsMPM, color='r', alpha=0.5)

                # Update new best score.
                #
                newMinFromMPM = float("inf")
                if len(genBestsMPM) > 0:
                    newMinFromMPM = min(genBestsMPM)
                #
                newMinFromSPM = float("inf")
                if len(genBestsSPM) > 0:
                    newMinFromSPM = min(genBestsSPM)
                #
                newMin = min(newMinFromMPM, newMinFromSPM)
                #
                if newMin < minFitness:
                    minFitness = newMin
    #
    # Plot best score line.
    for i in range(len(_genSizes)):
        for j in range(len(_mutChances)):
            axisT1[i][j].axhline(y=minFitness, color='b', alpha=0.5)


    # Time elapsed vs fitness.
    #
    figT2, axisT2 = plt.subplots(nrows=len(_genSizes), ncols=len(_mutChances))
    figT2.suptitle('SinglePointCrossover(Green) and MultiPointCrossover(Red)', fontsize=8)
    #
    for genSize_i in range(len(_genSizes)):
        genSize = _genSizesDirNames[genSize_i]
        #
        for mutChance_i in range(len(_mutChances)):
            mutChance = _mutChancesDirNames[mutChance_i]
            #
            axisT2[genSize_i][mutChance_i].set_title("GenSize: {}%, Mut: {}%".format(str(_genSizes[genSize_i]), str(_mutChances[mutChance_i] * 100)), fontsize=6)
            axisT2[genSize_i][mutChance_i].set_xlabel("Generation Time Elapsed(s)", fontsize=6)
            axisT2[genSize_i][mutChance_i].set_ylabel("Fitness (USD)", fontsize=6)
            axisT2[genSize_i][mutChance_i].grid(zorder=0)
            axisT2[genSize_i][mutChance_i].axhline(y=minFitness, color='b', alpha=0.5)
            for runNum in _runsDirNames:
                #
                filepathSPM = getFilePath(_dirSPM, genSize, mutChance, runNum)
                (genTimesSPM, genBestsSPM) = getTimeElapsedAndFitness(filepathSPM)
                axisT2[genSize_i][mutChance_i].plot(genTimesSPM, genBestsSPM, color='g', alpha=0.5)
                #
                filepathMPM = getFilePath(_dirMPM, genSize, mutChance, runNum)
                (genTimesMPM, genBestsMPM) = getTimeElapsedAndFitness(filepathMPM)
                axisT2[genSize_i][mutChance_i].plot(genTimesMPM, genBestsMPM, color='r', alpha=0.5)


    # Generation vs standard deviation.
    #
    figT3, axisT3 = plt.subplots(nrows=len(_genSizes), ncols=len(_mutChances))
    figT3.suptitle('SinglePointCrossover(Green) and MultiPointCrossover(Red)', fontsize=8)
    #
    minFitness = float("inf")
    #
    for genSize_i in range(len(_genSizes)):
        genSize = _genSizesDirNames[genSize_i]
        #
        for mutChance_i in range(len(_mutChances)):
            mutChance = _mutChancesDirNames[mutChance_i]
            #
            axisT3[genSize_i][mutChance_i].set_title(
                "GenSize: {}%, Mut: {}%".format(str(_genSizes[genSize_i]), str(_mutChances[mutChance_i] * 100)),
                fontsize=6)
            axisT3[genSize_i][mutChance_i].set_xlabel("Generation", fontsize=6)
            axisT3[genSize_i][mutChance_i].set_ylabel("Standard Deviation (0 to 1)", fontsize=6)
            axisT3[genSize_i][mutChance_i].grid(zorder=0)
            for runNum in _runsDirNames:
                #
                filepathSPM = getFilePath(_dirSPM, genSize, mutChance, runNum)
                (genTimesSPM, genStdsSPM) = getGenNumAndStdDev(filepathSPM)
                axisT3[genSize_i][mutChance_i].plot(genTimesSPM, genStdsSPM, color='g', alpha=0.5)
                #
                filepathMPM = getFilePath(_dirMPM, genSize, mutChance, runNum)
                (genTimesMPM, genStdssMPM) = getGenNumAndStdDev(filepathMPM)
                axisT3[genSize_i][mutChance_i].plot(genTimesMPM, genStdssMPM, color='r', alpha=0.5)


    plt.show()


if __name__ == "__main__":
    main()
