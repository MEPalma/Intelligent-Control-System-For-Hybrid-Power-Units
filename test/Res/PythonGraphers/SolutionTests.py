import os
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from scipy import stats
import statistics


# Directory.
_dir = os.path.join(os.path.dirname(os.path.abspath(__file__)), '../InContextPerformances/')


_scenarios = range(1, 31)
_scenariosDirNames = list(map(lambda i: "Scenario" + str(i), _scenarios))


_runs = range(1, 7)
_runsDirNames = list(map(lambda i: "Genetics/Run" + str(i), _runs))


def getNoHybridFilePath(scenarioName):
    return _dir + scenarioName + "/NoHybridSimulatedResult.txt"


def getSimulatedFilePath(scenarioName):
    return _dir + scenarioName + "/SimulatedResult.txt"


def getGeneticFilePath(scenarioName, runName):
    return _dir + scenarioName + "/" + runName + "/GADebug.csv"


def getScenarioGeneticCosts(scenarioName):
    allCosts = []
    for r in _runsDirNames:
        filepath = getGeneticFilePath(scenarioName, r)
        try:
            df = pd.read_csv(filepath)
            costs = list(df[" BestGene"])
            allCosts += costs

        except:
            print("[EXCLUDED]: " + filepath)
            return []

    return list(allCosts)


def getBestGeneticCostInScenario(scenarioName):
    allCosts = getScenarioGeneticCosts(scenarioName)
    return min(allCosts)


def getScenarioNoHybridCost(scenarioName):
    filepath = getNoHybridFilePath(scenarioName)
    cost = 0
    try:
        with open(filepath, 'r') as file:
            lines = file.readlines()
            file.close()
            lastLine = lines[-1]
            cost = float(lastLine.split(" ")[1])
    except:
        print("[EXCLUDED]: " + filepath)
        return cost

    return cost


def getScenarioSimulationCost(scenarioName):
    filepath = getSimulatedFilePath(scenarioName)
    cost = 0
    try:
        with open(filepath, 'r') as file:
            lines = file.readlines()
            file.close()
            lastLine = lines[-1]  # eg: USD 4.66938681 (1.0000USD)
            cost = float(lastLine.split(" ")[1])
    except:
        print("[EXCLUDED]: " + filepath)
        return cost

    return cost


def main():

    logs = []
    for s in _scenariosDirNames:
        nohybrid = getScenarioNoHybridCost(s)
        simulated = getScenarioSimulationCost(s)
        gen = getBestGeneticCostInScenario(s)
        logs.append((nohybrid, simulated, gen))
    #
    print("Logs: " + str(logs))
    #

    logsPercents = []
    for (o, h, n) in logs:
        op = 100
        hp = h * 100 / o
        on = n * 100 / o
        logsPercents.append((op, hp, on))
    #
    print("Logs in percentages: " + str(logsPercents))

    avgO = 100 # sum(map((lambda x: x[0]), logsPercents))/len(logsPercents)
    avgH = sum(map((lambda x: x[1]), logsPercents)) / len(logsPercents)
    avgN = sum(map((lambda x: x[2]), logsPercents)) / len(logsPercents)
    avgHN = sum(map((lambda x: x[1]-x[2]), logsPercents)) / len(logsPercents)
    #
    print("Average Gains: " + str((avgO, avgH, avgN)))
    print("Average Gain to standard hybrid: " + str(avgHN))
    print("MAX % Gained: " + str(max(list(map((lambda x: x[1] - x[2]), logsPercents)))))

    stdH = statistics.pstdev(map((lambda x: x[1]), logsPercents))
    stdN = statistics.pstdev(map((lambda x: x[2]), logsPercents))
    #
    print("StdDev Gains:" + str((stdH, stdN)))

    # Histogram of distribution of N gains.
    binsize = 15
    plt.figure()
    plt.hist(list(map((lambda x: x[1] - x[2]), logsPercents[0:8])), density=True, bins=binsize, alpha=0.5, label="Vehicle1")
    plt.hist(list(map((lambda x: x[1] - x[2]), logsPercents[8:16])), density=True, bins=binsize, alpha=0.5, label="Vehicle2")
    plt.hist(list(map((lambda x: x[1] - x[2]), logsPercents[16:24])), density=True, bins=binsize, alpha=0.5, label="Vehicle3")
    plt.hist(list(map((lambda x: x[1] - x[2]), logsPercents[24:])), density=True, bins=binsize, alpha=0.5, label="Vehicle4")
    plt.xlabel('Percentage Gain Over Simulated Solution')
    plt.ylabel('Bin size')
    plt.title('Clustering of Gain Amounts Around Scenarios')
    plt.legend(loc=2)
    plt.draw()

    # Bar Chart of 3 modes gains.
    # Per %.
    # plt.bar(_scenarios, list(map((lambda x: x[0]), logs)), label="Non Hybrid")
    # plt.bar(_scenarios, list(map((lambda x: x[1]), logs)), label="Simulated Hybrid")
    # plt.bar(_scenarios, list(map((lambda x: x[2]), logs)), label="Itinerary-Suited Hybrid")
    # plt.grid(color='#95A5A5', linestyle='--', linewidth=2, axis='y', alpha=0.7)
    # plt.xlabel('Test Scenario (index)')
    # plt.ylabel('Cost (USD)')
    # plt.title('Cost reduction per scenario.')
    # plt.legend(loc=2)
    # plt.draw()
    #
    # Per USD.
    # plt.bar(_scenarios, list(map((lambda x: x[0]), logsPercents)), label="Non Hybrid")
    # plt.bar(_scenarios, list(map((lambda x: x[1]), logsPercents)), label="Simulated Hybrid")
    # plt.bar(_scenarios, list(map((lambda x: x[2]), logsPercents)), label="Itinerary-Suited Hybrid")
    # plt.grid(color='#95A5A5', linestyle='--', linewidth=2, axis='y', alpha=0.7)
    # plt.xlabel('Test Scenario (index)')
    # plt.ylabel('Percentage Cost')
    # plt.title('Percentage cost gained per scenario.')
    # plt.legend(loc=2)
    # plt.draw()

    plt.show()


if __name__ == "__main__":
    main()
