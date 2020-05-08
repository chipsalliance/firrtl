#!/usr/bin/env python3
# See LICENSE for license details.

import argparse, csv, copy, json
from abc import ABC, abstractmethod
from collections import OrderedDict
from scipy.stats.mstats import gmean
from statistics import mean, median, stdev

class Statistic(ABC):

    @property
    @abstractmethod
    def name(self):
        pass

    @abstractmethod
    def compute(self, data):
        pass

class Mean(Statistic):

    name = 'mean'

    def compute(self, data):
        return mean(data)

class GeometricMean(Statistic):

    name = 'geomean'

    def compute(self, data):
        return gmean(data)

class Median(Statistic):

    name = 'median'

    def compute(self, data):
        return median(data)

class StandardDeviation(Statistic):

    name = 'stdev'

    def compute(self, data):
        return stdev(data)

statistics = { Mean().name: Mean(),
               GeometricMean().name: GeometricMean(),
               Median().name: Median(),
               StandardDeviation().name: StandardDeviation() }

def parseargs():
    parser = argparse.ArgumentParser("Compute FIRRTL Benchmark Statistics",
                                     formatter_class=argparse.ArgumentDefaultsHelpFormatter)
    parser.add_argument('files', type=argparse.FileType('r'), nargs='+', help='Input CSV files')
    parser.add_argument('--output', type=argparse.FileType('w'), default='-', help='Output JSON file to write')
    parser.add_argument('--pretty-print', action='store_const', const=4, help='Pretty print JSON output')
    return parser.parse_args()

def writeCSV(dictionary):
    for jvm in dictionary:
        for rev in dictionary[jvm]:
            for des in dictionary[jvm][rev]:
                for met in dictionary[jvm][rev][des]:
                    for stat in dictionary[jvm][rev][des][met]['stats']:
                        print('{},{},{},{}-{}, {}'.format(jvm, rev, des, met, stat, dictionary[jvm][rev][des][met]['stats'][stat]))
    pass

def main():
    args = parseargs()

    aggregate = {}
    emptyDict = { 'memory': { 'raw': [], 'stats': {} },
                  'runtime': { 'raw': [], 'stats': {} } }

    for f in args.files:
        for row in csv.DictReader(f, quoting=csv.QUOTE_NONNUMERIC):
            jvm, rev, des, m, r = row['jvm'], row['revision'], row['design'], row['memory'], row['runtime']
            aggregate.setdefault(jvm, {}).setdefault(rev, OrderedDict()).setdefault(des, copy.deepcopy(emptyDict))['memory']['raw'].append(m)
            aggregate[jvm][rev][des]['runtime']['raw'].append(r)

    for a in aggregate:
        for b in aggregate[a]:
            for c in aggregate[a][b]:
                for d in aggregate[a][b][c]:
                    for s in statistics:
                        aggregate[a][b][c][d]['stats'][s] = statistics[s].compute(aggregate[a][b][c][d]['raw'])

    foo = {}
    for jvm in aggregate:
        for rev in aggregate[jvm]:
            for des in aggregate[jvm][rev]:
                for met in aggregate[jvm][rev][des]:
                    for stat in aggregate[jvm][rev][des][met]['stats']:
                        foo.setdefault(jvm, {}).setdefault(met, {}).setdefault(rev, OrderedDict()).setdefault(stat, {})[des] = aggregate[jvm][rev][des][met]['stats'][stat]

    json.dump(foo, args.output, indent=args.pretty_print)

if __name__ == '__main__':
    main()
