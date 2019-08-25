#!/usr/bin/env python3

from statistics import median, stdev
import argparse
import numbers

from common import *

def parseargs():
    parser = argparse.ArgumentParser("Benchmark FIRRTL")
    parser.add_argument('--designs', type=str, nargs='+',
                        help='FIRRTL input files to use as benchmarks')
    parser.add_argument('--versions', type=str, nargs='+', default=['HEAD'],
                        help='FIRRTL commit hashes to benchmark')
    parser.add_argument('--iterations', '-N', type=int, default=10,
                        help='Number of times to run each benchmark')
    parser.add_argument('--jvms', type=str, nargs='+', default=['java'],
                        help='JVMs to use')
    return parser.parse_args()

def main():
    args = parseargs()
    designs = args.designs
    check_designs(designs)
    hashes = get_version_hashes(args.versions)
    jars = build_firrtl_jars(hashes)
    jvms = args.jvms
    N = args.iterations
    info = [['java', 'revision', 'design', 'max heap', 'SD', 'runtime', 'SD']]
    for java in jvms:
        print("Running with '{}'".format(java))
        for hashcode, jar in jars.items():
            print("Benchmarking {}...".format(hashcode))
            revision = hashcode
            java_title = java
            for design in designs:
                print('Running {}...'.format(design))
                (sizes, runtimes) = zip(*[run_firrtl(java, jar, design) for i in range(N)])
                norm_sizes = norm_max_set_sizes(sizes)
                info.append([java_title, revision, design, median(norm_sizes), stdev(norm_sizes), median(runtimes), stdev(runtimes)])
                java_title = ''
                revision = ''

    for line in info:
        formated = ['{:0.2f}'.format(elt) if isinstance(elt, numbers.Real) else elt for elt in line]
        print(','.join(formated))

if __name__ == '__main__':
    main()
