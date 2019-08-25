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

def extract_serialization_times(output):
    strs = re.findall('Serialization took (\d+)', output, re.MULTILINE)
    return [int(s) for s in strs]

def run_bench_serialization(java, jar, design, n):
    java_cmd = java.split()
    cmd = time() + java_cmd + ['-cp', jar, 'firrtl.benchmark.hot.Serialization', design, str(n)]
    result = subprocess.run(cmd, stderr=subprocess.PIPE, stdout=subprocess.PIPE)
    if result.returncode != 0 :
        print(result.stdout)
        print(result.stderr)
        sys.exit(1)
    runtimes = extract_serialization_times(result.stdout.decode('utf-8'))
    print("\n".join(['{} ms'.format(t) for t in runtimes]))
    #assert len(runtimes) == n
    return runtimes

def main():
    args = parseargs()
    designs = args.designs
    check_designs(designs)
    hashes = get_version_hashes(args.versions)
    jars = build_firrtl_benchmark_jars(hashes)
    jvms = args.jvms
    N = args.iterations
    assert N > 1, "Number of iterations must be > 1"
    info = [['java', 'revision', 'design', 'runtime', 'SD']]
    for java in jvms:
        print("Running with '{}'".format(java))
        for hashcode, jar in jars.items():
            print("Benchmarking {}...".format(hashcode))
            revision = hashcode
            java_title = java
            for design in designs:
                print('Running {}...'.format(design))
                runtimes = run_bench_serialization(java, jar, design, N)
                info.append([java_title, revision, design, median(runtimes), stdev(runtimes)])
                java_title = ''
                revision = ''

    for line in info:
        formated = ['{:0.2f}'.format(elt) if isinstance(elt, numbers.Real) else elt for elt in line]
        print(','.join(formated))

if __name__ == '__main__':
    main()
