#!/usr/bin/env python3
# SPDX-License-Identifier: Apache-2.0


import subprocess
from statistics import median, stdev
import argparse
from collections import OrderedDict
import os
import numbers

from monitor_job import monitor_job

# Currently hardcoded
def get_firrtl_repo():
    cmd = ['git', 'rev-parse', '--show-toplevel']
    result = subprocess.run(cmd, stderr=subprocess.PIPE, stdout=subprocess.PIPE)
    assert result.returncode == 0
    return result.stdout.rstrip()

firrtl_repo = get_firrtl_repo()

def run_firrtl(java, jar, design):
    java_cmd = java.split()
    cmd = java_cmd + ['-cp', jar, 'firrtl.stage.FirrtlMain', '-i', design,'-o','out.v','-X','verilog']
    print(' '.join(cmd))
    resource_use = monitor_job(cmd)
    size = resource_use.maxrss // 1024 # KiB -> MiB
    runtime = resource_use.wall_clock_time
    return (size, runtime)

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

def get_version_hashes(versions):
    res = subprocess.run(['git', '-C', firrtl_repo, 'fetch'])
    if res.returncode != 0:
        print("Warning, unable to git fetch in {}! May cause errors finding commits.".format(firrtl_repo))
    hashes = OrderedDict()
    for version in versions :
        res = subprocess.run(['git', '-C', firrtl_repo, 'rev-parse', '--short', version], stdout=subprocess.PIPE)
        assert res.returncode == 0, '"{}" is not a legal revision!'.format(version)
        hashcode = res.stdout.decode('utf-8').rstrip()
        if hashcode in hashes :
            print('{} and {} are the same revision!'.format(version, hashes[hashcode]))
        else :
            hashes[hashcode] = version
    return hashes

def clone_and_build(hashcode):
    repo = 'firrtl.{}'.format(hashcode)
    jar = 'firrtl.{}.jar'.format(hashcode)
    if os.path.exists(jar):
        print('{} already exists, skipping jar creation'.format(jar))
    else :
        if os.path.exists(repo):
            assert os.path.isdir(repo), '{} already exists but isn\'t a directory!'.format(repo)
        else :
            res = subprocess.run(['git', 'clone', firrtl_repo, repo])
            assert res.returncode == 0
        res = subprocess.run(['git', '-C', repo, 'checkout', hashcode])
        assert res.returncode == 0
        res = subprocess.run(['make', '-C', repo, 'build-scala'])
        assert res.returncode == 0
        res = subprocess.run(['cp', '{}/utils/bin/firrtl.jar'.format(repo), jar])
        assert res.returncode == 0
        res = subprocess.run(['rm', '-rf', repo])
        assert res.returncode == 0
    return jar

def build_firrtl_jars(versions):
    jars = OrderedDict()
    for hashcode, version in versions.items():
        jars[hashcode] = clone_and_build(hashcode)
    return jars

def check_designs(designs):
    for design in designs:
        assert os.path.exists(design), '{} must be an existing file!'.format(design)


def main():
    args = parseargs()
    designs = args.designs
    check_designs(designs)
    hashes = get_version_hashes(args.versions)
    jars = build_firrtl_jars(hashes)
    jvms = args.jvms
    N = args.iterations
    info = [['java', 'revision', 'design', 'max heap (MiB)', 'SD', 'runtime (s)', 'SD']]
    for java in jvms:
        print("Running with '{}'".format(java))
        for hashcode, jar in jars.items():
            print("Benchmarking {}...".format(hashcode))
            revision = hashcode
            java_title = java
            for design in designs:
                print('Running {}...'.format(design))
                (sizes, runtimes) = zip(*[run_firrtl(java, jar, design) for i in range(N)])
                info.append([java_title, revision, design, median(sizes), stdev(sizes), median(runtimes), stdev(runtimes)])
                java_title = ''
                revision = ''

    for line in info:
        formated = ['{:0.2f}'.format(elt) if isinstance(elt, numbers.Real) else elt for elt in line]
        print(','.join(formated))

if __name__ == '__main__':
    main()
