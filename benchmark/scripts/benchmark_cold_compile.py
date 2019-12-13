#!/usr/bin/env python3
# See LICENSE for license details.

import subprocess
import re
from statistics import median, stdev
import sys
import argparse
from collections import OrderedDict
import os
import numbers
import csv

# Currently hardcoded
def get_firrtl_repo():
    cmd = ['git', 'rev-parse', '--show-toplevel']
    result = subprocess.run(cmd, stderr=subprocess.PIPE, stdout=subprocess.PIPE)
    assert result.returncode == 0
    return result.stdout.rstrip()

firrtl_repo = get_firrtl_repo()

platform = ""
if sys.platform == 'darwin':
    sys.stderr.write("Running on MacOS\n")
    platform = 'macos'
elif sys.platform.startswith("linux"):
    sys.stderr.write("Running on Linux\n")
    platform = 'linux'
else :
    raise Exception('Unrecognized platform ' + sys.platform)

def time():
    return { 'macos': ['/usr/bin/time', '-l'],
             'linux': ['/usr/bin/time', '-v'] }[platform]

def extract_max_size(output):
    regex = { 'macos': '(\d+)\s+maximum resident set size',
              'linux': 'Maximum resident set size[^:]*:\s+(\d+)' }[platform]

    m = re.search(regex, output, re.MULTILINE)
    if m :
        return int(m.group(1))
    else :
        raise Exception('Max set size not found!')

def extract_run_time(output):
    res = None
    regex = { 'macos': '(\d+\.\d+)\s+real',
              'linux': 'Elapsed \(wall clock\) time \(h:mm:ss or m:ss\): ([0-9:.]+)' }[platform]
    m = re.search(regex, output, re.MULTILINE)
    if m :
        text = m.group(1)
        if platform == 'macos':
            return float(text)
        if platform == 'linux':
            parts = text.split(':')
            if len(parts) == 3:
                return float(parts[0]) * 3600 + float(parts[1]) * 60 + float(parts[0])
            if len(parts) == 2:
                return float(parts[0]) * 60 + float(parts[1])
    raise Exception('Runtime not found!')

def run_firrtl(java, jar, design):
    java_cmd = java.split()
    cmd = time() + java_cmd + ['-cp', jar, 'firrtl.stage.FirrtlMain', '-i', design,'-o','out.v','-X','verilog']
    result = subprocess.run(cmd, stderr=subprocess.PIPE, stdout=subprocess.PIPE)
    if result.returncode != 0 :
        sys.stderr.write(result.stdout)
        sys.stderr.write(result.stderr)
        sys.exit(1)
    size = extract_max_size(result.stderr.decode('utf-8'))
    runtime = extract_run_time(result.stderr.decode('utf-8'))
    return (size, runtime)

class GetVersionHashes(argparse.Action):
    def __call__(self, parser, namespace, values, option_string=None):
        res = subprocess.run(['git', '-C', firrtl_repo, 'fetch'])
        assert res.returncode == 0, '"{}" must be an existing repo!'.format(firrtl_repo)
        hashes = OrderedDict()
        for version in values:
            res = subprocess.run(['git', '-C', firrtl_repo, 'rev-parse', '--short', version], stdout=subprocess.PIPE)
            assert res.returncode == 0, '"{}" is not a legal revision!'.format(version)
            hashcode = res.stdout.decode('utf-8').rstrip()
            if hashcode in hashes :
                sys.stderr.write('{} and {} are the same revision!'.format(version, hashes[hashcode]))
            else :
                hashes[hashcode] = version
        setattr(namespace, self.dest, hashes)

def parseargs():
    parser = argparse.ArgumentParser("Benchmark FIRRTL",
                                     formatter_class=argparse.ArgumentDefaultsHelpFormatter)
    parser.add_argument('--designs', type=argparse.FileType('r'), nargs='+',
                        help='FIRRTL input files to use as benchmarks')
    parser.add_argument('--versions', type=str, nargs='+', default=['HEAD'], action=GetVersionHashes,
                        help='FIRRTL commit hashes to benchmark')
    parser.add_argument('--iterations', '-N', type=int, default=10,
                        help='Number of times to run each benchmark')
    parser.add_argument('--jvms', type=str, nargs='+', default=['java'],
                        help='JVMs to use')
    parser.add_argument('--output', type=argparse.FileType('w'), default='-', help='Output CSV file to write')
    return parser.parse_args()

def clone_and_build(hashcode):
    repo = 'firrtl.{}'.format(hashcode)
    jar = 'firrtl.{}.jar'.format(hashcode)
    if os.path.exists(jar):
        sys.stderr.write('{} already exists, skipping jar creation\n'.format(jar))
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

# /usr/bin/time -v on Linux returns size in kbytes
# /usr/bin/time -l on MacOS returns size in Bytes
def mem_to_MB(size):
    d = { 'linux': 1000.0,
          'macos': 1000000.0 }[platform]
    return size / d

def main():
    args = parseargs()
    fieldnames = ['jvm', 'revision', 'design', 'iteration', 'memory', 'runtime']
    writer = csv.DictWriter(args.output, fieldnames=fieldnames, quoting=csv.QUOTE_NONNUMERIC)
    jars = build_firrtl_jars(args.versions)
    writer.writeheader()
    for java in args.jvms:
        sys.stderr.write('- java: {}\n'.format(java))
        for hashcode, jar in jars.items():
            sys.stderr.write('  - hashcode: {}\n'.format(hashcode))
            for design in args.designs:
                sys.stderr.write('    - design: {}\n'.format(design.name))
                for i in range(args.iterations):
                    memory, runtime = run_firrtl(java, jar, design.name)
                    sys.stderr.write('      - memory: {:0.0f} MB\n'.format(mem_to_MB(memory)))
                    sys.stderr.write('        runtime: {:0.2f} s\n'.format(runtime))
                    writer.writerow(
                        {'jvm':       java,
                         'revision':  hashcode,
                         'design':    design.name,
                         'iteration': i,
                         'memory':    mem_to_MB(memory),
                         'runtime':   runtime
                        })

if __name__ == '__main__':
    main()
