
import subprocess
import re
import sys
from collections import OrderedDict
import os

def get_firrtl_repo():
    cmd = ['git', 'rev-parse', '--show-toplevel']
    result = subprocess.run(cmd, stderr=subprocess.PIPE, stdout=subprocess.PIPE)
    assert result.returncode == 0
    return result.stdout.rstrip()

firrtl_repo = get_firrtl_repo()

platform = ""
if sys.platform == 'darwin':
    print("Running on MacOS")
    platform = 'macos'
elif sys.platform.startswith("linux"):
    print("Running on Linux")
    platform = 'linux'
else :
    raise Exception('Unrecognized platform ' + sys.platform)

def time():
    if platform == 'macos':
        return ['/usr/bin/time', '-l']
    if platform == 'linux':
        return ['/usr/bin/time', '-v']

def extract_max_size(output):
    regex = ''
    if platform == 'macos':
        regex = '(\d+)\s+maximum resident set size'
    if platform == 'linux':
        regex = 'Maximum resident set size[^:]*:\s+(\d+)'

    m = re.search(regex, output, re.MULTILINE)
    if m :
        return int(m.group(1))
    else :
        raise Exception('Max set size not found!')

def extract_run_time(output):
    m = re.search('Total FIRRTL Compile Time: (\d+)', output, re.MULTILINE)
    if m :
        return int(m.group(1))
    else :
        raise Exception('Runtime not found!')

def get_version_hashes(versions):
    res = subprocess.run(['git', '-C', firrtl_repo, 'fetch'])
    assert res.returncode == 0, '"{}" must be an existing repo!'.format(firrtl_repo)
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

def _clone_and_build_helper(hashcode, cmd, filename):
    s = filename.split(".")
    prefix = s[0]
    ext = s[1]
    repo = 'firrtl.{}'.format(hashcode)
    jar = '{}.{}.{}'.format(prefix, hashcode, ext)
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
        res = subprocess.run(cmd, cwd=repo)
        assert res.returncode == 0
        res = subprocess.run(['cp', '{}/utils/bin/{}'.format(repo, filename), jar])
        assert res.returncode == 0
        res = subprocess.run(['rm', '-rf', repo])
        assert res.returncode == 0
    return jar

def clone_and_build(hashcode):
    return _clone_and_build_helper(hashcode, ["sbt", "assembly"], "firrtl.jar")

def clone_and_build_benchmark(hashcode):
    cmd = ["sbt", "project benchmark", "assembly"]
    return _clone_and_build_helper(hashcode, cmd, "firrtl-benchmark.jar")


def run_firrtl(java, jar, design):
    java_cmd = java.split()
    cmd = time() + java_cmd + ['-cp', jar, 'firrtl.stage.FirrtlMain', '-i', design,'-o','out.v','-X','verilog']
    result = subprocess.run(cmd, stderr=subprocess.PIPE, stdout=subprocess.PIPE)
    if result.returncode != 0 :
        print(result.stdout)
        print(result.stderr)
        sys.exit(1)
    size = extract_max_size(result.stderr.decode('utf-8'))
    runtime = extract_run_time(result.stdout.decode('utf-8'))
    print('{} B, {} ms'.format(size, runtime))
    return (size, runtime)


def build_firrtl_jars(versions):
    jars = OrderedDict()
    for hashcode, version in versions.items():
        jars[hashcode] = clone_and_build(hashcode)
    return jars

def build_firrtl_benchmark_jars(versions):
    jars = OrderedDict()
    for hashcode, version in versions.items():
        jars[hashcode] = clone_and_build_benchmark(hashcode)
    return jars

def check_designs(designs):
    for design in designs:
        assert os.path.exists(design), '{} must be an existing file!'.format(design)

# /usr/bin/time -v on Linux returns size in kbytes
# /usr/bin/time -l on MacOS returns size in Bytes
def norm_max_set_sizes(sizes):
    div = None
    if platform == 'linux':
        d = 1000.0
    if platform == 'macos':
        d = 1000000.0
    return [s / d for s in sizes]

