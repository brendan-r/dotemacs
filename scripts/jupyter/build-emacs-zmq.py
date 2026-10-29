#!/usr/bin/env python3
"""Build the installed emacs-zmq module with draft-enabled static libzmq.

Run with the dedicated Jupyter environment's Python (Python 3.12+).
A C/C++ compiler and make must be available. No system libraries are changed.
"""
import argparse
import hashlib
from pathlib import Path
import shutil
import subprocess
import sys
import tarfile
import urllib.request

parser = argparse.ArgumentParser(description=__doc__)
parser.add_argument("--package-dir", type=Path,
                    help="installed zmq package directory, if more than one exists")
args = parser.parse_args()
if sys.version_info < (3, 12):
    parser.error("Python 3.12+ is required; run scripts/jupyter/setup-jupyter.sh")
if sys.platform not in ("darwin", "linux"):
    parser.error("This build script supports macOS and Linux")
if args.package_dir:
    package = args.package_dir.resolve()
else:
    packages = list((Path(__file__).resolve().parents[2] / "elpa").glob("zmq-*/zmq.el"))
    if len(packages) != 1:
        parser.error("Select the installed zmq package with --package-dir")
    package = packages[0].parent
if not (package / "src/emacs-zmq.c").is_file():
    parser.error("The selected package does not contain the emacs-zmq C sources")
compiler = shutil.which("cc")
if not all(shutil.which(tool) for tool in ("cc", "c++", "make")):
    parser.error("Install a C/C++ compiler and make before running this script")
cmake = Path(sys.executable).parent / "cmake"
if not cmake.is_file():
    parser.error("CMake is missing from this Python environment; run scripts/jupyter/setup-jupyter.sh")
work = Path(sys.prefix) / "build/emacs-zmq"
work.mkdir(parents=True, exist_ok=True)
archive = work / "libzmq-4.3.5.tar.gz"
expected = "6c972d1e6a91a0ecd79c3236f04cf0126f2f4dfbbad407d72b4606a7ba93f9c6"
if not archive.exists():
    with urllib.request.urlopen(
            "https://codeload.github.com/zeromq/libzmq/tar.gz/refs/tags/v4.3.5",
            timeout=60) as response:
        archive.write_bytes(response.read())
if hashlib.sha256(archive.read_bytes()).hexdigest() != expected:
    raise SystemExit("libzmq archive checksum mismatch")
source = work / "libzmq-4.3.5"
if not source.exists():
    with tarfile.open(archive) as tar:
        tar.extractall(work, filter="data")
build = work / "build"
subprocess.run([str(cmake), "-S", str(source), "-B", str(build),
                "-DCMAKE_POLICY_VERSION_MINIMUM=3.5", "-DCMAKE_BUILD_TYPE=Release",
                "-DCMAKE_POSITION_INDEPENDENT_CODE=ON", "-DENABLE_DRAFTS=ON",
                "-DBUILD_SHARED=OFF", "-DBUILD_STATIC=ON", "-DBUILD_TESTS=OFF",
                "-DWITH_TLS=OFF", "-DWITH_LIBBSD=OFF", "-DWITH_LIBSODIUM=OFF",
                "-DENABLE_CURVE=OFF", "-DWITH_DOCS=OFF"], check=True)
subprocess.run([str(cmake), "--build", str(build), "--parallel", "4"], check=True)
suffix = ".dylib" if sys.platform == "darwin" else ".so"
output = package / ("emacs-zmq" + suffix)
temporary = output.with_name(output.name + ".new")
sources = [package / "src" / (name + ".c") for name in
           ("socket", "context", "msg", "constants", "util", "core", "poll", "emacs-zmq")]
libs = ["-lc++"] if sys.platform == "darwin" else ["-lstdc++", "-pthread"]
subprocess.run([compiler, "-O2", "-shared", "-fPIC", "-DZMQ_BUILD_DRAFT_API=1",
                "-I" + str(source / "include"), *map(str, sources),
                str(build / "lib/libzmq.a"), *libs, "-o", str(temporary)], check=True)
temporary.replace(output)
print("Built", output)
