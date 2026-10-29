#!/bin/sh
# Install the Jupyter CLI, Python kernel, and Emacs ZeroMQ module.
# Requires Python 3.12+, a C/C++ compiler, and make.
# Optional first argument selects a separate environment for testing.
set -eu
jupyter_env=${1:-"$HOME/.local/share/emacs/jupyter"}
python_cmd=${EMACS_JUPYTER_PYTHON:-python3}
if [ -x "$jupyter_env/bin/python" ]; then
    python_cmd="$jupyter_env/bin/python"
fi
if ! "$python_cmd" -c 'import sys, venv; sys.exit(sys.version_info < (3, 12))'; then
    echo "Jupyter setup needs Python 3.12+ with venv support." >&2
    echo "Set EMACS_JUPYTER_PYTHON to its executable; see scripts/jupyter/jupyter.org." >&2
    exit 1
fi
for build_tool in cc c++ make; do
    if ! "$build_tool" --version >/dev/null 2>&1; then
        echo "Jupyter setup needs a working $build_tool." >&2
        echo "macOS: install Command Line Tools with xcode-select --install." >&2
        echo "Debian: install build-essential. See scripts/jupyter/jupyter.org." >&2
        exit 1
    fi
done
if [ ! -x "$jupyter_env/bin/python" ]; then
    "${EMACS_JUPYTER_PYTHON:-python3}" -m venv "$jupyter_env"
fi
"$jupyter_env/bin/python" -m pip install --disable-pip-version-check \
    'ipykernel==7.4.0' 'jupyter-client==8.10.0' 'cmake==4.4.4'
"$jupyter_env/bin/python" -m ipykernel install --sys-prefix \
    --name python3 --display-name 'Python (Emacs)'
"$jupyter_env/bin/jupyter" kernelspec list
# Build the Emacs module from its installed package sources.
setup_dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
if [ "$#" -ge 2 ]; then
    "$jupyter_env/bin/python" "$setup_dir/build-emacs-zmq.py" --package-dir "$2"
else
    "$jupyter_env/bin/python" "$setup_dir/build-emacs-zmq.py"
fi
