"""Python REPL demo — see ../python-workflow.org.
First execution offers a process picker; C-c C-p chooses again.
C-c C-c sends this file; C-M-x sends the current function.
C-Return sends a complete unit and steps past it; EOF stops without resending.
C-c C-z visits/returns from the REPL.
Run the file twice: run_count should increment. Nothing is installed here.
See plotnine-repl.py for non-blocking native plots.
"""
import sys

run_count = globals().get("run_count", 0) + 1


def summarize(values):
    """Try C-Return on the def line, or C-M-x anywhere in this function."""
    total = sum(values)

    return {"total": total, "mean": total / len(values)}


values = [2, 4, 6]
print("Run:", run_count, "Summary:", summarize(values))

# Select these two lines and C-c C-r to check persistent state:
run_count += 1
print("Persistent counter:", run_count)

# Put point on this expression and press C-Return: expect 8.
4 + 4
