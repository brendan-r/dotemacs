"""Non-blocking plotnine in Python or IPython — see ../python-workflow.org.
C-c C-c sends this file; C-c C-p chooses a different process.
Uses the local ggplot subclass in plotnine_window.py, not upstream ggplot.
Each run replaces the contents of the same native window without blocking.
On macOS, select the MacOSX backend before imports if it is not the default.
Close finished plots with plt.close("all") at the REPL.
"""
values = [2, 6, 4, 8]

import matplotlib.pyplot as plt
import pandas as pd
from plotnine import aes, geom_line, geom_point, labs, theme, theme_minimal
# Local same-window subclass; see plotnine_window.py (tested: plotnine 0.15.8).
from plotnine_window import ggplot

plt.ion()  # p.show() returns without waiting for native windows to close.
data = pd.DataFrame({"sample": range(1, len(values) + 1), "value": values})
p = (
    ggplot(data, aes("sample", "value"))
    + geom_line(color="#7c3aed")
    + geom_point(color="#7c3aed", size=3)
    + labs(title="Plotnine — same-window subclass", x="Sample", y="Value")
    + theme_minimal()
    + theme(figure_size=(6, 3))
)
p.show()


p = (
    ggplot(data, aes("sample", "value"))
    + geom_line(color="red")
    + geom_point(color="blue", size=3)
    + labs(title="Plotnine — A slightly different plot", x="Sample", y="Value")
    + theme_minimal()
    + theme(figure_size=(6, 3))
)
p.show()

# Matplotlib bar chart: select this section and send it with C-c C-r.
# It replaces the Plotnine chart in the same native window.
fig = plt.figure("Python workflow", clear=True)
ax = fig.subplots()
ax.bar(["A", "B", "C", "D"], values, color="#0d9488")
ax.set(title="Matplotlib — bar chart", xlabel="Sample", ylabel="Value")
fig.tight_layout()
fig.canvas.draw_idle()
plt.show(block=False)


# Matplotlib scatter chart: select this section and send it with C-c C-r.
# It also replaces the contents of the same native window.
fig = plt.figure("Python workflow", clear=True)
ax = fig.subplots()
ax.scatter(range(1, len(values) + 1), values, color="#ea580c", s=80)
ax.set(title="Matplotlib — scatter chart", xlabel="Sample", ylabel="Value")
ax.grid(True, alpha=0.3)
fig.tight_layout()
fig.canvas.draw_idle()
plt.show(block=False)
