"""Local same-window ggplot subclass for the Python/Quarto demos.

Tested with plotnine 0.15.8. _create_figure and p9GridSpec are PRIVATE
plotnine APIs: retest this adapter after upgrading plotnine. It is not a
plotnine feature or an Emacs setting, and does not patch the upstream class.
Only plots built with this subclass reuse the named window, per Python process.
"""
import matplotlib.pyplot as plt
from plotnine import ggplot as BasePlot
from plotnine._mpl.gridspec import p9GridSpec


class ggplot(BasePlot):
    """Ordinary ggplot rendering, but replace one named figure's contents."""

    def _create_figure(self):
        # Retain the existing native window/canvas; discard its old axes/artists.
        # This also allows the next plot to have a different facet layout.
        self.figure = plt.figure("Python workflow", clear=True)
        # Recreate plotnine's layout grid on that figure, as upstream does.
        self._gridspec = p9GridSpec(1, 1, self.figure)
