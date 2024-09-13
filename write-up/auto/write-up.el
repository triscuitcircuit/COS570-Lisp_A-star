(TeX-add-style-hook
 "write-up"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-class-options
                     '(("article" "11pt") ("memoir" "10pt" "a4paper" "showtrims")))
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("inputenc" "utf8") ("fontenc" "T1") ("ulem" "normalem") ("geometry" "margin=1in") ("caption" "labelfont=bf") ("biblatex" "natbib=true")))
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "path")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "url")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "nolinkurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperbaseurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperimage")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperref")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "href")
   (add-to-list 'LaTeX-verbatim-macros-with-delims-local "path")
   (TeX-run-style-hooks
    "latex2e"
    "article"
    "art11"
    "inputenc"
    "fontenc"
    "graphicx"
    "grffile"
    "longtable"
    "wrapfig"
    "rotating"
    "ulem"
    "amsmath"
    "textcomp"
    "amssymb"
    "capt-of"
    "hyperref"
    "geometry"
    "enumitem"
    "memoir"
    "memoir10"
    "caption"
    "biblatex"
    "tikz"
    "sectsty"
    "parskip")
   (LaTeX-add-labels
    "sec:orga9fa7be"
    "sec:org324f7bd"
    "orgc580a74"
    "sec:org48546dc"
    "sec:org5c464e7"
    "sec:org7be21d5"
    "sec:org05e792a"
    "sec:org5373bbe"
    "sec:org5931d51"
    "org250b1db"
    "org46c008b"
    "sec:org611dba1"
    "sec:orga0198a0"
    "sec:org8c1bba5"
    "tab:org0169a68"
    "tab:org744414d"
    "sec:org3367bb8"
    "sec:org9dcfce5"
    "sec:orga0ed532"
    "sec:org64db69e"
    "sec:orgcae5942"
    "sec:org4c570c0"))
 :latex)

