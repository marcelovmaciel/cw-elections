(TeX-add-style-hook
 "presentation_marcelovmaciel"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-class-options
                     '(("beamer" "xcolor={svgnames}")))
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("inputenc" "utf8") ("ulem" "normalem") ("biblatex" "backend=biber")))
   (add-to-list 'LaTeX-verbatim-environments-local "semiverbatim")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "href")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperref")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperimage")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperbaseurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "nolinkurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "url")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "path")
   (add-to-list 'LaTeX-verbatim-macros-with-delims-local "path")
   (TeX-run-style-hooks
    "latex2e"
    "../images/table_counterfactuals"
    "beamer"
    "beamer10"
    "inputenc"
    "ulem"
    "float"
    "caption"
    "subcaption"
    "graphicx"
    "biblatex"
    "tikz"
    "pgfplots")
   (TeX-add-symbols
    "checkmark")
   (LaTeX-add-labels
    "fig:saari_nurmi")
   (LaTeX-add-bibliographies
    "refs"))
 :latex)

