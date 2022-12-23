(TeX-add-style-hook
 "w4-essay"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-class-options
                     '(("article" "hidelinks" "11pt")))
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("inputenc" "utf8") ("fontenc" "T1") ("ulem" "normalem") ("geometry" "a4paper" "width=150mm" "top=25mm" "bottom=25mm") ("biblatex-chicago" "authordate" "strict" "backend=biber" "bibencoding=inputenc")))
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
    "longtable"
    "wrapfig"
    "rotating"
    "ulem"
    "amsmath"
    "amssymb"
    "capt-of"
    "hyperref"
    "caption"
    "subcaption"
    "float"
    "xcolor"
    "geometry"
    "biblatex-chicago")
   (LaTeX-add-labels
    "sec1"
    "sec2"
    "Tab:Tcpairwise"
    "fig:saari_nurmi"
    "fig:counts"
    "tbl:subtab1"
    "tbl:subtab2"
    "tbl:tab1"
    "fig:positional4c"
    "fig:notac1"
    "fig:notbc1"
    "fig:notah1"
    "fig:c1dropping"
    "appendix:transfer"
    "appendix:inferred1"
    "lab:inferred1"
    "appendix:transfer2_results"
    "fig:notac2"
    "fig:notbc2"
    "fig:notahc2"
    "fig:c2dropping")
   (LaTeX-add-bibliographies
    "~/Main/Org/org-roam-mvm/bib/refs"))
 :latex)

