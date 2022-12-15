(TeX-add-style-hook
 "paper"
 (lambda ()
   (setq TeX-command-extra-options
         "\"-shell-escape\"")
   (TeX-add-to-alist 'LaTeX-provided-class-options
                     '(("llncs" "runningheads")))
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("fontenc" "T1")))
   (add-to-list 'LaTeX-verbatim-environments-local "lstlisting")
   (add-to-list 'LaTeX-verbatim-environments-local "minted")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "lstinline")
   (add-to-list 'LaTeX-verbatim-macros-with-delims-local "lstinline")
   (TeX-run-style-hooks
    "latex2e"
    "llncs"
    "llncs10"
    "fontenc"
    "graphicx"
    "listings"
    "minted")
   (LaTeX-add-labels
    "limithashcons"
    "fig:hashcons"
    "limitexplicit")
   (LaTeX-add-bibliographies
    "references"))
 :latex)

