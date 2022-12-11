(TeX-add-style-hook
 "paper"
 (lambda ()
   (setq TeX-command-extra-options
         "\"-shell-escape\"")
   (TeX-add-to-alist 'LaTeX-provided-class-options
                     '(("llncs" "runningheads")))
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("fontenc" "T1")))
   (TeX-run-style-hooks
    "latex2e"
    "llncs"
    "llncs10"
    "fontenc"
    "graphicx")
   (LaTeX-add-labels
    "tab1"
    "fig1")
   (LaTeX-add-bibliographies
    "references"))
 :latex)

