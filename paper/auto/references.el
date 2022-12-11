(TeX-add-style-hook
 "references"
 (lambda ()
   (setq TeX-command-extra-options
         "\"-shell-escape\"")
   (LaTeX-add-bibitems
    "kiselyov:sharing"))
 '(or :bibtex :latex))

