(TeX-add-style-hook
 "references"
 (lambda ()
   (setq TeX-command-extra-options
         "\"-shell-escape\"")
   (LaTeX-add-bibitems
    "kiselyov:sharing"
    "gill:observablesharing"
    "carette:finallytagless"
    "ershov1958:consing"
    "filliatre:typesafeconsing"
    "elliott:compilingembedded"))
 '(or :bibtex :latex))

