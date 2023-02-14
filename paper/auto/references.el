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
    "elliott:compilingembedded"
    "thai2021type"
    "odonnell:embedding"
    "clements2001little"
    "10.1145/2775053.2658771"))
 '(or :bibtex :latex))

