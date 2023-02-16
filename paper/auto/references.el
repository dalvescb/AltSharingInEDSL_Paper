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
    "jovanovic2014yin"
    "svenningsson2013combining"
    "clements2001little"))
 '(or :bibtex :latex))

