(eval-region (line-beginning-position 2)
             (point-max))

;; ^ C-x C-e here ^

(require 'ob-clojure)
(require 'cider)

(setq org-babel-clojure-backend 'cider
      org-export-html-style-include-scripts nil
      org-export-html-style-include-default nil
      org-export-htmlize-output-type 'css)
