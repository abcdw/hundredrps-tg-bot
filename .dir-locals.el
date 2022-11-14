((cider-repl-mode
  . ((eval . (define-key cider-repl-mode-map (kbd "s-r")
                         (lambda () (interactive)
                           (cider-interactive-eval "(integrant.repl/reset)"))))))
 (clojure-mode
  . ((eval . (define-key clojure-mode-map (kbd "s-r")
                         (lambda () (interactive)
                           (cider-interactive-eval "(integrant.repl/reset)"))))
     (cider-preferred-build-tool . clojure-cli)
     (cider-clojure-cli-aliases . ":dev:test:build"))))
