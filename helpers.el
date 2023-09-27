;;; helpers.el -*- lexical-binding: t; -*-

(defmacro magic-table (template data)
  "Apply the template to each set of data."
  ;; Example:
  ;; (magic-table
  ;;  `(message (concat "Lisp is " (,v 0)))
  ;;  '(("great")
  ;;    ("awful")))
  `(-each ,data
    (lambda (row)
      (setq v (lambda (n)
                (nth n row)))
      (eval ,template))))

(provide 'helpers)
