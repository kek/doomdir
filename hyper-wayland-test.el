;;; hyper-wayland-test.el --- Hyper transport tests -*- lexical-binding: t; -*-
(require 'ert)
(load (expand-file-name "hyper-wayland.el" (file-name-directory load-file-name)) nil t)

(defun ke/test-hyper-translation (input)
  (let ((unread-command-events (append (kbd input) nil)))
    (read-key-sequence-vector "")))

(ert-deftest hyper-wayland-preserves-modifiers ()
  (dolist (pair '(("<f20> x" . "H-x")
                  ("C-<f20> C-x" . "H-C-x")
                  ("S-<f20> X" . "H-X")
                  ("M-<f20> M-x" . "H-M-x")
                  ("s-<f20> s-x" . "H-s-x")
                  ("C-M-S-s-<f20> C-M-S-s-x" . "H-C-M-S-s-x")
                  ("<f20> <left>" . "H-<left>")
                  ("<f20> é" . "H-é")
                  ("<f20> RET" . "H-RET")))
    (should (equal (ke/test-hyper-translation (car pair)) (vconcat (kbd (cdr pair)))))))

(ert-deftest hyper-wayland-leaves-ordinary-input-alone ()
  (should (equal (ke/test-hyper-translation "x") (vconcat (kbd "x"))))
  (should (equal (ke/test-hyper-translation "<f19>") (kbd "<f19>"))))

(ert-deftest hyper-wayland-prefix-per-event ()
  (let ((unread-command-events (append (kbd "<f20> x <f20> x") nil)))
    (should (equal (read-key-sequence-vector "") (kbd "H-x")))
    (should (equal (read-key-sequence-vector "") (kbd "H-x")))))
