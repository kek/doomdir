;;; hyper-wayland.el --- Native Wayland Hyper adapter -*- lexical-binding: t; -*-

;; Niri's XKB map supplies genuine Hyper outside Emacs. xremap sends
;; F20 followed by the original key in Emacs, preserving other modifiers.
;; F20 is reserved for this transport (F24 is the physical Hyper carrier).
(defun ke/hyper-wayland-translate (_prompt)
  "Add Hyper to the keyboard event following the transport prefix."
  (let ((event (read-event)))
    (vector (event-convert-list
             (cons 'hyper
                   (append (event-modifiers event)
                           (list (event-basic-type event))))))))

;; xremap preserves extra modifiers on both events, including the prefix.
(dotimes (bits 32)
  (let ((mods nil))
    (dotimes (bit 5)
      (when (/= 0 (logand bits (ash 1 bit)))
        (push (nth bit '(control meta shift super alt)) mods)))
    (define-key key-translation-map
                (vector (event-convert-list (append mods '(f20))))
                #'ke/hyper-wayland-translate)))

(provide 'hyper-wayland)
;;; hyper-wayland.el ends here
