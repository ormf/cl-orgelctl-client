(defun edit-orgelctl-preset (str ref)
  (set-buffer (find-file-noselect orgel-preset-file))
  (delete-region (point-min) (point-max))
  (insert "(in-package :cl-orgelctl)\n\n;;; preset: ")
  (insert (format "%s\n\n" ref))
  (insert
   (replace-regexp-in-string ") :" ")\n :"
                             (replace-regexp-in-string "\n +(" " (" 
                                                       (replace-regexp-in-string "cl-orgelctl::" ""
                                                                                 (replace-regexp-in-string "orm-utils:" "" str)))))
;;;  (insert (format "\n\n(state-store-curr-preset %s)" ref))
  (insert "\n\n;;; (save-route-presets)")
  (delete-region (point) (point-max))
  (goto-char 34)
  (forward-line)
  (forward-line)
  (indent-sexp)
  (forward-line)
  (forward-line)
  (forward-line)
  (forward-line)
  (save-buffer)
  )

(defun next-orgelctl-preset ()
    (interactive)
    (slime-repl-send-string "(cl-orgelctl::next-route-preset)"))

(defun previous-orgelctl-preset ()
    (interactive)
    (slime-repl-send-string "(cl-orgelctl::previous-route-preset)")
    (save-excursion
      (switch-to-buffer (get-buffer "curr-preset.lisp"))))

(define-key lisp-mode-map (kbd "M-<left>") 'previous-orgelctl-preset)
(define-key lisp-mode-map (kbd "M-<right>") 'next-orgelctl-preset)

(save-excursion
  (switch-to-buffer (get-buffer "curr-preset.lisp")))



