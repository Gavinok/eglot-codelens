;; eglot-codelens.el --- Add support for codelenses to eglot -*- lexical-binding: t -*-

;; Author: Gavin Freeborn <Gavinfreeborn@gmail.com>
;; URL: https://example.com/package-name.el
;; Version: 0.1
;; Package-Requires: ((emacs "29.0"))
;; Keywords: eglot

;;; Commentary:
;;; Code:
;;; Extending eglot to support code lenses

;;;; Findings
;; Lenses often support the option to be used as a code action
;; some servers rely on custom code actions implemented by the client
;; - [[https://github.com/emacs-lsp/lsp-mode/issues/2250]] mentions this
;;;; Contribute this upstream
;; https://www.fosskers.ca/en/blog/contributing-to-emacs

(require 'cl-generic)
(require 'cl-lib)
(require 'seq)
(require 'eglot)

(defgroup eglot-codelens nil "Code Lens support for eglot."
  :group 'eglot)

(defcustom eglot-codelens-debounce 0.001
  "The delay in seconds between file modifications and updating Code Lenses."
  :type 'float
  :group 'eglot-codelens)

(defcustom eglot-codelens-append-to-line t
  "Determine of Code Lenses are placed above the line or at the end of it."
  :type 'boolean
  :group 'eglot-codelens)

;; TODO Make this buffer local
(defvar-local eglot-codelens-overlays nil
  "Code Lens overlays in the current file.")

;; TODO Make this buffer local
(defvar-local eglot-codelens--refresh-timer nil
  "Timer used for debouncing lens refreshes.")

(cl-defmethod eglot-client-capabilities :around (_server)
  "Let the language SERVER know that we support Code Lenses."
  (let ((base (cl-call-next-method)))
    (setf (cl-getf
           (cl-getf base :workspace)
           :codeLens)
          '(:refreshSupport t))
    base))

(defun eglot-codelens--setup-hooks ()
  "Setup the hooks to be used for any buffer managed by Eglot."
  (run-with-timer 2 nil #'eglot-delayed-lens-update)
  (add-hook 'eglot--document-changed-hook #'eglot-delayed-lens-update nil t))

(defun eglot-codelens-delete-overlays ()
  "Clear all the overlays used for Code Lenses."
  (mapc #'delete-overlay  eglot-codelens-overlays)
  (setq-local eglot-codelens-overlays nil))

(defun eglot-codelens-force-refresh-lens ()
  "Force request and redisplay Code Lenses."
  (interactive)
  (if (eglot-current-server)
      (progn
        (eglot-codelens-delete-overlays)
        (eglot-codelens-apply-code-lenses))))

(defun eglot-delayed-reset-timer ()
  (eglot-codelens-force-refresh-lens)
  (setq-local eglot-codelens--refresh-timer nil))
(defun eglot-delayed-lens-update (&rest _)
  "Update lenses after a small delay to ensure the server is up to date."
  (when eglot-codelens--refresh-timer
    (cancel-timer eglot-codelens--refresh-timer))
  (setq-local eglot-codelens--refresh-timer
              (run-with-timer eglot-codelens-debounce
                              nil
                              #'eglot-delayed-reset-timer)))

(defun eglot-codelens-execute-current-lens ()
  "Inspect the current overlays at point and attempt to execute it."
  (interactive)
  (let ((lenses (cl-loop for i in (overlays-at (point))
                         when (overlay-get i 'eglot-codelens-overlay)
                         collect (overlay-get i 'eglot-codelens-overlay))))
    (pcase (length lenses)
      (1 (eglot-codelens-execute (car lenses)))
      (0 (user-error "No lenses found"))
      (lenses (user-error "Too many lenses %s" lenses)))))

(defun eglot-codelens-make-overlay-for-lens (command range)
  "Insert overlays for each corresponding lens's COMMAND and RANGE."
  (let* ((start-line (thread-first
                       range
                       (cl-getf :start)
                       (cl-getf :line)))
         ;; TODO indent the codelens as needed
         (ol (make-overlay (progn (goto-char (point-min))
                                  (forward-line start-line)
                                  (pos-bol))
                           (pos-eol)))
         (text  (concat
                 (if eglot-codelens-append-to-line
                     (propertize "| " 'face 'eglot-parameter-hint-face)
                   ;; This looks really bad when these are not in order
                   (make-string (thread-first
                                  range
                                  (cl-getf :start)
                                  (cl-getf :character))
                                ?\s))
                 (propertize (cl-getf command :title)
                             'face 'eglot-parameter-hint-face
                             'cursor t
                             'pointer 'hand
                             'mouse-face 'highlight
                             'keymap (let ((map (make-sparse-keymap)))
                                       (define-key map [mouse-1]
                                                   (lambda () (interactive)
                                                     (eglot-codelens-execute command)))
                                       map))
                 (if eglot-codelens-append-to-line
                     " "
                   "\n"))))
    ;; Try to combine all the lenses into a single overlay so we can
    ;; use this text property to prevent the cursor from ending up on
    ;; the right side of the overlay
    ;; taken from [[file:~/.emacs.d/elpa/flymake-1.3.7/flymake.el::put-text-property 0 1 'cursor t summary][eol overlays from flymake]]
    (put-text-property 0 1 'cursor t text)
    (overlay-put ol (if eglot-codelens-append-to-line
                        'after-string
                      'before-string)
                 text)
    (overlay-put ol 'eglot-codelens-overlay (list :command command
                                                  :range range))
    (overlay-put ol 'cursor-face 'error)
    (add-to-list  'eglot-codelens-overlays ol)))

(defun eglot-codelens-apply-code-lenses ()
  "Request and display code lenses using Eglot."
  ;; TODO look into making this async
  (save-excursion
    (let ((code-lenses
           ;; request code lenses from the server
           (jsonrpc-request
            (eglot--current-server-or-lose)
            :textDocument/codeLens
            (list :textDocument (eglot--TextDocumentIdentifier)))))
      (seq-map (lambda (lens)
                 (eglot-codelens-make-overlay-for-lens
                  ;; Construct a lens with a resolved command
                  ;; TODO consider just modifying the lens if that is faster
                  (or (cl-getf lens :command)
                      ;; Resolve the command in case none was originally provided
                      (cl-getf (jsonrpc-request (eglot--current-server-or-lose)
                                                :codeLens/resolve
                                                lens)
                               :command))
                  (cl-getf lens :range)))
               code-lenses))))

(defun eglot-codelens-execute (command)
  "Execute a the COMMAND for a specific Code Lens."
  (eglot-execute (eglot--current-server-or-lose)
                 command)
  (eglot-codelens-force-refresh-lens))

(cl-defmethod eglot-handle-request
  (_server (_method (eql workspace/codeLens/refresh))
           &key &allow-other-keys)
  (eglot-codelens-force-refresh-lens))

(define-minor-mode eglot-codelens-mode
  "Minor mode for displaying Code Lenses with Eglot."
  :global nil
  (cond
   (eglot-codelens-mode (if (not (eglot-server-capable :codeLensProvider))
                            (progn (message "[eglot-codelens] Server does not support codelenses")
                                   (eglot-codelens-mode -1))
                          (when eglot--managed-mode (eglot-codelens--setup-hooks))
                          ;; A bit of an ugly hack
                          (advice-add 'eglot--managed-mode-off :after #'eglot-codelens-delete-overlays)))
   (t (eglot-codelens-delete-overlays)
      (remove-hook 'eglot--document-changed-hook #'eglot-delayed-lens-update t)
      ;; needed to resolve our hack
      (advice-remove 'eglot--managed-mode-off #'eglot-codelens-delete-overlays))))

(with-eval-after-load 'desktop
  (add-to-list 'desktop-minor-mode-handlers '(eglot-codelens-mode . ignore)))

(provide 'eglot-codelens)
;;; eglot-codelens.el ends here
