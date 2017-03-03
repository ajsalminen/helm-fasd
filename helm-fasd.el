;;; helm-fasd.el --- Helm source for fasd

;; Author: Antti Salminen <antti.salminen@gmail.com>
;; URL: https://github.com/ajsalminen/helm-fasd
;; Package-Requires: ((helm "2.0"))

;;; Commentary:

;;; Code:
(require 'helm)

(defvar helm-fasd-command nil)

(defface helm-fasd-finish
  '((t (:foreground "Green")))
  "Face used in mode line when fasd process returns."
  :group 'helm-fasd)

(defun helm-fasd-set-command ()
  "Return the used fasd command."
  (setq helm-fasd-command "fasd -R -l -a"))

(defun helm-fasd-init ()
  "Initialize async locate process for `helm-source-fasd'."
  (let ((cmd (concat "fasd -R -l -a " (shell-quote-argument (car (split-string helm-pattern " "))))
             ))
    (helm-log "Starting helm-fasd process")
    (helm-log "Command line used was:\n\n%s"
              (concat ">>> " (propertize cmd 'face 'font-lock-comment-face) "\n\n"))
    (prog1
        (start-process-shell-command
         "fasd-process" helm-buffer
         cmd)
      (set-process-sentinel
       (get-buffer-process helm-buffer)
       #'(lambda (_process event)
           (if (string= event "finished\n")
               (with-helm-window
                 (setq mode-line-format
                       '(" " mode-line-buffer-identification " "
                         (:eval (format "L%s" (helm-candidate-number-at-point))) " "
                         (:eval (propertize
                                 (format "[fasd process finished - (%s results)]"
                                         (max (1- (count-lines
                                                   (point-min) (point-max)))
                                              0))
                                 'face 'helm-fasd-finish))))
                 (force-mode-line-update))
             (helm-log "Error: Fasd %s"
                       (replace-regexp-in-string "\n" "" event))))))))


(defclass helm-fasd-source (helm-source-async helm-type-file)
  ((init :initform 'helm-fasd-set-command)
   (candidates-process :initform 'helm-fasd-init)
   (history :initform 'helm-file-name-history)
   (keymap :initform helm-generic-files-map)
   (help-message :initform helm-generic-file-help-message)
   (candidate-number-limit :initform 9999)
   (mode-line :initform helm-read-file-name-mode-line-string)))

(defvar helm-source-fasd
  (helm-make-source "fasd" 'helm-fasd-source
    ))

;;;###autoload
(defun helm-fasd ()
  "Helm source for fasd."
  (interactive)
  (require 'helm-mode)
  (let ((helm-ff-transformer-show-only-basename nil))
    (helm :sources 'helm-source-fasd
          :buffer "*helm fasd*"
          )))


;;;###autoload
(defun helm-find-fasd-add-file ()
  "Add file to fasd database."
  (unless (executable-find "fasd") (error "Helm-search-fasd: cannot find the fasd executable"))
  (let ((file (if (string= major-mode "dired-mode") dired-directory (buffer-file-name))))
    (start-process "*fasd*" nil "fasd" "--add" file)))

;;;###autoload
(add-hook 'find-file-hook 'helm-find-fasd-add-file)
;;;###autoload
(add-hook 'dired-mode-hook 'helm-find-fasd-add-file)

(provide 'helm-fasd)
;;; helm-fasd.el ends here
