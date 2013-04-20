;;; shoes-off-log.el - logging for shoes-off rcirc processes

;;; Commentary

;; Actually this is quite a generic RCIRC logger but we do check for a
;; `:shoes-off' property in the logging hook.

(require 'huskie)

(defcustom shoes-off-log-dir "~/.rcirc-logs"
  "The directory where logs should be kept.

If nil, logs aren't kept."
  :group 'shoes-off
  :type 'directory)

(defconst shoes-off-log-name "shoes-off-log"
  "The name for the huskie logger.")

(defun shoes-off-log (str)
  "Log through huskie"
  (huskie-log str shoes-off-log-name))

(defun shoes-off-log-test (str)
  (huskie-log str shoes-off-log-name))

;;;###autoload
(defun shoes-off-log-init ()
  "Initialize logging for shoes-off."
  (when shoes-off-log-dir
    (huskie-set-script
     shoes-off-log-name
     (concat 
      "while read line;"
      "do echo $line > %s-$(date \"+%%Y%%m%%d%%H\");"
      "done"))
    (huskie-bind-logname->filename
     shoes-off-log-name
     (format "%s/my-log" (expand-file-name shoes-off-log-dir)))
    (add-hook 'rcirc-print-hooks 'shoes-off-write-log-hook)))

(defun shoes-off/hook-test (process)
  (and shoes-off-log-dir)
  (process-get process :shoes-off))

(defun shoes-off-write-log-hook (process sender response target text)
  ;; we should check that process is a shoes-off process
  ;; and not an ordinary rcirc process
  (when (shoes-off/hook-test process)
    (when (bufferp target)
      (setq target (with-current-buffer buffer rcirc-target)))
    ;; Sometimes buffer is not anything at all!
    (unless (or (null target) (string= target ""))
      (shoes-off-log
       ;; Print the line into the logger
       (concat
        ;; Should we send some indication of the target?
        (format-time-string "%Y-%m-%d@%H:%M ")
        (format "{%s} {%s} "
                (rcirc-user-nick sender) target)
        (unless (string= response "PRIVMSG")
          (concat "/" (downcase response) " "))
        text "\n")))))

(provide 'shoes-off-log)

;;; shoes-off-log.el ends here
