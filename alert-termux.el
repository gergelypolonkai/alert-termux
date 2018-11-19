;;; alert-termux --- alert.el notifications on Termux

;;; Commentary:
;;
;; Alert style that uses termux-notification to send alerts.
;;
;; The the required command, termux-notification can be found in the termux-api package, and
;; requires the Termux API app to be installed alongside with Termux.

;;; Code:

(require 'alert)

(defcustom alert-termux-command (executable-find "termux-notification")
  "Path to the termux-notification command.
This is found in the termux-api package, and it requires the Termux API addon
app to be installed."
  :type 'file
  :group 'alert)

(defun alert-termux-notify (info)
  "Send INFO using termux-notification.
Handles :TITLE and :MESSAGE keywords from the INFO plist."
  (if alert-termux-command
      (let ((args (nconc
                   (when (plist-get info :title)
                     (list "-t" (alert-encode-string (plist-get info :title))))
                   (list "-c" (alert-encode-string (plist-get info :message))))))
        (apply #'call-process alert-termux-command nil
               (list (get-buffer-create " *termux-notification output*") t)
               nil args))
    (alert-message-notify info)))

(alert-define-style 'termux
                    :title "Notify using termux"
                    :notifier #'alert-termux-notify)

(provide 'alert-termux)
;;; alert-termux.el ends here
