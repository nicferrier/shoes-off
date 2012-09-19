;;; shoes-off.el --- irc bouncer

;; Copyright (C) 2012  Nic Ferrier

;; Author: Nic Ferrier <nferrier@ferrier.me.uk>
;; Keywords: comm
;; Maintainer: Nic Ferrier <nferrier@ferrier.me.uk>
;; Created: 19th September 2012
;; Version: 0.0.6
;; Package-Requires: ((kv "0.0.5")(anaphora "0.0.2"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Bounce an irc session through a server socket.

;; Please take your shoes off before getting on the bouncy castle.

;;; Code:

(require 'anaphora) ; packaged
(require 'cl)
(require 'kv)
(require 'rcirc)

(defgroup shoes-off nil
  "An irc bouncer."
  ;;:version "22.1"
  ;;:link '(custom-manual "(rcirc)")
  :prefix "shoes-off-"
  :group 'applications)

(defcustom shoes-off-config '()
  "The bouncer configuration.

A list of plists describing the configuration of the bouncer.

IMPORTANT: The server-alist is can only be ONE SERVER. The
bouncer cannot service multiple upstream sessions right now."
  :group 'shoes-off
  :type
  '(repeat
    (plist
     :options
     ((:username string)
      (:password string)
      (:server-alist
       (alist
        :key-type string
        :value-type
        (plist :options
               ((:nick string)
                (:port integer)
                (:user-name string)
                (:password string)
                (:full-name string)
                (:channels (repeat string))
                (:encryption (choice (const tls)
                                     (const plain)))))))))))

(defvar shoes-off--sessions (make-hash-table :test 'equal)
  "Hashtable of bouncer sessions keyed by username.

Stores the connections to upstream IRC servers.")

(defun shoes-off--puthash (process property name value)
  "Make NAME[VALUE] on a hashtable at PROPERTY on the PROCESS."
  (puthash
   name value
   (or
    (process-get process property)
    (let ((h (make-hash-table :test 'equal)))
      (process-put process property h)
      h))))

(defun shoes-off--auth-check (username password)
  "Check the USERNAME and PASSWORD against the db.

Returns the full auth details of the user if auth passes."
  (loop for bouncer in shoes-off-config
       if (and
           (equal
            username
            (plist-get bouncer :username))
           (equal
            password
            (plist-get bouncer :password)))
       return bouncer))

(defun shoes-off-auth (bouncer-buffer)
  "Retrieve auth details from BOUNCER-BUFFER.

Successful retrieval moves the point forward and returns a plist
with the following data and keys:

 :pass the password user
 :nick the irc nick claimed
 :user the username
 :user-info further information about the user

Unsuccessful auth makes no changes and returns `nil'."
  (with-current-buffer bouncer-buffer
    (let* (details
           (pt
            (save-excursion
              (goto-char (point-min))
              (when (re-search-forward "^PASS \\(.*\\)\n" nil t)
                (setq details (plist-put details :pass (match-string 1)))
                (when (re-search-forward "^NICK \\(.*\\)\n" nil t)
                  (setq details (plist-put details :nick (match-string 1))))
                (when (re-search-forward "^USER \\(.*?\\) \\(.*\\)\n" nil t)
                  (setq details (plist-put details :user (match-string 1)))
                  (setq details (plist-put details :user-info (match-string 2)))
                  (point))))))
      (when (and (plist-get details :pass)
                 (plist-get details :user)
                 (plist-get details :nick))
        (goto-char pt))
        details)))

(defconst shoes-off--cmd-numbers
  '(("WELCOME" . 1)
    ("YOURHOST" . 2)
    ("CREATED" . 3)
    ("MYINFO" . 4)
    ("BOUNCE" . 5)
    )
  "Command numbers for each of the IRC commands.")

(defconst shoes-off--cmd-names
  (mapcar
   (lambda (pair)
     (cons (cdr pair) (car pair)))
   shoes-off--cmd-numbers))

(defun shoes-off--send-command (process cmd data)
  "Send the CMD with the DATA to the PROCESS using IRC protocol."
  (let* ((hostname (process-get process :shoes-off-server-name))
         (cmd-num (aget shoes-off--cmd-numbers cmd))
         (cmd-str
          (format "%s %03d %s\n"
                  hostname
                  cmd-num
                  data)))
    (process-send-string process cmd-str)))

(defun shoes-off--get-session (process)
  "Get the associated session from the client process."
  (let* ((auth (process-get
                process
                :shoes-off-authenticated))
         (user (plist-get auth :username)))
    (gethash user shoes-off--sessions)))


(defconst shoes-off--cache-response-welcome-commands
  '("WELCOME" "YOURHOST" "CREATED" "MYINFO" "BOUNCE")
  "The command responses we cache on the session for welcome.

When a user connects to the bouncer we use these responses to
generate the welcome.

What's cached is the full text response of the command.")

(defun shoes-off--send-welcome (process)
  "Send the welcome stuff to PROCESS, a server connection."
  (let* ((session (shoes-off--get-session process))
         (hostname (substring
                    (process-get session :shoes-off-server-name)
                    1))
         (welcome-cache (process-get session :shoes-off-welcome-cache)))
    (process-put process :shoes-off-server-name hostname)
    (loop for cmd in shoes-off--cache-response-welcome-commands
       do (shoes-off--send-command
           process cmd (gethash cmd welcome-cache)))
    ;; then do lusers and motd
    ;;send-command to user's process and pull it back here
    (rcirc-send-string session "LUSERS")
    (rcirc-send-string session "MOTD")
    ;; Finally send the join responses for any channels we have
    (let ((hash (process-get session :shoes-off-channel-cache)))
      (when hash
        (maphash
         (lambda (channel response)
           ;; Have to send these directly
           (process-send-string process response)) hash)))))

(defun shoes-off--authenticate (process auth-details)
  "Mark the PROCESS authenticated."
  (process-put process :shoes-off-authenticated it)
  ;; Mark the upstream session
  (let ((session (shoes-off--get-session process)))
    (process-put session :shoes-off-connection process)))

(defun shoes-off--get-or-create-process-buffer (process)
  "Get the process buffer (or create it)."
  (or
   (process-buffer process)
   (let* ((port (cadr (process-contact process)))
          (buffer (get-buffer-create
                   (format " *shoes-off-%s*" port))))
     (with-current-buffer buffer
       (erase-buffer))
     (set-process-buffer process buffer)
     (process-buffer process))))

(defun shoes-off--handle-request (process authenticated request)
  "Handle the request."
  (destructuring-bind (command &rest args) (split-string request " ")
    (let ((session (shoes-off--get-session process)))
      (rcirc-send-string session request))))

(defun shoes-off--filter (process data)
  "Stuff from the bouncer's client."
  (let ((procbuf (shoes-off--get-or-create-process-buffer process)))
    (with-current-buffer procbuf
      (save-excursion
        (goto-char (point-max))
        (insert data)))
    (let ((authenticated (process-get process :shoes-off-authenticated)))
      (unless authenticated
        (awhen (shoes-off-auth (process-buffer process))
          (destructuring-bind (&key pass user user-info nick) it
            (awhen (shoes-off--auth-check user pass)
              (setq authenticated it)
              (shoes-off--authenticate process authenticated)
              ;; Send the welcome back to the bouncer user
              (shoes-off--send-welcome process)))))
      ;; Done with auth... try and deal with other commands
      (with-current-buffer procbuf
        (awhile (re-search-forward "[^\n]+\n" nil t)
          (goto-char it) ; DO move pointq
          (shoes-off--handle-request
           process authenticated (match-string 0)))))))

(defun shoes-off--sentinel (process status)
  "The sentinel on the bouncer server socket."
  (message
   "RCIRC BOUNCER KLAXON: [%s] %s"
   process status))

(defun shoes-off--log-fn (server con msg)
  "Join the rcirc bouncer client socket to the server."
  (process-put con :server server))

(defvar rcrirc-bouncer--server-process nil
  "The server socket.")

;;;###autoload
(defun shoes-off--make-server (port)
  "Make the listening server socket."
  (let ((buf (get-buffer-create "*shoes-off*")))
    (make-network-process
     :name "*shoes-off*"
     :buffer buf
     :server t
     :nowait 't
     :host nil ; see elnode for interesting rules about this
     :service port
     :coding '(raw-text-unix . raw-text-unix)
     :family 'ipv4
     :filter 'shoes-off--filter
     :sentinel 'shoes-off--sentinel
     :log 'shoes-off--log-fn
     :plist (list :shoes-off-example-prop t))))

(defvar shoes-off-start--port-history nil)

;;;###autoload
(defun shoes-off-start (port)
  "Start the bouncer daemon on PORT."
  (interactive (list
                (read-from-minibuffer
                 "Port: " nil nil nil
                 'shoes-off-start--port-history)))
  (setq rcrirc-bouncer--server-process
        (shoes-off--make-server (string-to-number port))))

;;;###autoload
(defun shoes-off-stop ()
  "Stop the bouncer daemon."
  (interactive)
  (delete-process rcrirc-bouncer--server-process))


;; Bouncer setup

(defun shoes-off--welcome (process text)
  (process-put process
               :shoes-off-server-name
               (car (split-string text " "))))

(defun shoes-off--receive-hook (process cmd sender args text)
  "Hook attached to rcirc to interpret the upstream irc server."
  (condition-case nil
      (let ((cmdstr (or
                     (aget shoes-off--cmd-names (string-to-number cmd))
                     cmd)))
        (when (equal cmdstr "WELCOME")
          (shoes-off--welcome process text))
        (when (member
               cmdstr
               shoes-off--cache-response-welcome-commands)
          (shoes-off--puthash
           process :shoes-off-welcome-cache cmdstr text))
        (when (equal cmdstr "JOIN")
          (shoes-off--puthash
           process :shoes-off-channel-cache (car args) text))
        ;; if we have a current bouncer con then send stuff there
        (awhen (process-get process :shoes-off-connection)
          (rcirc-send-string it text)))
    (error "whoops! something went wrong!!!")))

;; Setup the receive hook for the upstream IRC connection
(add-hook
 'rcirc-receive-message-hooks
 'shoes-off--receive-hook)

;;;###autoload
(defun shoes-off-start-session (username)
  "Start the bouncer for USERNAME.

Initiates the upstream IRC connections for the user."
  (destructuring-bind
        (&key
         username
         password
         server-alist)
      ;; Find the bouncer config by the username
      (loop for bouncer in shoes-off-config
         if (equal
             username
             (plist-get bouncer :username))
         return bouncer)
    ;; rcirc-connect has args in a particular order
    (destructuring-bind
          (server
           &key
           nick port user-name
           password full-name
           channels) (car server-alist)
      ;; Connect the client socket
      (let* (encryption ; hacked for now
             (connection
              (rcirc-connect
               server port nick user-name
               full-name channels password encryption)))
        (puthash
         username
         connection
         shoes-off--sessions)))))

(provide 'shoes-off)

;;; shoes-off.el ends here
