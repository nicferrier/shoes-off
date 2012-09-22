;;; shoes-off.el --- irc bouncer

;; Copyright (C) 2012  Nic Ferrier

;; Author: Nic Ferrier <nferrier@ferrier.me.uk>
;; Keywords: comm
;; Maintainer: Nic Ferrier <nferrier@ferrier.me.uk>
;; Created: 19th September 2012
;; Version: 0.1.8
;; Package-Requires: ((kv "0.0.5")(anaphora "0.0.4"))

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
(require 'assoc)

(defgroup shoes-off nil
  "An irc bouncer."
  ;;:version "22.1"
  ;;:link '(custom-manual "(rcirc)")
  :prefix "shoes-off-"
  :group 'applications)

(defcustom shoes-off-server-port "6901"
  "The TCP port the bouncer server will run on."
  :group 'shoes-off
  :type 'string)

(defcustom shoes-off-config '()
  "The bouncer configuration.

A list of plists describing the configuration of the bouncer.

IMPORTANT: The server-alist can be multiple servers. The bouncer
will start all of them and qualify access to them like
`username@servername'."
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
  "Hashtable of bouncer sessions keyed by username@servername.

The values are the upstream connections to the IRC servers.
These are always `rcirc' connections.")

(defun shoes-off--puthash (process property name value)
  "Make NAME[VALUE] on a hashtable at PROPERTY on the PROCESS."
  (puthash
   name value
   (or
    (process-get process property)
    (let ((h (make-hash-table :test 'equal)))
      (process-put process property h)
      h))))

(defun shoes-off--service-details (spec)
  "Get the username and the service from the SPEC.

The SPEC is like 'username@service' or just 'username'.

A cons is returned of `username . service' or `t' if there is no
service."
  (if (string-match
       "^\\(.*\\)@\\([A-Za-z.-]+\\)$"
       spec)
      (list (match-string 1 spec)
            (match-string 2 spec))
      ;; else
      (list spec t)))

(defun shoes-off--server-name (process)
  "Get the server name from the PROCESS.

The server name is set from the WELCOME message when the session
is started.  It's possible that the WELCOME message never arrives
so we provide a default."
  (aif (process-get process :shoes-off-server-name)
      (substring it 1)
    ;; this is a bad choice... do something better
    "localhost"))

(defun shoes-off--auth-check (username-spec password)
  "Check the USERNAME-SPEC and PASSWORD against the db.

The USERNAME-SPEC normally comes from the USER option in the IRC
session.  We support selection of target sessions by postfixed
usernames.  For example if you have 2 sessions:

  team.example.com
  irc.freenode.org

Then you can connect to irc.freenode.org by connecting with
username:

   username@irc.freenode.net

Not using the postfix will just connect the first session in the
config that matches the username.

The regex used to find the username allows @'s in the username.

If successful returns the bouncer spec with the username
transformed to the server qualified version.

Returns `nil' if auth is not found."
  (destructuring-bind (username service)
      (shoes-off--service-details username-spec)
    (loop for bouncer in shoes-off-config
       if (and
           (equal
            username
            (plist-get bouncer :username))
           (equal
            password
            (plist-get bouncer :password))
           (or (eq service t)
               (assoc service (plist-get bouncer :server-alist))))
       ;; Return the bouncer with the username transformed
       return (let ((username (plist-get bouncer :username))
                    (server
                     (if (eq service t)
                         ;; Pick the first off the server alist
                         (car (plist-get bouncer :server-alist))
                         ;; else pick the specified one
                         (assoc service (plist-get bouncer :server-alist)))))
                (plist-put
                 (copy-list bouncer)
                 :username
                 (format "%s@%s" username (car server)))))))


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
        (goto-char pt)
        details))))

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
  (let* ((hostname (shoes-off--server-name process))
         (cmd-num (aget shoes-off--cmd-numbers cmd))
         (cmd-str
          (format "%s %03d %s\n"
                  hostname
                  cmd-num
                  data)))
    (process-send-string process cmd-str)))

(defun shoes-off--get-auth-details (process)
  "Get the auth details from the process."
  (process-get process :shoes-off-authenticated))

(defun shoes-off--get-session (process)
  "Get the associated session from the client process."
  (let* ((auth (shoes-off--get-auth-details process))
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
         (hostname (shoes-off--server-name process))
         (welcome-cache (process-get session :shoes-off-welcome-cache)))
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
	   (message "shoes: sending JOIN |%s| to [%s]" response channel)
           (process-send-string process (concat response "\n"))) hash)))))

(defun shoes-off--authenticate (process auth-details)
  "Mark the PROCESS authenticated."
  (process-put process :shoes-off-authenticated auth-details)
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
      (if authenticated
          ;; Done with auth... try and deal with other commands
          (with-current-buffer procbuf
            (awhile (re-search-forward "[^\n]+\n" nil t)
              (goto-char it) ; DO move point
              (shoes-off--handle-request
               process authenticated (match-string 0))))
          ;; Else try authentication
          (awhen (shoes-off-auth (process-buffer process))
            (destructuring-bind (&key pass user user-info nick) it
              (awhen (shoes-off--auth-check user pass)
                (let ((bouncer-spec it))
                  ;; Note - what goes on the process is the bouncer-spec
                  (setq authenticated bouncer-spec)
                  (shoes-off--authenticate process authenticated)
                  ;; Send the welcome back to the bouncer user
                  (shoes-off--send-welcome process)))))))))

(defun shoes-off--sentinel (process status)
  "The sentinel on the bouncer server socket."
  (message
   "RCIRC BOUNCER KLAXON: [%s] %s"
   process status))

(defun shoes-off--log-fn (server con msg)
  "Join the rcirc bouncer client socket to the server."
  (process-put con :server server))

(defvar shoes-off--server-process nil
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
  (setq shoes-off--server-process
        (shoes-off--make-server (string-to-number port))))

;;;###autoload
(defun shoes-off-stop ()
  "Stop the bouncer daemon."
  (interactive)
  (delete-process shoes-off--server-process))


;; Bouncer setup

(defun shoes-off--welcome (process text)
  (process-put process
               :shoes-off-server-name
               (car (split-string text " "))))
(defun shoes-off--process-nick (process)
  "Get the NICK off the rcirc PROCESS."
  (with-current-buffer
      (process-buffer process)
    (substring-no-properties rcirc-nick 0)))

(defun shoes-off--join (process args text)
  "Process JOIN messages."
  (let (nick
        (mynick (shoes-off--process-nick process))
        (channel (car args)))
    (string-match "[|:]+\\([^!]+\\).*" text)
    (setq nick (match-string 1 text))
    (when (equal nick mynick)
      ;; Fix the join response
      (string-match "^\\(.*\\) JOIN \\(.*\\)" text)
      (shoes-off--puthash
       process :shoes-off-channel-cache channel
       (concat (match-string 1 text) " JOIN " channel)))))

(defun shoes-off--part (process args text)
  "Remove channels from the join cache that we're parting."
  (let (nick
        (mynick (shoes-off--process-nick process))
        (channel (car args)))
    (string-match "[|:]+\\([^!]+\\).*" text)
    (setq nick (match-string 1 text))
    (when (equal nick mynick)
      (remhash
       channel
       (process-get process :shoes-off-channel-cache)))))

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
        (when (equal cmdstr "PART")
          (message "shoes-off [%s] %s (%s) |%s|" process cmd args text)
          (shoes-off--part process args text))
        (when (equal cmdstr "JOIN")
          (shoes-off--join process args text))
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
  (interactive "MUsername to startup: ")
  (if shoes-off-server-port
      (unless shoes-off--server-process
        (shoes-off-start shoes-off-server-port)))
  (unless shoes-off-config
    (error "no shoes-off config loaded?"))
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
    (loop for server-config in server-alist
       do
         (destructuring-bind
               (server
                &key
                nick port user-name
                password full-name
                channels) server-config
           ;; Connect the client socket
           (let* (encryption ; hacked for now
                  (connection
                   (rcirc-connect
                    server port nick user-name
                    full-name channels password encryption)))
             (puthash
              (format "%s@%s" username server)
              connection
              shoes-off--sessions))))))

(provide 'shoes-off)

;;; shoes-off.el ends here
