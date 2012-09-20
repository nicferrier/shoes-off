(require 'ert)
(require 'shoes-off)
(require 'kv)
(require 'fakir)

(ert-deftest shoes-off--send-command ()
  (fakir-mock-process :irc ()
    (shoes-off--send-command :irc "YOURHOST" "welcome blah blah")
    (with-current-buffer (fakir-get-output-buffer)
      (should
       (equal
        "hostname 002 welcome blah blah\n"
        (buffer-substring-no-properties
         (point-min)(point-max)))))))

(ert-deftest shoes-off--send-welcome ()
  "Checks the caching of welcomes works."
  :expected-result :failed
  ;; double fakir?
  (fakir-mock-process :ircserver
      ((:shoes-off-welcome-cache
        #s(hash-table
           data ("YOURHOST"
                 "welcome blah blah"
                 "CREATED" "server made someday or other"
                 "MYINFO" "blah"
                 "BOUNCE" "blah"))))
    (let ((shoes-off--sessions (make-hash-table :test 'equal)))
      (puthash "testuser" :ircserver shoes-off--sessions)
      (fakir-mock-process :ircclient
          ((:shoes-off-authenticated
            '(:user "testuser"
              :pass "secret"
              :user-info "0 blah")))
        (shoes-off--send-welcome :ircclient)
        (with-current-buffer (fakir-get-output-buffer)
          (should
           (equal
            "hostname 002 welcome blah blah
hostname 003 server made someday or other
hostname 004 blah
hostname 005 blah\n"
            (buffer-substring-no-properties
             (point-min)(point-max)))))))))

(ert-deftest shoes-off-auth ()
  (should
   (equal
    (sort '((nick . "testnick")
            (pass . "secret")
            (user . "testuser")
            (user-info . "0 * :unknown")) 'kvcmp)
    (sort (kvplist->alist ; we have to cmp with an alist, it's easier
           (with-temp-buffer
             (insert "PASS secret
NICK testnick
USER testuser 0 * :unknown
SOMEOTHER message\n")
             (shoes-off-auth (current-buffer)))) 'kvcmp))))

(ert-deftest shoes-off-auth-filter ()
  (let ((shoes-off--users
         '((:server-alist
            ("test.example.com"
             :nick "testnick"
             :port 6668
             :user-name "testuser"
             :password "secret"
             :full-name "Test User"
             :channels ("#test"))
            :username "testuser"
            :password "secret"))))
    (fakir-mock-process :irc
        ((:buffer "PASS secret
NICK testnick
USER testuser 0 * :unknown
SOMEOTHER message\n"))
      (flet ((shoes-off--authenticate (process details)
               t)
             (shoes-off--send-welcome (process)
               t))
        (shoes-off--filter :irc "OTHERCOMMAND\n")
        (with-current-buffer (fakir-get-output-buffer)
          (buffer-substring (point-min)(point-max)))))))

(ert-deftest shoes-off-start ()
  "Test the bouncer startup.

Fake's the IRC connect."
  (let (connected
        (shoes-off--sessions ; mock to an empty
         (make-hash-table :test 'equal))
        (shoes-off--users
         '((:server-alist
            ("irc.example.com"
             :nick "test-nick"
             :port 6667
             :user-name "testuser"
             :password "secret"
             :full-name "Mr Test User"
             :channels ("#emacs"))
            :username "testuser"
            :password "secret"))))
    (flet ((rcirc-connect ; match the rcirc-connect arglist exactly
               (server
                &optional
                port nick user-name
                full-name startup-channels password encryption)
             (setq connected t)))
      (shoes-off-start "testuser")
      (should connected))))

(ert-deftest shoes-off--auth-check ()
  "Test login."
  (let (connected
        (shoes-off--users
         '((:server-alist
            ("irc.example.com"
             :nick "test-nick"
             :port 6667
             :user-name "testuser"
             :password "secret"
             :full-name "Mr Test User"
             :channels ("#emacs"))
            :username "testuser"
            :password "secret"))))
    (should (shoes-off--auth-check "testuser" "secret"))
    (should
     (equal
      (car shoes-off--users)
      (shoes-off--auth-check "testuser" "secret")))))

;;; tests ends here
