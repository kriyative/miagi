;;; miagi.el --- miagi is a gmail interface

;; Copyright (C) 2013

;; Author: Ram Krishnan <kriyative@gmail.com>
;; Keywords: mail

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Code:

(require 'imap)
(require 'mailheader)
(require 'message)
(require 'smtpmail)

(defvar miagi-imap-buffer nil)
(defvar miagi-account-name nil)
(defvar miagi-user-mail-address nil)
(defvar miagi-user-mail-password nil)
(defvar miagi-smtp-info nil)
(defvar miagi-folders nil)
(defvar miagi-current-folder nil)
(defvar miagi-current-message nil)

(defvar miagi-summary-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "g" 'miagi-get-messages)
    (define-key map "\C-j" 'miagi-open-message)
    (define-key map (kbd "RET") 'miagi-open-message)
    (define-key map "c" 'miagi-compose)
    (define-key map "r" 'miagi-reply)
    (define-key map "n" 'miagi-open-next-message)
    (define-key map "p" 'miagi-open-previous-message)
    (define-key map "b" 'miagi-folder-select)
    (define-key map "/" 'miagi-search)
    (define-key map "i" 'miagi-open-inbox)
    (define-key map " " 'miagi-message-next-page)
    (define-key map (kbd "DEL") 'miagi-message-previous-page)
    (define-key map "B" 'miagi-open-message-browse)
    map)
  "Keymap for `miagi-summary-mode'.")

(define-derived-mode miagi-summary-mode fundamental-mode "Miagi Summary"
  "A major mode for Miagi mailbox summary buffers."
  (make-local-variable 'miagi-imap-buffer)
  (make-local-variable 'miagi-account-name)
  (make-local-variable 'miagi-user-mail-address)
  (make-local-variable 'miagi-user-mail-password)
  (make-local-variable 'miagi-smtp-info)
  (make-local-variable 'miagi-folders)
  (make-local-variable 'miagi-current-folder)
  (make-local-variable 'miagi-current-message)
  (setq buffer-read-only t))

(defun miagi-get-summary-buffer (account)
  (let* ((buf-name (format "*miagi-summary: %s*" account))
         (buf (get-buffer buf-name)))
    (unless buf
      (setq buf (get-buffer-create buf-name))
      (with-current-buffer buf
        (miagi-summary-mode)))
    buf))

(defvar miagi-summary-buffer nil)

(defvar miagi-message-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "r" 'miagi-message-reply)
    (define-key map "n" 'miagi-message-open-next)
    (define-key map "p" 'miagi-message-open-previous)
    (define-key map " " 'scroll-up-command)
    (define-key map (kbd "DEL") 'scroll-down-command)
    map)
  "Keymap for `miagi-message-mode'.")

(define-derived-mode miagi-message-mode message-mode "Miagi Message"
  "A major mode for Miagi message buffers."
  (make-local-variable 'miagi-summary-buffer)
  (setq buffer-read-only t
        buffer-undo-list t))

(defun miagi-get-message-buffer ()
  (let* ((buf-name (format "*miagi-message: %s*" miagi-account-name))
         (buf (get-buffer buf-name))
         (summary-buf (current-buffer)))
    (unless buf
      (setq buf (get-buffer-create buf-name))
      (with-current-buffer buf
        (miagi-message-mode)
        (setq message-auto-save-directory nil)
        (setq miagi-summary-buffer summary-buf)))
    buf))

(defun miagi-open-connection ()
  (unless (imap-opened miagi-imap-buffer)
    (let ((server "imap.gmail.com")
          (port 993))
      (setq miagi-imap-buffer (imap-open server port 'ssl nil
                                         (or miagi-imap-buffer
                                             (generate-new-buffer
                                              (format " *imap* %s:%d-%s"
                                                      server
                                                      (or port 0)
                                                      miagi-account-name))))))
    (imap-authenticate miagi-user-mail-address
                       miagi-user-mail-password
                       miagi-imap-buffer)))

(defun miagi-close ()
  (imap-logout miagi-imap-buffer))

(defun miagi-folder-list ()
  (with-current-buffer miagi-imap-buffer
    (imap-mailbox-list "" "*")))

(defun miagi-load-folders ()
  (setq miagi-folders (sort (miagi-folder-list) 'string-lessp)))

(defun miagi-select-folder (mb)
  (setq miagi-current-folder mb)
  (rename-buffer (format "*miagi-summary: %s+%s*"
                         miagi-account-name
                         miagi-current-folder))
  (force-mode-line-update)
  (imap-mailbox-select mb nil miagi-imap-buffer))

(defun miagi-open-inbox ()
  "Open the INBOX mailbox."
  (interactive)
  (miagi-select-folder "INBOX")
  (miagi-get-messages))

(defun miagi-format-date (date-str)
  (let* ((parsed-time (date-to-time date-str))
         (days-diff (- (time-to-days (current-time))
                       (time-to-days parsed-time))))
    (cond
     ((< days-diff 1)
      (format-time-string "%l:%M %p" parsed-time))
     ((< days-diff 365)
      (format-time-string "%b %e" parsed-time))
     (t
      (format-time-string "%m/%d/%y" parsed-time)))))

;; (miagi-format-date "Mon, 18 Mar 2013 21:47:39 -0700")

(defun miagi-name-or-address (addr-str)
  (let ((faddr (first addr-str)))
    (or (miagi-decode-string (elt faddr 0))
        (concat (elt faddr 2) "@" (elt faddr 3)))))

(defun miagi-format-address-full (addr-str)
  (when (sequencep addr-str)
    (let ((faddr (first addr-str)))
      (concat (or (elt faddr 0) "")
              " <" (elt faddr 2) "@" (elt faddr 3) ">"))))

(defun miagi-ensure-open ()
  (miagi-open-connection)
  (miagi-select-folder (or miagi-current-folder "INBOX")))

(defconst miagi-envelope-date-index 0)
(defconst miagi-envelope-subject-index 1)
(defconst miagi-envelope-from-index 2)
(defconst miagi-envelope-sender-index 3)
(defconst miagi-envelope-reply-to-index 4)
(defconst miagi-envelope-to-index 5)
(defconst miagi-envelope-cc-index 6)
(defconst miagi-envelope-bcc-index 7)
(defconst miagi-envelope-in-reply-to-index 8)
(defconst miagi-envelope-message-id-index 9)

(defun miagi-message-get (uid prop)
  (imap-message-get uid prop miagi-imap-buffer))

(defun miagi-decode-string (string)
  (when string (rfc2047-decode-string string)))

(defun miagi-summary-update-properties-at-point (&optional uid)
  (when-let (uid (or uid (get-text-property (point) 'uid)))
    (let ((start (line-beginning-position))
          (end (line-end-position)))
      (if (member "\\Seen" (miagi-message-get uid 'FLAGS))
          (put-text-property start end 'face 'default)
        (put-text-property start end 'face 'bold)))))

(defun miagi-render-summary (env)
  (let ((start (point)))
    (insert
     (format " %8s" (miagi-format-date (elt env miagi-envelope-date-index))))
    (insert
     " "
     (truncate-string-to-width (miagi-name-or-address
                                (elt env miagi-envelope-from-index))
                               21 nil ?  t))
    (insert
     " "
     (truncate-string-to-width (miagi-decode-string
                                (or (elt env miagi-envelope-subject-index)
                                    ""))
                               (- (window-width) (current-column) 2)
                               nil nil t))
    (put-text-property start (point) 'uid uid)
    (miagi-summary-update-properties-at-point uid)
    (newline)))

(defun miagi-get-messages-1 (expr)
  (miagi-ensure-open)
  (let ((inhibit-read-only t))
    (let* ((uids (imap-search expr miagi-imap-buffer))
           (missing (remove-if (lambda (uid)
                                 (miagi-message-get uid 'ENVELOPE))
                               uids)))
      (when missing
        (with-current-buffer miagi-imap-buffer
          (imap-fetch missing "(FLAGS ENVELOPE)")))
      (save-excursion
        (delete-region (point-min) (point-max))
        (dolist (uid (reverse uids))
          (when-let (env (miagi-message-get uid 'ENVELOPE))
            (miagi-render-summary env)))))))

(defun miagi-get-messages ()
  "Get the latest messages in the currently selected folder."
  (interactive)
  (let ((start-date (format-time-string
                     "%d-%b-%Y"
                     (time-subtract (current-time) (days-to-time 15)))))
    (miagi-get-messages-1 (format "SINCE %s" start-date))))

(defun miagi-folder-select ()
  "Switch to a different folder in the summary view."
  (interactive)
  (miagi-ensure-open)
  (let ((folder (completing-read "Folder: " (miagi-folder-list))))
    (miagi-select-folder folder)
    (miagi-get-messages)))

(defun miagi-search (&optional terms)
  "Search the current folder using Gmail's search feature."
  (interactive "sSearch: ")
  (miagi-get-messages-1 (format "X-GM-RAW %S" terms)))

(defun miagi-zap-cr-chars (start end)
  (save-excursion
    (goto-char start)
    (while (search-forward "\r" end t)
      (replace-match "" nil t))))

;;; save the original definition of imap-parse-fetch before overriding
;;; it.
(setf (symbol-function 'imap-parse-fetch-original)
      (symbol-function 'imap-parse-fetch))

(defun imap-parse-fetch-x-gm-labels ()
  (mapcar (lambda (f)
            (if (and (stringp f) (eq ?\" (aref f 0)))
                (car (read-from-string f))
              f))
          (imap-parse-flag-list)))

(setq imap-parse-fetch-handlers
  '((X-GM-LABELS . imap-parse-fetch-x-gm-labels)))

(defun imap-parse-fetch-handlerp (token)
  (assoc token imap-parse-fetch-handlers))

(defun imap-parse-fetch-handler-invoke (token)
  (let ((handler (cdr (assoc token imap-parse-fetch-handlers))))
    (cons token (funcall handler))))

;; overriding the definition from imap.el, to add support for parsing
;; Gmail specific fetch options
(defun imap-parse-fetch (response)
  (when (eq (char-after) ?\()
    (let (uid flags envelope internaldate rfc822 rfc822header rfc822text
	      rfc822size body bodydetail bodystructure flags-empty
              extended-meta)
      ;; Courier can insert spurious blank characters which will
      ;; confuse `read', so skip past them.
      (while (let ((moved (skip-chars-forward " \t")))
	       (prog1 (not (eq (char-after) ?\)))
		 (unless (= moved 0) (backward-char))))
	(imap-forward)
	(let ((token (read (current-buffer))))
	  (imap-forward)
	  (cond ((imap-parse-fetch-handlerp token)
                 (push (imap-parse-fetch-handler-invoke token) extended-meta))
                ((eq token 'UID)
		 (setq uid (condition-case ()
			       (read (current-buffer))
			     (error))))
		((eq token 'FLAGS)
		 (setq flags (imap-parse-flag-list))
		 (if (not flags)
		     (setq flags-empty 't)))
		((eq token 'ENVELOPE)
		 (setq envelope (imap-parse-envelope)))
		((eq token 'INTERNALDATE)
		 (setq internaldate (imap-parse-string)))
		((eq token 'RFC822)
		 (setq rfc822 (imap-parse-nstring)))
		((eq token 'RFC822.HEADER)
		 (setq rfc822header (imap-parse-nstring)))
		((eq token 'RFC822.TEXT)
		 (setq rfc822text (imap-parse-nstring)))
		((eq token 'RFC822.SIZE)
		 (setq rfc822size (read (current-buffer))))
		((eq token 'BODY)
		 (if (eq (char-before) ?\[)
		     (push (list
			    (upcase (imap-parse-fetch-body-section))
			    (and (eq (char-after) ?<)
				 (buffer-substring (1+ (point))
						   (search-forward ">" nil t)))
			    (progn (imap-forward)
				   (imap-parse-nstring)))
			   bodydetail)
		   (setq body (imap-parse-body))))
		((eq token 'BODYSTRUCTURE)
		 (setq bodystructure (imap-parse-body))))))
      (when uid
	(setq imap-current-message uid)
	(imap-message-put uid 'UID uid)
	(and (or flags flags-empty) (imap-message-put uid 'FLAGS flags))
	(and envelope (imap-message-put uid 'ENVELOPE envelope))
	(and internaldate (imap-message-put uid 'INTERNALDATE internaldate))
	(and rfc822 (imap-message-put uid 'RFC822 rfc822))
	(and rfc822header (imap-message-put uid 'RFC822.HEADER rfc822header))
	(and rfc822text (imap-message-put uid 'RFC822.TEXT rfc822text))
	(and rfc822size (imap-message-put uid 'RFC822.SIZE rfc822size))
	(and body (imap-message-put uid 'BODY body))
	(and bodydetail (imap-message-put uid 'BODYDETAIL bodydetail))
	(and bodystructure (imap-message-put uid 'BODYSTRUCTURE bodystructure))
        (and extended-meta (imap-message-put uid 'EXTENDEDMETA extended-meta))
	(run-hooks 'imap-fetch-data-hook)))))

(defun miagi-parse-body-structure (body-structure &optional prefix)
  (let ((index 0))
    (reduce (lambda (plist el)
              (incf index)
              (cond
               ((not (listp el)) plist)
               ((listp (car el))
                (append plist
                        (miagi-parse-body-structure el
                                                    (format "%s%d."
                                                            (or prefix "")
                                                            index))))
               ((equal "BOUNDARY" (car el)) plist)
               (t
                (condition-case c
                    (destructuring-bind (major minor charset
                                               i1 i2 encoding len
                                               i3 attachment &rest ignorables)
                        el
                      (append plist
                              (list
                               (append
                                (list :ref (format "%s%d" (or prefix "") index)
                                      :content-type (concat (downcase major)
                                                            "/"
                                                            (downcase minor))
                                      :encoding (downcase encoding)
                                      :content-length len)
                                (when (and charset
                                           (equal "CHARSET" (car charset)))
                                  (list :charset (cadr charset)))
                                (when attachment
                                  (list :attachment
                                        (cadr
                                         (assoc "FILENAME" (cdr attachment)))))))))
                  (error
                   (message "parse-error: %S %S" c el)
                   nil)))))
            body-structure
            :initial-value '())))

(defun miagi-message-find-part (pbs ctype)
  (find ctype pbs :test 'equal :key (lambda (x) (getf x :content-type))))

(defvar miagi-decoder-alist
  '(("quoted-printable" . quoted-printable-decode-string)
    ("base64" . base64-decode-string)))

(defun miagi-decode-part (part-info string)
  (let ((string (replace-regexp-in-string "\r" "" string))
        (encoding (getf part-info :encoding)))
    (if-let (decoder (cdr (assoc encoding miagi-decoder-alist)))
        (funcall decoder string)
      string)))

(defun miagi-message-cache-dir (account uid)
  (assert (not (null account)))
  (file-name-as-directory
   (expand-file-name (format "~/Mail/miagi/%s/%s" account uid))))

(defun ensure-directory (dir)
  (unless (file-directory-p dir)
    (mkdir dir t))
  dir)

(defconst miagi-visible-headers
  '(from to cc bcc subject date sender))

(defun miagi-insert-message-headers (account uid)
  (let* ((cache-dir (ensure-directory (miagi-message-cache-dir account uid)))
         (cache-file (concat cache-dir "headers"))
         (imap-buffer miagi-imap-buffer)
         (headers (with-temp-buffer
                    (if (file-exists-p cache-file)
                        (insert-file-contents-literally cache-file)
                      (progn
                        (with-current-buffer imap-buffer
                          (imap-fetch uid 'RFC822.HEADER))
                        (insert
                         (imap-message-get uid 'RFC822.HEADER imap-buffer))
                        (write-region (point-min) (point-max) cache-file)
                        (goto-char (point-min))))
                    (miagi-zap-cr-chars (point-min) (point-max))
                    (mail-header-extract))))
    (with-current-buffer (miagi-get-message-buffer)
      (dolist (h miagi-visible-headers)
        (when-let (header (assq h headers))
          (insert
           (format "%s: %s\n"
                   (capitalize (symbol-name (car header)))
                   (miagi-decode-string (cdr header))))))
      (newline))))

(defun ensure-list (x)
  (if (listp x) x (list x)))

(defun wrap-list (x)
  (if (listp (car x)) x (list x)))

(defun miagi-message-body-structure (uid)
  (let ((bs (or (miagi-message-get uid 'BODYSTRUCTURE)
                (progn
                  (with-current-buffer miagi-imap-buffer
                    (imap-fetch uid "(BODYSTRUCTURE)"))
                  (miagi-message-get uid 'BODYSTRUCTURE)))))
    (miagi-parse-body-structure (wrap-list bs))))

(defun miagi-select-text-part (body-structure &optional preferences)
  (let ((preferences (or preferences '("text/html" "text/plain"))))
    (destructuring-bind (preferred alternative)
        preferences
      (let ((text-part (miagi-message-find-part body-structure preferred)))
        (if text-part
            (list text-part t)
          (let ((alt-part (miagi-message-find-part body-structure alternative)))
            (list alt-part nil)))))))

(defun miagi-select-text-part-prefer-text (body-structure)
  (miagi-select-text-part body-structure '("text/plain" "text/html")))

(defun miagi-select-text-part-prefer-html (body-structure)
  (miagi-select-text-part body-structure '("text/html" "text/plain")))

(defun text-link-click (button)
  (let ((browse-url-browser-function 'browse-url-default-browser))
    (browse-url (button-get button 'url))))

(defun text-linkify (start end)
  (save-excursion
    (goto-char start)
    (while (re-search-forward "\\(http[s]*://[^ \n]*\\)" end t)
      (let ((beg (match-beginning 0))
            (end (match-end 0)))
        (make-text-button beg end
                          'url (buffer-substring-no-properties beg end)
                          'action 'text-link-click)))))

(defun miagi-insert-message-body (account uid body-structure)
  (let ((cache-dir (ensure-directory (miagi-message-cache-dir account uid))))
    (destructuring-bind (body-part htmlp)
        (miagi-select-text-part-prefer-html body-structure)
      (let* ((part-ref (getf body-part :ref))
             (cache-file (concat cache-dir part-ref))
             (beg (with-current-buffer (miagi-get-message-buffer)
                    (point))))
        (if (and (file-readable-p cache-file)
                 (not (file-directory-p cache-file)))
            (with-current-buffer (miagi-get-message-buffer)
              (insert-file-contents-literally cache-file))
          (progn
            (when body-part
              (with-current-buffer miagi-imap-buffer
                (imap-fetch uid (format "BODY[%s]" (getf body-part :ref))))
              (let ((body (miagi-decode-part body-part
                                             (third
                                              (first
                                               (miagi-message-get uid 'BODYDETAIL))))))
                (with-current-buffer (miagi-get-message-buffer)
                  (let ((buffer-file-coding-system
                         (when-let (charset (getf body-part :charset))
                           (let ((cs (intern (downcase charset))))
                             (if (memq cs coding-system-list)
                                 cs
                               'raw-text)))))
                    (set-buffer-file-coding-system buffer-file-coding-system t)
                    (insert body)
                    (let ((coding-system-for-write 'raw-text))
                      (write-region beg (point) cache-file))))))))
        (with-current-buffer (miagi-get-message-buffer)
          (if htmlp
              (let ((w3m-fill-column (- (window-width) 2))
                    (w3m-display-inline-images nil)
                    (browse-url-browser-function))
                (w3m-region beg (point-max)))
            (let ((fill-column (- (window-width) 2)))
              (longlines-mode 1)
              (miagi-zap-cr-chars beg (point-max))))
          (text-linkify beg (point-max)))))))

(defun miagi-open-attachment (button)
  (let ((browse-url-browser-function 'browse-url-default-browser))
    (browse-url (browse-url-file-url (button-get button 'file)))))

(defun miagi-insert-message-attachments (account uid body-structure)
  (let ((cache-dir (ensure-directory (miagi-message-cache-dir account uid)))
        (parts (remove-if-not (lambda (p) (getf p :attachment)) body-structure)))
    (dolist (part parts)
      (destructuring-bind (&key ref content-type encoding attachment &allow-other-keys)
          part
        (let* ((filename (replace-regexp-in-string "[ \t]*" "" attachment))
               (cache-file (concat cache-dir filename)))
          (unless (file-exists-p cache-file)
            (with-current-buffer miagi-imap-buffer
              (imap-fetch uid (format "BODY[%s]" ref)))
            (let ((body (miagi-decode-part part
                                           (third
                                            (first
                                             (miagi-message-get uid 'BODYDETAIL))))))
              (with-temp-buffer
                (insert body)
                (let ((coding-system-for-write 'raw-text))
                  (write-region (point-min) (point-max) cache-file)))))
          (with-current-buffer (miagi-get-message-buffer)
            (goto-char (point-max))
            (newline)
            (insert-text-button (concat "[" filename "]")
                                'action 'miagi-open-attachment
                                'file cache-file)))))))

(defun miagi-open-message ()
  "View the message at point in a Miagi Message buffer"
  (interactive)
  (miagi-ensure-open)
  (when-let (uid (get-text-property (point) 'uid))
    (unless (eq uid miagi-current-message)
      (let ((inhibit-read-only t)
            (account-name miagi-account-name))
        (with-current-buffer (miagi-get-message-buffer)
          (longlines-mode -1)
          (setq miagi-current-message uid)
          (delete-region (point-min) (point-max)))
        (miagi-insert-message-headers account-name uid)
        (let ((body-structure (miagi-message-body-structure uid)))
          (miagi-insert-message-body account-name uid body-structure)
          (miagi-insert-message-attachments account-name uid body-structure))
        (with-current-buffer (miagi-get-message-buffer)
          (goto-char (point-min)))
        (miagi-summary-update-properties-at-point uid)
        (display-buffer (miagi-get-message-buffer)
                        'display-buffer-use-some-window)
        (setq miagi-current-message uid)))))

(defun miagi-open-message-browse ()
  "Open the message at point using an external HTML browser"
  (interactive)
  (miagi-open-message)
  (when-let (uid (get-text-property (point) 'uid))
    (let ((body-structure (miagi-message-body-structure uid)))
      (destructuring-bind (body-part htmlp)
          (miagi-select-text-part-prefer-html body-structure)
        (let* ((part-ref (getf body-part :ref))
               (account miagi-account-name)
               (cache-dir (ensure-directory (miagi-message-cache-dir account uid)))
               (cache-file (concat cache-dir part-ref)))
          (let ((browse-url-browser-function 'browse-url-default-browser))
            (browse-url
             (browse-url-file-url cache-file))))))))

(defun miagi-open-next-message ()
  "Move to the next message in the folder, and open it."
  (interactive)
  (next-line)
  (miagi-open-message))

(defun miagi-open-previous-message ()
  "Move to the previous message in the folder, and open it."
  (interactive)
  (previous-line)
  (miagi-open-message))

(defun miagi-message-open-next ()
  (interactive)
  (with-current-buffer miagi-summary-buffer
    (miagi-open-next-message)))

(defun miagi-message-open-previous ()
  (interactive)
  (with-current-buffer miagi-summary-buffer
    (miagi-open-previous-message)))

(defun miagi-message-next-page ()
  "Scroll forward the message buffer"
  (interactive)
  (let ((other-window-scroll-buffer (miagi-get-message-buffer)))
    (scroll-other-window)))

(defun miagi-message-previous-page ()
  "Scroll backward the message buffer"
  (interactive)
  (let ((other-window-scroll-buffer (miagi-get-message-buffer)))
    (scroll-other-window '-)))

(defun miagi-compose-mail-1 (&optional to cc subject in-reply-to continue
                                       switch-function yank-action)
  (destructuring-bind (&key user server port stream-type &allow-other-keys)
      miagi-smtp-info
    (let ((smtpmail-smtp-server server)
          (smtpmail-smtp-service port)
          (smtpmail-smtp-service 'starttls)
          (user-mail-address user)
          (smtpmail-auth-credentials (list (list server port user nil)))
          (smtpmail-starttls-credentials (list (list server port user nil))))
      (compose-mail to
                    (or subject "")
                    (let ((hdrs '(("x-mailer" . "emacs-miagi-0.0.1"))))
                      (when cc (push (cons "cc" cc) hdrs))
                      (when in-reply-to (push (cons "in-reply-to" in-reply-to) hdrs))
                      hdrs)
                    continue
                    switch-function
                    yank-action))))

(defun miagi-setup-compose ()
  ;; fixme: this should be buffer local or dynamically bound,
  ;; currently it's side-effecting the global binding, which is the
  ;; only I could get the right smtp credentials in the composition
  ;; buffer
  (setq smtpmail-smtp-user (third (first smtpmail-auth-credentials))))

(defun miagi-compose ()
  (interactive)
  (let ((message-setup-hook (cons 'miagi-setup-compose message-setup-hook)))
    (miagi-compose-mail-1)))

(defun miagi-setup-reply ()
  (goto-char (point-max))
  (message-yank-original))

(defun miagi-reply (&optional uid)
  "Compose a reply to the message at point"
  (interactive)
  (miagi-open-message)
  (when-let (uid (or uid (get-text-property (point) 'uid)))
    (let ((env (miagi-message-get uid 'ENVELOPE))
          (message-setup-hook (cons 'miagi-setup-reply message-setup-hook)))
      (miagi-compose-mail-1 (miagi-format-address-full
                             (or (elt env miagi-envelope-reply-to-index)
                                 (elt env miagi-envelope-from-index)
                                 (elt env miagi-envelope-sender-index)))
                            (miagi-format-address-full
                             (elt env miagi-envelope-cc-index))
                            (concat "Re: "
                                    (message-strip-subject-re
                                     (elt env miagi-envelope-subject-index)))
                            (elt env miagi-envelope-message-id-index)
                            nil
                            nil
                            (miagi-get-message-buffer)))))

(defun miagi-message-reply ()
  (interactive)
  (with-current-buffer miagi-summary-buffer
    (miagi-reply)))

(defun miagi-open-account (account-plist)
  (destructuring-bind (&key name email password smtp &allow-other-keys)
      account-plist
    (with-current-buffer (miagi-get-summary-buffer name)
      (setq miagi-account-name name)
      (unless miagi-user-mail-address
        (setq miagi-user-mail-address (or email
                                          (read-from-minibuffer "Email address: "))))
      (unless miagi-user-mail-password
        (setq miagi-user-mail-password (or password
                                           (read-passwd "Password: "))))
      (setq miagi-smtp-info smtp)
      (unless miagi-imap-buffer
        (miagi-open-connection))
      (miagi-load-folders)
      (miagi-get-messages)
      (switch-to-buffer (current-buffer)))))

(defvar miagi-accounts nil)

(defun miagi (&optional account)
  (interactive (list (completing-read "Account: " (mapcar 'car miagi-accounts))))
  (let ((acct (cdr (assoc account miagi-accounts))))
    (miagi-open-account acct)))

(setq starttls-extra-arguments nil ; '("--insecure")
      message-cite-style nil
      mml-enable-flowed nil)

(provide 'miagi)
