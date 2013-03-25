# MIAGI is a Gmail interface (for Emacs)

MIAGI is a simple Gmail client written in Emacs Lisp, intended to run
inside Emacs.

It supports basic IMAP based access to Gmail accounts, as well as some
support for Google's IMAP extensions. Also, it supports connecting to
multiple Gmail accounts concurrently.

To get started, add the kriyative elpa repo to `package-archives` like
so:

    (add-to-list 'package-archives '("kriyative" . "http://kriyative.github.com/elpa/packages/"))
    (package-initialize)
    (package-refresh-contents)

Once, the repo has been added, `miagi` can be installed from the
packages list. After it's been installed, add the following to the
.emacs.d/init.el (or other Emacs initialization) file:

    (require 'miagi)
    (setq miagi-accounts
          '(("gmail-account-1" . (:name "gmail-account-1"
                                  :email "your-gmail-id@gmail.com"
                                  :smtp (:user "your-gmail-id@gmail.com"
                                               :server "smtp.gmail.com"
                                               :port 587
                                               :stream-type ssl)))
            ...))

Once, `miagi` has loaded, you can start and open any of the accounts
setup specified in `miagi-accounts` using the following:

    M-x miagi [RET] <account-name> [RET]

Disclaimer: This is a very early release of the software and is
expected to be bug ridden and likely to cause major and irreparable
damage to your system, but hopefully it won't :)
