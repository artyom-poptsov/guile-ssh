;;; .dir-locals.el -- Per-directory local variables for GNU Emacs 23 and later.

((nil         . ((fill-column  . 78)
                 (tab-width    .  8)))
 (c-mode      . ((c-file-style . "gnu")))
 (scheme-mode . ((indent-tabs-mode . nil)
                 (eval . (put 'test-assert 'scheme-indent-function 1))
                 (eval . (put 'test-assert-with-log 'scheme-indent-function 1))
                 (eval . (put 'test-error-with-log 'scheme-indent-function 1)))))

;;; .dir-locals.el ends here

