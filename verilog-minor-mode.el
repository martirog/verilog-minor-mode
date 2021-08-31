(defvar vminor-ctags-verilog-def
  '("--language=none"
    "--regex=\"/^[ \\t]*\\(static\\|local\\|virtual\\|protected\\|interface\\)*[ \\t]*class[ \\t]*\\([0-9a-zA-Z\\$_]+\\)/\\2/\""
    "--regex=\"/^[ \\t]*\\(static\\|local\\|virtual\\|protected\\)*[ \\t]*task[ \\t]*\\(static\\|automatic\\)?[ \\t]*\\([0-9a-zA-Z\\$_]+::\\)?[ \\t]*[0-9a-zA-Z\\$_]+/\\3/\""
    "--regex=\"/^[ \\t]*\\(static\\|local\\|virtual\\|protected\\)*[ \\t]*function[ \\t]*\\([0-9a-zA-Z\\$_]+\\)?[ \\t]*\\([0-9a-zA-Z\\$_]+::\\)?[ \\t]*[0-9a-zA-Z\\$_]+/\\3/\""
    "--regex=\"/^[ \\t]*module[ \\t]*\\([0-9a-zA-Z\\$_]+\\)/\\1/\""
    "--regex=\"/^[ \\t]*package[ \\t]*\\([0-9a-zA-Z\\$_]+\\)/\\1/\""
    "--regex=\"/^[ \\t]*program[ \\t]*\\([0-9a-zA-Z\\$_]+\\)/\\1/\""
    "--regex=\"/^[ \\t]*interface[ \\t]*\\([0-9a-zA-Z\\$_]+\\)/\\1/\""
    "--regex=\"/^[ \\t]*typedef[ \\t]+.*[ \\t]+\\([0-9a-zA-Z\\$_]+\\)[ \\t]*;/\\1/\""
    "--regex=\"/^[ \\t]*\\`define[ \\t]*\\([0-9a-zA-Z\\$_]+\\)/\\`\\1/\""
    "--regex=\"/}[ \\t]*\\([0-9a-zA-Z\\$_]+\\)[ \\t]*;/\\1/\""
    "--regex=\"/^[ \\t]*\\(static\\|local\\|private\\|rand\\)*[ \\t]*\\(shortint\\|int\\|longint\\)[ \\t]*unsigned?\\([ \\t]*[.+]\\)*[ \\t]*\\([0-9a-zA-Z\\$_]+\\)/\\4/\""
    "--regex=\"/^[ \\t]*\\(static\\|local\\|private\\|rand\\)*[ \\t]*\\(byte\\|bit\\|logic\\|reg\\|integer\\|time\\)\\([ \\t]*[.+]\\)*[ \\t]*\\([0-9a-zA-Z\\$_]+\\)/\\4/\""
    "--regex=\"/^[ \\t]*\\(static\\|local\\|private\\)*[ \\t]*\\(real\\|shortreal\\|chandle\\|string\\|event\\)\\([ \\t]*[.+]\\)*[ \\t]*\\([0-9a-zA-Z\\$_]+\\)/\\4/\""
    "--regex=\"/\\(input\\|output\\|inout\\)?[ \\t]*\\([.+]\\)*[ \\t]*\\(wire\\|reg\\|logic\\)[ \\t]*\\([.+]\\)*[ \\t]*\\(#\\(\\(.+\\)\\|[ \\t]+\\)\\)\\)?[ \\t]*\\([0-9a-zA-Z\\$_]+\\)/\\8/\""
    "--regex=\"/\\(parameter\\|localparam\\).+\\([0-9a-zA-Z\\$_]+\\)[ \\t]*=/\\2\""
  "define how ctags should find system verilog tags"))

(defvar vminor-path-to-repos nil
  "list of cons: repos to search for systemverilog files . exclude list")

(defvar vminor-file-extention
  '("v" "sv" "vh" "svh")
  "file extentions to use in the search for files to tag")

(defvar vminor-tag-path (concat (getenv "HOME") "/") ;nil
  "path to puth the TAGS file")

(defvar vminor-tag-file-name "vminor_TAGS"
  "name of the TAGS file")

(defvar vminor-use-vc-root-for-tags t
  "you only want tags for the current repo and that repo uses GIT")

(defvar vminor-sv-key-words
  '("begin" "end"
    "module" "endmodule"
    "initial" "always"
    "function" "endfunction"
    "task" "endtask"
    "package" "endpackage"
    "class" "endclass"
    "extends" "implements"
    "static" "automatic"
    "foreach" "break"
    "semaphore"
    "covergroup" "coverpoint" "cross"
    "push_back" "push_front" "pop_back" "pop_front"
    "$display" "$sformatf")
  "System verilog keywords and functions")

(defvar vminor-common-uvm
  '("uvm_object" "`uvm_object_utils"
    "uvm_component" "`uvm_component_utils")
  "System verilog keywords and functions")

(defun vminor-regen-tags()
  "regenerate the tags file using ctags. so you need to have ctags in your path for this to work"
  (interactive) ; make this ask for files and paths
  (let ((tag-file (concat vminor-tag-path vminor-tag-file-name))
        (repos vminor-path-to-repos))
    ; delete excisting TAGS file
    (if (file-exists-p tag-file)
        (progn
          (message "deleting file: %s" tag-file)
          (delete-file tag-file)))
    ; iterate over repos
    (dolist (rep repos)
      (let ((cmd "find")
            (exclutions (cdr rep))
            (repo (car rep))
            (ctags-switches vminor-ctags-verilog-def)
            (extentions vminor-file-extention)
            (first t))
        (setq cmd (concat cmd " " repo))
        ; iterate over paths in the repo to ignore
        (dolist (elem exclutions cmd)
          (if (null first)
              (setq cmd (concat cmd " -or")))
          (setq cmd (concat cmd " -path \"*" elem "*\" -prune"))
          (setq first nil))
        ; iterate over file extentions to search in
        (dolist (elem extentions cmd)
          (if (null first)
              (setq cmd (concat cmd " -or")))
          (setq cmd (concat cmd " -name \"*" elem "\""))
          (setq first nil))
        ; add ctags command
        (let ((etags-run (car (directory-files (invocation-directory) t ".*etags"))))
             (setq cmd (concat cmd " | xargs " etags-run " -a"))) ; a hack for now
        (dolist (elem ctags-switches cmd)
          (setq cmd (concat cmd " " elem)))
        (setq cmd (concat cmd " -o " tag-file))
        (shell-command cmd)))))

(defadvice xref-find-definitions (around refresh-etags activate)
   "Rerun etags and reload tags if tag not found and redo find-tag.
   If buffer is modified, ask about save before running etags."
   (condition-case err
       ad-do-it
     (error (and (buffer-modified-p)
                 (not (ding))
                 (y-or-n-p "Buffer is modified, save it? ")
                 (save-buffer))
            (vminor-regen-tags)
            ad-do-it)))

; copied from https://www.emacswiki.org/emacs/HippieExpand
; this need to be updated to allow for dollar sign($) or back tick in front of the word
(defun he-tag-beg ()
  "find the start of the start of the system verilog expression. this only looks at words for now(so I lied in the first sentece)"
  (let ((p
         (save-excursion
           (backward-word 1)
           (if (memq (preceding-char) '(?$ ?`))
               (backward-char))
           (point))))
    p))

(defun check-for-tags-table ()
  "check if the tags are loaded and if not check if it can be regenerated"
  ;This needs update to check if vc-root fails
    (if (null (get-buffer vminor-tag-file-name))
        (cond
         ((not (null vminor-path-to-repos))
          (vminor-regen-tags)
          (visit-tags-table (concat vminor-tag-path vminor-tag-file-name))
          t)
         ((not (null vminor-use-vc-root-for-tags))
          (add-to-list 'vminor-path-to-repos (cons (vc-root-dir) nil))
          (vminor-regen-tags)
          (visit-tags-table (concat vminor-tag-path vminor-tag-file-name))
          t)
         (t nil))
      t))

; copied from https://www.emacswiki.org/emacs/HippieExpand
(defun tags-complete-tag (string predicate what)
  "find compleations from tags table"
  (save-excursion
    (if (check-for-tags-table)
      (if (eq what t)
          (all-completions string (tags-completion-table) predicate)
        (try-completion string (tags-completion-table) predicate))
      nil)))

(defun try-expand-tag (old)
  "try to find compleations in tags"
  (find-expand-tag old 'tags-complete-tag))

(defun try-expand-sv (old)
  "try to find compleation from the system verilog spec"
  (find-expand-tag old vminor-sv-key-words))

(defun try-expand-common-uvm (old)
  "try to find compleation from the most commenly used UVM structures"
  (find-expand-tag old vminor-common-uvm))

(defun find-expand-tag (old what)
  "find tags from a function or a list this is made to fit in make-hippie-expand-function"
  (unless  old
    (he-init-string (he-tag-beg) (point))
    (setq tags-he-expand-list (sort
                          (all-completions he-search-string what) 'string-lessp)))
  (while (and tags-he-expand-list
              (he-string-member (car tags-he-expand-list) he-tried-table))
    (setq tags-he-expand-list (cdr tags-he-expand-list)))
  (if (null tags-he-expand-list)
      (progn
        (when old (he-reset-string))
        ())
    (he-substitute-string (car tags-he-expand-list))
    (setq tags-he-expand-list (cdr tags-he-expand-list))
    t))

; compose the compleation order
(defalias 'vminor-expand-abbrev (make-hippie-expand-function
                                 '(try-expand-dabbrev
                                   try-expand-sv
                                   try-expand-common-uvm
                                   try-expand-dabbrev-visible
                                   try-expand-tag)))

(defun vminor-verilog-tab ()
  "extend the verilog mode tab so that if the verilog-mode tab has no affect and we are at the end of a word we use the vminor-expand-abbrev function"
  (interactive)
  (message "my version")
  (let ((boi-point
           (save-excursion
             (back-to-indentation)
             (point))))
    (electric-verilog-tab)
    (if (and
         (save-excursion
          (back-to-indentation)
          (= (point) boi-point))
         (looking-at "\\>"))
        (vminor-expand-abbrev nil)
      (setq he-num -1))))

(require 'verilog-mode)
(define-minor-mode verilog-minor-mode
  "set up verilog minor mode"
  :lighter " vmin"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map "\t" 'vminor-verilog-tab)
            (define-key map (kbd "C-c a") 'hs-toggle-hiding)
            map)
  (add-to-list 'tags-table-list
               (concat vminor-tag-path vminor-tag-file-name))
  (add-hook 'verilog-mode-hook 'hs-minor-mode)
  (add-to-list 'hs-special-modes-alist (list 'verilog-mode (list verilog-beg-block-re-ordered 0) "\\<end\\>" nil 'verilog-forward-sexp-function))
  (flyspell-prog-mode))

(provide 'verilog-minor-mode)
