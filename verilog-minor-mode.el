;; -*- lexical-binding: t -*-

(defvar vminor-ctags-verilog-def
  '("--language=none"
    "--regex=\"/^[ \\t]*\\(static\\|local\\|virtual\\|protected\\|interface\\)*[ \\t]*class[ \\t]*\\([0-9a-zA-Z\\$_]+\\)/\\2/\""
    "--regex=\"/^[ \\t]*\\(static\\|local\\|virtual\\|protected\\)*[ \\t]*task[ \\t]*\\(static\\|automatic\\)?[ \\t]*\\([0-9a-zA-Z\\$_]+::\\)*?\\([0-9a-zA-Z\\$_]+\\)?[ \\t]*[0-9a-zA-Z\\$_]+/\\4/\""
    "--regex=\"/^[ \\t]*\\(static\\|local\\|virtual\\|protected\\)*[ \\t]*function[ \\t]*\\([0-9a-zA-Z\\$_]+\\)?[ \\t]*\\([0-9a-zA-Z\\$_]+::\\)*?\\([0-9a-zA-Z\\$_]+\\)?[ \\t]*[0-9a-zA-Z\\$_]+/\\4/\""
    "--regex=\"/^[ \\t]*module[ \\t]*\\([0-9a-zA-Z\\$_]+\\)/\\1/\""
    "--regex=\"/^[ \\t]*package[ \\t]*\\([0-9a-zA-Z\\$_]+\\)/\\1/\""
    "--regex=\"/^[ \\t]*program[ \\t]*\\([0-9a-zA-Z\\$_]+\\)/\\1/\""
    "--regex=\"/^[ \\t]*interface[ \\t]*\\([0-9a-zA-Z\\$_]+\\)/\\1/\""
    "--regex=\"/^[ \\t]*typedef[ \\t]+.*[ \\t]+\\([0-9a-zA-Z\\$_]+\\)[ \\t]*;/\\1/\""
    "--regex=\"/^[ \\t]*\\`define[ \\t]*\\([0-9a-zA-Z\\$_]+\\)/\\`\\1/\""
    "--regex=\"/^.*}[ \\t]*\\([0-9a-zA-Z\\$_]+\\)[ \\t]*;/\\1/\""
    "--regex=\"/^.*)[ \\t]*\\([0-9a-zA-Z\\$_]+\\)[ \\t]*;/\\1/\""
    ;"--regex=\"/^[ \\t]*\\(static\\|local\\|private\\|rand\\)*[ \\t]*\\(shortint\\|int\\|longint\\)[ \\t]*unsigned?\\([ \\t]*[.+]\\)*[ \\t]*\\([0-9a-zA-Z\\$_]+\\)/\\4/\""
    ;"--regex=\"/^[ \\t]*\\(static\\|local\\|private\\|rand\\)*[ \\t]*\\(byte\\|bit\\|logic\\|reg\\|integer\\|time\\)\\([ \\t]*[.+]\\)*[ \\t]*\\([0-9a-zA-Z\\$_]+\\)/\\4/\""
    ;"--regex=\"/^[ \\t]*\\(static\\|local\\|private\\)*[ \\t]*\\(real\\|shortreal\\|chandle\\|string\\|event\\)\\([ \\t]*[.+]\\)*[ \\t]*\\([0-9a-zA-Z\\$_]+\\)/\\4/\""
    ;"--regex=\"/^[ \\t]*\\(input\\|output\\|inout\\)?[ \\t]*\\([.+]\\)*[ \\t]*\\(wire\\|reg\\|logic\\)[ \\t]*\\([.+]\\)*[ \\t]*\\(#\\(\\(.+\\)\\|[ \\t]+\\)\\)?[ \\t]*\\([0-9a-zA-Z\\$_]+\\)/\\8/\""
    "--regex=\"/^[ \\t]*\\(parameter\\|localparam\\).*[ \\t]+\\([0-9a-zA-Z\\$_]+\\)[ \\t]*=.*;/\\2/\"")
  "define how ctags should find system verilog tags")

(defvar vminor-path-to-repos nil
  "list of cons: repos to search for systemverilog files . exclude list")

(defvar vminor-file-extention
  '("v" "sv" "vh" "svh")
  "file extentions to use in the search for files to tag")

(defvar vminor-tag-path (concat (getenv "HOME") "/") ;nil
  "path to puth the TAGS file")

(defvar vminor-tag-file-post-fix "vminor_TAGS"
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
    "constraint"
    "push_back" "push_front" "pop_back" "pop_front"
    "$display" "$sformatf")
  "System verilog keywords and functions")

(defvar vminor-common-uvm
  '("uvm_object" "`uvm_object_utils"
    "uvm_component" "`uvm_component_utils")
  "System verilog keywords and functions")

(defun vminor--run-etags (repo exclutions ctags-switches extentions tag-file)
  "Generate the find/etags commandline and run it"
  (let ((cmd "find")
        (first t))
    (setq cmd (concat cmd " " repo))
                                        ; iterate over paths in the repo to ignore
    (let ((first_it t))
      (dolist (elem exclutions cmd)
        (if (null first)
            (setq cmd (concat cmd " -or")))
        (when first_it
          (setq cmd (concat cmd " \\("))
          (setq first_it nil))
        (setq cmd (concat cmd " -path \"*" elem "*\""))
        (setq first nil))
      (when (null first_it)
        (setq cmd (concat cmd " \\) -prune"))))
                                        ; iterate over file extentions to search in
    (let ((first_it t))
      (dolist (elem extentions cmd)
        (if (null first)
            (setq cmd (concat cmd " -or")))
        (when first_it
          (setq cmd (concat cmd " \\("))
          (setq first_it nil))
        (setq cmd (concat cmd " -name \"*\\." elem "\""))
        (setq first nil))
      (when (null first_it)
        (setq cmd (concat cmd " \\) -print"))))
                                        ; add ctags command
    (let ((etags-run (car (directory-files (invocation-directory) t ".*etags"))))
      (setq cmd (concat cmd " | xargs " etags-run " -a"))) ; a hack for now
    (dolist (elem ctags-switches cmd)
      (setq cmd (concat cmd " " elem)))
    (setq cmd (concat cmd " -o " tag-file))
    (shell-command cmd)))

(defun vminor--generate-tag-file-name (tag-repo)
  "generate the tag file name for a given path"
  (concat vminor-tag-path "/" (md5 tag-repo) vminor-tag-file-post-fix))

(defun vminor-regen-tags(regen_all)
  "regenerate the tags file using ctags. so you need to have ctags in your path for this to work"
  (interactive "p")
  (let ((repos vminor-path-to-repos))
    ; delete excisting TAGS file
    ; iterate over repos
    (dolist (rep repos)
      (let ((exclutions (car (cdr rep)))
            (static (cdr (cdr rep)))
            (repo (car rep))
            (ctags-switches vminor-ctags-verilog-def)
            (extentions vminor-file-extention)
            (tag-file (vminor--generate-tag-file-name (car rep))))
        (when (or (not static) regen_all)
          (if (file-exists-p tag-file)
              (progn
                (message "deleting file: %s" tag-file)
                (delete-file tag-file)))
          (vminor--run-etags repo exclutions ctags-switches extentions tag-file))))))

(defadvice xref-find-definitions (around refresh-etags activate)
   "Rerun etags and reload tags if tag not found and redo find-tag.
   If buffer is modified, ask about save before running etags."
   (condition-case err
       ad-do-it
     (error (and (buffer-modified-p)
                 (not (ding))
                 (y-or-n-p "Buffer is modified, save it? ")
                 (save-buffer))
            (vminor-regen-tags nil)
            ad-do-it)))

; copied from https://www.emacswiki.org/emacs/HippieExpand
; this need to be updated to allow for dollar sign($) or back tick in front of the word
(defun vminor--tag-beg ()
  "find the start of the start of the system verilog expression. this only looks at words for now(so I lied in the first sentece)"
  (let ((p
         (save-excursion
           (backward-word 1)
           (if (memq (preceding-char) '(?$ ?`))
               (backward-char))
           (point))))
    p))

(defun vminor--generate-tags-list (repo-list)
  (let ((res '()))
    (dolist (rep repo-list res)
      (let* ((exclutions (car (cdr rep)))
             (static (cdr (cdr rep)))
             (repo (car rep))
             (tag-name (vminor--generate-tag-file-name repo)))
        (add-to-list 'res tag-name)))))

(defun vminor--check-for-tags-table ()
  "check if the tags are loaded and if not check if it can be regenerated"
  ;This needs update to check if vc-root fails
    (if (null (get-buffer vminor-tag-file-name))
        (cond
         ((not (null vminor-path-to-repos))
          (vminor-regen-tags nil)
          (setq tags-table-list (vminor--generate-tags-list vminor-path-to-repos))
          t)
         ((not (null vminor-use-vc-root-for-tags))
          (add-to-list 'vminor-path-to-repos (cons (vc-root-dir) nil))
          (vminor-regen-tags nil)
          (setq tags-table-list (vminor--generate-tags-list vminor-path-to-repos))
          t)
         (t nil))
      t))

; copied from https://www.emacswiki.org/emacs/HippieExpand
(defun vminor--tags-complete-tag (string predicate what)
  "find compleations from tags table"
  (save-excursion
    (if (vminor--check-for-tags-table)
      (if (eq what t)
          (all-completions string (tags-completion-table) predicate)
        (try-completion string (tags-completion-table) predicate))
      nil)))

(defun vminor--try-expand-tag (old)
  "try to find compleations in tags"
  (vminor--find-expand-tag old 'vminor--tags-complete-tag))

(defun vminor--try-expand-sv (old)
  "try to find compleation from the system verilog spec"
  (vminor--find-expand-tag old vminor-sv-key-words))

(defun vminor--try-expand-common-uvm (old)
  "try to find compleation from the most commenly used UVM structures"
  (vminor--find-expand-tag old vminor-common-uvm))

(defun vminor--find-expand-tag (old what)
  "find tags from a function or a list this is made to fit in make-hippie-expand-function"
  (unless  old
    (he-init-string (vminor--tag-beg) (point))
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


(require 'icrib-buffer-and-tag-compleation nil t)
(if (featurep 'icrib-buffer-and-tag-compleation)
    (defun vminor--expand-abbrev (dummy)
      (let ((init-string (buffer-substring-no-properties (vminor--tag-beg) (point))))
        (icrib-buffer-and-tag-compleation init-string '("verilog-mode") nil vminor-sv-key-words)))

                                        ; compose the compleation order
  (defalias 'vminor--expand-abbrev (make-hippie-expand-function
                                   '(try-expand-dabbrev
                                     vminor--try-expand-sv
                                     vminor--try-expand-common-uvm
                                     try-expand-dabbrev-visible
                                     vminor--try-expand-tag))))



(defun vminor-verilog-tab ()
  "extend the verilog mode tab so that if the verilog-mode tab has no affect and we are at the end of a word we use the vminor--expand-abbrev function"
  (interactive)
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
        (vminor--expand-abbrev nil)
      (setq he-num -1))))

(require 'verilog-mode)
(define-minor-mode verilog-minor-mode
  "set up verilog minor mode"
  :lighter " vmin"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map "\t" 'vminor-verilog-tab)
            (define-key map (kbd "C-c a") 'hs-toggle-hiding)
            map)
  (setq tags-table-list
               (vminor--generate-tags-list vminor-path-to-repos))
  (add-hook 'verilog-mode-hook 'hs-minor-mode)
  (add-to-list 'hs-special-modes-alist (list 'verilog-mode (list verilog-beg-block-re-ordered 0) "\\<end\\>" nil 'verilog-forward-sexp-function))
  (flyspell-prog-mode))

(provide 'verilog-minor-mode)
