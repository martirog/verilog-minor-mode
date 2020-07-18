(defvar vminor-ctags-verilog-def
  '("--langdef=systemverilog"
    "--langmap=systemverilog:.v.vg.sv.svh.tv.vinc"
    "--regex-systemverilog=\"/^\\s*(\\b(static|local|virtual|protected)\\b)*\\s*\\bclass\\b\\s*(\\b\\w+\\b)/\\3/c,class/\""
    "--regex-systemverilog=\"/^\\s*(\\b(static|local|virtual|protected)\\b)*\\s*\\btask\\b\\s*(\\b(static|automatic)\\b)?\\s*(\\w+::)?\\s*(\\b\\w+\\b)/\\6/t,task/\""
    "--regex-systemverilog=\"/^\\s*(\\b(static|local|virtual|protected)\\b)*\\s*\\bfunction\\b\\s*(\\b(\\w+)\\b)?\\s*(\\w+::)?\\s*(\\b\\w+\\b)/\\6/f,function/\""
    "--regex-systemverilog=\"/^\\s*\\bmodule\\b\\s*(\\b\\w+\\b)/\\1/m,module/\""
    "--regex-systemverilog=\"/^\\s*\\bpackage\\b\\s*(\\b\\w+\\b)/\\1/P,package/\""
    "--regex-systemverilog=\"/^\\s*\\bprogram\\b\\s*(\\b\\w+\\b)/\\1/p,program/\""
    "--regex-systemverilog=\"/^\\s*\\binterface\\b\\s*(\\b\\w+\\b)/\\1/i,interface/\""
    "--regex-systemverilog=\"/^\\s*\\btypedef\\b\\s+.*\\s+(\\b\\w+\\b)\\s*;/\\1/e,typedef/\""
    "--regex-systemverilog=\"/^\\s*`define\\b\\s*(\\w+)/`\\1/d,define/\""
    "--regex-systemverilog=\"/}\\s*(\\b\\w+\\b)\\s*;/\\1/e,typedef/\""
    "--regex-systemverilog=\"/^\\s*(\\b(static|local|private|rand)\\b)*\\s*(\\b(shortint|int|longint)\\b)\\s*(\\bunsigned\\b)?(\\s*\\[.+\\])*\\s*(\\b\\w+\\b)/\\7/v,variable/\""
    "--regex-systemverilog=\"/^\\s*(\\b(static|local|private|rand)\\b)*\\s*(\\b(byte|bit|logic|reg|integer|time)\\b)(\\s*\\[.+\\])*\\s*(\\b\\w+\\b)/\\6/v,variable/\""
    "--regex-systemverilog=\"/^\\s*(\\b(static|local|private)\\b)*\\s*(\\b(real|shortreal|chandle|string|event)\\b)(\\s*\\[.+\\])*\\s*(\\b\\w+\\b)/\\6/v,variable/\""
    "--regex-systemverilog=\"/(\\b(input|output|inout)\\b)?\\s*(\\[.+\\])*\\s*(\\b(wire|reg|logic)\\b)\\s*(\\[.+\\])*\\s*(#((.+)|\\S+)))?\\s*(\\b\\w+\\b)/\\9/v,variable/\""
    "--regex-systemverilog=\"/(\\b(parameter|localparam)\\b).+(\\b\\w+\\b)\\s*=/\\3/a,parameter/\""
    "--systemverilog-kinds=+ctfmpied")
  "define how ctags should find system verilog tags")

(defvar vminor-path-to-repos nil
  "list of cons: repos to search for systemverilog files . exclude list")

(defvar vminor-file-extention
  '("v" "sv" "vh" "svh")
  "file extentions to use in the search for files to tag")

(defvar vminor-tag-path "/home/martin/" ;nil
  "path to puth the TAGS file")

(defvar vminor-tag-file-name "vminor_TAGS"
  "name of the TAGS file")

(defun vminor-regen-tags()
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
        (setq cmd (concat cmd " | xargs ctags -a -e"))
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
(defun he-tag-beg ()
  (let ((p
         (save-excursion
           (backward-word 1)
           (point))))
    p))

(defun tags-complete-tag (string predicate what)
  (save-excursion
    (if (fboundp 'tags-completion-table)
     (if (eq what t)
         (all-completions string (tags-completion-table) predicate)
       (try-completion string (tags-completion-table) predicate))
     nil)))

(defun try-expand-tag (old)
  (unless  old
    (he-init-string (he-tag-beg) (point))
    (setq tags-he-expand-list (sort
                          (all-completions he-search-string 'tags-complete-tag) 'string-lessp)))
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


(defalias 'vminor-expand-abbrev (make-hippie-expand-function
                                 '(try-expand-dabbrev
                                   try-expand-dabbrev-visible
                                   try-expand-tag)))

(defun vminor-verilog-tab ()
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
        (vminor-expand-abbrev nil))))

(add-to-list 'tags-table-list
      (concat vminor-tag-path vminor-tag-file-name))

(define-minor-mode verilog-minor-mode
  "Get your foos in the right places."
  :lighter " vmin"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map "\t" 'vminor-verilog-tab)
            map))

(provide 'verilog-minor-mode)

  ;:keymap (let ((map (make-sparse-keymap)))
  ;          (define-key map (kbd "C-c f") 'insert-foo)
  ;          map))
