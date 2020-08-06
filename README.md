This is not a well tested project

Verilog Minor mode is an small add on to verilog-mode to get:


1. Automatic generation of TAGS
* you can set vminor-path-to-repos to '(path_to_repo . (list of ignor paths in the repo or nil))
  you can set several paths
* you can leave it as is and if it is in a version control that emacs understand it will find the root of the repo and generate the tags form there
* you can set vminor-use-vc-root-for-tags to nil if you dont van it

2. get TAG compleation.
It first uses the normal verilog tag function, if that does not does not change anything and you are at the end of a word it will
try to complate the word with hippie-expand. it first checks the buffer you are in for compleation, then a list of common sv
commands then common uvm classes/functions then all visible buffers then it checks if there are any compleation in teh tags.

3. it adds hide-show with more than just begin..end. and binds the hs-toggle to C-c a.
