---
layout: post
title: CBT interactive mode
---

I recently provide a working prototype of an [interactive mode](https://github.com/mchav/cbt/commit/e88d426e934544728a505e13f6c8c1b9288fe5ac "Interactive") for CBT.
Interactive mode provides a thin layer over the CBT shell which prefixes all commands not in your `PATH` variable with `cbt`. Without interactive mode you would have to do the following:

`$ cbt tools createMain`


`$ cbt tools createBasicBuild`


`$ cbt compile`


`$ cbt run`


Now, you can simplify this by entering a cbt context and having `cbt` implicitly prefixed to these commands.

`$ cbt interactive`

`cbt> tools createMain`

`cbt> tools createBasicBuild`

`cbt> compile`

`cbt> run`


### The implementation

Code:

    interactive=false

    if [ "$1" = "interactive" ]; then
      interactive=true
    else
      stage1 $*
    fi

    while [ "$interactive" = true ]; do
      while IFS="" read -r -e -d $'\n' -p 'cbt> ' options; do
        history -s "$options"
        if [ "$options" = "quit" ]; then
          interactive=false
          break
        else
          curr_command="$(echo $options | { read first rest ; echo $first ; })"
          which $curr_command 2>&1 > /dev/null
          option_available=$?
          if [ ! $option_available -eq 0 ]; then
            cbt "$options"
          else
            eval $options
          fi
          
        fi
      done
    done

The layer is just a while loop in a bash script. The code checks for the command in your path and if not available runs it on cbt.