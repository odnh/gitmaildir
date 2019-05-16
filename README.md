# gitmaildir

A git overlay for maildir, providing a strongly consistent index and allowing easy implementation of extra functionality.

## The idea

Maildir is a format for storing emails that uses the locking of filesystems so that the mail clients themselves do not have to worry about concurrency problems. However this comes at the cost of not being able to maintain a strongly consistent index of the mail store. gitmaildir seeks to address this problem by providing a git overlay on top of the maildir. Clients can then be written to interact with the executable managing the directory. However, clients can also continue to interact with the maildir as normal with the use of a daemon process translating between the representations in the background. More details can be found in my dissertation here: [odnh/gitmaildir-diss](https://github.com/odnh/gitmaildir-diss)

## Structure

There are three main parts:
- `gitmaildir`: A library for interacting with the maildir
- `gitmaildir-cli`: A command line tool to interact with the maildir
- `gitmaildir-daemon`: A daemon process to allow standard maildir clients to interact with a git maildir
- `gitmaildir-unix`: Extra modules to provide backedn implementations for running on unix systems (ie locking)

## Building

### Dependencies:

This requires you to already have a working opam installation. If you do not, then consult the [manual](https://opam.ocaml.org/doc/Install.html) for how to install it.
At the time of writing you need to be using OCaml 4.06.1 so that it is old enough to use `email_message` and new enough to use the current verison of `dune`.

- For gitmaildir library: `core, lwt, cmdliner, git`
- To run on unix: `flock, git-unix`
- To use plugins: `email_message, lambdasoup`, the `plain_branch` plugin also requires the Python package `http2text`
- Can be installed using opam: `opam install core lwt cmdliner git-unix email_message lambdasoup flock`

### Build and run:

- Build: `dune build`
- Install: `dune install <package>`
- Run: `gitmaildir_cli <options>`

## Evaluation

The folder `evaluation` contains the code used to evaluate the performance of gitmaildir in comparison to maildir and mbox. Descriptions of its contents are contained within.
