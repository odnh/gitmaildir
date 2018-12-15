# gitmaildir

A git overlay for maildir, providing a strongly consistent index and allowing easy implementation of extra functionality.

## The idea

Maildir is a format for storing emails that uses the locking of filesystems so that the mail clients themselves do not have to worry about concurrency problems. However this comes at the cost of not being able to maintain a strongly consistent index of the mail store. gitmaildir seeks to address this problem by providing a git overlay on top of the maildir. Clients can then be written to interact with the executable managing the directory. However, clients can also continue to interact with the maildir as normal with the use of a daemon process translating between the representations in the background. More details can be found in my dissertation here: [odnh/gitmaildir-diss](https://github.com/odnh/gitmaildir-diss)

## Usage

Will be updated as the project comes together.
