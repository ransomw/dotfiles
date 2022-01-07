lo-fi email client setup
====

*Status Fall 2020*

Gmail fails to authenticate IMAP with
`mbsync` (and `sylpheed` for control).

Currently accessing email via Chrome only, considering
[fastmail](http://www.fastmail.com) account.

local storage
----

since i often want to view my email on my computer without an
internet connection, i typically run a program to backup messages
to local storage (my laptop's harddrive).

previously, i'd used
[getmail](http://pyropus.ca/software/getmail/)
but i'm currently trying out `mbsync`, and it appears
pretty great:  one of the linux kernel maintainers
has contributed to it, so it likely works very well
(in terms of "perf" or speed) with the linux filesystem.
using this program, i hope to think about my emails more
[simply](https://en.wikipedia.org/wiki/Directed_graph)
and
[beautifully](https://en.wikipedia.org/wiki/Algebraic_graph_theory),
because (and not to be
[snarky](https://en.wikipedia.org/wiki/Snark_(graph_theory))),
mail will always be there.

anyways, i setup a folder to store the mail in,
linked `mbsyncrc` in this repository to `~/.mbsyncrc`,
and initialized the backup

```shell
mkdir -p ~/mail/mbsync
ln -s /absolute/path/to/this/repo/mbsync ~/.mbsyncrc
mbsync init
```

the first run took a while, and now i can run
`mbsync get` to quickly backup my email.

**SSL certificate**

it's possible to use an SSL certificate
(specified by the `CertificateFile` line in `mbsyncrc`)
shipped with one's system
in order to establish a secure connection with an remote imap server.
however, in case the handshake between the imap server (gmail in this
case) and `mbsync` fails with a system certificate, it could also be
possible to grab a certificate directly from the server.

this did, in fact, turn out to be the case at one time.
so i ran
```shell
shutils$ ./get_gmail_cert.sh > ~/.gmail.crt
```
_once_ and made the corresponding update to `mbsyncrc`
to start using a valid certificate.

mutt
----

once emails are stored locally,
i use [(neo)?mutt](https://www.neomutt.org) to make sense of them.
on my system, the `muttrc` file in this directory is linked to
`~/.mutt/muttrc`.

note in particular that a few [newline-delimited]
address and regular-expression lists
are expected to exist in `~/.config/email/`.

**deleting**

i find the group feature helpful for deleting
emails that i don't want.
after opening `mutt` from the shell,
i type `l` (for "limit") and enter the
_limit string_ `%L nope` at the prompt
(`nope` is defined in `muttrc`).
after reviewing the list of messages to be deleted,
pressing `D` opens a prompt with the current limit string,
and by pressing return, one may confirm deletion of the messages.
when mutt is exited with `q`, i confirm the "purge deleted messages".

note that the `mbsync` configuration does not propegate these
deletions back to the server, but the mail will remain deleted locally.
