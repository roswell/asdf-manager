# asdf-manager

Download and manage ASDF versions.

# Overview

Really, that's all it does. It's meant to be used as part of a larger Common
Lisp environment manager, the other components being [ql-manager][ql] to manage
different Quicklisp environments, and [Roswell][ros] to manage implementations.

# Usage

First, you have to create a manager object. You need to pass a directory where
the ASDF source trees will be stored.

```lisp
(defvar *manager* (make-instance 'asdf-manager:manager
                                 :directory #p"/path/to/some/directory/"))
```

Then you can start downloading ASDF versions:

```lisp
(asdf-manager:download-extract-delete *manager* :3.1.6.6)
```

This will download the source archive for version 3.1.6.6, extract it to
`<directory>/sources/asdf-3.1.6.6`, and delete the archive.

Available versions are stored in the `asdf-manager:+available-versions+` vector.

# License

Copyright (c) 2016 Fernando Borretti

Licensed under the MIT License.

[ql]: https://github.com/roswell/ql-manager
[ros]: https://github.com/roswell/roswell
