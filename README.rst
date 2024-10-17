Non-free Guix science packages
==============================

.. image:: https://guix.bordeaux.inria.fr/jobset/guix-science-nonfree/badge.svg?type=0
   :target: https://guix.bordeaux.inria.fr/jobset/guix-science-nonfree

This GNU Guix_ channel provides non-free scientific software.

See `Specifying Additional Channels`_ in the Guix manual for instructions how
to add it to your installation or simply add the following snippet to your
``channels.scm``:

.. code:: scheme

	(channel
	  (name 'guix-science-nonfree)
	  (url "https://github.com/guix-science/guix-science-nonfree.git")
	  (introduction
	   (make-channel-introduction
		"58661b110325fd5d9b40e6f0177cc486a615817e"
		(openpgp-fingerprint
		 "CA4F 8CF4 37D7 478F DA05  5FD4 4213 7701 1A37 8446"))))

.. _Guix: https://guix.gnu.org/
.. _Specifying Additional Channels: https://guix.gnu.org/manual/en/guix.html#Specifying-Additional-Channels

Binary substitutes for ``x86_64-linux`` are available from
https://guix.bordeaux.inria.fr, see `Getting Substitutes from Other
Servers`_ in the official manual. The signing key can be imported
using:

.. code:: console

	$ guix archive --authorize <<EOF
    (public-key
     (ecc
      (curve Ed25519)
      (q #89FBA276A976A8DE2A69774771A92C8C879E0F24614AAAAE23119608707B3F06#)))
	EOF

.. _Getting Substitutes from Other Servers: https://guix.gnu.org/manual/en/guix.html#Getting-Substitutes-from-Other-Servers

Contributing
------------

We accept software fulfilling the following criteria:

- *Not* `free and open source`_. Use guix-science_ otherwise.
- Related to scientific research or teaching.

.. _Free and open source: https://opensource.org/osd
.. _guix-science: https://codeberg.org/guix-science/guix-science

