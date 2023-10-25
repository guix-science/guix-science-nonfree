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

Contributing
------------

We accept software fulfilling the following criteria:

- *Not* `free and open source`_. Use guix-science_ otherwise.
- Related to scientific research or teaching.

.. _Free and open source: https://opensource.org/osd
.. _guix-science: https://github.com/guix-science/guix-science

