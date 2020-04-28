==============================================
QCRMS: Quality Control Report for Mass Spectrometry (QCRMS)-based Metabolomics experiments.
==============================================

|Git| |Build Status (Travis)| |License| |Coverage|

An **R package** to assess the data quality of Liquid Chromatography - Mass Spectrometry (LC-MS) and Direct Infusion Mass Spectrometry (DIMS)-based metabolomics experiments. QCRMS generates a report that containing a comprehensive set of quality control metrics and charts.

------------
Install
------------

Github, R 3.6.x
------------

.. code-block:: r

  install.packages("remotes")
  Sys.setenv(R_REMOTES_NO_ERRORS_FROM_WARNINGS="true")
  remotes::install_github('computational-metabolomics/pmp', ref="R_3.6.x")
  remotes::install_github('computational-metabolomics/qcrms')


Github, R 4.x.x
------------

.. code-block:: r

  install.packages("remotes")
  Sys.setenv(R_REMOTES_NO_ERRORS_FROM_WARNINGS="true")
  
  if (!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager")

  BiocManager::install("pmp")

  remotes::install_github('computational-metabolomics/qcrms')

------------
References
------------


.. |Build Status (Travis)| image:: https://travis-ci.com/computational-metabolomics/qcrms.svg?branch=master
    :target: https://travis-ci.com/computational-metabolomics/qcrms

.. |Git| image:: https://img.shields.io/badge/repository-GitHub-blue.svg?style=flat&maxAge=3600
   :target: https://github.com/computational-metabolomics/qcrms

.. |License| image:: https://img.shields.io/badge/licence-GNU_v3-teal.svg?style=flat&maxAge=3600
   :target: https://www.gnu.org/licenses/gpl-3.0.html

.. |Coverage| image:: https://codecov.io/gh/computational-metabolomics/qcrms/branch/master/graph/badge.svg
   :target: https://codecov.io/github/computational-metabolomics/qcrms?branch=master
