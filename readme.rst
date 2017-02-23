.. _readme:

===============================
 Elasticsearch Rolling Restart
===============================


  **NOTE:** You should read the Caveats_ section first, before proceeding with
  the rest of the document

.. contents:: **Contents Table**
    :depth: 3

This is a simple tool to perform a rolling restart on the given Elasticsearch
cluster. A rolling restart operation allows a whole Elasticsearch cluster to be
restarted keeping it online and operational with no downtime for end users, by
taking nodes offline one a time.

About Rolling Restarts
======================

A full Elasticsearch cluster restart may be required in various cases. Some of
the most common cases are upgrading the Elasticsearch version of the cluster, or
performing maintenance tasks to the Elasticsearch servers itself (such as
hardware, or OS related tasks). Also, for Elasticsearch setups that are based on
custom plugins to enhance their core functionality, such as analyzers, custom
scripts, etc, a full cluster restart may be required even for a single plugin
update.

Performing such tasks often, and for many different different clusters can
become quite a pain in the administrator's day-to-day operations. The current
tool aims to automate the task of performing a rolling cluster restart, by
implementing the common elasticsearch `Rolling Restart`_ algorithm.

.. _Rolling Restart:
    https://www.elastic.co/guide/en/elasticsearch/guide/master/_rolling_restarts.html

About the tool
==============

As described in the Caveats_ section, this is a testing tool, that should be
used with care. It will perform some actions on the given Elasticsearch nodes
that includes stopping the elasticsearch process on all nodes, once at a time,
using the Elasticsearch `Shutdown API`. Then the process will be re-started
using the ``systemctl start <service>`` command, executed remotely on the host
via SSH.

To perform the above actions, some sort of requirements must be met; otherwise
the tool will fail to run and the cluster may be left in inconsistent state.
All those requirements are listed in the Requirements_ section, below.

Looking at the `Getting Started`_ section, we see that the master node is given
separately from the rest cluster nodes. This is done in order to trigger a
single master re-elect operation per ElasticSearch cluster restart.

.. _Shutdown API:
    https://www.elastic.co/guide/en/elasticsearch/reference/1.5/cluster-nodes-shutdown.html

Requirements
------------

The following list of requirements must apply in order to use the current tool:

- ``SSH`` access to *all* nodes of the Elasticsearch cluster to be restarted
- ``sudo`` access on *all* nodes too, for running systemd related commands

Getting Started
===============

To see details about the tool usage, run ``rolling-restart -h/--help``

::

  $ rolling-restart --help

  Usage: rolling-restart (-H|--host HOSTNAME) [-p|--port PORT] [-s|--service SVC]

    ElasticSearch cluster rolling restart tool.

  Available options:
    -h,--help             Show this help text
    -H,--host HOSTNAME    The hostname or IP of an Elasticsearch node
    -p,--port PORT        The port of the Elasticsearch host (default: 9200)
    -s,--service SVC      The ES service name (default: elasticsearch.service)

Usage
-----

- In order to perform a rolling-restart operation you should run:

::

  $ rolling-restart --host es_node1.example.com:9200 --port 9200 \
        --service elasticsearch.service

  or simply,

  $ rolling-restart -H es_node1.example.com -p 9200 -s elasticsearch.service

  or even simpler,

  $ rolling-restart -H es_node1.example.com

Caveats
=======

- This tool is more a reason to write some Haskell code, rather than a tool to
  actually depend-on for restarting you cluster; use it with **extra** care.
- In case the tool fails or is interrupted while it is running, shard
  allocation may be remain disabled. In that case you should re-enable it
  manually.
- The Elasticsearch service name *must* be identical to all given nodes.

.. vim: set textwidth=79 :
.. Local Variables:
.. mode: rst
.. fill-column: 79
.. End:

