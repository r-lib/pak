# lockfile_create, lockfile_install

    Code
      writeLines(readLines("deps.lock"))
    Output
      {
        "lockfile_version": 1,
        "os": "<os>",
        "r_version": "<r-version>",
        "platform": "<platform>",
        "packages": [
          {
            "ref": "deps::.",
            "package": "pkg4-deps",
            "version": "1.0.0",
            "type": "deps",
            "direct": true,
            "binary": false,
            "dependencies": ["pkg2"],
            "vignettes": false,
            "needscompilation": false,
            "metadata": {
              "RemotePkgRef": "deps::.",
              "RemoteType": "deps"
            },
            "sources": [],
            "target": "src/contrib/pkg4-deps_1.0.0.tar.gz",
            "platform": "<platform>",
            "rversion": "*",
            "directpkg": true,
            "dep_types": ["Depends", "Imports", "LinkingTo"],
            "params": [],
            "install_args": "",
            "sysreqs": ""
          },
          {
            "ref": "pkg1",
            "package": "pkg1",
            "version": "1.0.0",
            "type": "standard",
            "direct": false,
            "binary": false,
            "dependencies": [],
            "vignettes": false,
            "needscompilation": false,
            "metadata": {
              "RemoteType": "standard",
              "RemotePkgRef": "pkg1",
              "RemoteRef": "pkg1",
              "RemoteRepos": "http://127.0.0.1:<port>/",
              "RemotePkgPlatform": "source",
              "RemoteSha": "1.0.0"
            },
            "sources": ["http://127.0.0.1:<port>//src/contrib/pkg1_1.0.0.tar.gz", "http://127.0.0.1:<port>//src/contrib/Archive/pkg1/pkg1_1.0.0.tar.gz"],
            "target": "src/contrib/pkg1_1.0.0.tar.gz",
            "platform": "<platform>",
            "rversion": "*",
            "directpkg": false,
            "sha256": "<sha256>",
            "filesize": 9999,
            "dep_types": ["Depends", "Imports", "LinkingTo"],
            "params": [],
            "install_args": "",
            "repotype": "cran"
          },
          {
            "ref": "pkg2",
            "package": "pkg2",
            "version": "1.0.0",
            "type": "standard",
            "direct": false,
            "binary": false,
            "dependencies": ["pkg1"],
            "vignettes": false,
            "needscompilation": false,
            "metadata": {
              "RemoteType": "standard",
              "RemotePkgRef": "pkg2",
              "RemoteRef": "pkg2",
              "RemoteRepos": "http://127.0.0.1:<port>/",
              "RemotePkgPlatform": "source",
              "RemoteSha": "1.0.0"
            },
            "sources": ["http://127.0.0.1:<port>//src/contrib/pkg2_1.0.0.tar.gz", "http://127.0.0.1:<port>//src/contrib/Archive/pkg2/pkg2_1.0.0.tar.gz"],
            "target": "src/contrib/pkg2_1.0.0.tar.gz",
            "platform": "<platform>",
            "rversion": "*",
            "directpkg": false,
            "sha256": "<sha256>",
            "filesize": 9999,
            "dep_types": ["Depends", "Imports", "LinkingTo"],
            "params": [],
            "install_args": "",
            "repotype": "cran"
          }
        ]
      }

---

    Code
      writeLines(readLines("dev.lock"))
    Output
      {
        "lockfile_version": 1,
        "os": "<os>",
        "r_version": "<r-version>",
        "platform": "<platform>",
        "packages": [
          {
            "ref": "deps::.",
            "package": "pkg4-deps",
            "version": "1.0.0",
            "type": "deps",
            "direct": true,
            "binary": false,
            "dependencies": ["pkg2"],
            "vignettes": false,
            "needscompilation": false,
            "metadata": {
              "RemotePkgRef": "deps::.",
              "RemoteType": "deps"
            },
            "sources": [],
            "target": "src/contrib/pkg4-deps_1.0.0.tar.gz",
            "platform": "<platform>",
            "rversion": "*",
            "directpkg": true,
            "dep_types": ["Depends", "Imports", "LinkingTo", "Suggests"],
            "params": [],
            "install_args": "",
            "sysreqs": ""
          },
          {
            "ref": "pkg1",
            "package": "pkg1",
            "version": "1.0.0",
            "type": "standard",
            "direct": false,
            "binary": false,
            "dependencies": [],
            "vignettes": false,
            "needscompilation": false,
            "metadata": {
              "RemoteType": "standard",
              "RemotePkgRef": "pkg1",
              "RemoteRef": "pkg1",
              "RemoteRepos": "http://127.0.0.1:<port>/",
              "RemotePkgPlatform": "source",
              "RemoteSha": "1.0.0"
            },
            "sources": ["http://127.0.0.1:<port>//src/contrib/pkg1_1.0.0.tar.gz", "http://127.0.0.1:<port>//src/contrib/Archive/pkg1/pkg1_1.0.0.tar.gz"],
            "target": "src/contrib/pkg1_1.0.0.tar.gz",
            "platform": "<platform>",
            "rversion": "*",
            "directpkg": false,
            "sha256": "<sha256>",
            "filesize": 9999,
            "dep_types": ["Depends", "Imports", "LinkingTo"],
            "params": [],
            "install_args": "",
            "repotype": "cran"
          },
          {
            "ref": "pkg2",
            "package": "pkg2",
            "version": "1.0.0",
            "type": "standard",
            "direct": false,
            "binary": false,
            "dependencies": ["pkg1"],
            "vignettes": false,
            "needscompilation": false,
            "metadata": {
              "RemoteType": "standard",
              "RemotePkgRef": "pkg2",
              "RemoteRef": "pkg2",
              "RemoteRepos": "http://127.0.0.1:<port>/",
              "RemotePkgPlatform": "source",
              "RemoteSha": "1.0.0"
            },
            "sources": ["http://127.0.0.1:<port>//src/contrib/pkg2_1.0.0.tar.gz", "http://127.0.0.1:<port>//src/contrib/Archive/pkg2/pkg2_1.0.0.tar.gz"],
            "target": "src/contrib/pkg2_1.0.0.tar.gz",
            "platform": "<platform>",
            "rversion": "*",
            "directpkg": false,
            "sha256": "<sha256>",
            "filesize": 9999,
            "dep_types": ["Depends", "Imports", "LinkingTo"],
            "params": [],
            "install_args": "",
            "repotype": "cran"
          },
          {
            "ref": "pkg3",
            "package": "pkg3",
            "version": "1.0.0",
            "type": "standard",
            "direct": false,
            "binary": false,
            "dependencies": ["pkg2"],
            "vignettes": false,
            "needscompilation": false,
            "metadata": {
              "RemoteType": "standard",
              "RemotePkgRef": "pkg3",
              "RemoteRef": "pkg3",
              "RemoteRepos": "http://127.0.0.1:<port>/",
              "RemotePkgPlatform": "source",
              "RemoteSha": "1.0.0"
            },
            "sources": ["http://127.0.0.1:<port>//src/contrib/pkg3_1.0.0.tar.gz", "http://127.0.0.1:<port>//src/contrib/Archive/pkg3/pkg3_1.0.0.tar.gz"],
            "target": "src/contrib/pkg3_1.0.0.tar.gz",
            "platform": "<platform>",
            "rversion": "*",
            "directpkg": false,
            "sha256": "<sha256>",
            "filesize": 9999,
            "dep_types": ["Depends", "Imports", "LinkingTo"],
            "params": [],
            "install_args": "",
            "repotype": "cran"
          }
        ]
      }

