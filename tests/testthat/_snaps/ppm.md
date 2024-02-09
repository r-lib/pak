# ppm_platforms

    Code
      as.data.frame(ppm_platforms())
    Output
                         name      os  binary_url distribution release binaries
      1               centos7   linux     centos7       centos       7     TRUE
      2               centos8   linux     centos8       centos       8     TRUE
      3                 rhel9   linux       rhel9   rockylinux       9     TRUE
      4            opensuse15   linux  opensuse15     opensuse      15     TRUE
      5           opensuse152   linux opensuse152     opensuse    15.2     TRUE
      6           opensuse153   linux opensuse153     opensuse    15.3     TRUE
      7           opensuse154   linux opensuse154     opensuse    15.4     TRUE
      8            opensuse42   linux  opensuse42     opensuse    42.3     TRUE
      9                 rhel7   linux     centos7       redhat       7     TRUE
      10                rhel8   linux     centos8       redhat       8     TRUE
      11 rhel9 (unused alias)   linux       rhel9       redhat       9     TRUE
      12               sles12   linux  opensuse42          sle    12.3     TRUE
      13               sles15   linux  opensuse15          sle      15     TRUE
      14              sles152   linux opensuse152          sle    15.2     TRUE
      15              sles153   linux opensuse153          sle    15.3     TRUE
      16              sles154   linux opensuse154          sle    15.4     TRUE
      17               xenial   linux      xenial       ubuntu   16.04     TRUE
      18               bionic   linux      bionic       ubuntu   18.04     TRUE
      19                focal   linux       focal       ubuntu   20.04     TRUE
      20                jammy   linux       jammy       ubuntu   22.04     TRUE
      21               buster   linux      buster       debian      10    FALSE
      22             bullseye   linux    bullseye       debian      11    FALSE
      23              windows windows        <NA>      windows     all     TRUE
      24                macOS   macOS        <NA>        macOS     all    FALSE

# ppm_r_versions

    Code
      as.data.frame(ppm_r_versions())
    Output
        r_version
      1       3.5
      2       3.6
      3       4.0
      4       4.1
      5       4.2

# ppm_snapshots

    Code
      ppm_snapshots()
    Output
      # A data frame: 10 x 2
         date                     id
         <dttm>                <int>
       1 2021-01-25 00:00:00  997643
       2 2021-01-26 00:00:00 1014755
       3 2021-01-27 00:00:00 1033374
       4 2021-01-28 00:00:00 1053473
       5 2021-01-29 00:00:00 1069075
       6 2021-02-01 00:00:00 1123445
       7 2021-02-02 00:00:00 1140568
       8 2021-02-03 00:00:00 1160641
       9 2021-02-04 00:00:00 1175516
      10 2021-02-05 00:00:00 1194160

