# detect_platform

    Code
      detect_platform()
    Output
      $os
      [1] "darwin20"
      
      $arch
      [1] "aarch64"
      
      $rver
      [1] "4.3"
      

---

    Code
      detect_platform()
    Output
      $os
      [1] "linux"
      
      $arch
      [1] "x86_64"
      
      $rver
      [1] "4.3"
      

# pak_repo

    Code
      pak_repo("devel")
    Output
      [1] "https://r-lib.github.io/p/pak/devel/"
    Code
      pak_repo("rc")
    Output
      [1] "https://r-lib.github.io/p/pak/rc/"
    Code
      pak_repo("stable")
    Output
      [1] "https://r-lib.github.io/p/pak/stable/"

# pak_repo_metadata

    Code
      pak_repo_metadata(meta_path)
    Output
         Package    Version                   Depends      Imports License
      1      pak 0.7.1.9000 R (>= 4.1), R (<= 4.1.99) tools, utils   GPL-3
      2      pak 0.7.1.9000 R (>= 4.2), R (<= 4.2.99) tools, utils   GPL-3
      3      pak 0.7.1.9000 R (>= 4.0), R (<= 4.0.99) tools, utils   GPL-3
      4      pak 0.7.1.9000 R (>= 4.1), R (<= 4.1.99) tools, utils   GPL-3
      5      pak 0.7.1.9000 R (>= 4.2), R (<= 10.0.0) tools, utils   GPL-3
      6      pak 0.7.1.9000 R (>= 3.5), R (<= 3.5.99) tools, utils   GPL-3
      7      pak 0.7.1.9000 R (>= 3.6), R (<= 10.0.0) tools, utils   GPL-3
      8      pak 0.7.1.9000 R (>= 3.5), R (<= 3.5.99) tools, utils   GPL-3
      9      pak 0.7.1.9000 R (>= 3.6), R (<= 3.6.99) tools, utils   GPL-3
      10     pak 0.7.1.9000 R (>= 4.0), R (<= 4.0.99) tools, utils   GPL-3
      11     pak 0.7.1.9000 R (>= 4.1), R (<= 4.1.99) tools, utils   GPL-3
      12     pak 0.7.1.9000 R (>= 4.2), R (<= 4.2.99) tools, utils   GPL-3
      13     pak 0.7.1.9000 R (>= 3.5), R (<= 3.5.99) tools, utils   GPL-3
      14     pak 0.7.1.9000 R (>= 3.6), R (<= 3.6.99) tools, utils   GPL-3
      15     pak 0.7.1.9000 R (>= 4.0), R (<= 4.0.99) tools, utils   GPL-3
      16     pak 0.7.1.9000 R (>= 4.1), R (<= 4.1.99) tools, utils   GPL-3
      17     pak 0.7.1.9000 R (>= 4.2), R (<= 4.2.99) tools, utils   GPL-3
      18     pak 0.7.1.9000 R (>= 3.6), R (<= 3.6.99) tools, utils   GPL-3
      19     pak 0.7.1.9000 R (>= 4.2), R (<= 4.2.99) tools, utils   GPL-3
      20     pak 0.7.1.9000 R (>= 4.0), R (<= 4.0.99) tools, utils   GPL-3
      21     pak 0.7.1.9000 R (>= 4.1), R (<= 4.1.99) tools, utils   GPL-3
      22     pak 0.7.1.9000 R (>= 3.5), R (<= 3.5.99) tools, utils   GPL-3
      23     pak 0.7.1.9000 R (>= 4.3), R (<= 4.3.99) tools, utils   GPL-3
      24     pak 0.7.1.9000 R (>= 4.3), R (<= 4.3.99) tools, utils   GPL-3
      25     pak 0.7.1.9000 R (>= 4.3), R (<= 4.3.99) tools, utils   GPL-3
      26     pak 0.7.1.9000 R (>= 4.3), R (<= 4.3.99) tools, utils   GPL-3
      27     pak 0.7.1.9000 R (>= 4.4), R (<= 10.0.0) tools, utils   GPL-3
      28     pak 0.7.1.9000 R (>= 4.4), R (<= 10.0.0) tools, utils   GPL-3
      29     pak 0.7.1.9000 R (>= 4.4), R (<= 10.0.0) tools, utils   GPL-3
      30     pak 0.7.1.9000 R (>= 4.4), R (<= 10.0.0) tools, utils   GPL-3
      31     pak 0.7.1.9000 R (>= 4.3), R (<= 4.3.99) tools, utils   GPL-3
      32     pak 0.7.1.9000 R (>= 4.4), R (<= 10.0.0) tools, utils   GPL-3
                                   MD5sum
      1  d6be0de2e85aa8342c757a888115adc9
      2  77a9de8a5bd3815c95cab2979003c5a8
      3  328e252deffd659aec6e43064a2e2bf2
      4  787a119f7993533006d76a33dbd1dfcf
      5  7abf05aa4c5357ddd040acbe726f5acb
      6  b80dfe3b102a3237710d879ed3d44940
      7  a86277023b0022c60e08b7e94c74428f
      8  6c6bbc82ef3af1b89ec48901e0f9b63f
      9  712df4dedd91ad73f142cab1c85f3dae
      10 770fdac818ca34c5c96f72cadbf19663
      11 a56088c5c5bc08d849fd5fc9dc94094f
      12 e191788534de3151a8f188c04ebd5693
      13 02fc35953e31c457c819153711c28e5e
      14 20b7fcd3fef5a55b24068d064a4cd17a
      15 eb447fc72541669ee093ee05ba7d6b36
      16 d34be60769b8fdbee82ebb05dc009c38
      17 5fdfd38a5f46f61defea709749595fcb
      18 2208b9b4b800a5f0d44efa1de205bf59
      19 9a88655ad28ec6529c11c416fd21c915
      20 6d3be1c70d7088901a54e0dddb708e9c
      21 06cdd31a21ff0c2335bc883af50bc5c0
      22 5850a1f5e38cab8a7db9773f2de4e11a
      23 1444df2f169ff3c7af48d7bd3a529704
      24 0b3a03b26e40dbc89403f0f8892775e7
      25 6f96832d6476a0db54bdedf0443ebaa9
      26 0fccc61c0d0b12bd2c5564e51670bfae
      27 cb096e6d657d7bfc4088822ee3982f22
      28 706e755e6e1501b90e06a8b8c5a60d11
      29 83aef98253311517a0511377a6fd6626
      30 4c2bc6b4c490f087e02e2b404f3e74b9
      31 28a94a92b84a1d21c8221a2c8c050006
      32 a006f1cc31c50cae59cbe5608a389654
                                                                   Sha256
      1  85207a14088d3994a2630ed16fb83beab551a95442bf2a0ed66c5dafe1bdfb97
      2  25efeb747b7993a3819d67528ba5f1a8290fbc24427a38fa52d24b1cfb6a3c74
      3  0cba9e8b54ceccdc42d08cc2e40b911b663cf03049eba020f5e792589ccd2a49
      4  fb09d65e9e54cad4e41ace6facbf5faae91b6f6a737877541e212bd1cda63a0d
      5  611305de3c47ae6bd7c15cfe3496c818d7b0ab782eed51eef92d390712a4fa0e
      6  8658957a1c533ad283410a8a31ce02acbf9158affe683ee07ba184bc65859e3e
      7  ffabce7c4ee411e0edb54914828a7d395aa7b54cb53371cb3b6c7c9b0586692a
      8  bc92fe327c62c6dd8ac230beacbc6e3ed6539250f3a8b2fef948d7eb46e2aa06
      9  8abda8c649b75dae0315a82ba3c0e1a795a0976ed75aa66883e4d7c3e47e0bae
      10 9b10dbea740c713b62832f4356e5a16d34617a70bfe843d1ec89c9a304ac1bf1
      11 d24f1bf6a3ae0703168106985dd87ab178192659cbc8da02f01996f9c1e37cb2
      12 3cd642c60cb2bf9151227b1304052f6ab5adc9de5e162bc9a74dc78793267427
      13 3b22ed3c2c3720d54b98a1e0e2099e787aa72d75ba1ac0e5f8f757cbeaf02bcd
      14 9a8dadc27c79a36517733992695e4b4f81bf3bb9426ac32008879b3a4fd3b92b
      15 c9055eccf9dbcc1e24a80e16471cfa0a10768af6f5683523f4995967ebef9dc9
      16 97dbb9c973408dc0fd845ae12b5f18f99c98b45e3f1cbb5ccf24311fa4a2f39b
      17 fba55bdabaa50e659923cf6350061cd23ffae46d936fcb079bdcef7dadc756ad
      18 c3b99af557c8419dcef121c397f5b2c1f6b1f4bf63194342ccd0f4e7d1eeaf85
      19 6dd8c002c066c8793f734eabc7ffb1b7fd8b12bbe9b96315398379223bb51110
      20 b64bd7690c1e979f9ac45c2dbd6425f20566040b03728f220283a77bd54a4421
      21 af45e6a5c251bd90a26549bd7019c086a0d1378bdcefebaf29ad109203042b5e
      22 c2658bf61db4c17452008560b4ae1b531057b6ca6c4e8e4134f59b27c9ad62b6
      23 92b641a9be98723f784f1f61c4a9c0dd06676ee352b7d3a12c226617702f9d18
      24 b89e0d3bc8306611eff5898558a87542d4901aaa8bc356462be725d4e1ed0dc4
      25 587c0563f9f7438ccabba61395d80d441629deedb4905376e08e148bb7b40077
      26 793ad956e592a2912c72218621654c03ed471d7383a013eb84294194fd643c70
      27 1854f28e0933d30937cbba4cbb82c646a4e764a44d6ff3c5baa9bea58d31dd1d
      28 4f8a6c5a99b76844c056b1748c1f878145d32deff9c2c6a42463ff5480ef9d4b
      29 5df7a4803661d3b6d1f184161f6c92d81f6c14861edd3c6868b3430fa996cd6f
      30 9914a7a37fa38ac35170763ae20b16d6f50d2f302cd73ad71ad803b75de1e3d1
      31 11aa764ad48c3fd6c1c65eac139ff0a621d1330a7fd0c0fc13b5d95396a126c6
      32 b11077403d6aab4c7bb6ed12981d11a11b530cffa5c05df757f273b7185d4bdc
         NeedsCompilation
      1                no
      2                no
      3                no
      4                no
      5                no
      6                no
      7                no
      8                no
      9                no
      10               no
      11               no
      12               no
      13               no
      14               no
      15               no
      16               no
      17               no
      18               no
      19               no
      20               no
      21               no
      22               no
      23               no
      24               no
      25               no
      26               no
      27               no
      28               no
      29               no
      30               no
      31               no
      32               no
                                                                      Built
      1      R 4.1.3; aarch64-apple-darwin20; 2024-01-26 05:37:06 UTC; unix
      2      R 4.2.3; aarch64-apple-darwin20; 2024-01-26 05:39:11 UTC; unix
      3     R 4.0.5; x86_64-apple-darwin17.0; 2024-01-26 05:40:36 UTC; unix
      4     R 4.1.3; x86_64-apple-darwin17.0; 2024-01-26 05:41:58 UTC; unix
      5     R 4.2.3; x86_64-apple-darwin17.0; 2024-01-26 05:43:21 UTC; unix
      6   R 3.5.3; x86_64-apple-darwin15.6.0; 2024-01-26 05:37:45 UTC; unix
      7   R 3.6.3; x86_64-apple-darwin15.6.0; 2024-01-26 05:39:06 UTC; unix
      8       R 3.5.3; x86_64-w64-mingw32; 2024-01-12 05:40:54 UTC; windows
      9       R 3.6.3; x86_64-w64-mingw32; 2024-01-26 05:37:15 UTC; windows
      10      R 4.0.5; x86_64-w64-mingw32; 2024-01-26 05:37:18 UTC; windows
      11      R 4.1.3; x86_64-w64-mingw32; 2024-01-26 05:41:13 UTC; windows
      12      R 4.2.3; x86_64-w64-mingw32; 2024-01-26 05:40:15 UTC; windows
      13        R 3.5.3; x86_64-pc-linux-gnu; 2023-12-30 05:34:53 UTC; unix
      14       R 3.6.3; x86_64-pc-linux-musl; 2024-01-26 05:34:35 UTC; unix
      15       R 4.0.5; x86_64-pc-linux-musl; 2024-01-26 05:34:36 UTC; unix
      16       R 4.1.3; x86_64-pc-linux-musl; 2024-01-26 05:34:36 UTC; unix
      17       R 4.2.3; x86_64-pc-linux-musl; 2024-01-26 05:34:41 UTC; unix
      18 R 3.6.3; aarch64-unknown-linux-musl; 2024-01-26 05:36:19 UTC; unix
      19 R 4.2.3; aarch64-unknown-linux-musl; 2024-01-26 05:36:41 UTC; unix
      20 R 4.0.5; aarch64-unknown-linux-musl; 2024-01-26 05:36:43 UTC; unix
      21 R 4.1.3; aarch64-unknown-linux-musl; 2024-01-26 05:36:42 UTC; unix
      22  R 3.5.3; aarch64-unknown-linux-gnu; 2023-12-30 05:36:34 UTC; unix
      23     R 4.3.2; aarch64-apple-darwin20; 2024-01-26 05:41:13 UTC; unix
      24 R 4.3.2; aarch64-unknown-linux-musl; 2024-01-26 05:36:55 UTC; unix
      25      R 4.3.2; x86_64-w64-mingw32; 2024-01-26 05:38:17 UTC; windows
      26       R 4.3.2; x86_64-pc-linux-musl; 2024-01-26 05:34:38 UTC; unix
      27       R 4.4.0; x86_64-pc-linux-musl; 2024-01-26 05:34:36 UTC; unix
      28      R 4.4.0; x86_64-w64-mingw32; 2024-01-26 05:39:41 UTC; windows
      29     R 4.4.0; aarch64-apple-darwin20; 2024-01-26 05:45:14 UTC; unix
      30 R 4.4.0; aarch64-unknown-linux-musl; 2024-01-26 05:36:40 UTC; unix
      31      R 4.3.2; x86_64-apple-darwin20; 2024-01-26 05:44:42 UTC; unix
      32      R 4.4.0; x86_64-apple-darwin20; 2024-01-26 05:46:03 UTC; unix
                                                 File
      1     pak_0.7.1.9000_R-4-1_aarch64-darwin20.tgz
      2     pak_0.7.1.9000_R-4-2_aarch64-darwin20.tgz
      3    pak_0.7.1.9000_R-4-0_x86_64-darwin17.0.tgz
      4    pak_0.7.1.9000_R-4-1_x86_64-darwin17.0.tgz
      5    pak_0.7.1.9000_R-4-2_x86_64-darwin17.0.tgz
      6  pak_0.7.1.9000_R-3-5_x86_64-darwin15.6.0.tgz
      7  pak_0.7.1.9000_R-3-6_x86_64-darwin15.6.0.tgz
      8       pak_0.7.1.9000_R-3-5_x86_64-mingw32.zip
      9       pak_0.7.1.9000_R-3-6_x86_64-mingw32.zip
      10      pak_0.7.1.9000_R-4-0_x86_64-mingw32.zip
      11      pak_0.7.1.9000_R-4-1_x86_64-mingw32.zip
      12      pak_0.7.1.9000_R-4-2_x86_64-mingw32.zip
      13     pak_0.7.1.9000_R-3-5_x86_64-linux.tar.gz
      14     pak_0.7.1.9000_R-3-6_x86_64-linux.tar.gz
      15     pak_0.7.1.9000_R-4-0_x86_64-linux.tar.gz
      16     pak_0.7.1.9000_R-4-1_x86_64-linux.tar.gz
      17     pak_0.7.1.9000_R-4-2_x86_64-linux.tar.gz
      18    pak_0.7.1.9000_R-3-6_aarch64-linux.tar.gz
      19    pak_0.7.1.9000_R-4-2_aarch64-linux.tar.gz
      20    pak_0.7.1.9000_R-4-0_aarch64-linux.tar.gz
      21    pak_0.7.1.9000_R-4-1_aarch64-linux.tar.gz
      22    pak_0.7.1.9000_R-3-5_aarch64-linux.tar.gz
      23    pak_0.7.1.9000_R-4-3_aarch64-darwin20.tgz
      24    pak_0.7.1.9000_R-4-3_aarch64-linux.tar.gz
      25      pak_0.7.1.9000_R-4-3_x86_64-mingw32.zip
      26     pak_0.7.1.9000_R-4-3_x86_64-linux.tar.gz
      27     pak_0.7.1.9000_R-4-4_x86_64-linux.tar.gz
      28      pak_0.7.1.9000_R-4-4_x86_64-mingw32.zip
      29    pak_0.7.1.9000_R-4-4_aarch64-darwin20.tgz
      30    pak_0.7.1.9000_R-4-4_aarch64-linux.tar.gz
      31     pak_0.7.1.9000_R-4-3_x86_64-darwin20.tgz
      32     pak_0.7.1.9000_R-4-4_x86_64-darwin20.tgz
                                                                                                        DownloadURL
      1  https://ghcr.io/v2/r-lib/pak/blobs/sha256:85207a14088d3994a2630ed16fb83beab551a95442bf2a0ed66c5dafe1bdfb97
      2  https://ghcr.io/v2/r-lib/pak/blobs/sha256:25efeb747b7993a3819d67528ba5f1a8290fbc24427a38fa52d24b1cfb6a3c74
      3  https://ghcr.io/v2/r-lib/pak/blobs/sha256:0cba9e8b54ceccdc42d08cc2e40b911b663cf03049eba020f5e792589ccd2a49
      4  https://ghcr.io/v2/r-lib/pak/blobs/sha256:fb09d65e9e54cad4e41ace6facbf5faae91b6f6a737877541e212bd1cda63a0d
      5  https://ghcr.io/v2/r-lib/pak/blobs/sha256:611305de3c47ae6bd7c15cfe3496c818d7b0ab782eed51eef92d390712a4fa0e
      6  https://ghcr.io/v2/r-lib/pak/blobs/sha256:8658957a1c533ad283410a8a31ce02acbf9158affe683ee07ba184bc65859e3e
      7  https://ghcr.io/v2/r-lib/pak/blobs/sha256:ffabce7c4ee411e0edb54914828a7d395aa7b54cb53371cb3b6c7c9b0586692a
      8  https://ghcr.io/v2/r-lib/pak/blobs/sha256:bc92fe327c62c6dd8ac230beacbc6e3ed6539250f3a8b2fef948d7eb46e2aa06
      9  https://ghcr.io/v2/r-lib/pak/blobs/sha256:8abda8c649b75dae0315a82ba3c0e1a795a0976ed75aa66883e4d7c3e47e0bae
      10 https://ghcr.io/v2/r-lib/pak/blobs/sha256:9b10dbea740c713b62832f4356e5a16d34617a70bfe843d1ec89c9a304ac1bf1
      11 https://ghcr.io/v2/r-lib/pak/blobs/sha256:d24f1bf6a3ae0703168106985dd87ab178192659cbc8da02f01996f9c1e37cb2
      12 https://ghcr.io/v2/r-lib/pak/blobs/sha256:3cd642c60cb2bf9151227b1304052f6ab5adc9de5e162bc9a74dc78793267427
      13 https://ghcr.io/v2/r-lib/pak/blobs/sha256:3b22ed3c2c3720d54b98a1e0e2099e787aa72d75ba1ac0e5f8f757cbeaf02bcd
      14 https://ghcr.io/v2/r-lib/pak/blobs/sha256:9a8dadc27c79a36517733992695e4b4f81bf3bb9426ac32008879b3a4fd3b92b
      15 https://ghcr.io/v2/r-lib/pak/blobs/sha256:c9055eccf9dbcc1e24a80e16471cfa0a10768af6f5683523f4995967ebef9dc9
      16 https://ghcr.io/v2/r-lib/pak/blobs/sha256:97dbb9c973408dc0fd845ae12b5f18f99c98b45e3f1cbb5ccf24311fa4a2f39b
      17 https://ghcr.io/v2/r-lib/pak/blobs/sha256:fba55bdabaa50e659923cf6350061cd23ffae46d936fcb079bdcef7dadc756ad
      18 https://ghcr.io/v2/r-lib/pak/blobs/sha256:c3b99af557c8419dcef121c397f5b2c1f6b1f4bf63194342ccd0f4e7d1eeaf85
      19 https://ghcr.io/v2/r-lib/pak/blobs/sha256:6dd8c002c066c8793f734eabc7ffb1b7fd8b12bbe9b96315398379223bb51110
      20 https://ghcr.io/v2/r-lib/pak/blobs/sha256:b64bd7690c1e979f9ac45c2dbd6425f20566040b03728f220283a77bd54a4421
      21 https://ghcr.io/v2/r-lib/pak/blobs/sha256:af45e6a5c251bd90a26549bd7019c086a0d1378bdcefebaf29ad109203042b5e
      22 https://ghcr.io/v2/r-lib/pak/blobs/sha256:c2658bf61db4c17452008560b4ae1b531057b6ca6c4e8e4134f59b27c9ad62b6
      23 https://ghcr.io/v2/r-lib/pak/blobs/sha256:92b641a9be98723f784f1f61c4a9c0dd06676ee352b7d3a12c226617702f9d18
      24 https://ghcr.io/v2/r-lib/pak/blobs/sha256:b89e0d3bc8306611eff5898558a87542d4901aaa8bc356462be725d4e1ed0dc4
      25 https://ghcr.io/v2/r-lib/pak/blobs/sha256:587c0563f9f7438ccabba61395d80d441629deedb4905376e08e148bb7b40077
      26 https://ghcr.io/v2/r-lib/pak/blobs/sha256:793ad956e592a2912c72218621654c03ed471d7383a013eb84294194fd643c70
      27 https://ghcr.io/v2/r-lib/pak/blobs/sha256:1854f28e0933d30937cbba4cbb82c646a4e764a44d6ff3c5baa9bea58d31dd1d
      28 https://ghcr.io/v2/r-lib/pak/blobs/sha256:4f8a6c5a99b76844c056b1748c1f878145d32deff9c2c6a42463ff5480ef9d4b
      29 https://ghcr.io/v2/r-lib/pak/blobs/sha256:5df7a4803661d3b6d1f184161f6c92d81f6c14861edd3c6868b3430fa996cd6f
      30 https://ghcr.io/v2/r-lib/pak/blobs/sha256:9914a7a37fa38ac35170763ae20b16d6f50d2f302cd73ad71ad803b75de1e3d1
      31 https://ghcr.io/v2/r-lib/pak/blobs/sha256:11aa764ad48c3fd6c1c65eac139ff0a621d1330a7fd0c0fc13b5d95396a126c6
      32 https://ghcr.io/v2/r-lib/pak/blobs/sha256:b11077403d6aab4c7bb6ed12981d11a11b530cffa5c05df757f273b7185d4bdc
                   OS    Arch RVersion
      1      darwin20 aarch64      4.1
      2      darwin20 aarch64      4.2
      3    darwin17.0  x86_64      4.0
      4    darwin17.0  x86_64      4.1
      5    darwin17.0  x86_64      4.2
      6  darwin15.6.0  x86_64      3.5
      7  darwin15.6.0  x86_64      3.6
      8       mingw32  x86_64      3.5
      9       mingw32  x86_64      3.6
      10      mingw32  x86_64      4.0
      11      mingw32  x86_64      4.1
      12      mingw32  x86_64      4.2
      13        linux  x86_64      3.5
      14        linux  x86_64      3.6
      15        linux  x86_64      4.0
      16        linux  x86_64      4.1
      17        linux  x86_64      4.2
      18        linux aarch64      3.6
      19        linux aarch64      4.2
      20        linux aarch64      4.0
      21        linux aarch64      4.1
      22        linux aarch64      3.5
      23     darwin20 aarch64      4.3
      24        linux aarch64      4.3
      25      mingw32  x86_64      4.3
      26        linux  x86_64      4.3
      27        linux  x86_64      4.4
      28      mingw32  x86_64      4.4
      29     darwin20 aarch64      4.4
      30        linux aarch64      4.4
      31     darwin20  x86_64      4.3
      32     darwin20  x86_64      4.4

