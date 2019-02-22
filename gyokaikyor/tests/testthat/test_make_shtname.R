context("Make shtname by prefecture")

expect_equal(make_shtname(prefec = "kumamoto", spcs = "katakuchi"), "カタクチ")
expect_equal(make_shtname(prefec = "kumamoto", spcs = "urume"), "ウルメ")
expect_equal(make_shtname(prefec = "kumamoto", spcs = "maiwashi"), "マイワシ")
expect_equal(make_shtname(prefec = "kumamoto", spcs = "sabarui"), "サバ類")
