(in-package :wild-package-inferred-system/test)

(in-suite :wild-package-inferred-system-suite)

(test sysdef-wild-package-inferred-system-search
  (signals empty-wild-system (wpis::sysdef-wild-package-inferred-system-search "no/such/wild/*/system/exists")))
