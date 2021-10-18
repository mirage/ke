(executable
 (name test)
 (libraries bigstringaf alcotest ke))

(rule
 (alias runtest)
 (deps
  (:exe test.exe))
 (action
  (run %{exe} --color=always)))
