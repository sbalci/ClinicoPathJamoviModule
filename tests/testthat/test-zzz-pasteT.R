# .pasteT() must behave exactly like the paste0() call it replaces (tools/i18n_wrap_templates.py).
test_that(".pasteT reproduces paste0 for scalars, vectors, NULL and numbers, and follows the template order", {
  ns <- intersect(c("ClinicoPath", "jsurvival", "meddecide", "OncoPath", "jjstatsplot", "ClinicoPathDescriptives"),
                  loadedNamespaces())
  ns <- Filter(function(p) exists(".pasteT", envir = asNamespace(p), inherits = FALSE), ns)
  skip_if(length(ns) == 0, ".pasteT not in a loaded namespace")
  pasteT <- get(".pasteT", envir = asNamespace(ns[1]))
  med <- 12.5; n <- c(3L, 40L); nothing <- NULL
  expect_identical(pasteT("The median was {med} months.", med = med), paste0("The median was ", med, " months."))
  expect_identical(pasteT("{n} patients", n = n), paste0(n, " patients"))
  expect_identical(pasteT("n = {n} ({med})", n = n, med = med), paste0("n = ", n, " (", med, ")"))
  expect_identical(pasteT("Shown{extra}.", extra = nothing), paste0("Shown", nothing, "."))
  expect_identical(pasteT("Shown{extra}.", extra = if (FALSE) " twice"), "Shown.")
  expect_identical(pasteT("{a}{b}", a = 0.1 + 0.2, b = 100000), paste0(0.1 + 0.2, 100000))
  expect_identical(pasteT("{x}", x = character(0)), paste0(character(0)))
  expect_identical(pasteT("no placeholders here"), "no placeholders here")
  expect_identical(pasteT("{b} sonra {a}", a = "ilk", b = "ikinci"), "ikinci sonra ilk")   # a translation may reorder
  expect_identical(pasteT("{a} and {a}", a = "x"), "x and x")
  expect_identical(pasteT("cost: {v} $1 \\1 {literal braces}", v = "a{b}"), "cost: a{b} $1 \\1 {literal braces}")
})
