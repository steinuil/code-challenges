import std/os/file
import std/os/path
import std/core/string
import std/core/list
import std/core/int
import std/core/maybe


fun is-safe-level(n1: int, n2: int) : total bool
  n2 > n1 && n1 + 3 >= n2


fun is-safe-ascending(ls: list<int>)
  match ls
    Nil -> True
    Cons(_, Nil) ->
      True
    Cons(n1, ls as Cons(n2, _)) ->
      if is-safe-level(n1, n2) then
        is-safe-ascending(ls)
      else
        False


fun is-tolerated-ascending2(ls: list<int>)
  match ls
    Nil ->
      True
    Cons(_, Nil) ->
      True
    Cons(n1, ls as Cons(n2, rest)) ->
      if is-safe-level(n1, n2) then
        is-tolerated-ascending2(ls)
      else
        is-safe-ascending(Cons(n1, rest))


fun is-tolerated-ascending(ls: list<int>)
  match ls
    Cons(n1, ls as Cons(n2, rest)) ->
      if is-safe-level(n1, n2) then
        is-tolerated-ascending2(ls)
      else
        is-safe-ascending(ls) || is-safe-ascending(Cons(n1, rest))


fun main()
  val lines-with-last = read-text-file(path("day_02.input")).list/lines
  val lines = list/take(lines-with-last, length(lines-with-last) - 1)

  val reports =
    lines.list/map fn(line)
      val report =
        sep/split(line, " ").list/map fn (num)
          parse-int(num).maybe/expect("invalid number: `" ++ num ++ "`")

      if maybe/head(report).maybe/expect("ls") < list/last(report).maybe/expect("ls") then
        report
      else
        reverse(report)

  val invalid-reports =
    reports.list/filter fn(report)
      not(is-safe-ascending(report))

  val valid-reports-count = length(reports) - length(invalid-reports)

  println("Part One: " ++ show(valid-reports-count))

  val valid-tolerated-reports =
    invalid-reports.filter(is-tolerated-ascending)

  val valid-tolerated-report-count =
    valid-reports-count + length(valid-tolerated-reports)

  println("Part Two: " ++ show(valid-tolerated-report-count))
