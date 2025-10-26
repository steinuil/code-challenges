import std/os/path
import std/os/file


fun find-mul(str: string, sum: int)
  match find(str, "mul(")
    Nothing ->
      sum
    Just(slice) ->
      match sslice/find(string(slice.after()), ",")
        Nothing ->
          sum
        Just(comma-slice) ->
          match parse-int(string(comma-slice.before()))
            Nothing ->
              find-mul(string(slice.after()), sum)
            Just(n) ->
              match find(string(comma-slice.after()), ")")
                Nothing ->
                  sum
                Just(closing-slice) ->
                  match parse-int(string(closing-slice.before()))
                    Nothing ->
                      find-mul(string(slice.after()), sum)
                    Just(n2) ->
                      find-mul(string(closing-slice.after()), sum + (n * n2))


fun remove-disabled-sections(str: string)
  match find(str, "don't()")
    Nothing ->
      str

    Just(dont-slice) ->
      match find(string(dont-slice.after), "do()")
        Nothing ->
          string(dont-slice.before())

        Just(do-slice) ->
          string(dont-slice.before()) ++ remove-disabled-sections(string(do-slice.after()))


fun main()
  val input = read-text-file(path("day_03.input"))

  val mul-sum = find-mul(input, 0)

  println("Part One: " ++ show(mul-sum))

  val cleaned-input = remove-disabled-sections(input)

  val mul-sum2 = find-mul(cleaned-input, 0)

  println("Part Two: " ++ show(mul-sum2))
