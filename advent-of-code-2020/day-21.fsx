#load "monke.fs"


let input = "day-21.input" |> Monke.IO.readLines


type Food =
    { Ingredients: Set<string>
      Allergens: Set<string> }


let parse =
    function
    | Monke.RegexMatch @"^(.+) \(contains (.+)\)$" [ ingredients; allergens ] ->
        { Ingredients =
              ingredients
              |> Monke.String.split [ " " ]
              |> Set.ofArray
          Allergens =
              allergens
              |> Monke.String.split [ ", " ]
              |> Set.ofArray }
    | s -> Monke.invalidInput s


let resolveAllergens inp =
    let uniqueAllergens =
        inp
        |> Seq.collect (fun food -> food.Allergens)
        |> Set.ofSeq

    let rec loop allergenToFoodMap remainingAllergens =
        if remainingAllergens |> Set.count = 0 then
            allergenToFoodMap
        else
            let identifiedIngredients =
                allergenToFoodMap
                |> Map.toSeq
                |> Seq.map (fun (_, v) -> v)
                |> Set.ofSeq

            let name, allergen =
                remainingAllergens
                |> Set.toSeq
                |> Seq.pick
                    (fun allergen ->
                        let ingredientsAppearingInAllRecipes =
                            inp
                            |> Seq.filter (fun food -> food.Allergens |> Set.contains allergen)
                            |> Seq.map
                                (fun food ->
                                    food.Ingredients
                                    |> Set.filter
                                        (fun ingredient ->
                                            identifiedIngredients
                                            |> Set.contains ingredient
                                            |> not))
                            |> Set.intersectMany

                        if Seq.length ingredientsAppearingInAllRecipes = 1
                        then Some(Seq.item 0 ingredientsAppearingInAllRecipes, allergen)
                        else None)

            loop (Map.add allergen name allergenToFoodMap) (Set.remove allergen remainingAllergens)

    loop Map.empty uniqueAllergens


let countPart1 inp =
    let allergenToIngredientMap = resolveAllergens inp

    let identifiedIngredients =
        allergenToIngredientMap
        |> Map.toSeq
        |> Seq.map (fun (_, v) -> v)
        |> Set.ofSeq

    inp
    |> Seq.sumBy
        (fun food ->
            Set.difference food.Ingredients identifiedIngredients
            |> Set.count)


input
|> Seq.map parse
|> countPart1
|> printfn "Part One: %d"


let part2 inp =
    resolveAllergens inp
    |> Map.toSeq
    |> Seq.sortBy fst
    |> Seq.map snd
    |> String.concat ","


input
|> Seq.map parse
|> part2
|> printfn "Part Two: %s"
