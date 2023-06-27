module ObjectTests

open Xunit

[<Fact>]
let ``Can add multiple things to environment`` () =
    let env = new Object.Environment()

    env.Set "Push" (new Object.Boolean(true))
    env.Set "Baby" (new Object.Boolean(true))
    env.Set "Yes" (new Object.Boolean(true))

    Assert.Equal(3, env.Count())