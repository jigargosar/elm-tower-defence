module Sequential exposing
    ( Generator
    , Seed
    , andThen
    , int
    , map
    , map2
    )


type Seed
    = Seed Int


type Generator a
    = Generator (Seed -> ( a, Seed ))


int : Generator Int
int =
    Generator (\(Seed seed) -> ( seed, Seed (seed + 1) ))


map : (a -> b) -> Generator a -> Generator b
map func (Generator genA) =
    Generator
        (\seed0 ->
            let
                ( a, seed1 ) =
                    genA seed0
            in
            ( func a, seed1 )
        )


map2 : (a -> b -> c) -> Generator a -> Generator b -> Generator c
map2 func (Generator genA) (Generator genB) =
    Generator
        (\seed0 ->
            let
                ( a, seed1 ) =
                    genA seed0

                ( b, seed2 ) =
                    genB seed1
            in
            ( func a b, seed2 )
        )


andThen : (a -> Generator b) -> Generator a -> Generator b
andThen callback (Generator genA) =
    Generator
        (\seed ->
            let
                ( result, newSeed ) =
                    genA seed

                (Generator genB) =
                    callback result
            in
            genB newSeed
        )
