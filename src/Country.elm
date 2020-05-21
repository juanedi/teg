module Country exposing
    ( Country(..)
    , decoder
    , svgId
    )

import Json.Decode as Decode exposing (Decoder)


type Country
    = Alaska
    | Arabia
    | Aral
    | Argentina
    | Australia
    | Borneo
    | Brasil
    | California
    | Canada
    | Chile
    | China
    | Colombia
    | Egypt
    | Ethiopia
    | France
    | Germany
    | Gobi
    | GreatBritain
    | Greenland
    | Iceland
    | India
    | Iran
    | Israel
    | Italy
    | Japan
    | Java
    | Kamchatka
    | Labrador
    | Madagascar
    | Malasya
    | Mexico
    | Mongolia
    | NewYork
    | Oregon
    | Peru
    | Poland
    | Russia
    | Sahara
    | SouthAfrica
    | Spain
    | Sumatra
    | Sweden
    | Syberia
    | Tartary
    | Taymir
    | Terranova
    | Turkey
    | Uruguay
    | Yukon
    | Zaire


svgId : Country -> String
svgId country =
    case country of
        Alaska ->
            "alaska"

        Arabia ->
            "arabia"

        Aral ->
            "aral"

        Argentina ->
            "argentina"

        Australia ->
            "australia-2"

        Borneo ->
            "borneo"

        Brasil ->
            "brasil"

        California ->
            "california"

        Canada ->
            "canada"

        Chile ->
            "chile"

        China ->
            "china"

        Colombia ->
            "colombia"

        Egypt ->
            "egypt"

        Ethiopia ->
            "ethiopia"

        France ->
            "france"

        Germany ->
            "germany"

        Gobi ->
            "gobi"

        GreatBritain ->
            "great_britain"

        Greenland ->
            "greenland"

        Iceland ->
            "iceland"

        India ->
            "india"

        Iran ->
            "iran"

        Israel ->
            "israel"

        Italy ->
            "italy"

        Japan ->
            "japan"

        Java ->
            "java"

        Kamchatka ->
            "kamchatka"

        Labrador ->
            "labrador"

        Madagascar ->
            "madagascar"

        Malasya ->
            "malasya"

        Mexico ->
            "mexico"

        Mongolia ->
            "mongolia"

        NewYork ->
            "new_york"

        Oregon ->
            "oregon"

        Peru ->
            "peru"

        Poland ->
            "poland"

        Russia ->
            "russia"

        Sahara ->
            "sahara"

        SouthAfrica ->
            "south_africa"

        Spain ->
            "spain"

        Sumatra ->
            "sumatra"

        Sweden ->
            "sweden"

        Syberia ->
            "syberia"

        Tartary ->
            "tartary"

        Taymir ->
            "taymir"

        Terranova ->
            "terranova"

        Turkey ->
            "turkey"

        Uruguay ->
            "uruguay"

        Yukon ->
            "yukon"

        Zaire ->
            "zaire"


decoder : Decoder Country
decoder =
    Decode.string
        |> Decode.andThen
            (\identifier ->
                case identifier of
                    "alaska" ->
                        Decode.succeed Alaska

                    "yukon" ->
                        Decode.succeed Yukon

                    "canada" ->
                        Decode.succeed Canada

                    "terranova" ->
                        Decode.succeed Terranova

                    "labrador" ->
                        Decode.succeed Labrador

                    "new_york" ->
                        Decode.succeed NewYork

                    "oregon" ->
                        Decode.succeed Oregon

                    "california" ->
                        Decode.succeed California

                    "mexico" ->
                        Decode.succeed Mexico

                    "greenland" ->
                        Decode.succeed Greenland

                    "argentina" ->
                        Decode.succeed Argentina

                    "uruguay" ->
                        Decode.succeed Uruguay

                    "chile" ->
                        Decode.succeed Chile

                    "peru" ->
                        Decode.succeed Peru

                    "brasil" ->
                        Decode.succeed Brasil

                    "colombia" ->
                        Decode.succeed Colombia

                    "israel" ->
                        Decode.succeed Israel

                    "arabia" ->
                        Decode.succeed Arabia

                    "turkey" ->
                        Decode.succeed Turkey

                    "iran" ->
                        Decode.succeed Iran

                    "gobi" ->
                        Decode.succeed Gobi

                    "mongolia" ->
                        Decode.succeed Mongolia

                    "india" ->
                        Decode.succeed India

                    "malasya" ->
                        Decode.succeed Malasya

                    "china" ->
                        Decode.succeed China

                    "kamchatka" ->
                        Decode.succeed Kamchatka

                    "syberia" ->
                        Decode.succeed Syberia

                    "taymir" ->
                        Decode.succeed Taymir

                    "tartary" ->
                        Decode.succeed Tartary

                    "aral" ->
                        Decode.succeed Aral

                    "japan" ->
                        Decode.succeed Japan

                    "australia-2" ->
                        Decode.succeed Australia

                    "sumatra" ->
                        Decode.succeed Sumatra

                    "borneo" ->
                        Decode.succeed Borneo

                    "java" ->
                        Decode.succeed Java

                    "russia" ->
                        Decode.succeed Russia

                    "sweden" ->
                        Decode.succeed Sweden

                    "poland" ->
                        Decode.succeed Poland

                    "germany" ->
                        Decode.succeed Germany

                    "italy" ->
                        Decode.succeed Italy

                    "france" ->
                        Decode.succeed France

                    "spain" ->
                        Decode.succeed Spain

                    "great_britain" ->
                        Decode.succeed GreatBritain

                    "iceland" ->
                        Decode.succeed Iceland

                    "sahara" ->
                        Decode.succeed Sahara

                    "zaire" ->
                        Decode.succeed Zaire

                    "south_africa" ->
                        Decode.succeed SouthAfrica

                    "egypt" ->
                        Decode.succeed Egypt

                    "ethiopia" ->
                        Decode.succeed Ethiopia

                    "madagascar" ->
                        Decode.succeed Madagascar

                    _ ->
                        Decode.fail ("Unrecognized country identifier: " ++ identifier)
            )
