-- layout codes
-- G : Girl
-- x : Ghost
-- l : Lighted tile

-- uppercase for lighted (visible) and lower for hidden
-- c : Candle
-- s : Small candle
-- t : Teddy
-- d : Door
-- k : Key
-- q : Skeleton
-- m : Match

levels = {
    Teddy = {
        num = 1,
        layout = {
            "oooooooo",
            "G t    T",
            "oooooooo",
        },
        message = "Find the TeddyBears!",
        help = "Use the arrows to move",
        candle_decline = 0,
    },
    {
        num = 2,
        layout = {
            " oTooo   ooTo  ",
            " olot  o oolo  ",
            " oloooolllolo  ",
            "ooloooolololooo",
            "o llll lll lll ",
            "o oooooooooooo ",
            "        G      ",
        },
        message = "LoL",
        candle_decline = 0,
    },
    {
        num = 3,
        layout = {
            "o  tooo      o ",
            "  oo    o      ",
            "     oooo  ooot",
            "  oooo G    o o",
            "oo     oooo    ",
            "t oo ooo     o ",
            "          o oot",
        },
        message = "Oh I'm lost.",
        candle_decline = 0,
    },
    Candle = {
        num = 4,
        layout = {
            "oto",
            "s o",
            "olo",
            "o o",
            "C G",
            "o o",
            "olo",
            "o o",
            "oto",
        },
        message = "Switch candles with Space before they burn out.",
        help = "If they do burn out, just walk over a lighted tile",
    },

    poop = {
        num = 5,
        layout = {
            "c             c",
            "o oooooooo oooo",
            "otoG   oo  oo c",
            "too  o  o     o",
            " ooooo l   oooo",
            "     o oco     ",
            "ooo  ooooo o oo",
            "c          o  c",
        },
        message = "I need candles..",
    },
}

