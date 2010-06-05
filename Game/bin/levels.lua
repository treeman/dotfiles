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

    Candle = {
        num = 2,
        layout = {
            "oto",
            "s o",
            "o o",
            "o o",
            "C G",
            "o o",
            "omo",
            "o o",
            "oto",
        },
        message = "Switch candles with Space before they burn out.",
    },

    poop = {
        num = 4,
        layout = {
            "oGoo oo ooTo",
            "c oo  o    o",
            "o m   l     ",
            "C oo  o    o",
            "o oo  o s   ",
            "otootoo ooTo",
        },
        message = "yomgtomgtomg",
    },

    beginning = {
        num = 5,
        layout = {
            "okcocooq   t o T",
            "T m      ooo o o",
            "o mooodooo o o Q",
            "G so   o   o o o",
            "ooooK  ol       ",
            " C     oooo  o  ",
            "     S    x  o c",
        },
        message = "big level holy shit",
    },
}

