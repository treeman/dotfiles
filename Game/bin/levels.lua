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
            "G t   cT",
            "oooooooo",
        },
        message = "Find the TeddyBears!",
    },

    Candle = {
        num = 2,
        layout = {
            "oto",
            "C o",
            "o o",
            "o o",
            "C G",
            "o o",
            "o o",
            "o o",
            "oto",
        },
        message = "Switch candles with Space when they're burning out.",
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
        num = 0,
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

