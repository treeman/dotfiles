onemangas = {
    "http://www.onemanga.com/One_Piece/",
    "http://www.onemanga.com/Hajime_no_Ippo/",
    "http://www.onemanga.com/Naruto/",
}

thousandmangas = {
    "http://www.1000manga.com/Historys_Strongest_Disciple_Kenichi/",
}

mangastreams = {
    "One Piece",
    "Naruto",
}

mangafoxs = {
    "http://www.mangafox.com/manga/history_s_strongest_disciple_kenichi/",
}

function get_manga_info()
    details = {}

    for k, site in pairs(onemangas) do
        big_site = conky_parse("${curl "..site.."}")
        if big_site ~= "" then
            table.insert(details, extract_onemanga_details(big_site))
        end
    end

    for k, site in pairs(thousandmangas) do
        big_site = conky_parse("${curl "..site.."}")
        if big_site ~= "" then
            table.insert(details, extract_thousandmanga_details(big_site))
        end
    end

    mangastream = conky_parse("${curl http://mangastream.com/manga}")
    if mangastream ~= "" then
        for k, manga in pairs(mangastreams) do
            table.insert(details, extract_mangastream_details(mangastream, manga))
        end
    end

    --for k, site in pairs(mangafoxs) do
        --big_site = conky_parse("${curl "..site.."}")
        --if big_site ~= "" then
            --table.insert(details, extract_mangafox_details(big_site))
        --end
    --end

    table.sort(details, function (a,b) return a.time > b.time end)

    checked_mangas = {}
    infos = {}
    for k, info in pairs(details) do
        if not checked_mangas[info.manga] then
            i = {}
            i.time = info.time
            i.str = format_manga_details(info)
            table.insert(infos, i)
            checked_mangas[info.manga] = true
        end
    end
    return infos
end

function extract_onemanga_details(site)
    latest = string.match(site, "<tr class=\"bg01\">(.-)</tr>")

    d = {}
    d.domain = "onemanga"
    d.manga = string.match(site, "<h1>%s*(.-)%s*Manga.-</h1>")
    link, d.chapter = string.match(latest, "<a href=\"(.-)\">.-(%d-)</a>")
    d.link = "http://www." .. d.domain .. link
    d.scans = string.match(latest, "<td class=\"ch%-scans%-by\">(.-)</td>")

    date_str = string.match(latest, "<td class=\"ch%-date\">(.-)</td>")
    date = {}
    date.month, date.day, date.year = string.match(date_str, "(...).-(%d+).-(%d%d%d%d)")
    date.month = rev_months[date.month]
    d.time = os.time(date)

    return d
end

function extract_thousandmanga_details(site)
    latest = string.match(site, "<tr class=\"bg01\">(.-)</tr>")

    d = {}
    d.domain = "1000manga"
    link, d.manga, d.chapter = string.match(latest, "<a href=\"(.-)\">(.-)%s(%d-)</a>")
    d.link = "http://www." .. d.domain .. link
    d.scans = string.match(latest, "<td class=\"ch%-scans%-by\">(.-)</td>")

    date_str = string.match(latest, "<td class=\"ch%-date\">(.-)</td>")
    date = {}
    date.month, date.day, date.year = string.match(date_str, "(...).-(%d+).-(%d%d%d%d)")
    date.month = rev_months[date.month]
    d.time = os.time(date)

    return d
end

function extract_mangastream_details(site, manga)
    section = string.match(site, "<th style=\"width:80%%\">"..manga.."</th>(.-)<th style=\"width:80%%\">")
    latest = string.match(section, "<tr>(.-)</tr>")
    d = {}
    d.domain = "mangastream"
    d.manga = manga
    link, d.chapter = string.match(latest, "<a href=\"(.-)\">%s*(%d+).-</a>")
    d.link = "http://www.".. d.domain .. link
    d.scans = ""

    date_str = string.match(latest, "<em>(.-)</em>")
    today = string.match(date_str, "Today")

    date = {}
    if today == nil then
        date.month, date.day, date.year = string.match(date_str, "(...).-(%d+).-(%d%d%d%d)")
        date.month = rev_months[date.month]
    else
        date = os.date("*t")
        date.sec = ""
        date.hour = ""
        date.min = ""
    end
    d.time = os.time(date)

    return d
end

function extract_mangafox_details(site)
    print(site)
    d.manga = string.match(site, "<h2>(.-)</h2>")
    print(d.manga)
    latest = string.match(site, "th.-(.-)th")
    print(latest)
    d = {}
    d.domain = "mangafox"
    link, d.chapter, d.chapter = string.match(latest, "<a href=\"(.-)\">Ch%.(%d+): (.-)</a>")
    d.link = "http://www." .. d.domain .. link
    d.scans = ""

    date = os.date("*t")
    date.sec = ""
    date.hour = ""
    date.min = ""
    d.time = os.time(date)

    return d
end

function format_manga_details(details)
    return details.manga.." "..details.chapter.." - "..details.domain
end
