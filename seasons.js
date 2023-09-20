const season_divs = document.querySelectorAll('[data-season]');

function parse_date(date) {
    const [monthStr, dayStr] = date.split(' ');
    const months = [
        "Jan", "Feb", "Mar", "Apr",
        "May", "Jun", "Jul", "Aug",
        "Sep", "Oct", "Nov", "Dec",
    ];
    const month = months.indexOf(monthStr);

    if (month === -1 || isNaN(dayStr)) {
        console.error('Invalid date format');
        return null;
    }

    const day = parseInt(dayStr);

    const year = new Date().getFullYear();
    return new Date(year, month, day);
}

season_divs.forEach((div) => {
    // Parse season range string
    const range = div.getAttribute('data-season').split(' - ');
    if (range.length !== 2) {
        console.error('Invalid season date range format');
        return;
    }

    // Get current date
    const date = new Date();
    const year = date.getFullYear();

    // Parse dates
    const start = parse_date(range[0].trim());
    const end = parse_date(range[1].trim());
    if (end < start) {
        end.setFullYear(year + 1);
    }

    // Check if in season
    const inSeason = start <= date && date <= end;
    if (!inSeason) {
        div.style.display = 'none';
    }
})
