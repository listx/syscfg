#!/bin/bash

set -o errexit
set -o nounset
set -o pipefail

get_first_monday() {
  local daynum
  local first_mon
  # Find the next monday.
  for day in $(seq 1 7); do
    daynum=$(date -d "$(date +%Y)-01-0$day" +%u)
    if (( daynum == 1 )); then
      first_mon=$(date -d "$(date +%Y)-01-0$day" +%F)
      break
    fi
  done
  printf "%s\n" "$first_mon"
}

get_week() {
  local weeks_future
  local date_cmd
  local first_fri
  local year
  local quarter
  local week
  local mon
  local fri
  first_mon=$1
  weeks_future=$2
  date_cmd=(date -d "$first_mon +$((weeks_future)) week")
  first_fri=$(date -d "$first_mon +4 days" +%F)

  year_quarter_week=$("${date_cmd[@]}" +"%Y-Q%q Week %V")
  mon=$(date -d "$first_mon +$weeks_future week" +%m/%d)
  fri=$(date -d "$first_fri +$weeks_future week" +%m/%d)
  printf "%s (%s -- %s)\n" \
    "${year_quarter_week}" \
    "${mon}" \
    "${fri}"
}

# Print timestamps for the whole year. Start with the first Monday of the year.
first_mon=$(get_first_monday)

for week in $(seq 0 52); do
  get_week "$first_mon" "$((week))"
done
