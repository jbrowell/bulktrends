make_tariff <- function(
  comcode,
  valid_from,
  valid_to,
  trackedmodel_ptr_id,
  sid,
  suffix = "80"
) {
  data.frame(
    comcode = comcode,
    valid_from = as.Date(valid_from),
    valid_to = as.Date(valid_to),
    trackedmodel_ptr_id = as.numeric(trackedmodel_ptr_id),
    sid = as.character(sid),
    suffix = suffix,
    stringsAsFactors = FALSE
  )
}

make_two_sid_fixture <- function(start1, end1, sid1, start2, end2, sid2) {
  make_tariff(
    comcode = c("0101299010", "0101299010"),
    valid_from = c(start1, start2),
    valid_to = c(end1, end2),
    trackedmodel_ptr_id = c(100, 200),
    sid = c(sid1, sid2)
  )
}
