tag_summs = \(summ, slot) {
  mapply(
    \(x,y)cbind(x[[slot]],
      tag=paste0("w:",x$meta$obs_weight,"//p:",x$meta$prior[2,1]),
      seq=y),
    summ, seq_along(summ), SIMPLIFY = FALSE
  ) %>% bind_rows %>%
  mutate(sim_id = as.numeric(as.factor(paste(seq, sim_id))), .by = tag) %>%
  select(-seq)
}
merge_summs = \(summ) {
  stats = tag_summs(summ, "stats")
  bckdx = tag_summs(summ, "backend_diagnostics")
  list(stats=stats,bckdx=bckdx)
}