add_patients_to_subset(
    patients = d$visit_detail %>% dplyr::select(person_id, visit_occurrence_id, visit_detail_id),
    subset_id = %subset_id%,
    output = output, r = r, m = m
)
