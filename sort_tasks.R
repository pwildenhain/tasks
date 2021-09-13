suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(glue))
suppressPackageStartupMessages(library(lubridate))
suppressPackageStartupMessages(library(readr))

task_list <- read_csv("task_list.csv", show_col_types = FALSE)

sorted_task_list <-
    task_list |>
    mutate(
        today = Sys.Date(),
        priority_adjustment = case_when(
            priority == "low" ~ 1,
            priority == "medium" ~ 2,
            priority == "high" ~ 3,
            TRUE ~ 0
        ),
        entry_date_mdy = mdy(entry_date),
        date_adjustment = case_when(
            today %m-% months(2) < entry_date_mdy ~ 1,
            today %m-% months(4) < entry_date_mdy ~ 2,
            today %m-% months(6) < entry_date_mdy ~ 3,
            TRUE ~ 0
        ),
        status_adjustment = case_when(
            status == "backlog" ~ 1,
            status == "selected" ~ 10,
            status == "done" ~ 0
        ),
        composite_score =
            date_adjustment
            * priority_adjustment
            * status_adjustment
    ) |>
    arrange(desc(composite_score)) |>
    select(names(task_list))

top_3_tasks <- head(sorted_task_list$task_name, 3)

cat("\nThe top 3 tasks are:\n")
glue("    - {top_3_tasks}")

write_csv(sorted_task_list, "task_list.csv")
