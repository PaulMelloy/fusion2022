plot_preference_flow <-
  function(dat,
           division = "Ryan",
           polling_booth = "The Gap",
           PartyColour = NULL) {
    
    # Acknowledgment to https://stackoverflow.com/questions/64511383/showing-flows-for-ggalluvium
    
    dop_ryan <- dat %>%
      filter(DivisionNm == division,
             PPNm == polling_booth) %>%
      mutate(Location = unique(paste(DivisionNm, PPNm, sep = " - "))) %>%
      spread(CalculationType, CalculationValue) %>%
      select(
        count = CountNum,
        party = PartyAb,
        pref = `Preference Count`,
        trans = `Transfer Count`,
        Location = Location
      )
    
    Location <- dop_ryan %>%
      distinct(Location) %>%
      pull()
    
    # Helper function to
    make_rows <- function(x) {
      # Name of party which gets dropped in this period
      dropped <- filter(x, trans < 0) %>% pull(party)
      if (length(dropped) > 0) {
        x <- filter(x, trans >= 0)
        # Replacements are added two times. Once for the period where the party drops out,
        # and also for the previous period
        xdrop <-
          mutate(
            x,
            party = dropped,
            pref = trans,
            trans = 0,
            is_drop = FALSE
          )
        xdrop1 <-
          mutate(xdrop,
                 count = count - 1,
                 to = party,
                 is_drop = FALSE)
        # For the parties to keep or which receive transfered votes have to adjust the number of votes
        xkeep <- mutate(x, pref = pref - trans, trans = 0)
        bind_rows(xdrop1, xdrop, xkeep)
      } else {
        x
      }
    }
    
    if (is.null(PartyColour)) {
      PC_V <- c(
        "purple",
        "red",
        "black",
        "forestgreen",
        "blue",
        "orange",
        "yellow",
        "pink",
        "lightblue"
      )
    }else{
      PartyColour <- unique(PartyColour[PartyColour$PartyAb %in% dat$PartyAb,c("PartyAb","colour")])
      PC_V <- PartyColour$colour
      names(PC_V) <- PartyColour$PartyAb
      
    }
    
    
      
    
    dop_ryan %>%
      # First: Convert count to a numeric. Add a "to" variable for second
      # party preference or the party where votes are transferred to. This variable
      # will later on be mapped on the "fill" aes
      mutate(to = party, count = as.numeric(as.character(count))) %>%
      group_by(party) %>%
      # Add identifier of obs. to drop. Obs. to drop are obs. of parties which
      # drop out in the following count
      mutate(is_drop = lead(trans, default = 0) < 0) %>%
      ungroup() %>%
      # Split obs. to be dropped by secondary party preference, i.e. in count 0 the
      # obs for party "IND" is replaced by seven obs. reflecting the secondary preference
      # for one of the other seven parties
      split(.$count) %>%
      map(make_rows) %>%
      bind_rows() %>%
      # Now drop original obs.
      filter(!is_drop, pref > 0) %>%
      # Add a unique identifier
      group_by(count, party) %>%
      mutate(id = paste0(party, row_number())) %>%
      ungroup() %>%
      # To make the flow chart work we have make the dataset complete, i.e. add
      # "empty" obs for each type of voter and each count
      complete(count, id, fill = list(
        pref = 0,
        trans = 0,
        is_drop = FALSE
      )) %>%
      # Fill up party and "to" columns
      mutate(across(c(party, to), ~ if_else(is.na(.), str_extract(id, "[^\\d]+"), .))) %>%
      # Filling up the "to" column with last observed value for "to" if any
      group_by(id) %>%
      mutate(
        last_id = ifelse(any(party != to), last(which(party != to)), NA),
        to = if_else(count >= last_id &
                       !is.na(last_id), to[last_id], to)
      ) %>%
      ungroup() %>%
      ggplot(aes(
        x = count,
        alluvium = id,
        stratum = to,
        y = pref,
        fill = to
      )) +
      geom_flow(alpha = 0.2, decreasing = TRUE) +
      geom_stratum(alpha = 0.5, decreasing = TRUE) +
      scale_fill_manual(values = PC_V,
                        name = "Party",
                        ) +
      theme_minimal() +
      ggtitle(paste("Preference flows for", Location))
  }