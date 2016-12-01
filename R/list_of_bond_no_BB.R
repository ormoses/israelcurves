#Create a list of bonds that has no cashflow in Bloomberg
create_bonds_no_BB <- function(bond_data) {
  cpi_before_2000 <- bond_data %>% filter(MATURITY>="2008-01-01" & ISSUE_DT<"2000-01-01") %>% arrange(ISSUE_DT)
  cpi_before_2000
}

#Take the list from the last function and create a bond list
make_list_of_bonds <- function(cpi_before_2000) {
  thelist <- list()
  for (i in 1:(nrow(cpi_before_2000))) {
    aaa <- cpi_before_2000[i,]
    newbond <- create_vanilla_bond(aaa[1,"ISSUE_DT"],
                                   aaa[1,"FIRST_CPN_DT"],
                                   round(as.numeric((aaa[1,"MATURITY"]-aaa[1,"FIRST_CPN_DT"])/365),0),
                                   aaa[1,"CPN"]/100,
                                   name=aaa[1,"SERIES"],
                                   known_CPI=aaa[1,"BASE_CPI"])
    thelist[[i]] <- newbond
  }
  thelist
}

#Merge the bond list from the last function and the bond list from create_all_bonds
merge_bond_lists <- function(bond_list, bond_list_no_bb) {
  append(bond_list_no_bb, bond_list)
}
