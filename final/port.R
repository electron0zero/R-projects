library(simmer)
library(simmer.plot)
library(ggplot2)

2+3

set.seed(42)

# create Simulation Envirenment
#env <- simmer()

ship_attrs <- function(){
    # get ship type as per 5:3:2=small:medium:big
    # determine ship type, 1 = small, 2= medium, 3 = big
    ship_type = sample(1:3,size=1, prob = c((5/10), (3/10), (2/10)))
    # randmly select berth to go
    # berth, 1 == berth1, 2 == berth2, 3 == berth3
    berth = "berth3"
    quan = 0
    unload_time = 0
    load_time = 0
    a = c("berth1", "berth2", "berth3")
    b = c("berth2", "berth3")
    
    if (ship_type == 1 ){
        berth = sample(a, size=1)
        quan = 1
        unload_time = rexp(1, 15)
        load_time = runif(1, 18, 30)
    }
    if (ship_type == 2 ){
        berth = sample(a, size=1)
        quan = 2
        unload_time = rexp(1, 30)
        load_time = runif(1, 26, 46)
    }
    if (ship_type == 3 ){
        berth = sample(b, size=1)
        quan = 4
        unload_time = rexp(1, 45)
        load_time = runif(1, 44, 68)
    }
    return(c(berth, quan, load_time, unload_time, ship_type))
}

# This chunk is reponsible for crash
ship <- trajectory() %>%
    log_("arrives at the port") %>%
    set_attribute("dat", ship_attrs()) %>%
    log_(attr["dat"]) %>%
    set_prioritization(attr["dat"][5]) %>%
    
    seize(attr["dat"][1], attr["dat"][2] ) %>%
    timeout(floor(attr["dat"][4])) %>%
    timeout(floor(attr["dat"][3])) %>%
    release(attr["dat"][1], attr["dat"][2]) %>%
    log_("leaves the port")

env %>%
    # add resources
    # berth1 resource is 2 // 1 medium ship == 2 small
    add_resource("berth1", 2) %>%
    # berth2 resource is 4 // 1 big ship == 2 medium == 4 small
    add_resource("berth2", 4) %>%
    # berth3 resource is 4 // 1 big ship == 2 medium == 4 small
    add_resource("berth3", 4) %>%
    # add ship generator with exponetial distribution
    add_generator("ship", ship, function() rexp(1, 1/26) ) %>%
    # start the simulation
    run(SIM_TIME)

plot(env, what = "resources", metric = "usage", c("berth1", "berth2", "berth3"))
plot(env, what = "resources", metric = "utilization", c("berth1", "berth2", "berth3"))
plot(env, what = "arrivals", metric = "activity_time")
plot(env, what = "arrivals", metric = "waiting_time")
plot(env, what = "arrivals", metric = "flow_time")

