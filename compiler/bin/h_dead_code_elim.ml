open G_tac_to_cfg

let eliminate_dead_code (graph : cfg) : cfg = 
  graph