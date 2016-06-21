import deltarpm

def export(x): 
  d = deltarpm.readDeltaRPM (x) 
  d1 = d['old_nevr'] 
  d2 = d['nevr'] 
  return (d1 + d2)