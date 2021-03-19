import os

os.system("wget https://www.oasis-brains.org/files/oasis_cross-sectional.csv")

for i in range(1, 13):
  file = "oasis_cross-sectional_disc{0}.tar.gz".format(i)
  os.system( "wget https://download.nrg.wustl.edu/data/{0}".format(file) )
  os.system( "tar -xvf {0}".format(file) )

  folder = "disc{0}".format(i)
  os.system( "mv {0}/* .".format(folder) )
  os.system( "rm -rf {0}".format(folder) )
  os.system( "rm -rf {0}".format(file) )
