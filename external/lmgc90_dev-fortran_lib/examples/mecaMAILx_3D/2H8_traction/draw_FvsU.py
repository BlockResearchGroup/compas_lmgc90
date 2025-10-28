import numpy as np
import matplotlib.pyplot as plt
a=np.genfromtxt("fn_un.txt")
#
plt.xlabel("gap (m)")
#
plt.ylabel("Fn (N)")

#
#draw the plot with a blue line 'b' (is default)
#
# other drawing styles -->
#
# 'r' red line, 'g' green line, 'y' yellow line
#
# 'ro' red dots as markers, 'r.' smaller red dots, 'r+' red pluses
#
# 'r--' red dashed line, 'g^' green triangles, 'bs' blue squares
#
# 'rp' red pentagons, 'r1', 'r2', 'r3', 'r4' well, check out the markers
#
#
plt.plot(a[:,0],a[:,1],'r.')
#
plt.show()
