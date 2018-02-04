#!/usr/bin/python3

# fractal_pil.py - by Hannah Leitheiser
#    Description: Generates the mandlebrot set in red
#    saved in .png image format.
#    run: python3 fractal_pil.py (in bash)
#   requires: PIL library

# Output a .png file with a rendering of the Mandlebrot fractal.
from PIL import Image

imageSize = (1000,800)

def Mandlebrot(x, y, maxIterations=31):
	"""Returns 0 if the point x + iy is in the Mandlebrot set 
	   or the number of iterations of f(z) = z^2 + c 
	   before exiting bounds."""
	(c, z) = (complex(x, y), 0)
	for i in range(maxIterations):
		z = z**2 + c
		if z.imag ** 2 + z.real ** 2 > 4:
			return i+1
	return 0

image = Image.new('RGB', imageSize) 
pixels = image.load()

for x in range(imageSize[0]):
	for y in range(imageSize[1]):
		# This involved some trial and error to center the graph.
		cordx = (x - imageSize[0]/2 - 0.3*imageSize[1]) 
		             / (imageSize[1]*0.5)
		cordy = (y - imageSize[1]/2) / (imageSize[1]*0.5)
		# We'll just vary the red.
		pixels[x,y] = (Mandlebrot(cordx, cordy)*8,0,0)

image.save('fractal.png')
image.close()
