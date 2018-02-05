#!/usr/bin/python3

# Output a .png file with a rendering of the Mandlebrot fractal.
from PIL import Image

mask = Image.open('mask.png')
maskpixels = mask.load()

imageSize = (630,300)

def Mandlebrot(x, y, offset,maxIterations=8):
	"""Returns 0 if the point x + iy is in the Mandlebrot set 
	or the number of iterations of f(z) = z^2 + c before exiting bounds."""
	(c, z) = (complex(x, y), 0)
	for i in range(maxIterations):
		z = z**2 + c
		if abs(z.imag) > 10 or abs(z.real) > 10:
			return 0
		maskx = int((z.real+offset) * (629/3.5))
		masky = int((z.imag+1.20) * (299/2))

		if maskx > 0 and maskx < 630 and masky > 0 and masky < 300:
			#print(maskpixels[maskx, masky])
			if maskpixels[maskx, masky] != (0,0,0,255):
				return (maskpixels[maskx, masky][0]//(i+1),
					maskpixels[maskx, masky][1]//(i+1),
					maskpixels[maskx, masky][2]//(i+1))
	return 0

image = Image.new('RGB', imageSize) 
pixels = image.load()

for frame in range(200):

	for x in range(imageSize[0]):
		for y in range(imageSize[1]):
			# This involved some trial and error to center the graph.
			cordx = ( (1-frame/200)*(x - imageSize[0]/2) - 0.3*imageSize[1]) / (imageSize[1]*0.5)
			cordy = ( (1-frame/200)*(y - imageSize[1]/2)) / (imageSize[1]*0.5)
			# We'll just vary the red.
			pixels[x,y] = Mandlebrot(cordx, cordy,-2+frame/20)

	image.save('fractal{:03}.png'.format(frame))

image.close()
