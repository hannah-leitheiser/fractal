#!/usr/bin/python3

# fractal_kivy.py - by Hannah Leitheiser
#    Description: Generates the mandlebrot set in red
#       In a user window or app.  User can drag the fractal
#       to reposition and double-tap to zoom.
#    run: python3 fractal_kivy.py (in bash)
#   requires: Kivy

from kivy.app import App
from kivy.uix.widget import Widget
from kivy.graphics import Color, Ellipse, Rectangle
from kivy.uix.boxlayout import BoxLayout
from kivy.uix.floatlayout import FloatLayout
from kivy.uix.image import AsyncImage
from array import array
from kivy.graphics.texture import Texture


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

class FractalWidget(Widget):


	def __init__(self, **kwargs):
		# make sure we aren't overriding any important functionality
		super(FractalWidget, self).__init__(**kwargs)
		self.offsetx = 0
		self.offsety = 0
		self.zoom=0.35
		self.bind(pos=self.update)
		self.bind(size=self.update)
		self.movable = True
		self.update()
        
	def update(self, *args):
		with self.canvas:
			# We're going to use a texture, which...works, I guess.
			self.texture = Texture.create(size=self.size)
			buf = []
			for y in range(self.height):
				for x in range(self.width):
					cordx = ((x - self.width/2 - self.offsetx) /
						(self.height*self.zoom))
					cordy = ((y - self.height/2 - self.offsety) /
						(self.height*self.zoom))
					buf.append( int(Mandlebrot(cordx, cordy)*8) )
					buf.append(0);
					buf.append(0);
			arr= array('B', buf)
			self.texture.blit_buffer(arr, colorfmt='rgb', bufferfmt='ubyte')
			Rectangle(texture=self.texture, pos=self.pos, size=self.size)
			self.movable = True

	def on_touch_down(self, touch):
		if self.collide_point(*touch.pos):
			if touch.is_double_tap and self.movable:
				self.zoom=self.zoom*2.0
				self.offsetx -= (touch.ox - self.width/2)
				self.offsety -= (touch.oy - self.height/2)
				self.offsetx*=2.0
				self.offsety*=2.0
				self.movable = False
				self.update()

	def on_touch_move(self, touch):
		if self.movable:
			# redraw the texture on the canvas as the user
			# slides it around.
			with self.canvas:
				Rectangle(texture=self.texture, 
				      pos=( (touch.x-touch.ox), 
				      (touch.y-touch.oy)), size=self.size)
    
	def on_touch_up(self, touch):
		# redraw the fractal with new offsets
		if self.movable:
			self.movable = False
			self.offsetx += (touch.x-touch.ox)
			self.offsety += (touch.y-touch.oy)
			self.update()
        	
        

class FractalApp(App):

	def build(self):
		return FractalWidget()


if __name__ == '__main__':
	FractalApp().run()
