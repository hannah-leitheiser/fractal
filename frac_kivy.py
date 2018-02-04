from kivy.app import App
from kivy.uix.widget import Widget
from kivy.graphics import Color, Ellipse, Rectangle

from kivy.uix.boxlayout import BoxLayout
from kivy.uix.floatlayout import FloatLayout
from kivy.uix.image import AsyncImage
from array import array
from kivy.graphics.texture import Texture

def mand(x, y):
	c = complex(x, y)
	z = 0
	for i in range(32):
		z = z**2 + c
		if z.imag ** 2 + z.real ** 2 > 4:
			return i
	return 0


class MyPaintWidget(Widget):


    def __init__(self, **kwargs):
        # make sure we aren't overriding any important functionality
        super(MyPaintWidget, self).__init__(**kwargs)
        self.offsetx = 0
        self.offsety = 0
        self.zoom=0.35
        self.bind(pos=self.update)
        self.bind(size=self.update)
        self.movable = True
        self.update()

        
    def update(self, *args):
        with self.canvas:
           self.texture = Texture.create(size=self.size)
           buf = []
           for y in range(self.height):
              for x in range(self.width):
                 cordx = (x - self.width/2 - self.offsetx) / (self.height*self.zoom)
                 cordy = (y - self.height/2 - self.offsety) / (self.height*self.zoom)
                 buf.append( int(mand(cordx, cordy)*8) )
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
                #with self.canvas:
                #    Rectangle(texture=self.texture, pos=( (self.width/2-touch.ox)-(self.width*0.5), (self.height/2-touch.oy)-(self.height*0.5)), size=(self.width*1.5,self.height*1.5))
                self.movable = False
                self.update()

    def on_touch_move(self, touch):
        if self.movable:
            with self.canvas:
                Rectangle(texture=self.texture, pos=( (touch.x-touch.ox), (touch.y-touch.oy)), size=self.size)
    def on_touch_up(self, touch):
        if self.movable:
            self.movable = False
            self.offsetx += (touch.x-touch.ox)
            self.offsety += (touch.y-touch.oy)
            self.update()
        	
        

class MyPaintApp(App):

    def build(self):
        return MyPaintWidget()


if __name__ == '__main__':
    MyPaintApp().run()



