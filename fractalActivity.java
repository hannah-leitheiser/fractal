/* FractalActivity.java, by Hannah Leitheiser (somewhat).  
   There are a lot of other files, but they were
   mostly a function of starting with a HelloWorld app.  
   Beyond a few text strings, this is all I changed.
   
   Draws the Mandlebrot set and allows you to zoom in by
   touching the canvas.
 */

/*
 * Copyright 2017 Google Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.google.android.instantapps.samples.hello.feature;

import android.content.Context;
import android.content.Intent;
import android.graphics.Bitmap;
import android.graphics.Canvas;
import android.graphics.Color;
import android.graphics.Paint;
import android.os.Bundle;
import android.support.v7.app.AppCompatActivity;
import android.view.MotionEvent;
import android.view.View;

import static android.graphics.Color.rgb;

/**
 * This Activity displays a simple hello world text and a button to open the GoodbyeActivity.
 * (and then I added stuff, so now it displays a fractal).
 */
public class FractalActivity extends AppCompatActivity {

    double zoom = 3;
    double offsetX = 0;
    double offsetY = 0;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(new MyView(this));


    }

    public class MyView extends View implements View.OnTouchListener
    {
        Paint paint = null;


        public MyView(Context context)
        {
            super(context);
            setOnTouchListener(this);
            paint = new Paint();
        }

        // Centers on where you touch, then zooms in; there's no going back.
        public boolean onTouch (View v, MotionEvent event) {
            zoom = zoom / 1.5;
            offsetX = offsetX - (event.getX() - ((getWidth()/2) + getLeft()));
            offsetY = offsetY - (event.getY() - ((getHeight()/2) + getTop()));
            offsetX = offsetX * 1.5;
            offsetY = offsetY * 1.5;
            v.invalidate();
            return false;
        }

        int mandlebrot(double x, double y) {
            double c_r = x;
            double c_i = y;
            double z_r = 0.0;
            double z_i = 0.0;
            for(int i = 1 ; i < 33 ; i++) {
                double z_r2 = (z_r * z_r) - (z_i * z_i) + c_r;
                z_i = (2 * z_r * z_i) + c_i;
                z_r = z_r2;
                if( ( (z_r * z_r) + (z_i * z_i)) > 4) {
                    return i;
                }
            }
            return 0;
        }


        @Override
        protected void onDraw(Canvas canvas)
        {
            super.onDraw(canvas);
            //canvas2 = canvas;
            int width = getWidth();
            int height = getHeight();
            
            /* Using a bitmap seems slightly faster than iterativly 
               setting pixels on the canvas.  Should be a shorter path to
               memory, I'd hope.  Sadly, on my Galaxy 5, this still runs
               pretty slow.  Too slow, I'd say, to be usable without coming
               up with some more clever way to render the fractal. */
            
            Bitmap bit;
            bit = Bitmap.createBitmap(width, height, Bitmap.Config.ARGB_8888);
            int[] intArray = new int[width*height];
            for(int x = 0 ; x < width ; x++)
                for(int y = 0 ; y < height ; y++) {
                    int iterations =
                            mandlebrot( zoom*(x-width/2 - offsetX) / height - 0.55,
                                    zoom * (y- height/2 - offsetY) / height);
                    intArray[(x + y*width)] = rgb(iterations*8, 0, 0);
                }
            bit.setPixels( intArray, 0, width,0,0,width,height);
            canvas.drawBitmap(bit,0, 0, null);
        }
    }

}


