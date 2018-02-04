/* 

frac_c.c
 Hannah Leitheiser

Produces a fractal saved as fractal_c.png.

compile: gcc frac_c.c -o frac_c -lpng -lm
dependencies: libpng and zlib
Ubuntu library installation: 
	sudo apt-get install libpng-dev
	sudo apt-get install zlib1g-dev

*/

#include <stdio.h>
#include <stdint.h>
#include <complex.h>
#include <malloc.h>
#include <png.h>

/* return 0 for points in the Mandlebrot set, 
	iteration count for those known to escape */
int mandlebrot(double complex c, int maxIterations) {
	double complex z = 0;
	for(unsigned int i = 0 ; i < maxIterations ; i++) {
		z = z * z + c;
		if(creal(z) * creal(z) + cimag(z) * cimag(z) > 4)
			return i+1;
		}
	return 0;
	}

int main(int argc, char *argv[]) {

	// Specify an output image size
	int width = 1000;
	int height = 800;

	/* Image handling is mostly copypaste from 
	   http://www.labbookpages.co.uk/software/imgProc/files/libPNG/makePNG.c,
	   just edited down a bit for less error control. */
	png_infop info_ptr = NULL;
	png_bytep row = NULL;
	FILE* fp = fopen("fractal_c.png", "wb");
	png_structp png_ptr = png_create_write_struct(PNG_LIBPNG_VER_STRING, 
			NULL, NULL, NULL);
	info_ptr = png_create_info_struct(png_ptr);
	png_init_io(png_ptr, fp);

	// Write header (8 bit colour depth)
	png_set_IHDR(png_ptr, info_ptr, width, height,
			8, PNG_COLOR_TYPE_RGB, PNG_INTERLACE_NONE,
			PNG_COMPRESSION_TYPE_BASE, PNG_FILTER_TYPE_BASE);

	// Set title
	png_text title_text;
		title_text.compression = PNG_TEXT_COMPRESSION_NONE;
		title_text.key = "Fractal";
		title_text.text = "Fractal";
		png_set_text(png_ptr, info_ptr, &title_text, 1);

	png_write_info(png_ptr, info_ptr);

	// Allocate memory for one row (3 bytes per pixel - RGB)
	row = (png_bytep) malloc(3 * width * sizeof(png_byte));

	// Write image data
	float scale = 2.0 / height;
	for (int y=0 ; y<height ; y++) {
		for (int x=0 ; x<width ; x++) {
			row[x*3]=(uint8_t)mandlebrot( (double)x * scale - 1.85 + 
				(((double)y * scale - 1)*I), 32 ) * 8;
			}
		png_write_row(png_ptr, row);
		}

	// End write
	png_write_end(png_ptr, NULL);

	fclose(fp);
	png_free_data(png_ptr, info_ptr, PNG_FREE_ALL, -1);
	png_destroy_write_struct(&png_ptr, (png_infopp)NULL);
	free(row);

	return 0;
	}
